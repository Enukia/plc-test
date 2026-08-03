(* lib/optimize.ml
 *
 * 编译器优化通道，作用于 lib/ir.ml 的 TAC 中间表示：
 *
 *   1. 常量折叠（constant folding）
 *      将编译期可知的运算（含基本块内的常量传播、代数化简、
 *      常量条件分支）折叠为直接赋值，减少运行时计算。
 *
 *   2. 尾递归优化（tail recursion optimization）
 *      将 "参数求值 + call 自身 + return 结果" 改写为
 *      "参数槽重赋值 + 跳回函数入口"，把递归调用变成循环，
 *      栈深度保持 O(1)。
 *
 *   3. 死代码消除（dead code elimination）
 *      删除不可达基本块、Goto/Return 之后的不可达指令，
 *      并通过活跃变量分析删除对临时变量/局部变量/参数的
 *      死存储（保留所有调用、分支与全局变量写回）。
 *)

open Ir

module S = Set.Make(String)

(* ---------- 通用工具 ---------- *)

(* 操作数键：Temp -> "T<n>"，Var -> "V<name>"；Const 无键 *)
let op_key = function
  | Const _ -> None
  | Temp t -> Some ("T" ^ string_of_int t)
  | Var v -> Some ("V" ^ v)

(* 遍历一条 TAC 指令中的所有操作数 *)
let iter_operands f = function
  | Assign (x, y) -> f x; f y
  | AssignBinOp (x, _, a, b) -> f x; f a; f b
  | AssignUnOp (x, _, a) -> f x; f a
  | IfGoto (a, _) | IfNotGoto (a, _) -> f a
  | Param a -> f a
  | Call (x, _, _) -> f x
  | Return (Some a) -> f a
  | Goto _ | Label _ | Return None -> ()

(* 统计扁平指令序列中出现的最大 Temp 编号 + 1，用于收缩栈帧 *)
let count_temps instrs =
  let m = ref (-1) in
  List.iter (iter_operands (function
    | Temp t -> if t > !m then m := t
    | _ -> ())) instrs;
  !m + 1

(* 把函数展开为带 Label 的扁平指令序列（第一个 Label 即入口标签） *)
let flatten_func (f: ir_func) : tac list =
  List.concat_map (fun b -> Label b.label :: b.instrs) (f.entry :: f.blocks)

(* 重新切分基本块；收缩临时变量编号；剔除不再被引用的局部变量 *)
let rebuild_func (f: ir_func) (instrs: tac list) : ir_func =
  let blocks = match instrs with [] -> [] | _ -> split_blocks instrs in
  let entry, rest =
    match blocks with
    | e :: r -> e, r
    | [] -> { label = f.entry.label; instrs = [] }, []
  in
  let used = ref S.empty in
  List.iter (iter_operands (function
    | Var v -> used := S.add v !used
    | _ -> ())) instrs;
  let locals =
    List.filter (fun l -> List.mem l f.params || S.mem l !used) f.locals
  in
  { f with entry; blocks = rest; temps = count_temps instrs; locals }

(* ---------- 常量折叠 ---------- *)

(* 将结果折叠到 32 位有符号整数，与 RV32I 运行时行为一致 *)
let wrap32 n =
  let m = n land 0xFFFFFFFF in
  if m >= 0x80000000 then m - 0x100000000 else m

let fold_binop op a b =
  match op with
  | Ast.Div | Ast.Mod when b = 0 -> None
  | _ -> Some (wrap32 (eval_binop op a b))

let fold_unop op a =
  match op with
  | Ast.Pos -> Some (wrap32 a)
  | Ast.Neg -> Some (wrap32 (-a))
  | Ast.Not -> Some (if a = 0 then 1 else 0)

(* 常量折叠 + 基本块内常量传播 + 代数化简。
 * 每个 Label 处清空已知值（块间不做跨路径传播，保证安全）。 *)
let const_fold (instrs: tac list) : tac list =
  let env = ref [] in
  let find k = List.assoc_opt k !env in
  let set k v = env := (k, v) :: List.remove_assoc k !env in
  let drop k = env := List.remove_assoc k !env in
  let clear () = env := [] in
  let subst o =
    match op_key o with
    | Some k -> (match find k with Some n -> Const n | None -> o)
    | None -> o
  in
  let record_def d v =
    match op_key d with Some k -> set k v | None -> ()
  in
  let invalidate d =
    match op_key d with Some k -> drop k | None -> ()
  in
  let out = ref [] in
  let emit i = out := i :: !out in
  List.iter (fun i ->
    match i with
    | Assign (d, s) when d = s -> ()  (* 自赋值无意义 *)
    | Assign (d, s) ->
        let s' = subst s in
        (match s' with
         | Const n -> emit (Assign (d, Const n)); record_def d n
         | _ -> emit (Assign (d, s')); invalidate d)
    | AssignBinOp (d, op, a, b) ->
        let a' = subst a and b' = subst b in
        (match a', b' with
         | Const x, Const y ->
             (match fold_binop op x y with
              | Some v -> emit (Assign (d, Const v)); record_def d v
              | None -> emit (AssignBinOp (d, op, a', b')); invalidate d)
         | _ ->
             let simplified =
               match op, a', b' with
               | Ast.Add, x, Const 0 | Ast.Add, Const 0, x -> Some x
               | Ast.Sub, x, Const 0 -> Some x
               | Ast.Sub, x, y when x = y -> Some (Const 0)
               | Ast.Mul, Const 0, _ | Ast.Mul, _, Const 0 -> Some (Const 0)
               | Ast.Mul, Const 1, x | Ast.Mul, x, Const 1 -> Some x
               | Ast.Div, x, Const 1 -> Some x
               | Ast.Mod, _, Const 1 -> Some (Const 0)
               | Ast.Eq, x, y when x = y -> Some (Const 1)
               | Ast.Ne, x, y when x = y -> Some (Const 0)
               | Ast.Lt, x, y when x = y -> Some (Const 0)
               | Ast.Gt, x, y when x = y -> Some (Const 0)
               | Ast.Le, x, y when x = y -> Some (Const 1)
               | Ast.Ge, x, y when x = y -> Some (Const 1)
               | _ -> None
             in
             (match simplified with
              | Some s -> emit (Assign (d, s)); invalidate d
              | None -> emit (AssignBinOp (d, op, a', b')); invalidate d))
    | AssignUnOp (d, op, a) ->
        let a' = subst a in
        (match a' with
         | Const n ->
             (match fold_unop op n with
              | Some v -> emit (Assign (d, Const v)); record_def d v
              | None -> emit (AssignUnOp (d, op, a')); invalidate d)
         | _ -> emit (AssignUnOp (d, op, a')); invalidate d)
    | IfGoto (a, l) ->
        let a' = subst a in
        (match a' with
         | Const n -> if n <> 0 then emit (Goto l)
         | _ -> emit (IfGoto (a', l)))
    | IfNotGoto (a, l) ->
        let a' = subst a in
        (match a' with
         | Const n -> if n = 0 then emit (Goto l)
         | _ -> emit (IfNotGoto (a', l)))
    | Param a -> emit (Param (subst a))
    | Call (d, fname, n) ->
        emit (Call (d, fname, n));
        (* 函数调用可能修改全局变量，作废所有 Var 的已知值 *)
        env := List.filter (fun (k, _) -> String.length k = 0 || k.[0] <> 'V') !env;
        invalidate d
    | Return (Some a) -> emit (Return (Some (subst a)))
    | Goto l -> emit (Goto l)
    | Label l -> emit (Label l); clear ()
    | Return None -> emit (Return None)
  ) instrs;
  List.rev !out

(* ---------- 尾递归优化 ---------- *)

(* 将 "Param*; Call f; Return t"（f 为当前函数）改写为参数重赋值 + 跳回入口。
 *
 * 注意：IR 流序中的 Params 与 codegen 实际装载顺序相反
 * （codegen 用 prepend 累积，再按 a0..a7 顺序消费），因此
 * 参数映射需要把流序 Params 反转后对应到形参表。
 *)
let tail_recursion (f: ir_func) (instrs: tac list) : tac list =
  let fname = f.fname in
  let params = f.params in
  let nparams = List.length params in
  let entry_label = f.entry.label in
  let is_param o = match o with Var v -> List.mem v params | _ -> false in
  let tmp = ref (count_temps instrs) in
  let fresh () = let t = !tmp in incr tmp; Temp t in
  let rec loop acc = function
    | [] -> List.rev acc
    | (Param o :: rest) as instrs when nparams > 0 ->
        let rec collect k acc_ps = function
          | (Param p) :: r when k > 0 -> collect (k - 1) (p :: acc_ps) r
          | r -> List.rev acc_ps, r
        in
        let ps, after = collect nparams [] instrs in
        (match after with
         | Call (d, callee, n) :: Return (Some d') :: rest'
           when callee = fname && n = nparams
             && List.length ps = nparams && d = d' ->
             (* 参数槽可能被参数表达式读取，先复制到临时变量再写回 *)
             let args = List.rev ps in
             let copies =
               List.map (fun a -> if is_param a then Some (fresh ()) else None) args
             in
             let pre =
               List.concat
                 (List.map2 (fun a c ->
                    match c with Some t -> [Assign (t, a)] | None -> [])
                    args copies)
             in
             let assigns =
               List.map2 (fun p (c, a) ->
                 Assign (Var p, match c with Some t -> t | None -> a))
                 params (List.combine copies args)
             in
             loop (List.rev_append (pre @ assigns @ [Goto entry_label]) acc) rest'
         | _ -> loop (Param o :: acc) rest)
    | Call (d, callee, 0) :: Return (Some d') :: rest
      when callee = fname && nparams = 0 && d = d' ->
        loop (Goto entry_label :: acc) rest
    | i :: rest -> loop (i :: acc) rest
  in
  loop [] instrs

(* ---------- 死代码消除 ---------- *)

(* 确保入口标签是全局唯一、可被汇编打印的标签
   （避免多个函数共用 fallback 名 "entry" 造成重复标签） *)
let normalize_entry (f: ir_func) (instrs: tac list) : ir_func * tac list =
  if f.entry.label <> "entry" then f, instrs
  else
    let l = fresh_label () in
    let instrs =
      match instrs with
      | Label _ :: rest -> Label l :: rest
      | _ -> Label l :: instrs
    in
    { f with entry = { f.entry with label = l } }, instrs

(* 截断基本块：Goto/Return 之后的指令不可达，直接删除 *)
let truncate_block (b: basic_block) : basic_block =
  let rec go acc = function
    | [] -> List.rev acc
    | ((Goto _ | Return _) as i) :: _ -> List.rev (i :: acc)
    | i :: rest -> go (i :: acc) rest
  in
  { b with instrs = go [] b.instrs }

let truncate_func (f: ir_func) : ir_func =
  { f with
    entry = truncate_block f.entry;
    blocks = List.map truncate_block f.blocks }

(* 每个基本块的后继块下标列表 *)
let block_succs (all: basic_block list) : int list list =
  let n = List.length all in
  let idx = Hashtbl.create 16 in
  List.iteri (fun i b -> Hashtbl.replace idx b.label i) all;
  let target l =
    match Hashtbl.find_opt idx l with Some j -> [j] | None -> []
  in
  List.mapi (fun i (b: basic_block) ->
    let next = if i + 1 < n then [i + 1] else [] in
    (* 块内所有分支/跳转的目标（Goto、IfGoto、IfNotGoto）都是后继 *)
    let targets = ref [] in
    List.iter (function
      | Goto l | IfGoto (_, l) | IfNotGoto (_, l) -> targets := l :: !targets
      | _ -> ()) b.instrs;
    let ts =
      List.concat_map target (List.rev !targets)
    in
    (* 仅当块以无条件跳转或返回结束时才没有顺延后继 *)
    let terminated =
      match List.rev b.instrs with
      | (Goto _ | Return _) :: _ -> true
      | _ -> false
    in
    if terminated then ts else ts @ next) all

(* 删除从入口不可达的基本块 *)
let remove_unreachable_blocks (f: ir_func) : ir_func =
  let all = f.entry :: f.blocks in
  let succs = Array.of_list (block_succs all) in
  let n = List.length all in
  let reachable = Array.make n false in
  let queue = Queue.create () in
  reachable.(0) <- true;
  Queue.push 0 queue;
  while not (Queue.is_empty queue) do
    let i = Queue.pop queue in
    List.iter (fun j ->
      if j >= 0 && j < n && not reachable.(j) then (
        reachable.(j) <- true;
        Queue.push j queue))
      succs.(i)
  done;
  let kept =
    List.filteri (fun i _ -> reachable.(i)) all
  in
  match kept with
  | e :: r -> { f with entry = e; blocks = r }
  | [] -> { f with entry = { label = f.entry.label; instrs = [] }; blocks = [] }

(* 活跃变量分析驱动的死存储消除。
 * 只删除对临时变量、局部变量、参数的死存储；
 * 调用、分支、返回以及全局变量写回一律保留。 *)
let dce (f: ir_func) : ir_func =
  let all = f.entry :: f.blocks in
  let succs = Array.of_list (block_succs all) in
  let n = List.length all in
  let killable_names =
    List.fold_left (fun s v -> S.add v s) S.empty (f.params @ f.locals)
  in
  let killable = function
    | Temp _ -> true
    | Var v -> S.mem v killable_names
    | Const _ -> false
  in
  let add_use o s = match op_key o with Some k -> S.add k s | None -> s in
  let kill_def o s =
    match op_key o with
    | Some k when killable o -> S.remove k s
    | _ -> s
  in
  let uses_of = function
    | Assign (_, y) -> [y]
    | AssignBinOp (_, _, a, b) -> [a; b]
    | AssignUnOp (_, _, a) -> [a]
    | IfGoto (a, _) | IfNotGoto (a, _) -> [a]
    | Param a -> [a]
    | Return (Some a) -> [a]
    | _ -> []
  in
  let def_of = function
    | Assign (x, _) | AssignBinOp (x, _, _, _) | AssignUnOp (x, _, _)
    | Call (x, _, _) -> Some x
    | _ -> None
  in
  let use_arr =
    Array.of_list
      (List.map (fun (b: basic_block) ->
         List.fold_left (fun s i ->
           List.fold_left (fun s o -> add_use o s) s (uses_of i))
           S.empty b.instrs) all)
  in
  let kill_arr =
    Array.of_list
      (List.map (fun (b: basic_block) ->
         List.fold_left (fun s i ->
           match def_of i with
           | Some d -> kill_def d s
           | None -> s) S.empty b.instrs) all)
  in
  let live_in = Array.make n S.empty in
  let live_out = Array.make n S.empty in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = n - 1 downto 0 do
      let li = S.union use_arr.(i) (S.diff live_out.(i) kill_arr.(i)) in
      if not (S.equal li live_in.(i)) then (live_in.(i) <- li; changed := true);
      let lo =
        List.fold_left (fun s j -> S.union s live_in.(j)) S.empty succs.(i)
      in
      if not (S.equal lo live_out.(i)) then (live_out.(i) <- lo; changed := true)
    done
  done;
  let rewrite (b: basic_block) i =
    let live = ref live_out.(i) in
    let instrs =
      List.fold_right (fun inst acc ->
        let keep, live' =
          match inst with
          | Assign (d, s) when d = s -> false, !live
          | Assign (d, s) ->
              let dead =
                match op_key d with
                | Some k when killable d -> not (S.mem k !live)
                | _ -> false
              in
              if dead then false, !live
              else true, kill_def d (add_use s !live)
          | AssignBinOp (d, _, a, b) ->
              let dead =
                match op_key d with
                | Some k when killable d -> not (S.mem k !live)
                | _ -> false
              in
              if dead then false, !live
              else true, kill_def d (add_use b (add_use a !live))
          | AssignUnOp (d, _, a) ->
              let dead =
                match op_key d with
                | Some k when killable d -> not (S.mem k !live)
                | _ -> false
              in
              if dead then false, !live
              else true, kill_def d (add_use a !live)
          | IfGoto (a, _) | IfNotGoto (a, _) -> true, add_use a !live
          | Param a -> true, add_use a !live
          | Return (Some a) -> true, add_use a !live
          | Call (d, _, _) -> true, kill_def d !live
          | Goto _ | Label _ | Return None -> true, !live
        in
        live := live';
        if keep then inst :: acc else acc)
        b.instrs []
    in
    { b with instrs }
  in
  { f with
    entry = rewrite f.entry 0;
    blocks = List.mapi (fun i b -> rewrite b (i + 1)) f.blocks }

(* 消除跳转到下一个基本块的冗余 Goto *)
let cleanup (f: ir_func) : ir_func =
  let all = f.entry :: f.blocks in
  let rec go acc = function
    | [] -> List.rev acc
    | [b] -> List.rev (b :: acc)
    | (b1: basic_block) :: (((b2: basic_block) :: _) as rest) ->
        let b1' =
          match List.rev b1.instrs with
          | (Goto l) :: tl when l = b2.label -> { b1 with instrs = List.rev tl }
          | _ -> b1
        in
        go (b1' :: acc) rest
  in
  match go [] all with
  | e :: r -> { f with entry = e; blocks = r }
  | [] -> f

(* 合并空基本块：空块（非入口）的所有跳转边重定向到其后继，
   然后删除空块，使输出更紧凑 *)
let merge_empty_blocks (f: ir_func) : ir_func =
  let all = f.entry :: f.blocks in
  let n = List.length all in
  let effective = Array.make n "" in
  for i = n - 1 downto 0 do
    let b = List.nth all i in
    if b.instrs = [] && i + 1 < n then effective.(i) <- effective.(i + 1)
    else effective.(i) <- b.label
  done;
  let rename = Hashtbl.create 16 in
  List.iteri (fun i (b: basic_block) ->
    if b.instrs = [] && effective.(i) <> b.label then
      Hashtbl.replace rename b.label effective.(i)) all;
  let rename_l l =
    match Hashtbl.find_opt rename l with Some l' -> l' | None -> l
  in
  let rewrite_block (b: basic_block) : basic_block =
    let instrs =
      List.map (function
        | Goto l -> Goto (rename_l l)
        | IfGoto (o, l) -> IfGoto (o, rename_l l)
        | IfNotGoto (o, l) -> IfNotGoto (o, rename_l l)
        | i -> i) b.instrs
    in
    { b with instrs }
  in
  let kept =
    List.filteri (fun i (b: basic_block) -> i = 0 || b.instrs <> []) all
    |> List.map rewrite_block
  in
  match kept with
  | e :: r -> { f with entry = e; blocks = r }
  | [] -> f

(* 优化收尾：按最终指令重新统计临时变量与局部变量，收缩栈帧 *)
let shrink_func (f: ir_func) : ir_func =
  let instrs = flatten_func f in
  let used = ref S.empty in
  List.iter (iter_operands (function
    | Var v -> used := S.add v !used
    | _ -> ())) instrs;
  let locals =
    List.filter (fun l -> List.mem l f.params || S.mem l !used) f.locals
  in
  { f with temps = count_temps instrs; locals }

(* ---------- 主流程 ---------- *)

let optimize_func (f: ir_func) : ir_func =
  let instrs = flatten_func f in
  let f, instrs = normalize_entry f instrs in
  let instrs = const_fold instrs in
  let instrs = tail_recursion f instrs in
  let instrs = const_fold instrs in
  let f = rebuild_func f instrs in
  let f = truncate_func f in
  let f = remove_unreachable_blocks f in
  let f = dce f in
  let f = merge_empty_blocks f in
  let f = cleanup f in
  let f = shrink_func f in
  f

(* 对整个 IR 程序做优化：逐个函数处理，全局变量保持不变 *)
let optimize_program (prog: ir_program) : ir_program =
  List.map (function
    | GlobalVar _ as g -> g
    | Function f -> Function (optimize_func f)
  ) prog
