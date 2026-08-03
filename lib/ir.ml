(* lib/ir.ml
 * 
 * 实现了一个编译器中间表示（IR）生成器。
 * 将抽象语法树（AST）转换为三地址码（TAC）形式的中级中间表示，
 * 并支持基本块划分、短路求值、循环控制流（break/continue）等功能。
 * 最后提供了一组打印函数，用于输出 IR 的可读形式。
 *)

type operand =
  | Const of int
  | Var of string
  | Temp of int

type tac =
  | Assign of operand * operand
  | AssignBinOp of operand * Ast.binop * operand * operand
  | AssignUnOp of operand * Ast.unop * operand
  | Goto of string
  | IfGoto of operand * string
  | IfNotGoto of operand * string
  | Label of string
  | Param of operand
  | Call of operand * string * int
  | Return of operand option

type basic_block = {
  label: string;
  instrs: tac list;
}

type ir_func = {
  fname: string;
  params: string list;
  locals: string list;
  temps: int;
  entry: basic_block;
  blocks: basic_block list;
}

type ir_program_item =
  | GlobalVar of string * int option
  | Function of ir_func

type ir_program = ir_program_item list

type gen = {
  mutable temp_cnt: int;
  mutable instrs: tac list;
  mutable locals: string list;
  mutable unique_cnt: int;
  mutable scopes: (string * string) list list;
}

(* 创建一个新的 IR 生成器状态，初始化临时变量和标签计数器 *)
let new_gen () = { temp_cnt = 0; instrs = []; locals = []; unique_cnt = 0; scopes = [] }

(* 生成一个新的临时变量（Temp n），并递增计数器 *)
let fresh_temp g =
  let t = g.temp_cnt in
  g.temp_cnt <- t + 1;
  Temp t

(* 生成一个新的标签（如 "L0"），并递增计数器 *)
 let global_label_cnt = ref 0

 let fresh_label () =
   let l = !global_label_cnt in
   global_label_cnt := l + 1;
   "L" ^ string_of_int l

(* 向当前生成器的指令列表中添加一条 TAC 指令 *)
let emit g i = g.instrs <- i :: g.instrs

(* 将变量名加入当前函数的局部变量列表（去重） *)
let add_local g name = if not (List.mem name g.locals) then g.locals <- name :: g.locals


(* 作用域感知的局部变量唯一命名：
   同名变量在不同作用域中会生成不同的 IR 名字（如 x$0、x$1），
   避免被遮蔽的变量在代码生成时塌缩到同一个栈槽。 *)

(* 进入新的局部作用域（对应一个花括号块或函数参数区） *)
let enter_scope g = g.scopes <- [] :: g.scopes

(* 退出当前作用域 *)
let exit_scope g =
  match g.scopes with
  | _ :: rest -> g.scopes <- rest
  | [] -> failwith "scope underflow"

(* 从内向外查找名字的局部重命名 *)
let rec find_binding name = function
  | [] -> None
  | scope :: rest ->
      (match List.assoc_opt name scope with
       | Some u -> Some u
       | None -> find_binding name rest)

(* 解析变量名：局部变量返回其唯一名；未找到则视为全局符号，保持原名 *)
let resolve_name g name =
  match find_binding name g.scopes with
  | Some u -> u
  | None -> name

(* 为当前作用域的新声明分配唯一名（$ 不会出现在用户标识符中，避免冲突） *)
let bind g name =
  let u = Printf.sprintf "%s$%d" name g.unique_cnt in
  g.unique_cnt <- g.unique_cnt + 1;
  (match g.scopes with
   | scope :: rest -> g.scopes <- ((name, u) :: scope) :: rest
   | [] -> g.scopes <- [[(name, u)]]);
  add_local g u;
  u


(* 表达式生成（支持短路计算） *)

(* 将 AST 表达式转换为 TAC 操作数，生成对应的中间代码 *)
let rec gen_expr g (e: Ast.expr) : operand =
  match e with
  | Ast.EInt n -> Const n
  | Ast.EId name -> Var (resolve_name g name)
  | Ast.EBinOp (op, e1, e2) ->
      (match op with
       | Ast.And -> gen_short_circuit g e1 e2 true
       | Ast.Or  -> gen_short_circuit g e1 e2 false
       | _ -> gen_normal_binop g op e1 e2)
  | Ast.EUnOp (op, e) ->
      let o = gen_expr g e in
      let result = fresh_temp g in
      emit g (AssignUnOp (result, op, o));
      result
  | Ast.ECall (fname, args) ->
      let arg_ops = List.rev (List.map (gen_expr g) args) in
      List.iter (fun a -> emit g (Param a)) arg_ops;
      let result = fresh_temp g in
      emit g (Call (result, fname, List.length args));
      result

(* 生成普通二元运算的 TAC 代码（非短路运算） *)
and gen_normal_binop g op e1 e2 =
  let o1 = gen_expr g e1 in
  let o2 = gen_expr g e2 in
  let result = fresh_temp g in
  emit g (AssignBinOp (result, op, o1, o2));
  result

(* 生成逻辑与/或的短路求值 TAC 代码 *)
and gen_short_circuit g e1 e2 is_and =
  let result = fresh_temp g in
  let short_l = fresh_label () in
  let end_l = fresh_label () in

  let o1 = gen_expr g e1 in
  emit g (Assign (result, o1));

  if is_and then
    emit g (IfNotGoto (result, short_l))
  else
    emit g (IfGoto (result, short_l));

  let o2 = gen_expr g e2 in
  emit g (Assign (result, o2));
  emit g (Goto end_l);

  emit g (Label short_l);
  let short_val = if is_and then Const 0 else Const 1 in
  emit g (Assign (result, short_val));

  emit g (Label end_l);
  result


(* 语句生成 *)

type loop_labels = {
  break_l: string;
  continue_l: string;
}

(* 将 AST 语句转换为 TAC 代码，支持 break/continue 和循环结构 *)
let rec gen_stmt g (loop: loop_labels option) (s: Ast.stmt) : unit =
  match s with
  | Ast.SBlock stmts ->
      enter_scope g;
      List.iter (gen_stmt g loop) stmts;
      exit_scope g
  | Ast.SEmpty -> ()
  | Ast.SExpr e -> 
      let _ = gen_expr g e in ()
  | Ast.SDecl (Ast.VarDecl (name, init)) ->
      let t = gen_expr g init in
      let u = bind g name in
      emit g (Assign (Var u, t))
  | Ast.SDecl (Ast.ConstDecl (name, init)) ->
      let t = gen_expr g init in
      let u = bind g name in
      emit g (Assign (Var u, t))
  | Ast.SAssign (name, e) ->
      let t = gen_expr g e in
      emit g (Assign (Var (resolve_name g name), t))
  | Ast.SIf (cond, then_s, else_s) ->
      let else_l = fresh_label () in
      let end_l = fresh_label () in
      let cond_t = gen_expr g cond in
      emit g (IfNotGoto (cond_t, else_l));
      gen_stmt g loop then_s;
      emit g (Goto end_l);
      emit g (Label else_l);
      Option.iter (gen_stmt g loop) else_s;
      emit g (Label end_l)
  | Ast.SWhile (cond, body) ->
      let cond_l = fresh_label () in
      let body_l = fresh_label () in
      let end_l = fresh_label () in
      let new_loop = { break_l = end_l; continue_l = cond_l } in

      
      emit g (Label cond_l);
      let cond_t = gen_expr g cond in
      emit g (IfNotGoto (cond_t, end_l));

      emit g (Label body_l);
      gen_stmt g (Some new_loop) body;
      emit g (Goto cond_l);

      emit g (Label end_l)
  | Ast.SBreak ->
      (match loop with
       | Some l -> emit g (Goto l.break_l)
       | None -> failwith "Break outside loop")
  | Ast.SContinue ->
      (match loop with
       | Some l -> emit g (Goto l.continue_l)
       | None -> failwith "Continue outside loop")
  | Ast.SReturn (Some e) ->
      let t = gen_expr g e in
      emit g (Return (Some t))
  | Ast.SReturn None ->
      emit g (Return None)


(* 基本块划分 *)

(* 将 TAC 指令列表按 Label 划分为基本块列表 *)
let split_blocks (instrs: tac list) : basic_block list =
  let rec split current_label current acc = function
    | [] ->
        let block = { label = current_label; instrs = List.rev current } in
        List.rev (block :: acc)
    | (Label l) :: rest ->
        let block = { label = current_label; instrs = List.rev current } in
        split l [] (block :: acc) rest
    | i :: rest ->
        split current_label (i :: current) acc rest
  in
  match instrs with
  | (Label l) :: rest -> split l [] [] rest
  | _ -> split "entry" [] [] instrs


(* 函数生成 *)

(* 将 AST 函数定义转换为完整的 IR 函数（含基本块划分） *)
let gen_func (f: Ast.func_def) : ir_func =
  let g = new_gen () in
  enter_scope g;
  let param_names = List.map (fun p -> bind g p) f.Ast.params in
  gen_stmt g None f.Ast.body;
  exit_scope g;

  (* FIX: 确保 void 函数有 return，避免空指令序列 *)
  (match f.Ast.retty with
   | "void" -> 
       if g.instrs = [] || 
          (match List.hd g.instrs with Return _ -> false | _ -> true) then
         emit g (Return None)
   | _ -> ());

  let all_instrs = List.rev g.instrs in
  let blocks = split_blocks all_instrs in

  (* FIX: 处理空块情况 *)
  match blocks with
  | [] ->
      { fname = f.Ast.name;
        params = param_names;
        locals = g.locals;
        temps = g.temp_cnt;
        entry = { label = "entry"; instrs = [] };
        blocks = [] }
  | entry :: rest ->
      { fname = f.Ast.name;
        params = param_names;
        locals = g.locals;
        temps = g.temp_cnt;
        entry;
        blocks = rest }


(* 程序生成 *)

(* 编译期常量求值：用于计算全局变量/常量的静态初值（含常量链、算术、比较等） *)
let eval_binop = Ast.(function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Mod -> ( mod )
  | Eq -> (fun a b -> if a = b then 1 else 0)
  | Ne -> (fun a b -> if a <> b then 1 else 0)
  | Lt -> (fun a b -> if a < b then 1 else 0)
  | Gt -> (fun a b -> if a > b then 1 else 0)
  | Le -> (fun a b -> if a <= b then 1 else 0)
  | Ge -> (fun a b -> if a >= b then 1 else 0)
  | And -> (fun a b -> if a <> 0 && b <> 0 then 1 else 0)
  | Or -> (fun a b -> if a <> 0 || b <> 0 then 1 else 0))

(* 在给定全局环境中求值表达式；无法确定时返回 None *)
let rec eval_const (env: (string * int) list) (e: Ast.expr) : int option =
  match e with
  | Ast.EInt n -> Some n
  | Ast.EId name -> List.assoc_opt name env
  | Ast.EBinOp (op, e1, e2) ->
      (* 对 && / || 做短路求值，避免 0 && (1/0) 之类的除零 *)
      (match op, eval_const env e1 with
       | Ast.And, Some 0 -> Some 0
       | Ast.Or, Some n when n <> 0 -> Some 1
       | _, Some a ->
           (match eval_const env e2 with
            | Some b ->
                (match op with
                 | Ast.Div | Ast.Mod when b = 0 -> None
                 | _ -> Some (eval_binop op a b))
            | None -> None)
       | _, None -> None)
  | Ast.EUnOp (op, e) ->
      (match eval_const env e with
       | Some n ->
           (match op with
            | Ast.Pos -> Some n
            | Ast.Neg -> Some (-n)
            | Ast.Not -> Some (if n = 0 then 1 else 0))
       | None -> None)
  | Ast.ECall _ -> None

(* 将 AST 程序（函数和全局变量声明列表）转换为 IR 程序 *)
let generate (prog: Ast.prog) : ir_program =
  (* 按声明顺序累积的全局常量/变量求值环境 *)
  let env = ref [] in
  List.filter_map (function
    | Ast.UFunc f -> Some (Function (gen_func f))
    | Ast.UDecl (Ast.VarDecl (name, init)) -> 
        let v = eval_const !env init in
        (match v with Some n -> env := (name, n) :: !env | None -> ());
        Some (GlobalVar (name, v))
    | Ast.UDecl (Ast.ConstDecl (name, init)) ->
        let v = eval_const !env init in
        (match v with Some n -> env := (name, n) :: !env | None -> ());
        Some (GlobalVar (name, v))
  ) prog


(* 打印 *)

(* 将操作数（Const/Var/Temp）转换为字符串 *)
let op_str = function
  | Const n -> string_of_int n
  | Var s -> s
  | Temp n -> "t" ^ string_of_int n

(* 将 AST 二元运算符转换为对应的字符串表示 *)
let binop_str = Ast.(function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Ne -> "!=" | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="
  | And -> "&&" | Or -> "||"
)

(* 将 AST 一元运算符转换为对应的字符串表示 *)
let unop_str = Ast.(function
  | Pos -> "+" | Neg -> "-" | Not -> "!"
)

(* 将单条 TAC 指令转换为可读的字符串 *)
let tac_str = function
  | Assign (x, y) -> Printf.sprintf "%s = %s" (op_str x) (op_str y)
  | AssignBinOp (x, op, y, z) ->
      Printf.sprintf "%s = %s %s %s" (op_str x) (op_str y) (binop_str op) (op_str z)
  | AssignUnOp (x, op, y) ->
      Printf.sprintf "%s = %s%s" (op_str x) (unop_str op) (op_str y)
  | Goto l -> "goto " ^ l
  | IfGoto (x, l) -> Printf.sprintf "if %s goto %s" (op_str x) l
  | IfNotGoto (x, l) -> Printf.sprintf "ifFalse %s goto %s" (op_str x) l
  | Label l -> l ^ ":"
  | Param x -> "param " ^ op_str x
  | Call (x, f, n) -> Printf.sprintf "%s = call %s, %d" (op_str x) f n
  | Return (Some x) -> "return " ^ op_str x
  | Return None -> "return"

(* 打印一个基本块的标签及其所有指令 *)
let dump_block b =
  Printf.printf "%s:
" b.label;
  List.iter (fun i -> Printf.printf "  %s
" (tac_str i)) b.instrs

(* 打印一个 IR 函数的完整信息（参数、局部变量、基本块） *)
let dump_func f =
  Printf.printf "\nfunc %s(%s):
" f.fname (String.concat ", " f.params);
  Printf.printf "  locals: [%s]
" (String.concat ", " f.locals);
  Printf.printf "  temps: %d\n\n" f.temps;
  dump_block f.entry;
  List.iter dump_block f.blocks

(* 打印整个 IR 程序（全局变量和所有函数） *)
let dump_ir prog =
  List.iter (function
    | GlobalVar (name, Some v) -> Printf.printf "global %s = %d
" name v
    | GlobalVar (name, None) -> Printf.printf "global %s
" name
    | Function f -> dump_func f
  ) prog
