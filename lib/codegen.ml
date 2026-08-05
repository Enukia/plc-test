(* lib/codegen.ml *)
open Ir

(* 唯一标签计数器，防止内联展开时的汇编标签冲突 *)
let inline_label_counter = ref 0

let gen_inline_label prefix =
  incr inline_label_counter;
  Printf.sprintf "%s_inline_%d" prefix !inline_label_counter

let power_of_two_shift n =
  if n <= 0 then None
  else
    let rec loop shift v =
      if v = 1 then Some shift
      else if v mod 2 <> 0 then None
      else loop (shift + 1) (v / 2)
    in
    loop 0 n

(* 辅助函数：列表切分 *)
let rec split_at n = function
  | xs when n <= 0 -> [], xs
  | [] -> [], []
  | x :: xs ->
      let prefix, suffix = split_at (n - 1) xs in
      x :: prefix, suffix

(* 计算栈槽偏移量映射表 *)
let compute_offsets (f: ir_func) =
  let local_slots = ref 0 in
  let map = Hashtbl.create 32 in
  
  (* 处理函数参数 *)
  List.iteri (fun i name ->
    if i < 8 then (
      incr local_slots;
      Hashtbl.add map (Var name) (-8 - 4 * !local_slots)
    ) else (
      (* 大于 8 个的参数由 Caller 压栈，在旧 SP（即当前 FP）的正偏移处 *)
      Hashtbl.add map (Var name) ((i - 8) * 4)
    )
  ) f.params;
  
  (* 处理其它未映射的局部变量 *)
  List.iter (fun name ->
    if not (Hashtbl.mem map (Var name)) then (
      incr local_slots;
      Hashtbl.add map (Var name) (-8 - 4 * !local_slots)
    )
  ) f.locals;
  
  (* 处理所有临时变量 *)
  for t = 0 to f.temps - 1 do
    incr local_slots;
    Hashtbl.add map (Temp t) (-8 - 4 * !local_slots)
  done;
  
  (!local_slots, map)

(* 将操作数的值加载到目标寄存器 *)
let load_op reg op map =
  match op with
  | Const n ->
      Printf.printf "    li %s, %d\n" reg n
  | Temp t ->
      let off = Hashtbl.find map (Temp t) in
      Printf.printf "    lw %s, %d(fp)\n" reg off
  | Var name ->
      if Hashtbl.mem map (Var name) then
        let off = Hashtbl.find map (Var name) in
        Printf.printf "    lw %s, %d(fp)\n" reg off
      else
        (* 找不到说明是全局变量 *)
        (Printf.printf "    la %s, %s\n" reg name;
         Printf.printf "    lw %s, 0(%s)\n" reg reg)

(* 将寄存器中的值写回到操作数对应的栈槽中 *)
let store_op reg op map =
  match op with
  | Const _ -> () (* 常量不可作为左值 *)
  | Temp t ->
      let off = Hashtbl.find map (Temp t) in
      Printf.printf "    sw %s, %d(fp)\n" reg off
  | Var name ->
      if Hashtbl.mem map (Var name) then
        let off = Hashtbl.find map (Var name) in
        Printf.printf "    sw %s, %d(fp)\n" reg off
      else
        (* 全局变量写回 *)
        (Printf.printf "    la t3, %s\n" name;
         Printf.printf "    sw %s, 0(t3)\n" reg)

(* 翻译单条 TAC 指令 *)
let emit_tac fname tac_inst map current_args =
  match tac_inst with
  | Assign (x, y) ->
      load_op "t0" y map;
      store_op "t0" x map

  | AssignBinOp (x, op, y, z) ->
      (match op with
       (* ================= 1. 乘法内联直接打印 ================= *)
       | Ast.Mul ->
            let emit_const_mul dest nonconst c =
              load_op "t0" nonconst map;
              (match c with
               | 0 -> Printf.printf "    li t2, 0\n"
               | 1 -> Printf.printf "    addi t2, t0, 0\n"
               | -1 -> Printf.printf "    sub t2, x0, t0\n"
               | _ ->
                   let sign = if c < 0 then -1 else 1 in
                   let abs_c = if c < 0 then -c else c in
                   (match power_of_two_shift abs_c with
                    | Some sh ->
                        Printf.printf "    slli t2, t0, %d\n" sh;
                        if sign < 0 then Printf.printf "    sub t2, x0, t2\n"
                    | None -> assert false));
              store_op "t2" dest map
            in
            let const_mul =
              match y, z with
              | Const c, nonconst | nonconst, Const c ->
                  (match c with
                   | 0 | 1 | -1 -> Some (nonconst, c)
                   | _ ->
                       let abs_c = if c < 0 then -c else c in
                       (match power_of_two_shift abs_c with
                        | Some _ -> Some (nonconst, c)
                        | None -> None))
              | _ -> None
            in
            (match const_mul with
             | Some (nonconst, c) -> emit_const_mul x nonconst c
             | None ->
            let lbl_mul_pos1 = gen_inline_label ".L_mul_pos1" in
            let lbl_mul_pos2 = gen_inline_label ".L_mul_pos2" in
            let lbl_loop     = gen_inline_label ".L_mul_loop" in
            let lbl_skip     = gen_inline_label ".L_mul_skip" in
            let lbl_end      = gen_inline_label ".L_mul_end" in
            let lbl_neg_result = gen_inline_label ".L_mul_neg_result" in
            
            load_op "t0" y map;       (* t0 = 被乘数 *)
            load_op "t1" z map;       (* t1 = 乘数 *)
            
            (* RV32I 内联乘法开始 *)
            (* ----- 符号处理 ----- *)
            Printf.printf "    li t6, 0\n";               (* 符号标记 *)
            Printf.printf "    bge t0, x0, %s\n" lbl_mul_pos1;
            Printf.printf "    sub t0, x0, t0\n";         (* t0 = -t0 *)
            Printf.printf "    xori t6, t6, 1\n";
            Printf.printf "%s:\n" lbl_mul_pos1;

            Printf.printf "    bge t1, x0, %s\n" lbl_mul_pos2;
            Printf.printf "    sub t1, x0, t1\n";         (* t1 = -t1 *)
            Printf.printf "    xori t6, t6, 1\n";
            Printf.printf "%s:\n" lbl_mul_pos2;

            (* ----- 无符号乘法 (原有逻辑) ----- *)
            Printf.printf "    li t2, 0\n";
            Printf.printf "%s:\n" lbl_loop;
            Printf.printf "    beqz t1, %s\n" lbl_end;
            Printf.printf "    andi t3, t1, 1\n";
            Printf.printf "    beqz t3, %s\n" lbl_skip;
            Printf.printf "    add t2, t2, t0\n";
            Printf.printf "%s:\n" lbl_skip;
            Printf.printf "    slli t0, t0, 1\n";
            Printf.printf "    srli t1, t1, 1\n";
            Printf.printf "    j %s\n" lbl_loop;
            Printf.printf "%s:\n" lbl_end;

            (* ----- 恢复有符号结果 ----- *)
            Printf.printf "    beqz t6, %s\n" lbl_neg_result;
            Printf.printf "    sub t2, x0, t2\n";
            Printf.printf "%s:\n" lbl_neg_result;
            (* RV32I 内联乘法结束 *)
            
            store_op "t2" x map       (* 将计算结果 t2 写回目标操作数 *)

       (* ================= 2. 除法与取模内联直接打印 ================= *)
            )

       | Ast.Div | Ast.Mod ->
           let lbl_start  = gen_inline_label ".L_divmod_start" in
           let lbl_n_pos  = gen_inline_label ".L_divmod_n_pos" in
           let lbl_d_pos  = gen_inline_label ".L_divmod_d_pos" in
           let lbl_loop   = gen_inline_label ".L_divmod_loop" in
           let lbl_skip   = gen_inline_label ".L_divmod_skip" in
           let lbl_end    = gen_inline_label ".L_divmod_end" in
           let lbl_q_pos  = gen_inline_label ".L_divmod_q_pos" in
           let lbl_r_pos  = gen_inline_label ".L_divmod_r_pos" in
           let lbl_finish = gen_inline_label ".L_divmod_finish" in
           
           load_op "t0" y map;       (* t0 = 被除数 N *)
           load_op "t1" z map;       (* t1 = 除数 D *)
           
           (* RV32I 内联除法/取模开始 *)
           (* 除 0 保护检查 *)
           Printf.printf "    bnez t1, %s\n" lbl_start;
           Printf.printf "    li t2, 0\n";          (* 除以0时商 Q=0 *)
           Printf.printf "    li t3, 0\n";          (* 余数 R=0 *)
           Printf.printf "    j %s\n" lbl_finish;
           
           Printf.printf "%s:\n" lbl_start;
           Printf.printf "    li t6, 0\n";          (* t6 记录商的最终符号 (1负0正) *)
           Printf.printf "    li t4, 0\n";          (* t4 记录余数的最终符号 (由N决定) *)
           
           (* 提取被除数 N 的绝对值 *)
           Printf.printf "    bge t0, x0, %s\n" lbl_n_pos;
           Printf.printf "    sub t0, x0, t0\n";    (* t0 = |N| *)
           Printf.printf "    li t6, 1\n";
           Printf.printf "    li t4, 1\n";
           Printf.printf "%s:\n" lbl_n_pos;
           
           (* 提取除数 D 的绝对值 *)
           Printf.printf "    bge t1, x0, %s\n" lbl_d_pos;
           Printf.printf "    sub t1, x0, t1\n";    (* t1 = |D| *)
           Printf.printf "    xori t6, t6, 1\n";    (* 异或决定商的符号 *)
           Printf.printf "%s:\n" lbl_d_pos;
           
           (* 无符号长除法核心状态机运算 *)
           Printf.printf "    li t2, 0\n";          (* 暂存商 Q = 0 *)
           Printf.printf "    li t3, 0\n";          (* 暂存余数 R = 0 *)
           Printf.printf "    li t5, 31\n";         (* 循环计数器 i = 31 *)
           
           Printf.printf "%s:\n" lbl_loop;
           Printf.printf "    bltz t5, %s\n" lbl_end;
           Printf.printf "    slli t3, t3, 1\n";    (* R = R << 1 *)
           Printf.printf "    srl a2, t0, t5\n";    (* N >> i (使用空闲的 a2 临时中转) *)
           Printf.printf "    andi a2, a2, 1\n";    (* 获取 N 的第 i 位 *)
           Printf.printf "    or t3, t3, a2\n";     (* R = R | bit *)
           Printf.printf "    blt t3, t1, %s\n" lbl_skip;
           Printf.printf "    sub t3, t3, t1\n";    (* R = R - D *)
           Printf.printf "    li a2, 1\n";
           Printf.printf "    sll a2, a2, t5\n";    (* 1 << i *)
           Printf.printf "    or t2, t2, a2\n";     (* Q = Q | (1 << i) *)
           Printf.printf "%s:\n" lbl_skip;
           Printf.printf "    addi t5, t5, -1\n";   (* i-- *)
           Printf.printf "    j %s\n" lbl_loop;
           Printf.printf "%s:\n" lbl_end;
           
           (* 恢复有符号商的符号 *)
           Printf.printf "    beqz t6, %s\n" lbl_q_pos;
           Printf.printf "    sub t2, x0, t2\n";
           Printf.printf "%s:\n" lbl_q_pos;
           
           (* 恢复有符号余数的符号 (余数符号与被除数 N 一致) *)
           Printf.printf "    beqz t4, %s\n" lbl_r_pos;
           Printf.printf "    sub t3, x0, t3\n";
           Printf.printf "%s:\n" lbl_r_pos;
           
           Printf.printf "%s:\n" lbl_finish;
           (* RV32I 内联除法/取模结束 --- *)
           
           (* 根据 TAC 操作码，决定把“商”还是“余数”写回内存 *)
           if op = Ast.Div then
             store_op "t2" x map                    (* t2 存的是商 *)
           else
             store_op "t3" x map                    (* t3 存的是余数 *)

       (* ================= 3. RV32I 原生支持的标准有符号/逻辑运算 ================= *)
       | _ ->
           load_op "t0" y map;
           load_op "t1" z map;
           (match op with
            | Ast.Add -> Printf.printf "    add t0, t0, t1\n"
            | Ast.Sub -> Printf.printf "    sub t0, t0, t1\n"
            | Ast.Eq  -> Printf.printf "    sub t0, t0, t1\n    sltiu t0, t0, 1\n"
            | Ast.Ne  -> Printf.printf "    sub t0, t0, t1\n    sltu t0, zero, t0\n"
            | Ast.Lt  -> Printf.printf "    slt t0, t0, t1\n"
            | Ast.Gt  -> Printf.printf "    slt t0, t1, t0\n"
            | Ast.Le  -> Printf.printf "    slt t0, t1, t0\n    xori t0, t0, 1\n"
            | Ast.Ge  -> Printf.printf "    slt t0, t0, t1\n    xori t0, t0, 1\n"
            | Ast.And -> Printf.printf "    and t0, t0, t1\n"
            | Ast.Or  -> Printf.printf "    or t0, t0, t1\n"
            | Ast.Mul | Ast.Div | Ast.Mod -> assert false (* 已在外部 match 处理 *)
           );
           store_op "t0" x map)

  (* 支持M扩展时启用 *)
  (*
  | AssignBinOp (x, op, y, z) ->
      load_op "t0" y map;
      load_op "t1" z map;
      (match op with
       | Ast.Add -> Printf.printf "    add t0, t0, t1\n"
       | Ast.Sub -> Printf.printf "    sub t0, t0, t1\n"
       | Ast.Mul -> Printf.printf "    mul t0, t0, t1\n"
       | Ast.Div -> Printf.printf "    div t0, t0, t1\n"
       | Ast.Mod -> Printf.printf "    rem t0, t0, t1\n"
       | Ast.Eq  -> Printf.printf "    sub t0, t0, t1\n    seqz t0, t0\n"
       | Ast.Ne  -> Printf.printf "    sub t0, t0, t1\n    snez t0, t0\n"
       | Ast.Lt  -> Printf.printf "    slt t0, t0, t1\n"
       | Ast.Gt  -> Printf.printf "    slt t0, t1, t0\n"
       | Ast.Le  -> Printf.printf "    slt t0, t1, t0\n    xori t0, t0, 1\n"
       | Ast.Ge  -> Printf.printf "    slt t0, t0, t1\n    xori t0, t0, 1\n"
       | Ast.And -> Printf.printf "    and t0, t0, t1\n"
       | Ast.Or  -> Printf.printf "    or t0, t0, t1\n");
      store_op "t0" x map
  *)

  | AssignUnOp (x, op, y) ->
      load_op "t0" y map;
      (match op with
       | Ast.Pos -> ()
       | Ast.Neg -> Printf.printf "    neg t0, t0\n"
       | Ast.Not -> Printf.printf "    seqz t0, t0\n");
      store_op "t0" x map

  | Goto l ->
      Printf.printf "    j %s\n" l

  | IfGoto (x, l) ->
      load_op "t0" x map;
      Printf.printf "    bnez t0, %s\n" l

  | IfNotGoto (x, l) ->
      load_op "t0" x map;
      Printf.printf "    beqz t0, %s\n" l

  | Label l ->
      (* 基本块外部有单独标签逻辑，通常 TAC 中的 Label 可在此打印或被块接管 *)
      Printf.printf "%s:\n" l

  | Param x ->
      current_args := x :: !current_args

  | Call (dest, callee, nargs) ->
      let call_args, rem = split_at nargs !current_args in
      current_args := rem;
      (* FIX: Params 累积时已是源码顺序（首个参数在头），不要再反转 *)
      let args = call_args in
      
      (* 如果调用的函数参数超过 8 个，需要为其在 sp 低位开辟动态传参空间 *)
      let extra_space = if nargs > 8 then ((nargs - 8) * 4 + 15) / 16 * 16 else 0 in
      if extra_space > 0 then
        Printf.printf "    addi sp, sp, -%d\n" extra_space;
        
      (* 传递参数 *)
      List.iteri (fun j arg ->
        if j < 8 then
          load_op (Printf.sprintf "a%d" j) arg map
        else
          (load_op "t0" arg map;
           Printf.printf "    sw t0, %d(sp)\n" ((j - 8) * 4))
      ) args;
      
      (* 执行调用 *)
      Printf.printf "    call %s\n" callee;
      
      (* 恢复动态开辟的传参栈空间 *)
      if extra_space > 0 then
        Printf.printf "    addi sp, sp, %d\n" extra_space;
        
      (* 保存返回值 *)
      store_op "a0" dest map

  | Return (Some x) ->
      load_op "a0" x map;
      Printf.printf "    j .L_epilogue_%s\n" fname

  | Return None ->
      Printf.printf "    j .L_epilogue_%s\n" fname

(* 翻译单个基本块 *)
let emit_block fname (b: basic_block) map current_args =
  Printf.printf "%s:\n" b.label;
  List.iter (fun inst -> emit_tac fname inst map current_args) b.instrs

(* 翻译单个函数 *)
let emit_function (f: ir_func) =
  let slots, map = compute_offsets f in
  (* 计算对齐 16 字节后的帧大小（8 字节用于保存 ra 和 fp） *)
  let framesize = ((8 + slots * 4 + 15) / 16) * 16 in
  
  Printf.printf "    .globl %s\n" f.fname;
  Printf.printf "%s:\n" f.fname;
  
  (* 函数序言 (Prologue) *)
  Printf.printf "    addi sp, sp, -%d\n" framesize;
  Printf.printf "    sw ra, %d(sp)\n" (framesize - 4);
  Printf.printf "    sw fp, %d(sp)\n" (framesize - 8);
  Printf.printf "    addi fp, sp, %d\n" framesize;
  
  (* 将传进来的前 8 个参数从寄存器转存到本地分配的栈槽 *)
  List.iteri (fun i name ->
    if i < 8 then
      let off = Hashtbl.find map (Var name) in
      Printf.printf "    sw a%d, %d(fp)\n" i off
  ) f.params;

  (* 打印函数入口标签：尾递归优化会跳回该标签。
     未优化时入口标签统一为 "entry"，无需打印，保持原输出。 *)
  if f.entry.label <> "entry" then
    Printf.printf "%s:\n" f.entry.label;
  
  (* 函数体翻译 (Body) *)
  let current_args = ref [] in
  List.iter (fun inst -> emit_tac f.fname inst map current_args) f.entry.instrs;
  List.iter (fun b -> emit_block f.fname b map current_args) f.blocks;
  
  (* 函数结语 (Epilogue) *)
  Printf.printf ".L_epilogue_%s:\n" f.fname;
  Printf.printf "    lw ra, -4(fp)\n";
  Printf.printf "    lw fp, -8(fp)\n";
  Printf.printf "    addi sp, sp, %d\n" framesize;
  Printf.printf "    ret\n\n"

(* 整个程序的代码生成主入口点 *)
let generate_riscv (prog: ir_program) =
  (* 打印全局段声明 *)
  Printf.printf "    .text\n\n";

  List.iter (function
    | GlobalVar (name, Some v) ->
        Printf.printf "    .globl %s\n" name;
        Printf.printf "    .data\n";
        Printf.printf "    .align 2\n";
        Printf.printf "%s:\n" name;
        Printf.printf "    .word %d\n\n" v
    | GlobalVar (name, None) ->
        Printf.printf "    .globl %s\n" name;
        Printf.printf "    .data\n";
        Printf.printf "    .align 2\n";
        Printf.printf "%s:\n" name;
        Printf.printf "    .space 4\n\n"
    | Function f ->
        Printf.printf "    .text\n";
        emit_function f
  ) prog
