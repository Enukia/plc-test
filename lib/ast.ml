(* 二元运算符类型 *)
type binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Gt | Le | Ge | And | Or
(* 一元运算符类型 *)
type unop = Pos | Neg | Not

(* 表达式：对应程序中产生值的最小单位 *)
type expr =
  | EInt of int                        (* 整数常量：如 42 *)
  | EId of string                      (* 标识符：如变量名 x *)
  | EBinOp of binop * expr * expr      (* 二元运算：如 a + b *)
  | EUnOp of unop * expr               (* 一元运算：如 -a 或 !cond *)
  | ECall of string * expr list        (* 函数调用：如 func(a, b) *)

(* 语句：对应程序执行的逻辑单元 *)
type stmt =
  | SBlock of stmt list                (* 语句块：{ stmt1; stmt2; ... } *)
  | SEmpty                             (* 空语句：; *)
  | SExpr of expr                      (* 表达式语句：x = a + b; *)
  | SDecl of decl                      (* 局部变量或常量声明 *)
  | SAssign of string * expr           (* 变量赋值：x = e; *)
  | SIf of expr * stmt * stmt option   (* 条件分支：if (e) s1 [else s2] *)
  | SWhile of expr * stmt              (* 循环：while (e) s *)
  | SBreak                             (* 退出循环：break; *)
  | SContinue                          (* 继续循环：continue; *)
  | SReturn of expr option             (* 返回语句：return [e]; *)

(* 声明：变量和常量的统称 *)
and decl =
  | VarDecl of string * expr           (* 变量声明：int x = e; *)
  | ConstDecl of string * expr         (* 常量声明：const int x = e; *)

(* 函数参数定义：ToyC 仅支持 int 类型，故只记录参数名 *)
type param = string

(* 函数定义结构体 *)
type func_def = {
  retty: string;                       (* 返回类型："int" 或 "void" *)
  name: string;                        (* 函数名 *)
  params: param list;                  (* 参数列表 *)
  body: stmt;                          (* 函数体（通常是一个 SBlock） *)
}

(* 顶层单元：程序由多个顶层声明或函数定义组成 *)
type top_unit = 
  | UDecl of decl                      (* 全局变量/常量声明 *)
  | UFunc of func_def                  (* 函数定义 *)

(* 整个程序：顶层单元的列表 *)
type prog = top_unit list

(* --- AST 可视化打印工具（用于调试和成员 B 验证） --- *)

(* 打印缩进辅助函数 *)
let print_indent n =
  for _ = 1 to n do print_string "  " done

(* 运算符转字符串 *)
let op_to_string = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Ne -> "!=" | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="
  | And -> "&&" | Or -> "||"

(* 递归打印表达式树 *)
let rec dump_expr indent = function
  | EInt n -> Printf.printf "Int(%d)\n" n
  | EId s -> Printf.printf "Id(%s)\n" s
  | EBinOp(op, l, r) ->
      Printf.printf "BinOp(%s)\n" (op_to_string op);
      dump_expr (indent + 1) l;
      dump_expr (indent + 1) r
  | EUnOp(_, e) -> Printf.printf "UnOp\n"; dump_expr (indent + 1) e
  | ECall(id, args) ->
      Printf.printf "Call %s\n" id;
      List.iter (dump_expr (indent + 1)) args

(* 递归打印语句树 *)
let rec dump_stmt indent = function
  | SBlock stmts ->
      print_indent indent; Printf.printf "Block:\n";
      List.iter (dump_stmt (indent + 1)) stmts
  | SEmpty -> print_indent indent; Printf.printf "Empty\n"
  | SExpr e -> print_indent indent; Printf.printf "ExprStmt: "; dump_expr 0 e
  | SDecl d -> dump_decl indent d
  | SAssign(id, e) ->
      print_indent indent; Printf.printf "Assign %s = " id; dump_expr 0 e
  | SIf(cond, s1, s2) ->
      print_indent indent; Printf.printf "If:\n";
      dump_expr (indent + 1) cond;
      dump_stmt (indent + 1) s1;
      (match s2 with Some s -> dump_stmt (indent + 1) s | None -> ())
  | SWhile(cond, body) ->
      print_indent indent; Printf.printf "While:\n";
      dump_expr (indent + 1) cond;
      dump_stmt (indent + 1) body
  | SBreak -> print_indent indent; Printf.printf "Break\n"
  | SContinue -> print_indent indent; Printf.printf "Continue\n"
  | SReturn e ->
      print_indent indent; Printf.printf "Return\n";
      (match e with Some ex -> dump_expr (indent + 1) ex | None -> ())

(* 递归打印声明 *)
and dump_decl indent = function
  | VarDecl(id, e) ->
      print_indent indent; Printf.printf "VarDecl %s = " id; dump_expr 0 e
  | ConstDecl(id, e) ->
      print_indent indent; Printf.printf "ConstDecl %s = " id; dump_expr 0 e

(* 打印顶层单元 *)
let dump_unit = function
  | UDecl d -> dump_decl 0 d
  | UFunc f ->
      Printf.printf "FuncDef %s (%s):\n" f.name f.retty;
      dump_stmt 1 f.body

(* 打印整个 AST 项目的入口 *)
let dump_ast prog =
  print_endline "--- AST DUMP ---";
  List.iter dump_unit prog;
  print_endline "----------------"