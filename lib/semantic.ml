(* lib/semantic.ml
 * 
 * 实现了一个语义分析器。
 * 基于符号表管理作用域链，对 AST 进行静态语义检查，
 * 包括变量声明、类型一致性、函数签名、控制流合法性、
 * 编译期常量约束、路径返回分析等。
 * 最后提供对外接口，在语义检查通过后调用 IR 生成器。
 *)

module StringMap = Map.Make(String)

type sym_kind = Var | Const | Func of int

type symbol = {
  name: string;
  kind: sym_kind;
}

type scope = symbol StringMap.t
type symbol_table = scope list

(* 创建空的符号表（仅含一个全局作用域） *)
let empty_table () = [StringMap.empty]

(* 进入一个新的作用域 *)
let enter_scope tbl = StringMap.empty :: tbl

(* 退出当前作用域 *)
let exit_scope = function _::rest -> rest | [] -> failwith "empty"

(* 在整个作用域链中按由内到外的顺序查找符号 *)
let rec find name = function
  | [] -> None
  | scope::rest -> 
      match StringMap.find_opt name scope with
      | Some s -> Some s | None -> find name rest

(* 仅在当前（最内层）作用域中查找符号 *)
let find_current name = function
  | [] -> None
  | scope::_ -> StringMap.find_opt name scope

(* 在当前作用域中添加符号，若已存在则报错 *)
let add_symbol sym = function
  | [] -> failwith "empty table"
  | scope::rest -> 
      if StringMap.mem sym.name scope then
        failwith ("Redeclaration of '" ^ sym.name ^ "'")
      else
        (StringMap.add sym.name sym scope)::rest

type semantic_error =
  | UndeclaredVar of string
  | Redeclaration of string
  | ConstAssign of string
  | MainNotFound
  | MainWrongReturnType
  | MainWrongParams
  | BreakOutsideLoop
  | ContinueOutsideLoop
  | ArgCountMismatch of string * int * int
  | VoidFuncReturnsValue of string
  | IntFuncMissingReturnValue of string    
  | MissingReturn of string        
  | ConstNotCompileTime of string 
  | FuncNotDeclared of string     
  | NotAFunction of string                 

(* 将语义错误转换为错误信息字符串 *)
let report_error = function
  | UndeclaredVar s -> "Error: Undeclared identifier '" ^ s ^ "'"
  | Redeclaration s -> "Error: Redeclaration of '" ^ s ^ "'"
  | ConstAssign s -> "Error: Cannot assign to constant '" ^ s ^ "'"
  | MainNotFound -> "Error: 'main' function not found"
  | MainWrongReturnType -> "Error: 'main' must return int"
  | MainWrongParams -> "Error: 'main' must take no parameters"
  | BreakOutsideLoop -> "Error: 'break' outside of loop"
  | ContinueOutsideLoop -> "Error: 'continue' outside of loop"
  | ArgCountMismatch (f, e, a) -> 
      Printf.sprintf "Error: Function '%s' expects %d argument(s), got %d" f e a
  | VoidFuncReturnsValue f -> 
      Printf.sprintf "Error: Void function '%s' cannot return a value" f
  | IntFuncMissingReturnValue f ->
      Printf.sprintf "Error: Function '%s' must return an integer value" f
  | MissingReturn f -> 
      Printf.sprintf "Error: Function '%s' may not return a value on all paths" f
  | ConstNotCompileTime s -> 
      Printf.sprintf "Error: Constant '%s' initializer is not compile-time constant" s
  | FuncNotDeclared s -> 
      Printf.sprintf "Error: Function '%s' called before declaration" s
  | NotAFunction s -> 
      Printf.sprintf "Error: '%s' is not a function" s

type check_state = {
  mutable sym_table: symbol_table;
  mutable in_loop: bool;
  mutable errors: semantic_error list;
  mutable has_main: bool;
  mutable current_func: string option;
  mutable current_func_retty: string option;  
}

(* 初始化语义检查状态 *)
let init_state () = {
  sym_table = empty_table ();
  in_loop = false;
  errors = [];
  has_main = false;
  current_func = None;
  current_func_retty = None;
}

(* 向错误列表中添加一条语义错误 *)
let add_err st err = st.errors <- err :: st.errors


(* 编译期常量检查 *)

(* 判断表达式是否为编译期可计算的常量（仅整数常量和 const 标识符） *)
let rec is_compile_time_expr st (e: Ast.expr) : bool =
  match e with
  | Ast.EInt _ -> true
  | Ast.EId name ->
      (match find name st.sym_table with
       | Some { kind = Const; _ } -> true
       | _ -> false)
  | Ast.EBinOp (_, e1, e2) ->
      is_compile_time_expr st e1 && is_compile_time_expr st e2
  | Ast.EUnOp (_, e) ->
      is_compile_time_expr st e
  | Ast.ECall _ -> false  


(* 表达式检查 *)

(* 递归检查表达式的语义合法性（变量是否声明、函数调用参数数量等） *)
let rec check_expr st (e: Ast.expr) : unit =
  match e with
  | Ast.EInt _ -> ()

  | Ast.EId name ->
      (match find name st.sym_table with
       | None -> add_err st (UndeclaredVar name)
       | Some _ -> ())

  | Ast.EBinOp (_, e1, e2) ->
      check_expr st e1;
      check_expr st e2

  | Ast.EUnOp (_, e) ->
      check_expr st e

  | Ast.ECall (fname, args) ->
      (match find fname st.sym_table with
       | None -> 
           add_err st (UndeclaredVar fname)
       | Some sym ->
           (match sym.kind with
            | Func expected_args ->
                let actual_args = List.length args in
                if expected_args <> actual_args then
                  add_err st (ArgCountMismatch (fname, expected_args, actual_args))
            | _ -> 
                add_err st (NotAFunction fname));   (* FIX: 用 NotAFunction 替代 UndeclaredVar *)
           List.iter (check_expr st) args)


(* 声明检查 *)

(* 检查变量或常量声明的合法性，处理重声明和编译期常量约束 *)
let check_decl st (d: Ast.decl) : unit =
  match d with
  | Ast.VarDecl (name, init) ->
      if find_current name st.sym_table <> None then
        add_err st (Redeclaration name)
      else (
        check_expr st init;
        let sym = { name; kind = Var } in
        st.sym_table <- add_symbol sym st.sym_table
      )

  | Ast.ConstDecl (name, init) ->
      if find_current name st.sym_table <> None then
        add_err st (Redeclaration name)
      else (
        if not (is_compile_time_expr st init) then
          add_err st (ConstNotCompileTime name);
        check_expr st init;
        let sym = { name; kind = Const } in
        st.sym_table <- add_symbol sym st.sym_table
      )


(* 判断语句中是否出现 break（用于 while(1) 永真循环的返回分析） *)
let rec stmt_contains_break (s: Ast.stmt) : bool =
  match s with
  | Ast.SBlock stmts -> List.exists stmt_contains_break stmts
  | Ast.SIf (_, then_s, else_s) ->
      stmt_contains_break then_s
      || (match else_s with Some s -> stmt_contains_break s | None -> false)
  | Ast.SWhile (_, body) -> stmt_contains_break body
  | Ast.SBreak -> true
  | _ -> false


(* 语句检查 + 路径返回分析 *)

(* 递归检查单条语句的语义，返回布尔值表示该分支是否保证有 return *)
let rec check_stmt st (s: Ast.stmt) : bool =
  match s with
  | Ast.SBlock stmts ->
      st.sym_table <- enter_scope st.sym_table;
      let returns = check_stmt_list st stmts in
      st.sym_table <- exit_scope st.sym_table;
      returns

  | Ast.SEmpty -> false

  | Ast.SExpr e -> 
      check_expr st e; 
      false

  | Ast.SDecl d -> 
      check_decl st d; 
      false

  | Ast.SAssign (name, e) ->
      (match find name st.sym_table with
       | None -> add_err st (UndeclaredVar name)
       | Some sym ->
           if sym.kind = Const then
             add_err st (ConstAssign name)
           else if sym.kind <> Var then
             add_err st (NotAFunction name);
           check_expr st e);
      false

  | Ast.SIf (cond, then_s, else_s) ->
      check_expr st cond;
      let then_returns = check_stmt st then_s in
      let else_returns = 
        match else_s with
        | Some s -> check_stmt st s
        | None -> false
      in
      (* 条件为编译期常量时，只分析实际可达的分支 *)
      (match cond with
       | Ast.EInt n when n <> 0 -> then_returns
       | Ast.EInt 0 -> else_returns
       | _ -> then_returns && else_returns)

  | Ast.SWhile (cond, body) ->
      check_expr st cond;
      let old_loop = st.in_loop in
      st.in_loop <- true;
      let _ = check_stmt st body in
      st.in_loop <- old_loop;
      (* while(1) 等永真条件且循环体无 break 时，控制流不会落出循环 *)
      (match cond with
       | Ast.EInt n when n <> 0 -> not (stmt_contains_break body)
       | _ -> false)

  | Ast.SBreak ->                                    
      if not st.in_loop then
        add_err st BreakOutsideLoop;
      false

  | Ast.SContinue ->                                 
      if not st.in_loop then
        add_err st ContinueOutsideLoop;
      false

  | Ast.SReturn e ->
      (match st.current_func_retty, e with
       | Some "void", Some _ ->
           add_err st (VoidFuncReturnsValue (Option.get st.current_func))
       | Some "int", None ->
           add_err st (IntFuncMissingReturnValue (Option.get st.current_func))  
       | _ -> ());
      Option.iter (check_expr st) e;
      true

(* 检查语句列表，返回是否保证 return（用于路径分析） *)
and check_stmt_list st (stmts: Ast.stmt list) : bool =
  match stmts with
  | [] -> false
  | s::rest ->
      (* 只要序列中任一语句保证终止（return 或永真循环），
         其后的语句即为不可达死代码，整体仍保证不落出函数尾。
         所有语句仍会被检查，错误收集不受影响。 *)
      check_stmt st s || check_stmt_list st rest


(* 顶层单元检查 *)

(* 检查顶层单元（全局声明或函数定义），包括 main 函数约束和函数体路径分析 *)
let check_top_unit st (tu: Ast.top_unit) : unit =
  match tu with
  | Ast.UDecl d -> check_decl st d

  | Ast.UFunc f ->
      if f.Ast.name = "main" then (
        st.has_main <- true;
        if f.Ast.retty <> "int" then
          add_err st MainWrongReturnType;
        if f.Ast.params <> [] then
          add_err st MainWrongParams
      );

      let func_sym = { 
        name = f.Ast.name; 
        kind = Func (List.length f.Ast.params) 
      } in
      (match find_current f.Ast.name st.sym_table with
       | Some _ -> add_err st (Redeclaration f.Ast.name)
       | None -> st.sym_table <- add_symbol func_sym st.sym_table);

      st.sym_table <- enter_scope st.sym_table;
      let old_func = st.current_func in
      let old_retty = st.current_func_retty in
      st.current_func <- Some f.Ast.name;
      st.current_func_retty <- Some f.Ast.retty;

      List.iter (fun pname ->
        let sym = { name = pname; kind = Var } in
        st.sym_table <- add_symbol sym st.sym_table
      ) f.Ast.params;

      let body_returns = check_stmt st f.Ast.body in
      if f.Ast.retty = "int" && not body_returns then
        add_err st (MissingReturn f.Ast.name);

      st.current_func <- old_func;
      st.current_func_retty <- old_retty;
      st.sym_table <- exit_scope st.sym_table


(* 程序级检查入口 *)

(* 对完整程序进行语义检查，返回收集到的所有错误列表 *)
let check_program (prog: Ast.prog) : semantic_error list =
  let st = init_state () in

  List.iter (check_top_unit st) prog;

  if not st.has_main then
    add_err st MainNotFound;

  List.rev st.errors


(* 对外接口：语义分析 + IR 生成 *)

(* 语义分析入口：检查通过后生成 IR，否则返回错误列表 *)
let analyze (prog: Ast.prog) : (Ir.ir_program, semantic_error list) result =
  let errs = check_program prog in
  if errs = [] then
    Ok (Ir.generate prog)
  else
    Error errs
