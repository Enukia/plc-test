(* main.ml *)
(* 优化开关：默认开启；-O0 可关闭，便于与未优化版本对比/回归 *)
let optimize_enabled = not (Array.exists (fun a -> a = "-O0") Sys.argv)

let () =
  try
    (* 1. 从标准输入读取 ToyC 源代码 *)
    let lexbuf = Lexing.from_channel stdin in
    
    (* 2. 词法与语法分析 *)
    let ast = Lib.Parser.prog Lib.Lexer.token lexbuf in
    
    (* 注释掉 AST 调试输出，不让它污染标准输出 *)
    (* Lib.Ast.dump_ast ast; *)

    (* 3. 语义分析与 IR 生成 *)
    (match Lib.Semantic.analyze ast with
    | Error errors ->
        List.iter
          (fun e -> Printf.fprintf stderr "%s\n" (Lib.Semantic.report_error e))
          errors;
        exit 1

    | Ok ir ->
        (* 在代码生成前运行优化通道：
           常量折叠 -> 尾递归 -> 死代码消除 *)
        let ir =
          if optimize_enabled then Lib.Optimize.optimize_program ir else ir
        in
        
        (* 调用汇编代码生成器，将 RV32I 汇编流打印到标准输出 *)
        Lib.Codegen.generate_riscv ir
    );

  with
  | Lib.Lexer.Error msg ->
      Printf.fprintf stderr "Lexical error: %s\n" msg; exit 1
  | Lib.Parser.Error ->
      Printf.fprintf stderr "Parsing error.\n"; exit 1
