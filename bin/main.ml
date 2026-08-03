(* main.ml *)
let () =
  try
    (* 1. 从标准输入读取 ToyC 源代码 *)
    let lexbuf = Lexing.from_channel stdin in
    
    (* 2. 词法与语法分析 *)
    let ast = Lib.Parser.prog Lib.Lexer.token lexbuf in
    
    (* 【修改】注释掉 AST 调试输出，不让它污染标准输出 *)
    (* Lib.Ast.dump_ast ast; *)

    (* 3. 语义分析与 IR 生成 *)
    (match Lib.Semantic.analyze ast with
    | Error errors ->
        List.iter
          (fun e -> Printf.fprintf stderr "%s\n" (Lib.Semantic.report_error e))
          errors;
        exit 1

    | Ok ir ->
        (* 【修改】注释掉原本的 IR 打印和成功提示 *)
        (* Printf.printf "Semantic check success!\n"; *)
        (* Lib.Ir.dump_ir ir *)
        
        (* 【关键】直接调用汇编代码生成器，将 RV32I 汇编流打印到标准输出 *)
        Lib.Codegen.generate_riscv ir
    );

    (* 【修改】注释掉尾部的统计信息，保持汇编文件纯净 *)
    (* Printf.printf "Success: Units parsed: %d\n" (List.length ast) *)

  with
  | Lib.Lexer.Error msg ->
      Printf.fprintf stderr "Lexical error: %s\n" msg; exit 1
  | Lib.Parser.Error ->
      Printf.fprintf stderr "Parsing error.\n"; exit 1