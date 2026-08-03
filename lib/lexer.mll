{
open Parser
exception Error of string
}

(* 基本正则表达式定义 *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let id = alpha (alpha | digit)*
let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse
  | "\xEF\xBB\xBF" { token lexbuf }
  | whitespace { token lexbuf }           (* 跳过空白符 *)
  | "//" [^ '\n']* { token lexbuf }       (* 跳过单行注释 *)
  | "/*" { multi_comment lexbuf }          (* 进入多行注释处理 *)
  
  (* 关键字 *)
  | "int"      { INT }
  | "void"     { VOID }
  | "const"    { CONST }
  | "if"       { IF }
  | "else"     { ELSE }
  | "while"    { WHILE }
  | "break"    { BREAK }
  | "continue" { CONTINUE }
  | "return"   { RETURN }
  
  (* 运算符与符号 *)
  | "+"  { PLUS }   | "-"  { MINUS }
  | "*"  { MUL }    | "/"  { DIV }    | "%"  { MOD }
  | "==" { EQ }     | "!=" { NE }
  | "<=" { LE }     | ">=" { GE }
  | "<"  { LT }     | ">"  { GT }
  | "&&" { AND }    | "||" { OR }
  | "!"  { NOT }    | "="  { ASSIGN }
  | "("  { LPAREN } | ")"  { RPAREN }
  | "{"  { LBRACE } | "}"  { RBRACE }
  | ","  { COMMA }  | ";"  { SEMI }
  
  (* 常量与标识符 *)
  | digit+ as n { NUMBER (int_of_string n) }
  | id as s     { ID s }
  
  | eof { EOF }                           (* 文件结束 *)
  | _ { raise (Error ("Unknown char: " ^ Lexing.lexeme lexbuf)) }

(* 多行注释递归处理 *)
and multi_comment = parse
  | "*/" { token lexbuf }                 (* 注释结束 *)
  | _    { multi_comment lexbuf }         (* 继续跳过 *)
  | eof  { raise (Error "Unterminated comment") }