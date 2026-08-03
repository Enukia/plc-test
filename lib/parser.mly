%{
open Ast
%}

/* Token 定义 */
%token <int> NUMBER
%token <string> ID
%token INT VOID CONST IF ELSE WHILE BREAK CONTINUE RETURN
%token PLUS MINUS MUL DIV MOD EQ NE LT GT LE GE AND OR NOT ASSIGN
%token LPAREN RPAREN LBRACE RBRACE COMMA SEMI EOF

/* 优先级与结合性配置 (从低到高) */
%nonassoc LOWER_THAN_ELSE   /* 解决悬挂 else 歧义 */
%nonassoc ELSE
%left OR
%left AND
%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left MUL DIV MOD
%right NOT POS NEG          /* 一元运算符最高优先级 */

/* 程序开始符号 */
%start <Ast.prog> prog

%%

/* 顶层规则：程序是顶层单元列表 */
prog:
  | u = list(comp_unit) EOF { u }

/* 
   顶层单元规则：
   由于 ToyC 中变量声明和函数定义都以 "INT ID" 开头，
   这里采用提取公共前缀的方法解决 LALR(1) 冲突。
*/
comp_unit:
  | CONST INT id = ID ASSIGN e = expr SEMI { UDecl (ConstDecl(id, e)) }
  | VOID id = ID LPAREN p = separated_list(COMMA, param_id) RPAREN b = block 
      { UFunc {retty = "void"; name = id; params = p; body = b} }
  | INT id = ID rest = decl_or_func 
      { match rest with
        | `Decl e -> UDecl (VarDecl(id, e))
        | `Func (p, b) -> UFunc {retty = "int"; name = id; params = p; body = b} 
      }

/* 辅助规则：区分是后续赋值语句（声明）还是左括号（函数） */
decl_or_func:
  | ASSIGN e = expr SEMI { `Decl e }
  | LPAREN p = separated_list(COMMA, param_id) RPAREN b = block { `Func (p, b) }

/* 参数规则：int x */
param_id:
  | INT id = ID { id }

/* 声明规则（内部使用） */
decl:
  | CONST INT id = ID ASSIGN e = expr SEMI { ConstDecl(id, e) }
  | INT id = ID ASSIGN e = expr SEMI { VarDecl(id, e) }

/* 语句块规则 */
block:
  | LBRACE s = list(stmt) RBRACE { SBlock s }

/* 语句核心规则 */
stmt:
  | b = block { b }
  | SEMI { SEmpty }
  | e = expr SEMI { SExpr e }
  | id = ID ASSIGN e = expr SEMI { SAssign(id, e) }
  | d = decl { SDecl d }
  | IF LPAREN e = expr RPAREN s1 = stmt %prec LOWER_THAN_ELSE { SIf(e, s1, None) }
  | IF LPAREN e = expr RPAREN s1 = stmt ELSE s2 = stmt { SIf(e, s1, Some s2) }
  | WHILE LPAREN e = expr RPAREN s = stmt { SWhile(e, s) }
  | BREAK SEMI { SBreak }
  | CONTINUE SEMI { SContinue }
  | RETURN e = option(expr) SEMI { SReturn e }

/* 表达式规则 */
expr:
  | n = NUMBER { EInt n }
  | id = ID { EId id }
  | id = ID LPAREN args = separated_list(COMMA, expr) RPAREN { ECall(id, args) }
  | LPAREN e = expr RPAREN { e }
  | e1 = expr PLUS e2 = expr  { EBinOp(Add, e1, e2) }
  | e1 = expr MINUS e2 = expr { EBinOp(Sub, e1, e2) }
  | e1 = expr MUL e2 = expr   { EBinOp(Mul, e1, e2) }
  | e1 = expr DIV e2 = expr   { EBinOp(Div, e1, e2) }
  | e1 = expr MOD e2 = expr   { EBinOp(Mod, e1, e2) }
  | e1 = expr EQ e2 = expr    { EBinOp(Eq, e1, e2) }
  | e1 = expr NE e2 = expr    { EBinOp(Ne, e1, e2) }
  | e1 = expr LT e2 = expr    { EBinOp(Lt, e1, e2) }
  | e1 = expr GT e2 = expr    { EBinOp(Gt, e1, e2) }
  | e1 = expr LE e2 = expr    { EBinOp(Le, e1, e2) }
  | e1 = expr GE e2 = expr    { EBinOp(Ge, e1, e2) }
  | e1 = expr AND e2 = expr   { EBinOp(And, e1, e2) }
  | e1 = expr OR e2 = expr    { EBinOp(Or, e1, e2) }
  | PLUS e = expr %prec POS   { EUnOp(Pos, e) }
  | MINUS e = expr %prec NEG  { EUnOp(Neg, e) }
  | NOT e = expr              { EUnOp(Not, e) }
%%