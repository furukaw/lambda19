%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN LBRACE RBRACE QQ
%token TRUE FALSE
%token LET IF THEN ELSE LAM APP DOT ARROW BAR COMMA BE PM AS PRINT
%token INL INR
%token <string> VAR
%token <string> STR
/* これは、数字には int 型の値が伴うことを示している */
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%right LAM
%right ELSE DOT
%left APP
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| VAR
        { Syntax.Var ($1) }
| TRUE
	{ Syntax.True }
| FALSE
	{ Syntax.False }
| LPAREN expr RPAREN
	{ $2 }

expr:
| simple_expr
	{ $1 }
| LET expr BE VAR DOT expr
        { Syntax.Let ($2, $4, $6) }
| IF expr THEN expr ELSE expr
        { Syntax.If ($2, $4, $6) }
| INL expr
        { Syntax.Inl ($2) }
| INR expr
        { Syntax.Inr ($2) }
| PM expr AS LBRACE INL VAR DOT expr COMMA INR VAR DOT expr RBRACE
        { Syntax.Pm ($2, $6, $8, $11, $13) }
| LAM VAR DOT expr
        { Syntax.Lam ($2, $4) }
| expr APP expr
        { Syntax.App ($1, $3) }
| PRINT VAR DOT expr
        { Syntax.Print ($2, $4) }
| PRINT STR DOT expr
        { Syntax.Print ($2, $4) }
