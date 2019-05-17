%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token BOOL ARROW PLUS
/* これは、数字には int 型の値が伴うことを示している */
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Input.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%left PLUS
%right ARROW
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| BOOL
        { Input.IBool }
| LPAREN expr RPAREN
	{ $2 }

expr:
| simple_expr
	{ $1 }
| expr ARROW expr
        { Input.IFun ($1, $3) }
| expr PLUS expr
        { Input.IPlus ($1, $3) }
