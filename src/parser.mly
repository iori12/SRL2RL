%{
open Syntax
%}

// リテラル
%token <string> VAR  // x, y, abc, ...
%token <int> CONST     // 0, 1, 2, ...

// 演算子
%token PLUS     // '+'
%token MINUS    // '-'
%token CARET	// '^'
%token ASTERISK // '*'
%token SLASH    // '/'
%token EQUAL    // '='
%token LESS     // '<'
%token GREATER  // '>'
%token BELOW    // "<="
%token ABOVE	// ">="
%token NOT	// "!="

// 括弧
%token LBRA     // '['
%token RBRA     // ']'

// キーワード
%token IF	// "if"
%token THEN	// "then"
%token ELSE	// "else"
%token FI	// "fi"
%token FROM	// "from"
%token DO	// "do"
%token LOOP	// "loop"
%token UNTIL	// "until"
%token PUSH	// "push"
%token POP	// "pop"
%token SKIP 	// "skip"
%token TOP	// "TOP"
%token EMPTY	// "EMPTY"

// 制御記号
%token SEMI // ';'

%start main
%type <Syntax.blk> main

%%

// 開始記号
main:
  | blk SEMI
    { $1 }
;

// 変数
var:
  | VAR
    { Var $1 }

;

// 式
exp:
  // 定数
  | CONST
    { EConst $1 }

  // 変数
  | var
    { EVar $1 }

  // x[e]
  | var LBRA exp RBRA 
    { EIn($1,$3) }

  // e1 +　e2
  | exp PLUS exp
    { ETime($1, OPlus(Plus), $3) }

  // e1 - e2
  | exp MINUS exp
    { ETime($1, OPlus(Minus), $3) }

  // e1 ^ e2
  | exp CARET exp
    { ETime($1, OPlus(Caret), $3) }

  // e1 * e2
  | exp ASTERISK exp
    { ETime($1, Time, $3) }
  
  // e1 / e2
  | exp SLASH exp
    { ETime($1, Div, $3) }
    
  // e1 = e2
  | exp EQUAL exp
    { ETime($1, Equal, $3) }
  
  // e1 < e2
  | exp LESS exp
    { ETime($1, Less, $3) }
    
  // e1 > e2
  | exp GREATER exp
    { ETime($1, Greater, $3) }

  // e1 <= e2
  | exp BELOW exp
    { ETime($1, Less_Eq, $3) } 

  // e1 >= e2
  | exp ABOVE exp
    { ETime($1, Greater_Eq, $3) }

  // e1 != e2
  | exp NOT exp
    { ETime($1, Not_Eq, $3) }

  // top x
  | TOP var
    { ETop $2 }
 
  // empty x
  | EMPTY var
    { EEmpty $2 }
;

// a:Step

step:
  // x += e 
  | var PLUS EQUAL exp
    { Plus_Eq($1, Plus, $4) }

  // x -= e
  | var MINUS EQUAL exp
    { Plus_Eq($1, Minus, $4) }

  // x ^= e
  | var CARET EQUAL exp
    { Plus_Eq($1, Caret, $4) }

  // x[e1] += e2
  | var LBRA exp RBRA PLUS EQUAL exp
    { Plus_In($1, $3, Plus, $7) }

  // x[e1] -= e2
  | var LBRA exp RBRA MINUS EQUAL exp
    { Plus_In($1, $3, Minus, $7) }

  // x[e1] ^= e2
  | var LBRA exp RBRA CARET EQUAL exp
    { Plus_In($1, $3, Caret, $7) }

  // push x x
  | PUSH var var
    { Push($2, $3) }

  // pop x x
  | POP var var
    { Pop($2, $3) }

  // skip
  | SKIP
    { Skip }
;

// b:Blk

blk:
  // step
  | step
    { SStep $1 }

  // if e1 then b1 else b2 fi e2
  | IF exp THEN blk ELSE blk FI exp
    { SIf($2, $4, $6, $8) }

  // b b
  | blk blk
    { SCon($1, $2) }

  // from e1 do b1 loop b2 until e2
  | FROM exp DO blk LOOP blk UNTIL exp
    { SFrom($2, $4, $6, $8) }

// p:srl

srl:
  // srl
  | blk
    { SBLK($1) }
