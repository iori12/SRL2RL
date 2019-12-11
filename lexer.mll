{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''

rule token = parse
  (* 定数 *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      CONST (int_of_string str) }

  (* 演算子 *)
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '^'       { CARET }
  | '*'       { ASTERISK }
  | '/'       { SLASH }
  | '='       { EQUAL }
  | '<'       { LESS }
  | '>'	      { GREATER }
  | "<="      { BELOW  }
  | ">="      { ABOVE }
  | "!="      { NOT }

  (* 括弧 *)
  | '['	      { LBRA }
  | ']'       { RBRA }

  (* キーワード *)
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "fi"      { FI }
  | "from"    { FROM }
  | "do"      { DO }
  | "loop"    { LOOP }
  | "until"   { UNTIL }
  | "push"    { PUSH }
  | "pop"     { POP }
  | "skip"    { SKIP }
  | "top"     { TOP }
  | "empty"   { EMPTY }

  (* 変数 *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }

  (* 制御文字 *)
  | ';'	      { SEMI }

  (* スペース *)
  | space+    { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }