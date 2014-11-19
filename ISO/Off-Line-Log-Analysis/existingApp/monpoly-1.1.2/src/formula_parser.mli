type token =
  | LPA
  | RPA
  | LSB
  | RSB
  | COM
  | DOT
  | QM
  | EQ
  | LESS
  | INF
  | STR of (string)
  | NUM of (float)
  | TU of (int*char)
  | CARD
  | NOT
  | AND
  | OR
  | IMPL
  | EQUIV
  | EX
  | FA
  | PREV
  | NEXT
  | EVENTUALLY
  | ONCE
  | ALWAYS
  | PAST_ALWAYS
  | SINCE
  | UNTIL
  | END
  | EOF

val formula :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> MFOTL.formula
