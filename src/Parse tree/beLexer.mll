{
  open List
  open BeParser
  exception HALT
}

rule token = parse
  | ['0'-'9']* as x
                  { Int(int_of_string x) }
  | ['0'-'9'](['0'-'9']*)['.']['0'-'9']* as f
                  { Float(float_of_string f) }
  | [' ' '\t' '\n']    { token lexbuf } (* skip spaces *)
  | "T"             { TRUE }
  | "F"             { FALSE }
  | "("                { LPAREN }
  | ")"                { RPAREN }
  | ":-"               { TURNSTILE }
  | "+"                { PLUS }
  | "-"                { MINUS }
  | "*"                { TIMES }
  | "div"                { DIV }
  | "mod"                { MOD }
  | "^"                { EXPO }
  | "="                { EQUAL }
  | ">"                { GTR }
  | "<"                { LSR }
  | ">="                { GTE }
  | "<="                { LTE }
  | ","                { COMMA }
  | ";"                { SEMICOLON }
  | "."                { PERIOD }
  | "["                { LSQR }  
  | "]"                { RSQR }
  | "|"                { BAR }
  | "!"                { CUT }
  | "fail"             { FAIL }
  | "[halt]."          { raise HALT }
  | ['a'-'z' ''' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str
                       { String(str) }
  | ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str
                       { Var(str) }
  | _ as chr           { failwith ("lex error: "^(Char.escaped chr))}
  | eof                { EOF }
