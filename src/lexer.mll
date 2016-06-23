{
  let keywords = [
    ("let", Parser.LET);
    ("fun", Parser.FUN);
  ]

  exception EOF
}

rule token = parse
    [' ' '\t' '\n']+ { token lexbuf }
  | eof { raise EOF }
  | '(' { Parser.LPAREN }
  | ')' { Parser.RPAREN }
  | ":=" { Parser.DEF }
  | "->" { Parser.RARROW }
  | ";;" { Parser.SEMISEMI }
  | ['a'-'z'] [''' '_' 'a'-'z' 'A'-'Z' '0'-'9']* as id
    { try List.assoc id keywords with Not_found -> Parser.LIDENT id }
