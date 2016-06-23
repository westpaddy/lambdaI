%{
open Lang
%}

%token LET DEF FUN RARROW SEMISEMI LPAREN RPAREN
%token<string> LIDENT

%start<Lang.Term.var option * Lang.Term.t> top_phrase

%%

top_phrase:
  LET; x = LIDENT; DEF; e = expr; SEMISEMI;
    { (Some x, e) }
| e = expr; SEMISEMI;
    { (None, e) }
;;

expr:
  FUN; x = LIDENT; RARROW; e = expr;
    { Term.Abs (x, e) }
| e = app_expr;
    { e }
;;

app_expr:
  e1 = app_expr; e2 = a_expr;
    { Term.App (e1, e2) }
| e = a_expr;
    { e }
;;

a_expr:
  LPAREN; e = expr; RPAREN;
    { e }
| x = LIDENT;
    { Term.Var x }
;;
