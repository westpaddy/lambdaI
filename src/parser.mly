%{
open Term
%}

%token LET DEF FUN RARROW SEMISEMI LPAREN RPAREN
%token<string> LIDENT

%start<Term.var option * Term.t> top_phrase

%%

top_phrase:
  LET; x = LIDENT; DEF; e = expr; SEMISEMI;
    { (Some x, e) }
| e = expr; SEMISEMI;
    { (None, e) }
;;

expr:
  FUN; params = nonempty_list(LIDENT); RARROW; e = expr;
    { List.fold_right (fun x e -> Abs (x, e)) params e }
| e = app_expr;
    { e }
;;

app_expr:
  e1 = app_expr; e2 = a_expr;
    { App (e1, e2) }
| e = a_expr;
    { e }
;;

a_expr:
  LPAREN; e = expr; RPAREN;
    { e }
| x = LIDENT;
    { Var x }
;;
