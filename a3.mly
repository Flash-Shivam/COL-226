%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis33
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
disc_exp EOF                 { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
;

disc_exp:
disc_exp DISJ conj_exp {Disjunction($1,$3)}
| conj_exp {$1}
;

conj_exp:
conj_exp CONJ not_exp {Conjunction($1,$3)}
| not_exp {$1}
;

not_exp:
| NOT not_exp {Not($2)}
| NOT comparison_exp {Not($2)}
| comparison_exp {$1}

comparison_exp:
| comparison_exp EQ dp_exp {Equals($1,$3)}
| comparison_exp LT dp_exp {LessT($1,$3)}
| comparison_exp LT EQ dp_exp {LessTE($1,$4)}
| comparison_exp GT dp_exp {GreaterT($1,$3)}
| comparison_exp GT EQ dp_exp {GreaterTE($1,$4)}
| dp_exp {$1}
;
dp_exp:
| dp_exp PLUS drm_exp {Add($1,$3)}
| dp_exp MINUS drm_exp {Sub($1,$3)}
| drm_exp {$1}
;
drm_exp:
| drm_exp TIMES abs_exp {Mult($1,$3)}
| drm_exp DIV abs_exp   {Div($1,$3)}
| drm_exp REM abs_exp   {Rem($1,$3)}
| abs_exp {$1}
;
abs_exp:
| ABS abs_exp {Abs($2)}
| neg_exp {$1}
;
neg_exp:
| TILDA ifte_exp {Negative($2)}
| ifte_exp {$1}
;
ifte_exp:
| IF disc_exp THEN disc_exp ELSE disc_exp FI {IfThenElse($2,$4,$6)}
| project_exp {$1}
;
project_exp: 
| PROJ LP INT COMMA INT RP project_exp {Project(($3,$5),$7)}
| tuple_exp {$1}
;
tuple_exp: 
| LP disc_exp comma_exp RP  {Tuple((List.length ($3))+1,$2::$3)}
| LP RP  {Tuple(0,[])}
| inparen_exp {$1} 
;

comma_exp:
comma_exp COMMA disc_exp {$1@[$3]}
| COMMA disc_exp  {[$2]}
;

inparen_exp:
| LP disc_exp RP {InParen($2)}
| constant {$1}
;
constant:
ID                                 { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
| INT                              { N($1) }		
| BOOL                             {B($1)}    /* To be interpreted as an integer with its value as tokenised   */
;
