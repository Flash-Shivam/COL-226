%{
    open A5
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ 
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF TINT TUNIT TBOOL ARROW COLON CMP
%start main 
%type <A5.expr> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
	anda				{ $1 }
	|anda EOF			{ $1 }
	;
		
anda:
	anda DISJ oro		{ And($1,$3) }
	|oro				{ $1 }
;
oro:
	oro CONJ nawt	{ Or($1,$3) }
	|nawt				{ $1 }
;
nawt:
	NOT nawt 			{ Not($2) }
	|CMP nawt                         { Cmp($2) }
	|comp				{ $1 }
;
comp:
	sub				{ $1 }	
;
sub:
	sub MINUS mults		{ Sub ($1,$3) }
	|sub PLUS mults		{ Plus ($1,$3) }
	|mults 					{ $1 }
;
mults:					
	mults DIV lasts			{ Div ($1,$3) }
	|mults TIMES lasts 		{ Mult ($1,$3) }
	|lasts					{ $1 }
;


lasts:
								
	|IF main THEN main ELSE main FI					{ If_Then_Else($2,$4,$6) } 
	|const											{ $1 }
;


const:
	ID											{ V($1) }
	|INT										{ Integer($1) }
	|BOOL										{ Bool($1) }
	| main LP main RP						{ App($1,$3)}
	|LP main RP									{ InParen($2) }
	|BACKSLASH main DOT main   				{Lambda($2,$4)}
;
