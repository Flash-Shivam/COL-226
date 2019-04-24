%{
    open A1
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
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF TINT TUNIT TBOOL ARROW COLON 
%start main de
%type <A1.definition> de 
%type <A1.exptree> main /* Return type */
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
	anda DISJ oro		{ Disjunction($1,$3) }
	|oro				{ $1 }
;
oro:
	oro CONJ nawt	{ Conjunction($1,$3) }
	|nawt				{ $1 }
;
nawt:
	NOT nawt 			{ Not($2) }
	|comp				{ $1 }
;
comp:
	comp GT sub			{ GreaterT ($1,$3) }
	|comp GT EQ sub 	{ GreaterTE($1,$4) }
	|comp LT EQ sub		{ LessTE ($1,$4) }
	|comp LT sub		{ LessT ($1,$3) }
	|comp EQ sub		{ Equals ($1,$3) } 
	|sub				{ $1 }	
;
sub:
	sub MINUS mults		{ Sub ($1,$3) }
	|sub PLUS mults		{ Add ($1,$3) }
	|mults 					{ $1 }
;
mults:					
	mults DIV negates			{ Div ($1,$3) }
	|mults TIMES negates 		{ Mult ($1,$3) }
	|mults REM negates			{ Rem ($1,$3) }
	|negates					{ $1 }
;

negates:
	|TILDA negates 		{ Negative($2) }
	|ABS negates		{ Abs($2) }
	|lasts				{ $1 }
;
lasts:
	LET de IN main END								{Let($2,$4)}
	|IF main THEN main ELSE main FI					{ IfThenElse($2,$4,$6) }
	|PROJ LP INT COMMA INT RP main					{ Project(($3,$5),$7) }
	|tupl											{ $1 } 
	|const											{ $1 }
;


terminals:
	|DEF ID COLON TINT EQ main 				{ Simple($2,(Tint),$6) }
	|DEF ID COLON TBOOL EQ main 				{ Simple($2,(Tbool),$6) }
	|DEF ID COLON TUNIT EQ main 				{ Simple($2,(Tunit),$6) }
	|DEF ID COLON TINT ARROW type_exp EQ main 				{ Simple($2,(Tfunc(Tint,$6)),$8) }
	|DEF ID COLON TBOOL ARROW type_exp  EQ main 			{ Simple($2,Tfunc(Tbool,$6),$8) }
	|DEF ID COLON TUNIT ARROW type_exp EQ main 				{ Simple($2,Tfunc(Tunit,$6),$8) }
	|LOCAL de IN de END						{ Local($2,$4) }
;



type_exp :
| TINT		  {Tint}
| TBOOL           {Tbool}
| TUNIT           {Tunit}
| LP type_exp TIMES type_expp RP   { Ttuple(($2)::($4)) } 
;;

type_expp:
	type_exp TIMES type_expp				{ [$1]@($3) }
	|type_exp							{ [$1] }
;


de:
	terminals								{$1}
	|de SEMICOLON terminals				{ match $1 with
										Sequence(a) -> Sequence(a@[$3])
										|_ -> Sequence([$1;$3])}
	|de PARALLEL terminals				{ match $1 with
										Parallel(a) -> Parallel(a@[$3])
										|_ -> Parallel([$1;$3])}
;
tupl:
	LP main COMMA maintemp RP		{ Tuple(List.length($4)+1,(($2)::($4))) } 
;
maintemp:
	main COMMA maintemp				{ [$1]@($3) }
	|main							{ [$1] }
;
const:
	ID											{ Var($1) }
	|INT										{ N($1) }
	|BOOL										{ B($1) }
	|LP main RP									{ InParen($2) }
	|LP main RP LP main RP						{ FunctionCall($2,$5)}
	|BACKSLASH ID COLON TINT DOT main   				{FunctionAbstraction($2,Tint,$6)}
	|BACKSLASH ID COLON TBOOL DOT main 				{FunctionAbstraction($2,Tbool,$6)}
	|BACKSLASH ID COLON TUNIT DOT main   				{FunctionAbstraction($2,Tunit,$6)}
;
