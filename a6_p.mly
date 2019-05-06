/* File parser.mly */
%{
    open A6_exp
%}

%token LP RP ADD MULT EOL EOF COLON COMMA EQ RETURN CALL DISPLAY
%token <int> INT
%token <char> FUN ID
%start main             /* the entry point */
%type <A6_exp.expr_tree> main                     /* Specifying the type to be returned for the grammar symbol main */
%%
main:
    ass_exp EOL                 { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
    | EOF                              { NULL }
;


ass_exp : 
ID COLON EQ INT               {Assign(VAR($1),NUM($4))}
| ID COLON EQ ID                  {Assign(VAR($1),VAR($4))}
| call_exp ;                      {$1}


call_exp : 
 CALL FUN LP comma_exp RP        {Call(FUNC($2),$4)}
| return_exp ;                   {$1}

comma_exp :
INT COMMA ID              			 { Tup(NUM($1),VAR($3)) }
| ID COMMA ID			   		 { Tup(VAR($1),VAR($3)) }
| INT COMMA INT					 { Tup(NUM($1),NUM($3)) }
| ID COMMA INT                                   { Tup(VAR($1),NUM($3)) }
;
return_exp :
RETURN ;               {RETURN}
| display_exp     {$1}
;

display_exp :
DISPLAY INT             { Display(NUM($2)) }
;   


/*TODO
 * Add support in the grammar for parenthesis
 *  - Adding the parenthesis should be able to change the parse tree to effectively modify precedence.
 *  E.g. 1+2*3  ==>        PLUS
 *                        /    \
 *                      NUM1   INTO
 *                            /    \
 *                         NUM 2  NUM 3
 *
 *  vs (1+2)*3  ==>        INTO
 *                        /    \
 *                     PLUS     NUM 3
 *                    /    \
 *                 NUM 1   NUM 2
 *
 * Try completing the calculator for basic arithmetic by adding division and subtraction, while respecting precedence
 * This will require changes right from the lexer.mll and parser.mly to the definition of print and evaluation functions in expression.ml
 *
 * ADVANCED
 * Try creating an expression for assigning new variables in the variable_set in the expression.ml file, so that they can be reused in a later evaluation statement.
 * E.g. myVar:=4.
 *      // Stores the integer value 4 corresponding to the string myVar in variable_set
 *
 *      myVar*3+1
 *      Answer: 13
 * */
