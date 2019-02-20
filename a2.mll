(*example scanner "043"   token list = [INT 0; INT 43]
scanner "+5";;
- : token list = [INT 5]
# scanner "0 + 5 ";;
- : token list = [INT 0; PLUS; INT 5]
# scanner "def12";;
- : token list = [ID "def"; INT 12]
# scanner "def 12";;
- : token list = [DEF; INT 12]
# scanner "Not";;
Exception: InvalidTokenstartingwith 'N'.
# scanner "-5";;
- : token list = [INT (-5)]
# scanner "3-2";;
- : token list = [INT 3; INT (-2)]
# scanner "3 - 2";;
- : token list = [INT 3; MINUS; INT 2]
# scanner "True";;
- : token list = [TRUE; ID "rue"]
# scanner "ais#";;
Exception: InvalidTokenstartingwith '#'.
 *)




{
  type token  = INT of int | AND | OR | EXP | TRUE | FALSE | NOT| EQ | GTA | GEQ | LTA | LEQ | ABS | RP | LP | DIV | MOD | MUL | ID of string | IF | THEN | ELSE | DELIMITER | PLUS | MINUS | DEF
  type passwd = Password of string
  exception InvalidTokenstartingwith of char ;;
}
let digit = ['0'-'9']
let fi = ['1'- '9']
let digits = digit*
let if = "if "
let else = "else "
let then = "then "
let and_ = "/\\"
let or_ = "\\/"
let nott = "not "
let abd = "abs "
let mod  = "mod "
let ex = ['^']
let t = ['T']
let f = ['F']
let m = ['*']
let e = ['=']
let gt = ['>']
let lt = ['<']
let geq = ">="
let leq = "<="
let d = "div "
let letter = ['a'-'z' 'A'-'Z']
let alpha = ['a'-'z'] letter*
let lpar = ['(']
let rpar = [')']

let integer = (['-''+']? fi digits)|(['-''+']? ['0'])
let id = letter+
let spl_char = ['*' '.' '$']
let letter_or_spl_char = letter|spl_char

rule

rd = parse
eof   {[]}
|integer as n  {(INT (int_of_string n))::(rd lexbuf)}
| '+' {PLUS::(rd lexbuf)}
| m {MUL::(rd lexbuf)}
| '-' {MINUS::(rd lexbuf)}
| mod {MOD::(rd lexbuf)}
| d  {DIV::(rd lexbuf)}
|gt {GTA::(rd lexbuf)}
| geq {GEQ::(rd lexbuf)}
|lt {LTA::(rd lexbuf)}
|leq {LEQ::(rd lexbuf)}
| e {EQ::(rd lexbuf)}
| t {TRUE::(rd lexbuf)}
| f {FALSE::(rd lexbuf)}
|abd {ABS::(rd lexbuf)}
| nott {NOT::(rd lexbuf)}
|lpar {LP::(rd lexbuf)}
|rpar {RP::(rd lexbuf)}
| ex {EXP::(rd lexbuf)}
| and_  {AND::(rd lexbuf)}
|alpha as s {(ID s)::(rd lexbuf)}
| or_  {OR::(rd lexbuf)}
| if {IF::(rd lexbuf)}
| else {ELSE::(rd lexbuf)}
| ';' {DELIMITER::(rd lexbuf)}
| then {THEN::(rd lexbuf)}
|" " {rd lexbuf}
| "def " {DEF::(rd lexbuf)}
| _ as u {raise (InvalidTokenstartingwith u)}

{

  let scanner s = rd (Lexing.from_string s)

}
