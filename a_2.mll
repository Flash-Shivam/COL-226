{
  open A_3
  exception InvalidTokenstartingwith of char ;;
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a3.ml)
*)
let digit = ['0'-'9']
let fi = ['1'- '9']
let digits = digit*
let if = "if"
let else = "else"
let then = "then"
let and_ = "/\\"
let or_ = "\\/"
let nott = "not"
let abd = "abs"
let mod  = "mod"
let ex = ['^']
let t = ['T']
let f = ['F']
let m = ['*']
let e = ['=']
let gt = ['>']
let lt = ['<']
let geq = ">="
let leq = "<="
let d = "div"
let ud = ['_']
let ap = ['\'']
let letter = ['a'-'z' 'A'-'Z']
let alpha = ['A'-'Z'] (letter*|digits|ud|ap)*
let lpar = ['(']
let rpar = [')']
let comma = [',']
let finish = "fi"
let til = ['~']
let proj = "proj"
let def = "def"
let local = "local"
let lett = "let"
let dot = '.'
let semi = ';'
let endd = "end"
let integer = ( fi digits)|( ['0'])
let cmp = "cmp"
let id = letter+
let spl_char = ['*' '.' '$']
let letter_or_spl_char = letter|spl_char


rule read = parse
eof   {EOF}
|integer as n  {(INT (int_of_string n))}
| '+' {PLUS}
| m {TIMES}
| ':' {COLON}
| '-' {MINUS}
| d  {DIV}
| e {EQ}
| t  {(BOOL true)}
| f {(BOOL false)}
| nott {NOT}
| dot {DOT}
|"\\" { BACKSLASH }
|lpar {LP}
|rpar {RP}
| and_  {CONJ}
| or_  {DISJ}
| if {IF}
| cmp {CMP}
| else {ELSE}
| then {THEN}
|" " {read lexbuf}
| finish {FI}
| comma {COMMA}
|alpha as s {(ID s)}
| _ as u {raise (InvalidTokenstartingwith u)}
