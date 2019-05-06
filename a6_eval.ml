open A6_exp 

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
let _ =
    Printf.printf "==> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let result = A6_p.main A6_l.token lexbuf in
             A6_exp.check result ; Printf.printf "\n==> "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
            done
        with A6_l.Eof ->
            exit 0
