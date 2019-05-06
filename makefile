all:
	ocamlc -c a6_exp.ml
	ocamllex a6_l.mll       # generates lexer.ml
	ocamlyacc a6_p.mly     # generates parser.ml and parser.mli
	ocamlc -c a6_p.mli
	ocamlc -c a6_l.ml
	ocamlc -c a6_p.ml
	ocamlc -c a6_eval.ml
	ocamlc -o calculate a6_exp.cmo a6_l.cmo a6_p.cmo a6_eval.cmo

clean:
	rm calculate *.cmo *.cmi *.mli a6_l.ml a6_p.ml
