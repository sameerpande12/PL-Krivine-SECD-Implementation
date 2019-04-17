all:
	ocamlc -c a0.mli
	ocamlc -c a0.ml
	ocamlc -c krivine.mli
	ocamlc -c krivine.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -c typechecker.mli
	ocamlc -c typechecker.ml

clean:
	rm  *.cmo *.cmi parser.mli  lexer.ml parser.ml
