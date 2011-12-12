all:
	ocamllex simpleconfig_lexer.mll
	ocamlyacc simpleconfig_parser.mly
	ocamlc -c simpleconfig_parser.mli
	ocamlc -I +pcre -c simpleconfig_lexer.ml
	ocamlc -c simpleconfig_parser.ml

debug:
	OCAMLRUNPARAM="p" ledit ocaml -I +pcre pcre.cma simpleconfig_*.cmo

run:
	ledit ocaml -I +pcre pcre.cma simpleconfig_*.cmo
