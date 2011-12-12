todobot_mod := todobot
todobot_src := $(todobot_mod)/bot.ml $(todobot_mod)/todobot.ml
todobot_parsegen := $(todobot_mod)/simpleconfig_parser.ml $(todobot_mod)/simpleconfig_lexer.ml
GENERATED += $(todobot_src:.ml=.cmx) $(todobot_src:.ml=.o) $(todobot_src:.ml=.cmi) \
	$(todobot_mod)/botclient.o $(todobot_mod)/glue.o $(todobot_mod)/bot.cmxa \
	$(todobot_mod)/*.a $(todobot_mod)/dllbot.so \
	$(todobot_parsegen) $(todobot_parsegen:.ml=.mli) $(todobot_parsegen:.ml=.o) \
	$(todobot_parsegen:.ml=.cmx) $(todobot_parsegen:.ml=.cmi) $(todobot_parsegen:.ml=.cmo)
OCAML_FLAGS += -I +pcre
CPLUS_FLAGS += -fexceptions

$(todobot_mod): $(B)/todobot

$(B)/todobot: $(todobot_mod)/todobot.ml $(todobot_mod)/bot.cmxa $(B)/todolib.cmxa $(todobot_mod)/simpleconfig_lexer.cmx $(todobot_mod)/simpleconfig_parser.cmx
	ocamlopt -o $@ $(OCAML_FLAGS) $(todobot_mod)/bot.cmxa \
		unix.cmxa str.cmxa pcre.cmxa $(B)/todolib.cmxa \
		$(todobot_mod)/simpleconfig_lexer.cmx \
		$(todobot_mod)/simpleconfig_parser.cmx $<

$(todobot_mod)/bot.cmxa $(todobot_mod)/libbot.a $(todobot_mod)/bot.a $(todobot_mod)/dllbot.so: $(todobot_mod)/botclient.o $(todobot_mod)/glue.o $(todobot_mod)/bot.cmx
	ocamlmklib -o bot -L$(todobot_mod) $^ -lgloox
	mv -f bot.cmxa libbot.a bot.a dllbot.so $(todobot_mod)

$(todobot_mod)/botclient.o: $(todobot_mod)/botclient.cc $(todobot_mod)/botclient.hh
	g++ $(CPLUS_FLAGS) -I/usr/include/gloox -I$(shell ocamlc -where) -o $@ -c $<

$(todobot_mod)/glue.o: $(todobot_mod)/glue.c $(todobot_mod)/botclient.hh
	g++ $(CPLUS_FLAGS) -I/usr/include/gloox -I$(shell ocamlc -where) -o $@ -c $<

$(todobot_mod)/simpleconfig_parser.mli $(todobot_mod)/simpleconfig_parser.ml: $(todobot_mod)/simpleconfig_parser.mly
	ocamlyacc $<

$(todobot_mod)/simpleconfig_lexer.ml: $(todobot_mod)/simpleconfig_lexer.mll
	ocamllex $<

$(todobot_mod)/simpleconfig_lexer.cmx: $(todobot_mod)/simpleconfig_parser.cmi
