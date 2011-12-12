cmdline_mod := cmdline
cmdline_src := $(cmdline_mod)/todo.ml
OCAMLC_LIBS += unix.cma str.cma
OCAMLOPT_LIBS += unix.cmxa str.cmxa
ocamlmklib_gen := readline.cma readline.cmxa libreadline.a readline.a dllreadline.so
ocamlmklib_gen_moved = $(patsubst %,$(cmdline_mod)/%,$(ocamlmklib_gen))
GENERATED += $(cmdline_src:.ml=.cmx) $(cmdline_src:.ml=.cmi) $(cmdline_src:.ml=.o) $(cmdline_src:.ml=.cmo) \
	$(ocamlmklib_gen_moved) $(cmdline_mod)/readline.[cmo]* $(cmdline_mod)/readline_stubs.*
ocamlmklib_command = ocamlmklib -o readline -l:libreadline$(SOEXT) -L$(cmdline_mod) $^

# annoying hackery indeed - special case for MacOS X
ifeq ($(shell test -d /opt/local/lib/ocaml && echo yes),yes)
ocamlmklib_command = ocamlmklib -o readline -lreadline -L$(cmdline_mod) $^
endif

# special cases for cygwin
ifeq ($(OSTYPE),cygwin)
ocamlmklib_gen := readline.cma readline.cmxa libreadline.a readline.a
ocamlmklib_command = ocamlmklib -o readline -lreadline.dll -L$(cmdline_mod) $^
endif

$(cmdline_mod): $(B)/todoopt $(B)/todo

$(B)/todo: $(B)/todolib.cma $(cmdline_mod)/libreadline.a $(cmdline_mod)/readline.cma $(cmdline_src)
	ocamlc -o $@ $(OCAML_FLAGS) $(OCAMLC_LIBS) $^

$(B)/todoopt: $(B)/todolib.cmxa $(cmdline_mod)/libreadline.a $(cmdline_mod)/readline.cmxa $(cmdline_src)
	ocamlopt -o $@ $(OCAML_FLAGS) $(OCAMLOPT_LIBS) $^

$(ocamlmklib_gen_moved): $(cmdline_mod)/readline_stubs.o $(cmdline_mod)/readline.cmo $(cmdline_mod)/readline.cmx
	$(ocamlmklib_command)
	mv -f $(ocamlmklib_gen) $(cmdline_mod)

$(cmdline_mod)/readline_stubs.o: $(cmdline_mod)/readline_stubs.c
	gcc $(C_FLAGS) -o $@ -c $<

$(cmdline_mod)/readline_stubs.c $(cmdline_mod)/readline.ml $(cmdline_mod)/readline.mli: $(cmdline_mod)/readline.idl
	camlidl -no-include $<
