todolib_mod := todolib
todolib_src := $(todolib_mod)/date.ml $(todolib_mod)/task.ml $(todolib_mod)/command.ml
todolib_files := $(todolib_src) $(todolib_src:.ml=.mli)
todolib_cmi := $(todolib_src:.ml=.cmi)
todolib_objs := $(todolib_src:.ml=.cmo)
todolib_opts := $(todolib_src:.ml=.cmx)

OCAML_SRC += $(todolib_src)
GENERATED += $(todolib_cmi) $(todolib_objs) $(todolib_opts) $(todolib_src:.ml=.o)

$(todolib_mod): $(B)/todolib.cma $(B)/todolib.cmxa

$(B)/todolib.cma $(B)/todolib.cmxa $(B)/libtodolib.a $(B)/todolib.a: $(todolib_objs) $(todolib_opts)
	@if [ ! -d $(B) ]; then echo mkdir $(B); mkdir $(B); fi
	ocamlmklib -custom -o todolib -L$(B) $(OCAMLMKLIB_FLAGS) $(OCAMLC_LIBS) $(todolib_objs) $(todolib_opts) $(OCAMLMKLIB_LIBS)
	mv -f todolib.cmxa todolib.cma todolib.a $(B)
	ln -fs todolib.a $(B)/libtodolib.a
