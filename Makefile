MODULES := todolib cmdline cgi todobot
B := build

OCAML_FLAGS := $(patsubst %,-I %,$(MODULES))
C_FLAGS := -fPIC -I$(shell ocamlc -where)
CPLUS_FLAGS := -fPIC
SOEXT = .so
#ifeq ($(shell test -d /opt/local/lib/ocaml && echo yes),yes)
#C_FLAGS += -I/opt/local/lib/ocaml
#SOEXT = .dylib
#endif

GENERATED := *~ */*~

OCAML_FILES = $(patsubst %,%/*.ml*,$(MODULES))
blah:
	@echo $(OSTYPE)

all: $(MODULES)

clean: cleancgi
	rm -f $(GENERATED)
	rm -rf $(B)

$(B):
	@if [ \! -d $(B) ]; then \
		echo mkdir $(B); \
		mkdir $(B); \
	fi

include $(patsubst %,%/module.mk,$(MODULES))

include depend.mk

depend.mk: $(OCAML_FILES)
	ocamldep $(OCAML_FLAGS) $(OCAML_FILES) >$@

.PHONY: clean all
.SUFFIXES: .cmo .cmi .ml .mli .cmx

.ml.cmo:
	ocamlc $(OCAML_FLAGS) -c $<

.mli.cmi:
	ocamlc $(OCAML_FLAGS) -c $<

.ml.cmx:
	ocamlopt $(OCAML_FLAGS) -c $<

.PHONY: clean cleancgi all $(MODULES)
