cgi_mod := cgi
cgi_src := $(cgi_mod)/jsonml.ml $(cgi_mod)/cgi.ml
OCAML_FLAGS += -I +pcre -I +netcgi2 -I +netsys -I +netstring -I +equeue -I $(cgi_mod)/json-wheel
GENERATED += $(B)/todo.cgi $(cgi_src:.ml=.cmx) $(cgi_src:.ml=.o) $(cgi_src:.ml=.cmi)

$(cgi_mod): $(B)/todo.cgi

$(B)/todo.cgi: $(cgi_mod)/cgi.ml $(cgi_mod)/jsonml.cmx $(B)/todolib.cmxa $(cgi_mod)/json-wheel/jsonwheel.cmxa
	ocamlopt -o $@ $(OCAML_FLAGS) unix.cmxa str.cmxa \
		$(B)/todolib.cmxa pcre.cmxa netsys.cmxa netstring.cmxa \
		equeue.cmxa netcgi.cmxa jsonwheel.cmxa $(cgi_mod)/jsonml.cmx $(cgi_mod)/cgi.ml

$(cgi_mod)/json-wheel/jsonwheel.cmxa:
	make -C $(cgi_mod)/json-wheel

$(cgi_mod)/jsonml.cmx: $(cgi_mod)/json-wheel/jsonwheel.cmxa

cleancgi:
	make -C $(cgi_mod)/json-wheel clean

installcgi: $(B)/todo.cgi
	sudo install -s -o nobody -g jde $< /var/www/todo
	sudo install -m 0644 \
		$(cgi_mod)/accessories-text-editor.png \
		$(cgi_mod)/todoicon.png \
		$(cgi_mod)/prototype*.js \
		$(cgi_mod)/JsonML.js \
		$(cgi_mod)/todo.js \
		$(cgi_mod)/todo.css \
		$(cgi_mod)/todo.xhtml \
		/var/www/todo
