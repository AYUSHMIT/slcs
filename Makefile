.PHONY: clean allclean test export
OCAMLC = ocamlopt.opt -thread -I /usr/lib/ocaml/camlimages graphics.cmxa camlimages_core.cmxa camlimages_graphics.cmxa unix.cmxa threads.cmxa 

dmc: dmc.cmx parser.cmx lexer.cmx image.cmx main.ml
	$(OCAMLC) $^ -o $@

test: dmc
	./dmc example-map/openstreetmap-pisa.bmp example-map/openstreetmap-pisa.dmc example-map/output.bmp

interactive-test: dmc
	rlfe ./dmc example-map/openstreetmap-pisa.bmp || ./dmc example-map/openstreetmap-pisa.bmp

parser.cmx: parser.ml
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c parser.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

%.ml: %.mly
	ocamlyacc -v $^

export: dmc almostclean

%.cmx: %.ml
	$(OCAMLC) -c $^ -o $@

almostclean: 
	rm -f *.cm* a.out *~ \#* *.o example-map/output*.bmp example-map/*~ example-map/\#*

clean: almostclean 
	rm -f dmc parser.ml lexer.ml parser.mli
