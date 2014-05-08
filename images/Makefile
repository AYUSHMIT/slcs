.PHONY: clean allclean test export
OCAMLC = ocamlopt.opt -thread -I /usr/lib/ocaml/camlimages graphics.cmxa camlimages_core.cmxa camlimages_graphics.cmxa unix.cmxa threads.cmxa 

csmc: csmc.cmx parser.cmx lexer.cmx image.cmx main.ml
	$(OCAMLC) $^ -o $@

test: csmc example-map/openstreetmap-pisa_output.bmp example-maze/maze-out.bmp

example-map/openstreetmap-pisa_output.bmp: example-map/openstreetmap-pisa.bmp example-map/openstreetmap-pisa.csmc
	./csmc example-map/openstreetmap-pisa.bmp example-map/openstreetmap-pisa.csmc example-map/openstreetmap-pisa_output.bmp

example-maze/maze-out.bmp: example-maze/maze.bmp example-maze/maze.csmc
	./csmc example-maze/maze.bmp example-maze/maze.csmc example-maze/maze-out.bmp

test_paper: test
	for f in example-map/openstreetmap-pisa*.bmp example-maze/maze*.bmp; do convert $$f ../../paper/figures/$$(basename $$f .bmp).pdf; done 
	cp example-map/openstreetmap-pisa.csmc ../../paper/figures
	cp example-maze/maze.csmc ../../paper/figures

interactive-test: csmc
	rlfe ./csmc example-map/openstreetmap-pisa.bmp example-map/openstreetmap-pisa.csmc || ./csmc example-map/openstreetmap-pisa.bmp example-map/openstreetmap-pisa.csmc

parser.cmx: parser.ml
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c parser.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

%.ml: %.mly
	ocamlyacc $^

export: csmc almostclean

%.cmx: %.ml
	$(OCAMLC) -c $^ -o $@

almostclean: 
	rm -f *.cm* a.out *~ \#* *.o example-map/*_output.bmp example-map/*~ example-map/\#*

clean: almostclean 
	rm -f csmc parser.ml lexer.ml parser.mli
