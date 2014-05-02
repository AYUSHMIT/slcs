.PHONY: clean allclean test export
OCAMLC = ocamlopt.opt -thread -I /usr/lib/ocaml/camlimages graphics.cmxa camlimages_core.cmxa camlimages_graphics.cmxa unix.cmxa threads.cmxa 

csmc: csmc.cmx parser.cmx lexer.cmx graphs.cmx main.ml
	$(OCAMLC) $^ -o $@

test: csmc
	./csmc example-maze/maze.csmc example-maze/maze

test_paper: test	
	cp example-maze/maze.csmc example-maze/maze-*-out.tex ../../paper/figures

interactive-test: csmc
	rlfe ./csmc example-maze/maze.csmc

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
	rm -f *.cm* a.out *~ \#* *.o example-maze/*-out*.tex example-maze/*~ example-maze/\#*

clean: almostclean 
	rm -f csmc parser.ml lexer.ml parser.mli
