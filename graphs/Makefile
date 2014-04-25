.PHONY: clean allclean test export
OCAMLC = ocamlopt.opt -thread -I /usr/lib/ocaml/camlimages graphics.cmxa camlimages_core.cmxa camlimages_graphics.cmxa unix.cmxa threads.cmxa 

csmc: csmc.cmx parser.cmx lexer.cmx graphs.cmx main.ml
	$(OCAMLC) $^ -o $@

test: csmc
	./csmc example-contaminated-area/contaminated-area.csmc example-contaminated-area/contaminated-area

test_paper: test	
	cp example-contaminated-area/contaminated-area.csmc example-contaminated-area/contaminated-area-*-out.tex ../paper/figures
	cd ../paper && pdflatex dmc-main

interactive-test: csmc
	rlfe ./csmc example-contaminated-area/contaminated-area.csmc

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
	rm -f *.cm* a.out *~ \#* *.o example-contaminated-area/*-out*.tex example-contaminated-area/*~ example-contaminated-area/\#*

clean: almostclean 
	rm -f csmc parser.ml lexer.ml parser.mli
