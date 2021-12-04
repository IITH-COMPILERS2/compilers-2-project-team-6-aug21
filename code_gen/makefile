OBJS = ast.cmo parser.cmo lexer.cmo semantic.cmo gen_python.cmo main.cmo 

gen_python: $(OBJS)
	ocamlc str.cma -o gen_python $(OBJS)

lexer.ml:lexer.mll
	ocamllex lexer.mll
parser.ml parser.mli:parser.mly
	ocamlyacc -v $<
%.cmo : %.ml
	ocamlc -c $<
%.cmi : %.mli 
	ocamlc -c $<
# parser.cmo:parser.cmi 
ast.cmo : ast.cmi
ast.cmx : ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmx parser.cmi
lexer.cmo : parser.cmi
lexer.cmx : parser.cmx
semantic.cmo : ast.cmi
semantic.cmx : ast.cmx
ast.cmi :
parser.cmi : ast.cmi
gen_python.cmo : ast.cmi
gen_python.cmx : ast.cmx

.PHONY : clean
clean:
	rm -rf parser.mli lexer.ml parser.ml *.cmi *.cmo gen_python parser.output
	

