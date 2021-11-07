OBJS = ast.cmo parser.cmo lexer.cmo semantic.cmo main.cmo 
*.txt	: semantic
	mkdir outputs && touch outputs/output1.txt
	./semantic < testCases/test1.am > outputs/output1.txt
	
	touch outputs/output2.txt
	./semantic < testCases/test2.am > outputs/output2.txt
	
	touch outputs/output3.txt
	./semantic < testCases/test3.am > outputs/output3.txt
	
	touch outputs/output4.txt
	./semantic < testCases/test4.am > outputs/output4.txt
	
	touch outputs/output5.txt
	./semantic < testCases/test5.am > outputs/output5.txt
	
semantic: $(OBJS)
	ocamlc str.cma -o semantic $(OBJS)

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

.PHONY : clean
clean:
	rm -rf parser.mli lexer.ml parser.ml *.cmi *.cmo semantic.output semantic parser.output
	rm -rf outputs

# ocamlyacc parser.mly  	parser.mli rpcal.ml
# ocamlc -c parser.mli   	parser.cmi
# ocamlc -c parser.ml  		parser.cmo
# ocamllex lexer.mll    	lexer.ml
# ocamlc -c lexer.ml 		lexer.cmi lexer.cmo
