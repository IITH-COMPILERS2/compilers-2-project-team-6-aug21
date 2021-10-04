OBJS = ast.cmo parser.cmo lexer.cmo main.cmo 
*.txt	: parser
	mkdir outputs && touch outputs/output1.txt
	./parser < testCases/test1.am > outputs/output1.txt
	
	touch outputs/output2.txt
	./parser < testCases/test2.am > outputs/output2.txt
	
	touch outputs/output3.txt
	./parser < testCases/test3.am > outputs/output3.txt
	
	touch outputs/output4.txt
	./parser < testCases/test4.am > outputs/output4.txt
	
	mkdir error-outputs && touch error-outputs/output1.txt
	./parser < error-codes/test1.am > error-outputs/output1.txt
	
	touch error-outputs/output2.txt
	./parser < error-codes/test2.am > error-outputs/output2.txt
	
	touch error-outputs/output3.txt
	./parser < error-codes/test3.am > error-outputs/output3.txt
	
	touch error-outputs/output4.txt
	./parser < error-codes/test4.am > error-outputs/output4.txt
	
	
	
parser : $(OBJS) # $(cmd)
	ocamlc -o parser lexer.cmo parser.cmo main.cmo 
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
ast.cmi :
parser.cmi : ast.cmi
clean:
	rm -rf parser.mli lexer.ml parser.ml *.cmi *.cmo parser parser.output
	rm -rf outputs error-outputs


