OBJS = parser.cmo lexer.cmo main.cmo 

*.txt	: parser
	mkdir outputs && touch outputs/output1.txt
	./parser < testCases/test1.am > outputs/output1.txt
	
	touch outputs/output2.txt
	./parser < testCases/test2.am > outputs/output2.txt
	
	touch outputs/output3.txt
	./parser < testCases/test3.am > outputs/output3.txt
	
	touch outputs/output4.txt
	./parser < testCases/test4.am > outputs/output4.txt
	
	touch outputs/output5.txt
	./parser < testCases/test5.am > outputs/output5.txt
	
	touch outputs/output6.txt
	./parser < testCases/test6.am > outputs/output6.txt
parser : $(OBJS) # $(cmd)
	ocamlc -o parser lexer.cmo parser.cmo main.cmo
lexer.ml:lexer.mll
	ocamllex lexer.mll
parser.ml parser.mli:parser.mly
	ocamlyacc $<
%.cmo : %.ml
	ocamlc -c $<
%.cmi : %.mli 
	ocamlc -c $<
parser.cmo:parser.cmi 


clean:
	rm -rf parser.mli lexer.ml parser.ml *.cmi *.cmo parser
	rm -rf outputs
