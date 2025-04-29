all:
	bison -d parser.y
	flex lexer.l
	gcc parser.tab.c lex.yy.c main.c -o parser

clean:
	-@rm -f *.exe *.o *.obj *.mod *.tab.* lex.yy.c 
