#include <stdio.h>

extern int yylex();
extern char *yytext;
extern FILE *yyin;

int main(int argc, char *argv[]) {
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            perror("Error opening file");
            return 1;
        }
    }

    while (yylex() != 0) {
        // Tokens ya impresos en el lexer
    }

    return 0;
}
