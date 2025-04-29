%{
#include <stdio.h>
#include <stdlib.h>
extern char *yytext;
void yyerror(const char *s);
int yylex(void);
%}

%token PROGRAM END INTEGER REAL IDENTIFIER NUMBER SYMBOL UNKNOWN

%%

programa:
    PROGRAM IDENTIFIER declaraciones END { printf("Programa válido.\n"); }
    ;

declaraciones:
    /* vacío */
    | declaraciones declaracion
    ;

declaracion:
    INTEGER IDENTIFIER
    | REAL IDENTIFIER
    | IDENTIFIER SYMBOL NUMBER
    ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error de sintaxis: %s (token: '%s')\n", s, yytext);
}
