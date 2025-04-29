%{
#include <stdio.h>
void yyerror(const char *s);
int yylex(void);
%}

%token PROGRAM END INTEGER REAL IDENTIFIER NUMBER SYMBOL UNKNOWN

%%

input:
    | input line
    ;

line:
    token_list '\n'
    ;

token_list:
    token
    | token_list token
    ;

token:
    PROGRAM    { printf("TOKEN: PROGRAM\n"); }
    | END      { printf("TOKEN: END\n"); }
    | INTEGER  { printf("TOKEN: INTEGER\n"); }
    | REAL     { printf("TOKEN: REAL\n"); }
    | IDENTIFIER { printf("TOKEN: IDENTIFIER (%s)\n", yytext); }
    | NUMBER   { printf("TOKEN: NUMBER (%s)\n", yytext); }
    | SYMBOL   { printf("TOKEN: SYMBOL (%s)\n", yytext); }
    | UNKNOWN  { printf("TOKEN: UNKNOWN (%s)\n", yytext); }
    ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Parse error: %s\n", s);
}
