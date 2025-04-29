%{
#include <stdio.h>
char *token_name(int token);
%}

IDENTIFIER   [A-Za-z_][A-Za-z0-9_]*
NUMBER       [0-9]+(\.[0-9]+)?
WHITESPACE   [ \t\r\n]+

%%

PROGRAM              { printf("TOKEN: PROGRAM\n"); }
END                  { printf("TOKEN: END\n"); }
INTEGER              { printf("TOKEN: INTEGER\n"); }
REAL                 { printf("TOKEN: REAL\n"); }
{IDENTIFIER}         { printf("TOKEN: IDENTIFIER (%s)\n", yytext); }
{NUMBER}             { printf("TOKEN: NUMBER (%s)\n", yytext); }
[=+\-*/(),]          { printf("TOKEN: SYMBOL (%s)\n", yytext); }
{WHITESPACE}         ; // ignorar espacios
.                    { printf("TOKEN: UNKNOWN (%s)\n", yytext); }

%%

int yywrap() { return 1; }
