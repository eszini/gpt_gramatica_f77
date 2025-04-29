%{
#include "parser.tab.h"
#include <stdio.h>
%}

%%

[ \t\r\n]+            ; // Ignorar espacios y saltos de línea
"PROGRAM"             return PROGRAM;
"END"                 return END;
"INTEGER"             return INTEGER;
"REAL"                return REAL;
[A-Za-z_][A-Za-z0-9_]* return IDENTIFIER;
[0-9]+(\.[0-9]+)?     return NUMBER;
[=+\-*/(),]           return SYMBOL;
.                     { printf("Unknown token: %s\n", yytext); return UNKNOWN; }

%%

int yywrap() { return 1; }
