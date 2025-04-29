#include <stdio.h>

int yyparse(void);

int main(int argc, char *argv[]) {
    if (argc > 1) {
        FILE *in = fopen(argv[1], "r");
        if (!in) {
            perror("File open error");
            return 1;
        }
        extern FILE *yyin;
        yyin = in;
    }
    return yyparse();
}
