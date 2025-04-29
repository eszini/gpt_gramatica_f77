#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE 1024
#define MAX_TOKEN 256
#define MAX_KEYWORDS 128

const char *keywords[] = {
    "PROGRAM", "SUBROUTINE", "FUNCTION", "END", "STOP", "RETURN", "CALL", "COMMON",
    "DATA", "DIMENSION", "IMPLICIT", "CONTINUE", "GOTO", "IF", "THEN", "ELSE", "ENDIF",
    "DO", "READ", "WRITE", "FORMAT", "PAUSE", "SAVE", "EQUIVALENCE", "ENTRY", "INTEGER",
    "REAL", "LOGICAL", "CHARACTER", "COMPLEX", "INCLUDE", "PARAMETER"
};

int keyword_count = sizeof(keywords) / sizeof(keywords[0]);

int is_keyword(const char *token) {
    for (int i = 0; i < keyword_count; i++) {
        if (strcasecmp(token, keywords[i]) == 0)
            return 1;
    }
    return 0;
}

int in_list(const char *token, char seen[][MAX_TOKEN], int count) {
    for (int i = 0; i < count; i++) {
        if (strcasecmp(token, seen[i]) == 0)
            return 1;
    }
    return 0;
}

void to_upper(char *s) {
    for (int i = 0; s[i]; i++) s[i] = toupper((unsigned char)s[i]);
}

void rutina_kpi(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "No se pudo abrir %s\n", filename);
        return;
    }

    int line_count = 0;
    int total_keywords = 0;
    int unique_keywords = 0;
    int nested_structures = 0;
    int complex_constructs = 0;

    char seen_keywords[MAX_KEYWORDS][MAX_TOKEN];
    char line[MAX_LINE];

    while (fgets(line, sizeof(line), fp)) {
        line_count++;
        char *token = strtok(line, " ,(); ");
        while (token != NULL) {
            char upper[MAX_TOKEN];
            strncpy(upper, token, MAX_TOKEN - 1);
            upper[MAX_TOKEN - 1] = '\0';
            to_upper(upper);

            if (is_keyword(upper)) {
                total_keywords++;
                if (!in_list(upper, seen_keywords, unique_keywords)) {
                    strncpy(seen_keywords[unique_keywords++], upper, MAX_TOKEN);
                }

                if (!strcmp(upper, "IF") || !strcmp(upper, "DO") || !strcmp(upper, "THEN") || !strcmp(upper, "ENDIF"))
                    nested_structures++;

                if (!strcmp(upper, "COMMON") || !strcmp(upper, "ENTRY") || !strcmp(upper, "EQUIVALENCE"))
                    complex_constructs++;
            }
            token = strtok(NULL, " ,();	");
        }
    }

    fclose(fp);

    int score =
        1 * line_count +
        1 * total_keywords +
        2 * unique_keywords +
        3 * nested_structures +
        3 * complex_constructs;

    printf("%s,%d,%d,%d,%d,%d,%d\n", filename,
           line_count, total_keywords, unique_keywords,
           nested_structures, complex_constructs, score);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Uso: %s lista_de_archivos.txt\n", argv[0]);
        return 1;
    }

    FILE *list = fopen(argv[1], "r");
    if (!list) {
        perror("No se pudo abrir el archivo de lista");
        return 1;
    }

    char filename[MAX_LINE];
    printf("archivo,line_count,keyword_count,unique_keywords,nested_structures,complex_constructs,score\n");

    while (fgets(filename, sizeof(filename), list)) {
        size_t len = strlen(filename);
        if (filename[len - 1] == '\n') filename[len - 1] = '\0';
        rutina_kpi(filename);
    }

    fclose(list);
    return 0;
}
