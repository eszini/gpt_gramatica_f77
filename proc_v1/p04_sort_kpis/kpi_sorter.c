#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 1024
#define MAX_ENTRIES 1000

typedef struct {
    int score;
    char filename[512];
} Entry;

int compare(const void *a, const void *b) {
    return ((Entry*)a)->score - ((Entry*)b)->score;
}

int main() {
    FILE *in = fopen("kpis.csv", "r");
    if (!in) {
        perror("Error opening kpis.csv");
        return 1;
    }

    Entry entries[MAX_ENTRIES];
    int count = 0;
    char line[MAX_LINE];

    // Leer encabezado y descartarlo
    fgets(line, sizeof(line), in);

    while (fgets(line, sizeof(line), in)) {
        char *token = strtok(line, ",");
        if (!token) continue;
        strncpy(entries[count].filename, token, sizeof(entries[count].filename) - 1);
        entries[count].filename[sizeof(entries[count].filename) - 1] = '\0';

        // Saltar los siguientes 5 campos
        for (int i = 0; i < 5; i++) strtok(NULL, ",");

        char *score_str = strtok(NULL, ", ");
        if (score_str) {
            entries[count].score = atoi(score_str);
            count++;
        }
    }

    fclose(in);

    qsort(entries, count, sizeof(Entry), compare);

    FILE *out = fopen("kpis_sorted.txt", "w");
    if (!out) {
        perror("Error opening output file");
        return 1;
    }

    for (int i = 0; i < count; i++) {
        fprintf(out, "%05d %s\n", 
		entries[i].score, entries[i].filename);
    }

    fclose(out);
    return 0;
}
