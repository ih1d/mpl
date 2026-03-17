#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

static char RUNTIME_ERROR[256] = "";

Dataframe* read_csv(const char* fp) { 
    FILE* fptr = fopen(fp, "r");

    if(fptr == NULL) {
        snprintf(RUNTIME_ERROR, 256, "cannot open file %s", fp);
        return NULL;
    }

    Column* cols;
    
}

Dataframe* read_fasta(const char* fp) { }

Dataframe* read_fastq(const char* fp) { }

void free_dataframe(Dataframe* df) { free(df); }