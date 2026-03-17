#ifndef RUNTIME_H
#define RUNTIME_H

/* Types */
typedef enum {
    COL_INT,
    COL_FLOAT,
    COL_STRING,
} ColType;

typedef struct {
    ColType column_type;
    int col_rows;
    void* data;
} Column;

typedef struct {
    int rows;
    int cols;
    char** column_names;
    Column* columns;
} Dataframe;

/* Functions */
Dataframe* read_csv(const char* file);
Dataframe* read_fasta(const char* file);
Dataframe* read_fastq(const char* file);
void print_df(Dataframe* df);
void free_df(Dataframe* df);
const char* runtime_last_error(void);

#endif