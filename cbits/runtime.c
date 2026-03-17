#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "include/runtime.h"

#define INIT_CAP 64
#define LINE_BUF 4096
#define ERR_BUF 256

static char RUNTIME_ERROR[ERR_BUF] = "";

const char* runtime_last_error(void) { return RUNTIME_ERROR; }

/* ── helpers ─────────────────────────────────────────────────────────── */

static char* strdup_trim(const char* s) {
    while (*s && isspace((unsigned char)*s)) s++;
    size_t len = strlen(s);
    while (len > 0 && isspace((unsigned char)s[len - 1])) len--;
    char* out = malloc(len + 1);
    if (!out) return NULL;
    memcpy(out, s, len);
    out[len] = '\0';
    return out;
}

/* Try to classify a trimmed string as INT, FLOAT, or STRING. */
static ColType guess_type(const char* s) {
    if (!s || !*s) return COL_STRING;
    const char* p = s;
    if (*p == '-' || *p == '+') p++;
    if (!*p) return COL_STRING;

    int has_dot = 0;
    while (*p) {
        if (*p == '.') {
            if (has_dot) return COL_STRING;
            has_dot = 1;
        } else if (!isdigit((unsigned char)*p)) {
            return COL_STRING;
        }
        p++;
    }
    return has_dot ? COL_FLOAT : COL_INT;
}

/* Split a line by delimiter into tokens.  Caller frees tokens and *out. */
static int split_line(const char* line, char delim, char*** out) {
    int cap = 16, n = 0;
    char** toks = malloc(cap * sizeof(char*));
    if (!toks) return -1;

    const char* start = line;
    for (;;) {
        const char* end = strchr(start, delim);
        size_t len = end ? (size_t)(end - start) : strlen(start);

        /* strip trailing \r\n */
        while (len > 0 && (start[len - 1] == '\n' || start[len - 1] == '\r'))
            len--;

        char* tok = malloc(len + 1);
        if (!tok) { free(toks); return -1; }
        memcpy(tok, start, len);
        tok[len] = '\0';

        if (n == cap) {
            cap *= 2;
            char** tmp = realloc(toks, cap * sizeof(char*));
            if (!tmp) { free(tok); free(toks); return -1; }
            toks = tmp;
        }
        toks[n++] = tok;

        if (!end) break;
        start = end + 1;
    }
    *out = toks;
    return n;
}

/* Strip a trailing newline / carriage-return in place. */
static void chomp(char* s) {
    size_t len = strlen(s);
    while (len > 0 && (s[len - 1] == '\n' || s[len - 1] == '\r'))
        s[--len] = '\0';
}

/* ── read_csv ────────────────────────────────────────────────────────── */

Dataframe* read_csv(const char* fp) {
    FILE* f = fopen(fp, "r");
    if (!f) {
        snprintf(RUNTIME_ERROR, ERR_BUF, "cannot open file %s", fp);
        return NULL;
    }

    char line[LINE_BUF];

    /* ── header ── */
    if (!fgets(line, LINE_BUF, f)) {
        snprintf(RUNTIME_ERROR, ERR_BUF, "empty csv file %s", fp);
        fclose(f);
        return NULL;
    }

    char** hdr_toks;
    int ncols = split_line(line, ',', &hdr_toks);
    if (ncols <= 0) {
        snprintf(RUNTIME_ERROR, ERR_BUF, "bad header in %s", fp);
        fclose(f);
        return NULL;
    }

    char** col_names = malloc(ncols * sizeof(char*));
    for (int i = 0; i < ncols; i++) {
        col_names[i] = strdup_trim(hdr_toks[i]);
        free(hdr_toks[i]);
    }
    free(hdr_toks);

    /* ── read all rows as strings first ── */
    int row_cap = INIT_CAP, nrows = 0;
    char*** rows = malloc(row_cap * sizeof(char**));

    while (fgets(line, LINE_BUF, f)) {
        chomp(line);
        if (line[0] == '\0') continue; /* skip blank lines */

        char** toks;
        int n = split_line(line, ',', &toks);
        if (n != ncols) {
            /* pad / truncate silently */
            toks = realloc(toks, ncols * sizeof(char*));
            for (int i = n; i < ncols; i++) toks[i] = strdup("");
        }
        if (nrows == row_cap) {
            row_cap *= 2;
            rows = realloc(rows, row_cap * sizeof(char**));
        }
        /* trim each cell */
        for (int i = 0; i < ncols; i++) {
            char* trimmed = strdup_trim(toks[i]);
            free(toks[i]);
            toks[i] = trimmed;
        }
        rows[nrows++] = toks;
    }
    fclose(f);

    /* ── detect column types from first non-empty value ── */
    ColType* types = malloc(ncols * sizeof(ColType));
    for (int c = 0; c < ncols; c++) {
        types[c] = COL_STRING;
        for (int r = 0; r < nrows; r++) {
            if (rows[r][c][0] != '\0') {
                types[c] = guess_type(rows[r][c]);
                break;
            }
        }
    }

    /* ── build columns ── */
    Column* columns = malloc(ncols * sizeof(Column));
    for (int c = 0; c < ncols; c++) {
        columns[c].column_type = types[c];
        columns[c].col_rows = nrows;

        switch (types[c]) {
        case COL_INT: {
            int* d = malloc(nrows * sizeof(int));
            for (int r = 0; r < nrows; r++)
                d[r] = atoi(rows[r][c]);
            columns[c].data = d;
            break;
        }
        case COL_FLOAT: {
            double* d = malloc(nrows * sizeof(double));
            for (int r = 0; r < nrows; r++)
                d[r] = atof(rows[r][c]);
            columns[c].data = d;
            break;
        }
        case COL_STRING: {
            char** d = malloc(nrows * sizeof(char*));
            for (int r = 0; r < nrows; r++)
                d[r] = strdup(rows[r][c]);
            columns[c].data = d;
            break;
        }
        }
    }
    free(types);

    /* free temporary row storage */
    for (int r = 0; r < nrows; r++) {
        for (int c = 0; c < ncols; c++) free(rows[r][c]);
        free(rows[r]);
    }
    free(rows);

    /* ── assemble dataframe ── */
    Dataframe* df = malloc(sizeof(Dataframe));
    df->rows = nrows;
    df->cols = ncols;
    df->column_names = col_names;
    df->columns = columns;
    return df;
}

/* ── read_fasta ──────────────────────────────────────────────────────
 *
 *  FASTA format:
 *    >id description text...
 *    ACGTACGT...
 *    ACGTACGT...
 *    >next_id ...
 *
 *  Produces a dataframe with columns: id, description, sequence
 * ─────────────────────────────────────────────────────────────────── */

Dataframe* read_fasta(const char* fp) {
    FILE* f = fopen(fp, "r");
    if (!f) {
        snprintf(RUNTIME_ERROR, ERR_BUF, "cannot open file %s", fp);
        return NULL;
    }

    int cap = INIT_CAP, n = 0;
    char** ids   = malloc(cap * sizeof(char*));
    char** descs = malloc(cap * sizeof(char*));
    char** seqs  = malloc(cap * sizeof(char*));

    char line[LINE_BUF];
    char* cur_seq = NULL;
    size_t seq_len = 0, seq_cap = 0;

    while (fgets(line, LINE_BUF, f)) {
        chomp(line);
        if (line[0] == '\0') continue;

        if (line[0] == '>') {
            /* flush previous record */
            if (cur_seq) {
                cur_seq[seq_len] = '\0';
                seqs[n - 1] = cur_seq;
                cur_seq = NULL;
            }

            /* grow arrays */
            if (n == cap) {
                cap *= 2;
                ids   = realloc(ids,   cap * sizeof(char*));
                descs = realloc(descs, cap * sizeof(char*));
                seqs  = realloc(seqs,  cap * sizeof(char*));
            }

            /* parse ">id description..." */
            char* p = line + 1; /* skip '>' */
            while (*p && isspace((unsigned char)*p)) p++;
            char* space = p;
            while (*space && !isspace((unsigned char)*space)) space++;

            size_t id_len = (size_t)(space - p);
            ids[n] = malloc(id_len + 1);
            memcpy(ids[n], p, id_len);
            ids[n][id_len] = '\0';

            while (*space && isspace((unsigned char)*space)) space++;
            descs[n] = strdup(space);
            seqs[n] = NULL;
            n++;

            seq_len = 0;
            seq_cap = 256;
            cur_seq = malloc(seq_cap);
        } else if (cur_seq) {
            size_t ll = strlen(line);
            if (seq_len + ll + 1 > seq_cap) {
                while (seq_len + ll + 1 > seq_cap) seq_cap *= 2;
                cur_seq = realloc(cur_seq, seq_cap);
            }
            memcpy(cur_seq + seq_len, line, ll);
            seq_len += ll;
        }
    }
    /* flush last record */
    if (cur_seq && n > 0) {
        cur_seq[seq_len] = '\0';
        seqs[n - 1] = cur_seq;
    }
    fclose(f);

    if (n == 0) {
        free(ids); free(descs); free(seqs);
        snprintf(RUNTIME_ERROR, ERR_BUF, "no records in fasta file %s", fp);
        return NULL;
    }

    /* build 3 string columns: id, description, sequence */
    Column* columns = malloc(3 * sizeof(Column));
    columns[0] = (Column){COL_STRING, n, ids};
    columns[1] = (Column){COL_STRING, n, descs};
    columns[2] = (Column){COL_STRING, n, seqs};

    char** col_names = malloc(3 * sizeof(char*));
    col_names[0] = strdup("id");
    col_names[1] = strdup("description");
    col_names[2] = strdup("sequence");

    Dataframe* df = malloc(sizeof(Dataframe));
    df->rows = n;
    df->cols = 3;
    df->column_names = col_names;
    df->columns = columns;
    return df;
}

/* ── read_fastq ──────────────────────────────────────────────────────
 *
 *  FASTQ format (4 lines per record):
 *    @id description...
 *    ACGTACGT...
 *    +
 *    IIIIIIII...
 *
 *  Produces a dataframe with columns: id, description, sequence, quality
 * ─────────────────────────────────────────────────────────────────── */

Dataframe* read_fastq(const char* fp) {
    FILE* f = fopen(fp, "r");
    if (!f) {
        snprintf(RUNTIME_ERROR, ERR_BUF, "cannot open file %s", fp);
        return NULL;
    }

    int cap = INIT_CAP, n = 0;
    char** ids   = malloc(cap * sizeof(char*));
    char** descs = malloc(cap * sizeof(char*));
    char** seqs  = malloc(cap * sizeof(char*));
    char** quals = malloc(cap * sizeof(char*));

    char line[LINE_BUF];

    while (fgets(line, LINE_BUF, f)) {
        chomp(line);
        if (line[0] == '\0') continue;

        /* line 1: @id description */
        if (line[0] != '@') continue; /* skip malformed, try to re-sync */

        if (n == cap) {
            cap *= 2;
            ids   = realloc(ids,   cap * sizeof(char*));
            descs = realloc(descs, cap * sizeof(char*));
            seqs  = realloc(seqs,  cap * sizeof(char*));
            quals = realloc(quals, cap * sizeof(char*));
        }

        char* p = line + 1;
        while (*p && isspace((unsigned char)*p)) p++;
        char* space = p;
        while (*space && !isspace((unsigned char)*space)) space++;

        size_t id_len = (size_t)(space - p);
        ids[n] = malloc(id_len + 1);
        memcpy(ids[n], p, id_len);
        ids[n][id_len] = '\0';

        while (*space && isspace((unsigned char)*space)) space++;
        descs[n] = strdup(space);

        /* line 2: sequence */
        if (!fgets(line, LINE_BUF, f)) break;
        chomp(line);
        seqs[n] = strdup(line);

        /* line 3: '+' separator (discard) */
        if (!fgets(line, LINE_BUF, f)) break;

        /* line 4: quality scores */
        if (!fgets(line, LINE_BUF, f)) break;
        chomp(line);
        quals[n] = strdup(line);

        n++;
    }
    fclose(f);

    if (n == 0) {
        free(ids); free(descs); free(seqs); free(quals);
        snprintf(RUNTIME_ERROR, ERR_BUF, "no records in fastq file %s", fp);
        return NULL;
    }

    /* 4 string columns: id, description, sequence, quality */
    Column* columns = malloc(4 * sizeof(Column));
    columns[0] = (Column){COL_STRING, n, ids};
    columns[1] = (Column){COL_STRING, n, descs};
    columns[2] = (Column){COL_STRING, n, seqs};
    columns[3] = (Column){COL_STRING, n, quals};

    char** col_names = malloc(4 * sizeof(char*));
    col_names[0] = strdup("id");
    col_names[1] = strdup("description");
    col_names[2] = strdup("sequence");
    col_names[3] = strdup("quality");

    Dataframe* df = malloc(sizeof(Dataframe));
    df->rows = n;
    df->cols = 4;
    df->column_names = col_names;
    df->columns = columns;
    return df;
}

/* ── write_csv ───────────────────────────────────────────────────── */

void write_csv(const char* file_name, Dataframe* df) {
    if (!df) {
        snprintf(RUNTIME_ERROR, ERR_BUF, "write_csv: NULL dataframe");
        return;
    }

    FILE* f = fopen(file_name, "w");
    if (!f) {
        snprintf(RUNTIME_ERROR, ERR_BUF, "cannot open file %s for writing", file_name);
        return;
    }

    /* write header */
    for (int c = 0; c < df->cols; c++) {
        if (c > 0) fputc(',', f);
        fputs(df->column_names[c], f);
    }
    fputc('\n', f);

    /* write rows */
    for (int r = 0; r < df->rows; r++) {
        for (int c = 0; c < df->cols; c++) {
            if (c > 0) fputc(',', f);
            switch (df->columns[c].column_type) {
            case COL_INT:
                fprintf(f, "%d", ((int*)df->columns[c].data)[r]);
                break;
            case COL_FLOAT:
                fprintf(f, "%g", ((double*)df->columns[c].data)[r]);
                break;
            case COL_STRING:
                fputs(((char**)df->columns[c].data)[r], f);
                break;
            }
        }
        fputc('\n', f);
    }

    fclose(f);
}

/* ── print_df ────────────────────────────────────────────────────── */

void print_df(Dataframe* df) {
    if (!df) return;

    /* compute column widths (header vs data) */
    int* widths = malloc(df->cols * sizeof(int));
    for (int c = 0; c < df->cols; c++) {
        widths[c] = (int)strlen(df->column_names[c]);
    }

    /* measure data widths */
    char buf[64];
    for (int c = 0; c < df->cols; c++) {
        for (int r = 0; r < df->rows; r++) {
            int len = 0;
            switch (df->columns[c].column_type) {
            case COL_INT:
                len = snprintf(buf, sizeof(buf), "%d", ((int*)df->columns[c].data)[r]);
                break;
            case COL_FLOAT:
                len = snprintf(buf, sizeof(buf), "%g", ((double*)df->columns[c].data)[r]);
                break;
            case COL_STRING:
                len = (int)strlen(((char**)df->columns[c].data)[r]);
                break;
            }
            if (len > widths[c]) widths[c] = len;
        }
    }

    /* print header */
    for (int c = 0; c < df->cols; c++) {
        if (c > 0) printf("  ");
        printf("%-*s", widths[c], df->column_names[c]);
    }
    printf("\n");

    /* print separator */
    for (int c = 0; c < df->cols; c++) {
        if (c > 0) printf("  ");
        for (int i = 0; i < widths[c]; i++) putchar('-');
    }
    printf("\n");

    /* print rows */
    for (int r = 0; r < df->rows; r++) {
        for (int c = 0; c < df->cols; c++) {
            if (c > 0) printf("  ");
            switch (df->columns[c].column_type) {
            case COL_INT:
                printf("%-*d", widths[c], ((int*)df->columns[c].data)[r]);
                break;
            case COL_FLOAT:
                printf("%-*g", widths[c], ((double*)df->columns[c].data)[r]);
                break;
            case COL_STRING:
                printf("%-*s", widths[c], ((char**)df->columns[c].data)[r]);
                break;
            }
        }
        printf("\n");
    }

    free(widths);
}

/* ── free_dataframe ──────────────────────────────────────────────── */

void free_df(Dataframe* df) {
    if (!df) return;

    for (int c = 0; c < df->cols; c++) {
        if (df->columns[c].column_type == COL_STRING) {
            char** strs = (char**)df->columns[c].data;
            for (int r = 0; r < df->columns[c].col_rows; r++)
                free(strs[r]);
        }
        free(df->columns[c].data);
        free(df->column_names[c]);
    }
    free(df->column_names);
    free(df->columns);
    free(df);
}
