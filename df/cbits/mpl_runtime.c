#include <arrow-glib/arrow-glib.h>
#include "mpl_runtime.h"
#include <stdio.h>

Table* read_csv(const char* path) {
    
    Error* err = NULL;
    InputStream* input = GARROW_INPUT_STREAM(garrow_memory_mapped_input_stream_new(path, &err));
    if(err) {
        fprintf(stderr, "read_csv: %s\n", err->message);
        free_error(err);
        return NULL;
    }
    
    CsvOptions* opts = garrow_csv_read_options_new();
    CsvReader* reader = garrow_csv_reader_new(input, opts, &err);

    if(err) {
        fprintf(stderr, "read_csv: %s\n", err->message);
        free_error(err);
        free_object(opts);
        free_object(input);
        return NULL;
    }

    Table* table = garrow_csv_reader_read(reader, &err);

    if(err) {
        fprintf(stderr, "read_csv: %s\n", err->message);
        free_error(err);
    }

    free_object(reader);
    free_object(opts);
    free_object(input);

    return table;
}

void free_error(Error* err) { g_error_free(err); }

void free_object(Object* o) { g_object_unref(o); }