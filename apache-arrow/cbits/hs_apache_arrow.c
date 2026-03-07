/* Author: Isaac H. Lopez Diaz
 * Description: Implementation of Arrow functions
*/

#include "hs_apache_arrow.h"
#include <stdio.h>

ArrowTable read_csv(const char* path) {

    GError* err = NULL;
    ArrowInputStream input = GARROW_INPUT_STREAM(garrow_memory_mapped_input_stream_new(path, &err));
    
    if (err) {
        fprintf(stderr, "read_csv: cannot open '%s': %s\n", path, err->message);
        g_error_free(err);
        return NULL;
    }

    ArrowCSVReadOptions opts = garrow_csv_read_options_new();
    ArrowCSVReader reader = garrow_csv_reader_new(input, opts, &err);

    if (err) {
        fprintf(stderr, "read_csv: reader error: %s\n", err->message);
        g_error_free(err);
        g_object_unref(opts);
        g_object_unref(input);
        return NULL;
    }
    
    ArrowTable table = garrow_csv_reader_read(reader, &err);

    if (err) {
        fprintf(stderr, "read_csv: read error: %s\n", err->message);
        g_error_free(err);
        table = NULL;
    }

    g_object_unref(reader);
    g_object_unref(opts);
    g_object_unref(input);
    return table;
}

void free_arrow_table(ArrowTable table) { if (table) g_object_unref(table); }

guint64 arrow_table_rows(ArrowTable table) { return garrow_table_get_n_rows(table); }

guint mpl_ncols(ArrowTable table) { return garrow_table_get_n_columns(table); }

const char* arrow_table_column_name(ArrowTable table, guint col) {
    
    ArrowSchema schema = garrow_table_get_schema(table);
    
    if (!schema) return NULL;

    ArrowField field = garrow_schema_get_field(schema, col);

    if (!field) {
        g_object_unref(schema);
        return NULL;
    }

    const char* name = garrow_field_get_name(field);

    g_object_unref(field);
    g_object_unref(schema);
    return name;
}