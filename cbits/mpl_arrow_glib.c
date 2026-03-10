#include <arrow-glib/arrow-glib.h>
#include <stdio.h>
#include "mpl_arrow_glib.h"

// Create a table from a CSV file
Table read_csv(const char* path) {
    GError* err = NULL;
    InputStream stream = GARROW_INPUT_STREAM(garrow_memory_mapped_input_stream_new(path, &err));
    if(err) {
        fprintf(stderr, "read_csv: %s\n", err->message);
        g_error_free(err);
        return NULL;
    }
    CSVReadOptions opts = garrow_csv_read_options_new();
    CSVReader reader = garrow_csv_reader_new(stream, opts, &err);
    
    if(err) {
        fprintf(stderr, "read_csv: %s\n", err->message);
        g_error_free(err);
        g_object_unref(opts);
        g_object_unref(stream);
        return NULL;
    }

    Table table = garrow_csv_reader_read(reader, &err);

    if(err) {
        fprintf(stderr, "read_csv: %s\n", err->message);
        g_error_free(err);
    }

    g_object_unref(stream);
    g_object_unref(opts);
    g_object_unref(reader);

    return table;
}

// Get number of rows
gint64 table_nrows(Table table) { return garrow_table_get_n_rows(table); }

// Get number of cols
guint table_ncols(Table table) { return garrow_table_get_n_columns(table); }

// Get name from a column
const gchar* column_name(Table table, guint i) {
    Schema schema = table_get_schema(table);
    Field field = schema_get_field(schema, i);
    const gchar* name = field_get_name(field);
    g_object_unref(schema);
    g_object_unref(field);
    return name;
}

// Get data from a column
ChunkedArray table_get_column_data(Table table, guint col) { return garrow_table_get_column_data(table, col); }

// Check if array is null
gboolean array_is_null(Array arr, gint64 offset) { return garrow_array_is_null(arr, offset); }

// Get ChunkedArray Chunk (Array)
Array chunked_array_get_chunk(ChunkedArray chunk, guint col) { return garrow_chunked_array_get_chunk(chunk, col); }

// Get Length of Array
gint64 array_length(Array arr) { return garrow_array_get_length(arr); }

Schema table_get_schema(Table table) { return garrow_table_get_schema(table); }
Field schema_get_field(Schema schema, guint i) { return garrow_schema_get_field(schema, i); }

// Get name from a field
const char* field_get_name(Field field) { return garrow_field_get_name(field); }

// Pretty print a table
void print_table(Table table, gint64 max_rows) {
    gint64 rows = table_nrows(table);
    guint cols = table_ncols(table);

    if(max_rows <= 0 || max_rows > rows) max_rows = rows;

    guint c;
    for(c = 0; c < cols; c++) {
        if(c > 0) printf("  ");
        printf("%-14s", column_name(table, c));
    }
    printf("\n");

    for(c = 0; c < cols; c++) {
        if(c > 0) printf("  ");
        printf("--------------");
    }
    printf("\n");

    gint64 r; /* for(c = 0; c < cols && r == 0; c++);*/

    for(r = 0; r < max_rows; r++) {
        for(c = 0; c < cols; c++) {
            if(c > 0) printf("  ");
            ChunkedArray chunk = table_get_column_data(table, c);
            gint64 offset = r;
            guint chunks = garrow_chunked_array_get_n_chunks(chunk);
            Array arr = NULL;
            guint ci;
            for(ci = 0; ci < chunks; ci++) {
                arr = chunked_array_get_chunk(chunk, ci);
                gint64 len = array_length(arr);
                if(offset < len) break;
                offset -= len;
                g_object_unref(arr);
                arr = NULL;
            }
            if(!arr || array_is_null(arr, offset)) printf("%-14s", "NA");
            else if(GARROW_IS_DOUBLE_ARRAY(arr)) printf("%-14.4f", garrow_double_array_get_value(GARROW_DOUBLE_ARRAY(arr), offset));
            else if (GARROW_IS_INT64_ARRAY(arr)) {
                printf("%-14lld", (long long)garrow_int64_array_get_value(
                    GARROW_INT64_ARRAY(arr), offset));
            } else if (GARROW_IS_STRING_ARRAY(arr)) {
                gchar* s = garrow_string_array_get_string(
                    GARROW_STRING_ARRAY(arr), offset);
                printf("%-14s", s);
                g_free(s);
            } else if (GARROW_IS_BOOLEAN_ARRAY(arr)) {
                printf("%-14s", garrow_boolean_array_get_value(
                    GARROW_BOOLEAN_ARRAY(arr), offset) ? "true" : "false");
            } else {
                printf("%-14s", "???");
            }

            if(arr) g_object_unref(arr);
            g_object_unref(chunk);
        }
        printf("\n");
    }
    
    if(max_rows < rows) printf("... (%lld more rows)\n", (long long) (rows - max_rows));
    printf("[%lld rows x %u cols]\n", (long long)rows, cols);
}

// free table
void table_free(Table table) {
    if(table) g_object_unref(table);
}