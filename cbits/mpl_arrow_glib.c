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

Field field_new(const char* column_name, GArrowDataType* arrow) { return garrow_field_new(column_name, arrow); }

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

// find column index by name, returns -1 if not found
gint find_column_index(Table table, const char* col_name) {
    guint cols = table_ncols(table);
    guint i;
    for(i = 0; i < cols; i++) {
        const gchar* name = column_name(table, i);
        if(g_strcmp0(name, col_name) == 0) return (gint)i;
    }
    return -1;
}

BooleanArrayBuilder build_boolean_array_builder() { return garrow_boolean_array_builder_new(); }

// Helper: build a boolean mask array from a column using a comparator
// cmp_mode: 0 = gt, 1 = lt, 2 = eq
BooleanArray build_boolean_array(Table table, gint col_idx, double value, int cmp_mode) {
    gint64 nrows = table_nrows(table);
    BooleanArrayBuilder builder = build_boolean_array_builder();
    GError* err = NULL;

    ChunkedArray chunk = table_get_column_data(table, (guint)col_idx);
    guint n_chunks = garrow_chunked_array_get_n_chunks(chunk);

    guint ci;
    for(ci = 0; ci < n_chunks; ci++) {
        Array arr = chunked_array_get_chunk(chunk, ci);
        gint64 len = array_length(arr);
        gint64 j;
        for(j = 0; j < len; j++) {
            gboolean result = FALSE;
            if(array_is_null(arr, j)) {
                result = FALSE;
            } else if(GARROW_IS_DOUBLE_ARRAY(arr)) {
                double v = garrow_double_array_get_value(GARROW_DOUBLE_ARRAY(arr), j);
                if(cmp_mode == 0) result = v > value;
                else if(cmp_mode == 1) result = v < value;
                else result = v == value;
            } else if(GARROW_IS_INT64_ARRAY(arr)) {
                gint64 v = garrow_int64_array_get_value(GARROW_INT64_ARRAY(arr), j);
                if(cmp_mode == 0) result = v > (gint64)value;
                else if(cmp_mode == 1) result = v < (gint64)value;
                else result = v == (gint64)value;
            } else {
                result = FALSE;
            }
            garrow_boolean_array_builder_append_value(GARROW_BOOLEAN_ARRAY_BUILDER(builder), result, &err);
            if(err) {
                fprintf(stderr, "build_mask: %s\n", err->message);
                g_error_free(err);
                err = NULL;
            }
        }
        g_object_unref(arr);
    }
    g_object_unref(chunk);

    BooleanArray boolArr = GARROW_BOOLEAN_ARRAY(
        garrow_array_builder_finish(GARROW_ARRAY_BUILDER(builder), &err));
    g_object_unref(builder);

    if(err) {
        fprintf(stderr, "build_mask finish: %s\n", err->message);
        g_error_free(err);
        return NULL;
    }
    return boolArr;
}

// Filter table: keep rows where column > value
Table filter_gt(Table table, const char* col_name, double value) {
    gint col_idx = find_column_index(table, col_name);
    if(col_idx < 0) {
        fprintf(stderr, "filter_gt: column '%s' not found\n", col_name);
        return NULL;
    }
    BooleanArray mask = build_boolean_array(table, col_idx, value, 0);
    if(!mask) return NULL;

    GError* err = NULL;
    Table result = garrow_table_filter(table, GARROW_BOOLEAN_ARRAY(mask), NULL, &err);
    g_object_unref(mask);
    if(err) {
        fprintf(stderr, "filter_gt: %s\n", err->message);
        g_error_free(err);
        return NULL;
    }
    return result;
}

// Filter table: keep rows where column < value
Table filter_lt(Table table, const char* col_name, double value) {
    gint col_idx = find_column_index(table, col_name);
    if(col_idx < 0) {
        fprintf(stderr, "filter_lt: column '%s' not found\n", col_name);
        return NULL;
    }
    BooleanArray mask = build_boolean_array(table, col_idx, value, 1);
    if(!mask) return NULL;

    GError* err = NULL;
    Table result = garrow_table_filter(table, GARROW_BOOLEAN_ARRAY(mask), NULL, &err);
    g_object_unref(mask);
    if(err) {
        fprintf(stderr, "filter_lt: %s\n", err->message);
        g_error_free(err);
        return NULL;
    }
    return result;
}

StringArrayBuilder build_string_array_builer() { return garrow_string_array_builder_new(); }
Int64ArrayBuilder build_int64_array_builder() { return garrow_int64_array_builder_new(); }

// Filter table: keep rows where column == value
Table filter_eq(Table table, const char* col_name, double value) {
    gint col_idx = find_column_index(table, col_name);
    if(col_idx < 0) {
        fprintf(stderr, "filter_eq: column '%s' not found\n", col_name);
        return NULL;
    }
    BooleanArray mask = build_boolean_array(table, col_idx, value, 2);
    if(!mask) return NULL;

    GError* err = NULL;
    Table result = garrow_table_filter(table, GARROW_BOOLEAN_ARRAY(mask), NULL, &err);
    g_object_unref(mask);
    if(err) {
        fprintf(stderr, "filter_eq: %s\n", err->message);
        g_error_free(err);
        return NULL;
    }
    return result;
}
// Build a two-column table from parallel arrays of strings and int64s
Table build_kmer_table(const char** kmers, const gint64* counts, gint64 n) {
    GError* err = NULL;

    // Build string column
    StringArrayBuilder str_builder = build_string_array_builer();
    gint64 i;
    for(i = 0; i < n; i++) {
        garrow_string_array_builder_append_string(str_builder, kmers[i], &err);
        if(err) {
            fprintf(stderr, "build_kmer_table: %s\n", err->message);
            g_error_free(err);
            g_object_unref(str_builder);
            return NULL;
        }
    }
    Array str_arr = GARROW_ARRAY(garrow_array_builder_finish(GARROW_ARRAY_BUILDER(str_builder), &err));
    g_object_unref(str_builder);
    if(err) {
        fprintf(stderr, "build_kmer_table: %s\n", err->message);
        g_error_free(err);
        return NULL;
    }

    // Build int64 column
    Int64ArrayBuilder int_builder = build_int64_array_builder();
    for(i = 0; i < n; i++) {
        garrow_int64_array_builder_append_value(int_builder, counts[i], &err);
        if(err) {
            fprintf(stderr, "build_kmer_table: %s\n", err->message);
            g_error_free(err);
            g_object_unref(int_builder);
            g_object_unref(str_arr);
            return NULL;
        }
    }
    Array int_arr = GARROW_ARRAY(garrow_array_builder_finish(GARROW_ARRAY_BUILDER(int_builder), &err));
    g_object_unref(int_builder);
    if(err) {
        fprintf(stderr, "build_kmer_table: %s\n", err->message);
        g_error_free(err);
        g_object_unref(str_arr);
        return NULL;
    }

    // Build schema with two fields
    Field kmer_field = field_new("kmer", GARROW_DATA_TYPE(garrow_string_data_type_new()));
    Field count_field = field_new("count", GARROW_DATA_TYPE(garrow_int64_data_type_new()));

    GList* fields = NULL;
    fields = g_list_append(fields, kmer_field);
    fields = g_list_append(fields, count_field);
    Schema schema = garrow_schema_new(fields);

    // Build chunked arrays (single chunk each)
    GList* str_chunks = g_list_append(NULL, str_arr);
    ChunkedArray str_chunked = garrow_chunked_array_new(str_chunks, &err);
    g_list_free(str_chunks);
    if(err) {
        fprintf(stderr, "build_kmer_table: %s\n", err->message);
        g_error_free(err);
        g_object_unref(str_arr);
        g_object_unref(int_arr);
        g_object_unref(schema);
        g_object_unref(kmer_field);
        g_object_unref(count_field);
        g_list_free(fields);
        return NULL;
    }

    GList* int_chunks = g_list_append(NULL, int_arr);
    ChunkedArray int_chunked = garrow_chunked_array_new(int_chunks, &err);
    g_list_free(int_chunks);
    if(err) {
        fprintf(stderr, "build_kmer_table: %s\n", err->message);
        g_error_free(err);
        g_object_unref(str_arr);
        g_object_unref(int_arr);
        g_object_unref(str_chunked);
        g_object_unref(schema);
        g_object_unref(kmer_field);
        g_object_unref(count_field);
        g_list_free(fields);
        return NULL;
    }

    // Build table
    ChunkedArray chunked_arrays[2] = { str_chunked, int_chunked };
    Table table = garrow_table_new_chunked_arrays(schema, chunked_arrays, 2, &err);

    // Cleanup
    g_list_free(fields);
    g_object_unref(schema);
    g_object_unref(kmer_field);
    g_object_unref(count_field);
    g_object_unref(str_arr);
    g_object_unref(int_arr);
    g_object_unref(str_chunked);
    g_object_unref(int_chunked);

    if(err) {
        fprintf(stderr, "build_kmer_table: %s\n", err->message);
        g_error_free(err);
        return NULL;
    }
    return table;
}

// free table
void table_free(Table table) {
    if(table) g_object_unref(table);
}