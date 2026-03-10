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

guint arrow_table_cols(ArrowTable table) { return garrow_table_get_n_columns(table); }

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

char* arrow_get_cell(ArrowTable table, guint col, guint64 row) {
    /* Get the chunked array for this column */
    GArrowChunkedArray* chunked = garrow_table_get_column_data(table, col);
    if (!chunked) return g_strdup("NULL");

    /* Walk chunks to find the right one */
    guint n_chunks = garrow_chunked_array_get_n_chunks(chunked);
    guint64 offset = row;

    for (guint i = 0; i < n_chunks; i++) {
        GArrowArray* arr = garrow_chunked_array_get_chunk(chunked, i);
        gint64 len = garrow_array_get_length(arr);

        if (offset < (guint64)len) {
            char* result = NULL;

            /* Check null first */
            if (garrow_array_is_null(arr, (gint64)offset)) {
                result = g_strdup("NA");
            }
            else if (GARROW_IS_STRING_ARRAY(arr)) {
                result = garrow_string_array_get_string(GARROW_STRING_ARRAY(arr), (gint64)offset);
            }
            else if (GARROW_IS_INT64_ARRAY(arr)) {
                gint64 val = garrow_int64_array_get_value(GARROW_INT64_ARRAY(arr), (gint64)offset);
                result = g_strdup_printf("%lld", (long long)val);
            }
            else if (GARROW_IS_INT32_ARRAY(arr)) {
                gint32 val = garrow_int32_array_get_value(GARROW_INT32_ARRAY(arr), (gint64)offset);
                result = g_strdup_printf("%d", val);
            }
            else if (GARROW_IS_DOUBLE_ARRAY(arr)) {
                gdouble val = garrow_double_array_get_value(GARROW_DOUBLE_ARRAY(arr), (gint64)offset);
                result = g_strdup_printf("%g", val);
            }
            else if (GARROW_IS_FLOAT_ARRAY(arr)) {
                gfloat val = garrow_float_array_get_value(GARROW_FLOAT_ARRAY(arr), (gint64)offset);
                result = g_strdup_printf("%g", (double)val);
            }
            else if (GARROW_IS_BOOLEAN_ARRAY(arr)) {
                gboolean val = garrow_boolean_array_get_value(GARROW_BOOLEAN_ARRAY(arr), (gint64)offset);
                result = g_strdup(val ? "true" : "false");
            }
            else {
                result = g_strdup("???");
            }

            g_object_unref(arr);
            g_object_unref(chunked);
            return result;
        }

        offset -= (guint64)len;
        g_object_unref(arr);
    }

    g_object_unref(chunked);
    return g_strdup("OUT_OF_RANGE");
}

gint arrow_table_cell_is_null(ArrowTable table, guint col, guint64 row) {
    GArrowChunkedArray* chunked = garrow_table_get_column_data(table, col);
    if (!chunked) return 1;

    guint n_chunks = garrow_chunked_array_get_n_chunks(chunked);
    guint64 offset = row;

    for (guint i = 0; i < n_chunks; i++) {
        GArrowArray* arr = garrow_chunked_array_get_chunk(chunked, i);
        gint64 len = garrow_array_get_length(arr);

        if (offset < (guint64)len) {
            gint result = garrow_array_is_null(arr, (gint64)offset) ? 1 : 0;
            g_object_unref(arr);
            g_object_unref(chunked);
            return result;
        }

        offset -= (guint64)len;
        g_object_unref(arr);
    }

    g_object_unref(chunked);
    return 1;  /* out of range = treat as null */
}