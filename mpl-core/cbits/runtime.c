/* cbits/runtime.c */

#include <arrow-glib/arrow-glib.h>
#include <stdio.h>

/* ---- Read CSV ---- */

GArrowTable* mpl_read_csv(const char* path) {
    GError* err = NULL;

    GArrowInputStream* input =
        GARROW_INPUT_STREAM(garrow_memory_mapped_input_stream_new(path, &err));
    if (err) { g_error_free(err); return NULL; }

    GArrowCSVReadOptions* opts = garrow_csv_read_options_new();

    GArrowCSVReader* reader = garrow_csv_reader_new(input, opts, &err);
    if (err) {
        g_error_free(err);
        g_object_unref(opts);
        g_object_unref(input);
        return NULL;
    }

    GArrowTable* table = garrow_csv_reader_read(reader, &err);
    if (err) { g_error_free(err); }

    g_object_unref(reader);
    g_object_unref(opts);
    g_object_unref(input);
    return table;
}

/* ---- Table info ---- */

gint64 mpl_nrows(GArrowTable* t) {
    return garrow_table_get_n_rows(t);
}

guint mpl_ncols(GArrowTable* t) {
    return garrow_table_get_n_columns(t);
}

const gchar* mpl_colname(GArrowTable* t, guint i) {
    GArrowSchema* s = garrow_table_get_schema(t);
    GArrowField* f = garrow_schema_get_field(s, i);
    const gchar* name = garrow_field_get_name(f);
    g_object_unref(f);
    g_object_unref(s);
    return name;
}

/* ---- Cell access ---- */

/* Find the right chunk and local offset for a global row index */
static GArrowArray* get_array_at(GArrowTable* t, guint col, gint64 row, gint64* local) {
    GArrowChunkedArray* chunked = garrow_table_get_column_data(t, col);
    guint n = garrow_chunked_array_get_n_chunks(chunked);
    gint64 offset = row;

    for (guint i = 0; i < n; i++) {
        GArrowArray* arr = garrow_chunked_array_get_chunk(chunked, i);
        gint64 len = garrow_array_get_length(arr);
        if (offset < len) {
            *local = offset;
            g_object_unref(chunked);
            return arr;  /* caller must g_object_unref */
        }
        offset -= len;
        g_object_unref(arr);
    }

    g_object_unref(chunked);
    return NULL;
}

/* Is this cell null? */
int mpl_is_null(GArrowTable* t, guint col, gint64 row) {
    gint64 local;
    GArrowArray* arr = get_array_at(t, col, row, &local);
    if (!arr) return 1;
    int result = garrow_array_is_null(arr, local) ? 1 : 0;
    g_object_unref(arr);
    return result;
}

/* Get any cell as a string. Caller must call mpl_free_string. */
gchar* mpl_get_cell(GArrowTable* t, guint col, gint64 row) {
    gint64 local;
    GArrowArray* arr = get_array_at(t, col, row, &local);
    if (!arr) return g_strdup("NULL");

    if (garrow_array_is_null(arr, local)) {
        g_object_unref(arr);
        return g_strdup("NA");
    }

    gchar* result = NULL;

    if (GARROW_IS_STRING_ARRAY(arr)) {
        result = garrow_string_array_get_string(GARROW_STRING_ARRAY(arr), local);
    } else if (GARROW_IS_INT64_ARRAY(arr)) {
        gint64 val = garrow_int64_array_get_value(GARROW_INT64_ARRAY(arr), local);
        result = g_strdup_printf("%lld", (long long)val);
    } else if (GARROW_IS_DOUBLE_ARRAY(arr)) {
        gdouble val = garrow_double_array_get_value(GARROW_DOUBLE_ARRAY(arr), local);
        result = g_strdup_printf("%g", val);
    } else if (GARROW_IS_BOOLEAN_ARRAY(arr)) {
        gboolean val = garrow_boolean_array_get_value(GARROW_BOOLEAN_ARRAY(arr), local);
        result = g_strdup(val ? "true" : "false");
    } else if (GARROW_IS_INT32_ARRAY(arr)) {
        gint32 val = garrow_int32_array_get_value(GARROW_INT32_ARRAY(arr), local);
        result = g_strdup_printf("%d", val);
    } else if (GARROW_IS_FLOAT_ARRAY(arr)) {
        gfloat val = garrow_float_array_get_value(GARROW_FLOAT_ARRAY(arr), local);
        result = g_strdup_printf("%g", (double)val);
    } else {
        result = g_strdup("???");
    }

    g_object_unref(arr);
    return result;
}

/* ---- Cleanup ---- */

void mpl_free(GArrowTable* t) {
    if (t) g_object_unref(t);
}

void mpl_free_string(gchar* s) {
    if (s) g_free(s);
}