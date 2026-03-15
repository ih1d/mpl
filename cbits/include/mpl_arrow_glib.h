#ifndef MPL_ARROW_GLIB_H
#define MPL_ARROW_GLIB_H

#include <arrow-glib/arrow-glib.h>

/* Typedefs */
typedef GArrowTable*                Table;
typedef GArrowInputStream*          InputStream;
typedef GArrowCSVReadOptions*       CSVReadOptions;
typedef GArrowCSVReader*            CSVReader;
typedef GArrowChunkedArray*         ChunkedArray;
typedef GArrowArray*                Array;
typedef GArrowSchema*               Schema;
typedef GArrowField*                Field;
typedef GArrowBooleanArray*         BooleanArray;
typedef GArrowBooleanArrayBuilder*  BooleanArrayBuilder;

/* Functions */
Table               read_csv(const char* path);
gint64              table_nrows(Table table);
guint               table_ncols(Table table);
Schema              table_get_schema(Table table);
Field               schema_get_field(Schema schema, guint i);
gint                find_column_index(Table table, const char* col_name);
const char*         field_get_name(Field field);
Array               chunked_array_get_chunk(ChunkedArray chunk, guint col);
gint64              array_length(Array arr);
ChunkedArray        table_get_column_data(Table table, guint col); 
gboolean            array_is_null(Array arr, gint64 offset);
void                print_table(Table table, gint64 max_rows);
Table               filter_gt(Table table, const char* col_name, double value);
Table               filter_lt(Table table, const char* col_name, double value);
Table               filter_eq(Table table, const char* col_name, double value);
BooleanArrayBuilder build_boolean_array_builder();
BooleanArray        build_boolean_array(Table table, gint col, double val, int cmp);
void                table_free(Table table);

#endif