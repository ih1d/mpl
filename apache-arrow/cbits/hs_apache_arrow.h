/* Author: Isaac H. Lopez Diaz
 * Header exposing apache-arrow-glib
*/

#ifndef HS_APACHE_ARROW_H
#define HS_APACHE_ARROW_H
#include <arrow-glib/arrow-glib.h>

/***** DATA type aliases *******/
typedef GArrowTable*            ArrowTable;
typedef GArrowInputStream*      ArrowInputStream;
typedef GArrowCSVReadOptions*   ArrowCSVReadOptions;
typedef GArrowCSVReader*        ArrowCSVReader;
typedef GArrowSchema*           ArrowSchema;
typedef GArrowField*            ArrowField;

/***** FUNCTIONS *******/
ArrowTable  read_csv(const char* path);
void        free_arrow_table(ArrowTable table);
guint       arrow_table_columns(ArrowTable table);
guint64     arrow_table_rows(ArrowTable table);
const char* arrow_table_column_name(ArrowTable table, guint col);
char*       arrow_get_cell(ArrowTable table, guint col, guint64 row);
gint        arrow_table_cell_is_null(ArrowTable table, guint col, guint64 row);

#endif