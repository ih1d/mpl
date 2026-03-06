/* Author: Isaac H. Lopez Diaz
 * Licensed under MIT
 * Bindings to arrow-glib
*/

#ifndef MPL_RUNTIME_H
#define MPL_RUNTIME_H

#include <arrow-glib/arrow-glib.h>

/* DATA aliases */
typedef GError                  Error;
typedef GArrowInputStream       InputStream;
typedef GArrowTable             Table;
typedef GArrowCSVReaderClass    CsvReaderClass;
typedef GArrowCSVReader         CsvReader;
typedef GArrowCSVReadOptions    CsvReadOptions;

/* Functions */
CsvReadOptions* new_csv_read_options(void);
CsvReader* new_csv_reader(InputStream* stream, CsvReadOptions* opts, Error** err);
Table* read_csv_reader(CsvReader* reader, Error** err);

#endif