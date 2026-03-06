/* Author: Isaac H. Lopez Diaz
 * Licensed under MIT
 * Bindings to arrow-glib
*/

#ifndef MPL_RUNTIME_H
#define MPL_RUNTIME_H

#include <arrow-glib/arrow-glib.h>

/* DATA type aliases */
typedef GArrowInputStream       InputStream;
typedef GArrowCSVReadOptions    CsvOptions;
typedef GError                  Error;
typedef GArrowCSVReader         CsvReader;
typedef GArrowTable             Table;
typedef GObject                 Object;

/* Function wrappers */
Table* read_csv(const char* path);

void free_error(Error* err);
void free_object(Object* o);

#endif