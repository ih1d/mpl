/* Author: Isaac H. Lopez Diaz
 * Header exposing apache-arrow-glib
*/

#ifndef HS_APACHE_ARROW_H
#define HS_APACHE_ARROW_H
#include <arrow-glib/arrow-glib.h>

/***** DATA type aliases *******/
/* Arrow Arrays */
typedef GArrowArray*                        ArrowArray;
typedef GArrowBinaryArray*                  ArrowBinaryArray;
typedef GArrowBooleanArray*                 ArrowBooleanArray;
typedef GArrowDate32Array*                  ArrowDate32Array;
typedef GArrowDate64Array*                  ArrowDate62Array;
typedef GArrowDayTimeIntervalArray*         ArrowDayTimeIntervalArray;
typedef GArrowDecimal128Array*              ArrowDecimal128Array; 
typedef GArrowDecimal256Array*              ArrowDecimal256Array;
typedef GArrowDoubleArray*                  ArrowDoubleArray;
typedef GArrowEqualOptions*                 ArrowEqualOptions;
typedef GArrowFixedSizeBinaryArray*         ArrowFixedSizeBinaryArray;
typedef GArrowFloatArray*                   ArrowFloatArray;
typedef GArrowHalfFloatArray*               ArrowHalfFloatArray;
typedef GArrowInt16Array*                   ArrowInt16Array;
typedef GArrowInt32Array*                   ArrowInt32Array;
typedef GArrowInt64Array*                   ArrowInt64Array; 
typedef GArrowInt8Array*                    ArrowInt8Array;
typedef GArrowLargeBinaryArray*             ArrowLargeBinaryArray;
typedef GArrowLargeStringArray*             ArrowLargeStringArray;
typedef GArrowMonthDayNanoIntervalArray*    ArrowMonthDayNanoIntervalArray;
typedef GArrowMonthIntervalArray*           ArrowMonthIntervalArray;
typedef GArrowNullArray*                    ArrowNullArray;
typedef GArrowNumericArray*                 ArrowNumericArray;
typedef GArrowPrimitiveArray*               ArrowPrimitiveArray;
typedef GArrowStringArray*                  ArrowStringArray;
typedef GArrowTime32Array*                  ArrowTime32Array;
typedef GArrowTime64Array*                  ArrowTime64Array;
typedef GArrowTimestampArray*               ArrowTimestampArray;
typedef GArrowUInt16Array*                  ArrowUInt16Array;
typedef GArrowUInt32Array*                  ArrowUInt32Array;
typedef GArrowUInt64Array*                  ArrowUInt64Array;
typedef GArrowUInt8Array*                   ArrowUInt8Array;

/* Arrow Array Function wrappers */
ArrowEqualOptions	arrow_equal_options_new(void);
/*
gboolean	garrow_equal_options_is_approx ()
GArrowArray *	garrow_array_import ()
gboolean	garrow_array_export ()
gboolean	garrow_array_equal ()
gboolean	garrow_array_equal_options ()
gboolean	garrow_array_equal_approx ()
gboolean	garrow_array_equal_range ()
gboolean	garrow_array_is_null ()
gboolean	garrow_array_is_valid ()
gint64	garrow_array_get_length ()
gint64	garrow_array_get_offset ()
gint64	garrow_array_get_n_nulls ()
GArrowBuffer *	garrow_array_get_null_bitmap ()
GArrowDataType *	garrow_array_get_value_data_type ()
GArrowType	garrow_array_get_value_type ()
GArrowArray *	garrow_array_slice ()
gchar *	garrow_array_to_string ()
GArrowArray *	garrow_array_view ()
gchar *	garrow_array_diff_unified ()
GArrowArray *	garrow_array_concatenate ()
GArrowNullArray *	garrow_null_array_new ()
GArrowBuffer *	garrow_primitive_array_get_buffer ()
GArrowBuffer *	garrow_primitive_array_get_data_buffer ()
GArrowBooleanArray *	garrow_boolean_array_new ()
gboolean	garrow_boolean_array_get_value ()
gboolean *	garrow_boolean_array_get_values ()
GArrowInt8Array *	garrow_int8_array_new ()
gint8	garrow_int8_array_get_value ()
const gint8 *	garrow_int8_array_get_values ()
GArrowUInt8Array *	garrow_uint8_array_new ()
guint8	garrow_uint8_array_get_value ()
const guint8 *	garrow_uint8_array_get_values ()
GArrowInt16Array *	garrow_int16_array_new ()
gint16	garrow_int16_array_get_value ()
const gint16 *	garrow_int16_array_get_values ()
GArrowUInt16Array *	garrow_uint16_array_new ()
guint16	garrow_uint16_array_get_value ()
const guint16 *	garrow_uint16_array_get_values ()
GArrowInt32Array *	garrow_int32_array_new ()
gint32	garrow_int32_array_get_value ()
const gint32 *	garrow_int32_array_get_values ()
GArrowUInt32Array *	garrow_uint32_array_new ()
guint32	garrow_uint32_array_get_value ()
const guint32 *	garrow_uint32_array_get_values ()
GArrowInt64Array *	garrow_int64_array_new ()
gint64	garrow_int64_array_get_value ()
const gint64 *	garrow_int64_array_get_values ()
GArrowUInt64Array *	garrow_uint64_array_new ()
guint64	garrow_uint64_array_get_value ()
const guint64 *	garrow_uint64_array_get_values ()
GArrowHalfFloatArray *	garrow_half_float_array_new ()
guint16	garrow_half_float_array_get_value ()
const guint16 *	garrow_half_float_array_get_values ()
GArrowFloatArray *	garrow_float_array_new ()
gfloat	garrow_float_array_get_value ()
const gfloat *	garrow_float_array_get_values ()
GArrowDoubleArray *	garrow_double_array_new ()
gdouble	garrow_double_array_get_value ()
const gdouble *	garrow_double_array_get_values ()
GArrowBinaryArray *	garrow_binary_array_new ()
GBytes *	garrow_binary_array_get_value ()
GArrowBuffer *	garrow_binary_array_get_buffer ()
GArrowBuffer *	garrow_binary_array_get_data_buffer ()
GArrowBuffer *	garrow_binary_array_get_offsets_buffer ()
GArrowLargeBinaryArray *	garrow_large_binary_array_new ()
GBytes *	garrow_large_binary_array_get_value ()
GArrowBuffer *	garrow_large_binary_array_get_buffer ()
GArrowBuffer *	garrow_large_binary_array_get_data_buffer ()
GArrowBuffer *	garrow_large_binary_array_get_offsets_buffer ()
GArrowStringArray *	garrow_string_array_new ()
gchar *	garrow_string_array_get_string ()
GArrowLargeStringArray *	garrow_large_string_array_new ()
gchar *	garrow_large_string_array_get_string ()
GArrowDate32Array *	garrow_date32_array_new ()
gint32	garrow_date32_array_get_value ()
const gint32 *	garrow_date32_array_get_values ()
GArrowDate64Array *	garrow_date64_array_new ()
gint64	garrow_date64_array_get_value ()
const gint64 *	garrow_date64_array_get_values ()
GArrowTimestampArray *	garrow_timestamp_array_new ()
gint64	garrow_timestamp_array_get_value ()
const gint64 *	garrow_timestamp_array_get_values ()
GArrowTime32Array *	garrow_time32_array_new ()
gint32	garrow_time32_array_get_value ()
const gint32 *	garrow_time32_array_get_values ()
GArrowTime64Array *	garrow_time64_array_new ()
gint64	garrow_time64_array_get_value ()
const gint64 *	garrow_time64_array_get_values ()
GArrowMonthIntervalArray *	garrow_month_interval_array_new ()
gint32	garrow_month_interval_array_get_value ()
const gint32 *	garrow_month_interval_array_get_values ()
GArrowDayTimeIntervalArray *	garrow_day_time_interval_array_new ()
GArrowDayMillisecond *	garrow_day_time_interval_array_get_value ()
GList *	garrow_day_time_interval_array_get_values ()
GArrowMonthDayNanoIntervalArray *	garrow_month_day_nano_interval_array_new ()
GArrowMonthDayNano *	garrow_month_day_nano_interval_array_get_value ()
GList *	garrow_month_day_nano_interval_array_get_values ()
GArrowFixedSizeBinaryArray *	garrow_fixed_size_binary_array_new ()
gint32	garrow_fixed_size_binary_array_get_byte_width ()
GBytes *	garrow_fixed_size_binary_array_get_value ()
GBytes *	garrow_fixed_size_binary_array_get_values_bytes ()
gchar *	garrow_decimal128_array_format_value ()
GArrowDecimal128 *	garrow_decimal128_array_get_value ()
gchar *	garrow_decimal256_array_format_value ()
GArrowDecimal256 *	garrow_decimal256_array_get_value ()
GArrowArray *	garrow_extension_array_get_storage ()
*/
#endif