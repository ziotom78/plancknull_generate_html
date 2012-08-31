#include "fitsio.h"

int fitsio_status = 0;

char * get_fitsio_error_message(void)
{
    if(fitsio_status == 0)
	return NULL;
    else
    {
	char string[80];
	fits_get_errstatus(fitsio_status, string);
	fitsio_status = 0;
	return strdup(string);
    }
}

void reset_fitsio_status(void)
{
    fitsio_status = 0;
}

#define OPEN_FILE_FOR_READ(fun_name, fitsio_fun)		\
    fitsfile * fun_name(const char * file_name)			\
    {								\
	fitsfile * fptr;					\
	fitsio_fun(&fptr, file_name, READONLY, &fitsio_status);	\
	if(fitsio_status != 0)					\
	    return NULL;					\
	else							\
	    return fptr;					\
    }

OPEN_FILE_FOR_READ(open_file_for_read, fits_open_file)
OPEN_FILE_FOR_READ(open_image_for_read, fits_open_image)
OPEN_FILE_FOR_READ(open_table_for_read, fits_open_table)
OPEN_FILE_FOR_READ(open_data_for_read, fits_open_data)

int close_file(fitsfile * fptr)
{
    fits_close_file(fptr, &fitsio_status);
    return fitsio_status == 0;
}

int get_num_of_hdus(fitsfile * fptr)
{
    int result = 0;
    fits_get_num_hdus(fptr, &result, &fitsio_status);
    return result;
}

int move_to_hdu_abs(fitsfile * fptr, int abs_num)
{
    fits_movabs_hdu(fptr, abs_num, NULL, &fitsio_status);
    return fitsio_status == 0;
}

int move_to_hdu_rel(fitsfile * fptr, int rel_num)
{
    fits_movrel_hdu(fptr, rel_num, NULL, &fitsio_status);
    return fitsio_status == 0;
}

#define DEFINE_GET_KEY(name, type, fits_type)				\
    int name(fitsfile * fptr, char * key_name)				\
    {									\
	type value = 0;							\
	fits_read_key(fptr, fits_type, key_name, &value, 0, &fitsio_status); \
	return value;							\
    }

DEFINE_GET_KEY(get_int_key, int, TINT)
DEFINE_GET_KEY(get_float_key, float, TFLOAT)
DEFINE_GET_KEY(get_double_key, double, TDOUBLE)

char * get_string_key(fitsfile * fptr, char * key_name)
{
    char buffer[80];
    fits_read_key(fptr, TSTRING, key_name, &buffer, NULL, &fitsio_status);
    return strdup(buffer);
}

long get_repeat_count_for_column(fitsfile * fptr, int column_num)
{
    long repeat_count;

    fits_get_bcolparms(fptr, column_num,
		       NULL, NULL, /* ttype, tunit */
		       NULL, /* typechar */
		       &repeat_count,
		       NULL, NULL, /* scale, zero */
		       NULL, NULL, /* nulval, disp */
		       &fitsio_status);
    return repeat_count;
}

long get_num_of_rows(fitsfile * fptr)
{
    long result = 0;
    fits_get_num_rows(fptr, &result, &fitsio_status);
    return result;
}

int get_num_of_columns(fitsfile * fptr)
{
    int result;
    fits_get_num_cols(fptr, &result, &fitsio_status);
    return result;
}

/******************************************************************************/
/* Column reading routines */

#define DEFINE_READ_COLUMN(name, c_type, fitsio_fn, fitsio_type)	\
    void name(fitsfile * fptr, int colnum, LONGLONG firstrow,		\
	      LONGLONG firstelem, LONGLONG nelem, c_type nulval, \
	      c_type * destination)					\
    {									\
	fitsio_fn(fptr, colnum, firstrow, firstelem, nelem, nulval,	\
		  destination, NULL, &fitsio_status);			\
    }

DEFINE_READ_COLUMN(read_short_col, short, fits_read_col_sht, TSHORT)
DEFINE_READ_COLUMN(read_ushort_col, unsigned short, fits_read_col_usht, TUSHORT)
DEFINE_READ_COLUMN(read_long_col, long, fits_read_col_lng, TLONG)
DEFINE_READ_COLUMN(read_ulong_col, unsigned long, fits_read_col_ulng, TULONG)
DEFINE_READ_COLUMN(read_float_col, float, fits_read_col_flt, TFLOAT)
DEFINE_READ_COLUMN(read_double_col, double, fits_read_col_dbl, TDOUBLE)

/******************************************************************************/
/* Column reading routines */

#define DEFINE_READ_FULL_COLUMN(name, c_type, fitsio_fn, fitsio_type)	\
    void name(fitsfile * fptr, int colnum, long elements_per_row,       \
	      long num_of_rows, c_type nulval, c_type * destination)	\
    {									\
        int row_idx;							\
	for(row_idx = 0; row_idx < num_of_rows; ++row_idx)		\
	{								\
	    if(fitsio_fn(fptr, colnum, row_idx + 1, 1, elements_per_row, \
			 nulval, &(destination[row_idx*elements_per_row]), \
			 NULL, &fitsio_status))				\
		return;							\
	}								\
    }

DEFINE_READ_FULL_COLUMN(read_full_short_col, short,
			fits_read_col_sht, TSHORT)
DEFINE_READ_FULL_COLUMN(read_full_ushort_col, unsigned short,
			fits_read_col_usht, TUSHORT)
DEFINE_READ_FULL_COLUMN(read_full_long_col, long,
			fits_read_col_lng, TLONG)
DEFINE_READ_FULL_COLUMN(read_full_ulong_col, unsigned long,
			fits_read_col_ulng, TULONG)
DEFINE_READ_FULL_COLUMN(read_full_float_col, float,
			fits_read_col_flt, TFLOAT)
DEFINE_READ_FULL_COLUMN(read_full_double_col, double,
			fits_read_col_dbl, TDOUBLE)
