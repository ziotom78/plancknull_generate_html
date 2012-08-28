(module fitsio
  ;; List of exported symbols
  (fits:get-error-string
   fits:reset-error-status
   fits:open-file-for-read
   fits:open-image-for-read
   fits:open-table-for-read
   fits:open-table-for-read
   fits:close-file
   fits:get-number-of-hdus
   fits:get-int-key
   fits:get-float-key
   fits:get-double-key
   fits:get-string-key
   fits:move-to-hdu-absolute
   fits:move-to-hdu-relative
   fits:get-num-of-rows
   fits:get-num-of-columns
   fits:read-short-column
   fits:read-ushort-column
   fits:read-long-column
   fits:read-ulong-column
   fits:read-float-column
   fits:read-double-column
   fits:read-entire-short-column
   fits:read-entire-ushort-column
   fits:read-entire-long-column
   fits:read-entire-ulong-column
   fits:read-entire-float-column
   fits:read-entire-double-column)

  (import chicken
	  scheme
	  foreign
	  srfi-4
	  srfi-18)

  (use srfi-4)
  (foreign-declare "#include \"fitsio.h\"")

  ;; This file contains a set of nice wrappers to CFITSIO routines
  (foreign-declare "#include \"chick_fitsio.c\"")

  (define-foreign-type fits-file (c-pointer "fitsfile"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define fits:get-error-string
    (foreign-lambda c-string* "get_fitsio_error_message"))

  (define fits:reset-error-status
    (foreign-lambda void "reset_fitsio_status"))

  ;; This syntax extension defines `name` to be a call to `fn`
  ;; (typically, a `foreign-lambda` statement) which is nicely wrapped
  ;; with some error checking. The `abort` call is triggered whenever
  ;; the C variable `fitsio_status` (defined in `chick_fitsio.c`) is
  ;; nonzero.
  (define-syntax define-fitsio-fn
    (syntax-rules ()
      ((_ <name> <fn> <args> ...)
       (define (<name> <args> ...)
	 (let ((result (<fn> <args> ...)))
	   (if (not (eq? (foreign-value "fitsio_status" int) 0))
	       (abort (fits:get-error-string))
	       result))))))

  (define-fitsio-fn
    fits:open-file-for-read
    (foreign-lambda fits-file "open_file_for_read" nonnull-c-string)
    file-name)

  (define-fitsio-fn
    fits:open-image-for-read
    (foreign-lambda fits-file "open_image_for_read" nonnull-c-string)
    file-name)

  (define-fitsio-fn
    fits:open-table-for-read
    (foreign-lambda fits-file "open_table_for_read" nonnull-c-string)
    file-name)

  (define-fitsio-fn
    fits:open-data-for-read
    (foreign-lambda fits-file "open_data_for_read" nonnull-c-string)
    file-name)

  (define-fitsio-fn
    fits:close-file
    (foreign-lambda bool "close_file" fits-file)
    file-ptr)

  (define-fitsio-fn
    fits:get-number-of-hdus
    (foreign-lambda int "get_num_of_hdus" fits-file)
    file-ptr)

  (define-fitsio-fn
    fits:get-int-key
    (foreign-lambda int "get_int_key" fits-file nonnull-c-string)
    file-ptr key-name)

  (define-fitsio-fn
    fits:get-float-key
    (foreign-lambda float "get_float_key" fits-file nonnull-c-string)
    file-ptr key-name)

  (define-fitsio-fn
    fits:get-double-key
    (foreign-lambda double "get_double_key" fits-file nonnull-c-string)
    file-ptr key-name)

  (define-fitsio-fn
    fits:get-string-key
    (foreign-lambda c-string* "get_string_key" fits-file nonnull-c-string)
    file-ptr key-name)

  (define-fitsio-fn
    fits:move-to-hdu-absolute
    (foreign-lambda bool "move_to_hdu_abs" fits-file int)
    file-name abs-position)

  (define-fitsio-fn
    fits:move-to-hdu-relative
    (foreign-lambda bool "move_to_hdu_rel" fits-file int)
    file-ptr rel-position)

  (define-fitsio-fn
    fits:get-num-of-rows
    (foreign-lambda long "get_num_of_rows" fits-file)
    file-ptr)

  (define-fitsio-fn
    fits:get-num-of-columns
    (foreign-lambda int "get_num_of_columns" fits-file)
    file-ptr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Column reading routines

  ;; Low-level C functions (not exported)

  (define-syntax def-lowl-read-fn
    (syntax-rules ()
      ((_ <dfn-name> <c-name> <type> <vector-type>)
       (define-fitsio-fn
	 <dfn-name>
	 (foreign-lambda void <c-name>
			 fits-file int long long long <type> <vector-type>)
	 ;; Param list
	 fptr column-num first-row-num first-element-num
	 num-of-elements null-value destination))))

  (def-lowl-read-fn read-short-column  "read_short_col"  short s16vector)
  (def-lowl-read-fn read-ushort-column "read_ushort_col" unsigned-short u16vector)
  (def-lowl-read-fn read-long-column   "read_long_col"   long s32vector)
  (def-lowl-read-fn read-ulong-column  "read_ulong_col"  unsigned-long u32vector)
  (def-lowl-read-fn read-float-column  "read_float_col"  float f32vector)
  (def-lowl-read-fn read-double-column "read_double_col" double f64vector)

  ;; High-level Scheme functions

  (define-syntax def-read-fn
    (syntax-rules ()
      ((_ <dfn-name> <make-vector> <low-level-fn>)
       (define (<dfn-name> fptr
			   column-num
			   first-row-num
			   first-element-num
			   num-of-elements
			   null-value)
	 (let ((vect (<make-vector> num-of-elements)))
	   (<low-level-fn> fptr
			   column-num
			   first-row-num
			   first-element-num
			   num-of-elements
			   null-value
			   vect)
	   vect)))))

  (def-read-fn fits:read-short-column  make-s16vector read-short-column)
  (def-read-fn fits:read-ushort-column make-u16vector read-ushort-column)
  (def-read-fn fits:read-long-column   make-s32vector read-long-column)
  (def-read-fn fits:read-ulong-column  make-u32vector read-ulong-column)
  (def-read-fn fits:read-float-column  make-f32vector read-float-column)
  (def-read-fn fits:read-double-column make-f64vector read-double-column)

  ;; TODO: make this work even when repcount > 1 (use fits_get_bcolparmsll)
  (define-syntax def-read-entire-col-fn
    (syntax-rules ()
      ((_ <dfn-name> <read-fn>)
       (define (<dfn-name> fptr column-num null-value)
	 (let ((num-of-elements (fits:get-num-of-rows fptr)))
	   (<read-fn> fptr column-num 1 1 num-of-elements null-value))))))

  (def-read-entire-col-fn fits:read-entire-short-column  fits:read-short-column)
  (def-read-entire-col-fn fits:read-entire-ushort-column fits:read-ushort-column)
  (def-read-entire-col-fn fits:read-entire-long-column   fits:read-long-column)
  (def-read-entire-col-fn fits:read-entire-ulong-column  fits:read-ulong-column)
  (def-read-entire-col-fn fits:read-entire-float-column  fits:read-float-column)
  (def-read-entire-col-fn fits:read-entire-double-column fits:read-double-column))
