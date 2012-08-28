(include "fitsio.scm") (import fitsio)

(define fptr (fits:open-file-for-read "/home/tomasi/work/test_data/null_tests/dx9/chdiff/LFI22-LFI23_SS4_cl.fits"))

(printf "Number of HDUs: ~a\n" (fits:get-number-of-hdus fptr))
(printf "BITPIX: ~a\n" (fits:get-int-key fptr "BITPIX"))

(if (not (fits:move-to-hdu-absolute fptr 2))
    (display "Error\n"))

(printf "Number of rows: ~a\n" (fits:get-num-of-rows fptr))
(printf "Number of columns: ~a\n" (fits:get-num-of-columns fptr))

(define x (fits:read-entire-double-column fptr 1 0.0))
(let ((n (f64vector-length x)))
  (printf  "~a elements: " n)
  (do ((i 0 (+ 1 i)))
      ((>= i n))
    (printf "~a ~a\n" i (f64vector-ref x i))))

(fits:close-file fptr)
