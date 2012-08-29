(include "fitsio.scm") (import fitsio)
(include "healpix.scm") (import healpix)

(let ((file-name "/home/tomasi/work/test_data/null_tests/dx9/surveydiff/70_SS3-SS2_cl.fits"))
  (printf "Number of components in file ~a:\n~a"
	  file-name
	  (healpix:num-of-components-in-map file-name))
  (define spectra (healpix:read-power-spectrum file-name))
  (printf "Number of spectra read from file: ~a\n"
	  (length spectra))
  (print "First values of the first spectrum: ")
  (do ((i 0 (+ 1 i)))
      ((>= i 5))
    (printf "~a " (f64vector-ref (car spectra) i)))
  (print "\n\nFirst values of the second spectrum: ")
  (do ((i 0 (+ 1 i)))
      ((>= i 5))
    (printf "~a " (f64vector-ref (cadr spectra) i))))
