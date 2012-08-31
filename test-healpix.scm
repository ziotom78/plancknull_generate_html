(include "fitsio.scm") (import fitsio)
(include "healpix.scm") (import healpix)

(let ((file-name "/home/tomasi/work/test_data/null_tests/dx9/surveydiff/70_SS3-SS2_map.fits"))
  (printf "Number of components in file ~a:\n~a"
	  file-name
	  (healpix:num-of-components-in-map file-name))
  (call-with-values (lambda () (healpix:read-map-as-floats file-name))
    (lambda (metadata components)
      (printf "Metadata: ~a\n" metadata)
      (printf "Number of f64vector variables: ~a\n"
	      (length components))
      (print "First values of the first map:\n")
      (do ((i 0 (+ 1 i)))
	  ((>= i 5))
	(printf "~a\n" (f32vector-ref (car components) i))))))
