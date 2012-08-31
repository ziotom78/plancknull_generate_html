(module healpix
  (healpix:nside->npix
   healpix:npix->nside
   healpix:num-of-components-in-map
   healpix:num-of-components-in-spectrum
   healpix:read-map-as-shorts
   healpix:read-map-as-ushorts
   healpix:read-map-as-longs
   healpix:read-map-as-ulongs
   healpix:read-map-as-floats
   healpix:read-map-as-doubles
   healpix:read-spectrum)
  
  (import chicken
	  scheme
	  foreign
	  srfi-1
	  srfi-13
	  srfi-18
	  fitsio)

  (use srfi-1)
  (use srfi-13)

  (foreign-declare "#include \"chick_healpix.c\"")

  (define (healpix:nside->npix nside)
    (* 12 nside nside))

  (define (healpix:npix->nside npix)
    (let ((nside-float (sqrt (/ npix 12.0))))
      (if (not (eq? (inexact->exact (floor (healpix:nside->npix nside-float)))
		    npix))
	  (raise (sprintf "Invalid value (~a) for NPIX in call to npix->nside"
			  npix))
	  (inexact->exact nside-float))))

  ;; This function would work both with maps and power spectra, but
  ;; for the sake of clarity we define it twice.
  (define (healpix:num-of-components-in-map fits-file-name)
    (fits:with-input-table fits-file-name
			   (lambda (fptr)
			     (fits:get-num-of-columns fptr))))
  (define healpix:num-of-components-in-spectrum
    healpix:num-of-components-in-map)

  ;; This syntax implements functions that read a FITS file containing
  ;; a map and return two values: (1) an a-list containing metadata
  ;; for the map, and (2) a list of SRFI-4 vectors, one vector for
  ;; each component in the map. The caller can specify which
  ;; components to load by using the optional argument (which defaults
  ;; to `#f`, that is, "read everything is in the map").
  (define-syntax def-read-map-as-???
    (syntax-rules ()
      ((_ <dfn-name> <column-read-fn> <null>)
       (define (<dfn-name> fits-file-name
			   #!optional (fields #f))
	 (fits:with-input-table
	  fits-file-name
	  (lambda (fptr)
	    (let ((fields-to-read (if fields
				      fields
				      (iota (fits:get-num-of-columns fptr)
					    1))))
	      (values (list
		       (cons 'nside (fits:get-int-key fptr "NSIDE"))
		       (cons 'ordering (string->symbol
					(string-downcase
					 (fits:get-string-key fptr "ORDERING")))))
		      (map (lambda (column)
			   (<column-read-fn> fptr column <null>))
			   fields-to-read)))))))))

  (def-read-map-as-??? healpix:read-map-as-shorts
    fits:read-full-short-column 0)
  (def-read-map-as-??? healpix:read-map-as-ushorts
    fits:read-full-ushort-column 0)
  (def-read-map-as-??? healpix:read-map-as-longs
    fits:read-full-long-column 0)
  (def-read-map-as-??? healpix:read-map-as-ulongs
    fits:read-full-ulong-column 0)
  (def-read-map-as-??? healpix:read-map-as-floats
    fits:read-full-float-column +nan.0)
  (def-read-map-as-??? healpix:read-map-as-doubles
    fits:read-full-double-column +nan.0)
	 
  ;; This function returns all the spectra in `fits-file-name` as a
  ;; list of `f64vector` elements (one per component).
  (define (healpix:read-spectrum fits-file-name)
    (fits:with-input-table
     fits-file-name
     (lambda (fptr)
       (let ((num-of-components (fits:get-num-of-columns fptr)))
	 (map (lambda (comp)
		(fits:read-full-double-column fptr comp 0.0))
	      (iota num-of-components 1))))))
)
