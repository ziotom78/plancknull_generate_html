(module healpix
  (healpix:nside->npix
   healpix:npix->nside
   healpix:num-of-components-in-map
   healpix:num-of-components-in-spectrum
   healpix:read-spectrum)
  
  (import chicken
	  scheme
	  srfi-1
	  srfi-18
	  fitsio)

  (use srfi-1)

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

  ;; This function returns all the spectra in `fits-file-name` as a
  ;; list of `f64vector` elements (one per component).
  (define (healpix:read-spectrum fits-file-name)
    (fits:with-input-table
     fits-file-name
     (lambda (fptr)
       (let ((num-of-components (fits:get-num-of-columns fptr)))
	 (map (lambda (comp)
		(fits:read-entire-double-column fptr comp 0.0))
	      (iota num-of-components 1))))))
)
