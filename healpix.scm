(module healpix
  (healpix:nside->npix
   healpix:npix->nside
   healpix:map-metadata
   healpix:map-components
   healpix:map-nside
   healpix:map-ordering
   healpix:map-component
   healpix:num-of-components-in-map
   healpix:num-of-components-in-spectrum
   healpix:angle->pixel-ring
   healpix:angle->pixel-nest
   healpix:angle->pixel
   healpix:read-map-as-shorts
   healpix:read-map-as-ushorts
   healpix:read-map-as-longs
   healpix:read-map-as-ulongs
   healpix:read-map-as-floats
   healpix:read-map-as-doubles
   healpix:read-spectrum
   healpix:map->bitmap
   healpix:get-float-map-extrema
   healpix:plot-bitmap-to-cairo-surface
   healpix:plot-gradient-bar
   healpix:map->png)
  
  (import chicken
	  scheme
	  foreign
	  srfi-18
	  fitsio)

  (use srfi-1)
  (use srfi-4)
  (use srfi-13)
  (use numbers)

  (require-extension cairo)

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

  ;; Return an a-list containing the metadata of the map
  (define (healpix:map-metadata map)
    (assert (pair? map))
    (car map))

  (define (healpix:map-components map)
    (assert (pair? map))
    (cdr map))

  (define (healpix:map-nside map)
    (let ((nside-entry (assq 'nside (healpix:map-metadata map))))
      (assert nside-entry)
      (cdr nside-entry)))

  (define (healpix:map-ordering map)
    (let ((ordering-entry (assq 'ordering (healpix:map-metadata map))))
      (assert ordering-entry)
      (cdr ordering-entry)))

  ;; Return the nth component of the map, starting from 1 (Healpix convention)
  (define (healpix:map-component map component)
    (list-ref (healpix:map-components map) (- component 1)))

  ;; This function would work both with maps and power spectra, but
  ;; for the sake of clarity we define it twice.
  (define (healpix:num-of-components-in-map map)
    (if (pair? map)
	(length (healpix:map-components map))
	(fits:with-input-table map
			       (lambda (fptr)
				 (fits:get-num-of-columns fptr)))))
  (define healpix:num-of-components-in-spectrum
    healpix:num-of-components-in-map)

  (define healpix:angle->pixel-ring
    (foreign-lambda long "ang2pix_ring" unsigned-int double double))

  (define healpix:angle->pixel-nest
    (foreign-lambda long "ang2pix_nest" unsigned-int double double))

  (define (healpix:angle->pixel nside theta phi #!optional (ordering 'ring))
    (let ((fn (if (eq? ordering 'ring)
		  healpix:angle->pixel-ring
		  healpix:angle->pixel-nest)))
      (fn nside theta phi)))

  ;; This syntax implements functions that read a FITS file containing
  ;; a map and return a pair, where the `car` is an a-list containing
  ;; metadata for the map, and the `cdr` is a list of SRFI-4 vectors
  ;; (one vector for each component in the map). The caller can specify
  ;; which components to load by using the optional argument (which
  ;; defaults to `#f`, that is, "read everything is in the map").
  (define-syntax def-read-map-as-???
    (syntax-rules ()
      ((_ <dfn-name> <column-read-fn> <null>)
       (define (<dfn-name> fits-file-name
			   #!key (fields #f))
	 (fits:with-input-table
	  fits-file-name
	  (lambda (fptr)
	    (let ((fields-to-read (if fields
				      fields
				      (iota (fits:get-num-of-columns fptr)
					    1))))
	      (cons (list
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

  ;; This function produces a bitmapped representation of the map
  ;; using a rectangular area of `width` times `height` screen points.
  ;; Each point is a 32-bit floating point number which can be (1)
  ;; `inf` (the point is outside the map), (2) `nan` (the point is
  ;; marked as `UNSEEN`), (3) any other floating-point value.
  (define-syntax def-map->bitmap
    (syntax-rules ()
      ((_ <fn-name> <c-fn-name> <vector-type> <make-vector-fn>)
       (define (<fn-name> map width height #!optional (component 1))
	 (let ((points (<make-vector-fn> (* width height)))
	       (plot-map (foreign-lambda void
					 <c-fn-name>
					 <vector-type> ; map pixels
					 unsigned-int ; nside
					 unsigned-int ; width
					 unsigned-int ; height
					 bool ; ring-ordering?
					 <vector-type>)))
	   (plot-map (healpix:map-component map component)
		     (healpix:map-nside map)
		     width
		     height
		     (eq? (healpix:map-ordering map) 'ring)
		     points)
	   points)))))

  (def-map->bitmap short-map->bitmap "plot_short_map"
    nonnull-s16vector make-s16vector)
  (def-map->bitmap ushort-map->bitmap "plot_ushort_map"
    nonnull-u16vector make-u16vector)
  (def-map->bitmap long-map->bitmap "plot_long_map"
    nonnull-s32vector make-s32vector)
  (def-map->bitmap ulong-map->bitmap "plot_ulong_map"
    nonnull-u32vector make-u32vector)
  (def-map->bitmap float-map->bitmap "plot_float_map"
    nonnull-f32vector make-f32vector)
  (def-map->bitmap double-map->bitmap "plot_double_map"
    nonnull-f64vector make-f64vector)

  (define (healpix:map->bitmap map width height #!optional (comp 1))
    (let ((vector (healpix:map-component map comp)))
      (cond ((s16vector? vector) (short-map->bitmap map width height comp))
	    ((u16vector? vector) (ushort-map->bitmap map width height comp))
	    ((s32vector? vector) (long-map->bitmap map width height comp))
	    ((u32vector? vector) (ulong-map->bitmap map width height comp))
	    ((f32vector? vector) (float-map->bitmap map width height comp))
	    ((f64vector? vector) (double-map->bitmap map width height comp)))))

  (define (healpix:get-float-map-extrema map #!optional (component 1))
    (let ((get-map-extrema (foreign-lambda void "get_map_extrema"
					   nonnull-f32vector
					   unsigned-int
					   (c-pointer float)
					   (c-pointer float))))
      (let-location ((min float)
		     (max float))
		    (get-map-extrema (healpix:map-component map component)
				     (healpix:map-nside map)
				     (location min)
				     (location max))
		    (cons min max))))

  (define (healpix:plot-bitmap-to-cairo-surface cairo-context
						origin
						size
						map-extrema
						bitmap
						bmp-size)
    (let ((c_fn (foreign-lambda void "plot_bitmap_to_cairo_surface"
				c-pointer
				double
				double
				double
				double
				double
				double
				nonnull-f32vector
				unsigned-int
				unsigned-int)))
      (c_fn cairo-context
	    (car origin) (cdr origin)
	    (car size) (cdr size)
	    (car map-extrema) (cdr map-extrema)
	    bitmap
	    (car bmp-size) (cdr bmp-size))))

  (define (healpix:plot-gradient-bar cairo-context
				     origin
				     size
				     map-extrema
				     measure-unit-string
				     #!optional (vertical? #f))
    (let ((c_fn (foreign-lambda void "plot_gradient_bar"
				c-pointer
				double
				double
				double
				double
				double
				double
				c-string
				bool)))
      (c_fn cairo-context
	    (car origin) (cdr origin)
	    (car size) (cdr size)
	    (car map-extrema) (cdr map-extrema)
	    measure-unit-string
	    vertical?)))

  (define (healpix:map->png map file-name
			    bmp-width bmp-height #!optional (component 1))
    (let* ((bitmap (healpix:map->bitmap map bmp-width bmp-height))
	   (map-extrema (healpix:get-float-map-extrema map component))
	   (cairo-surface (cairo-image-surface-create CAIRO_FORMAT_RGB24
						      bmp-width
						      bmp-height))
	   (cairo-context (cairo-create cairo-surface)))
      (healpix:plot-bitmap-to-cairo-surface cairo-context
					    (cons 0.0 0.0)
					    (cons bmp-width bmp-height)
					    map-extrema
					    bitmap
					    (cons bmp-width bmp-height))
      (if (not (eq? CAIRO_STATUS_SUCCESS
		    (cairo-surface-write-to-png cairo-surface file-name)))
	  (raise (sprintf "Unable to write file ~a" file-name)))))
)
