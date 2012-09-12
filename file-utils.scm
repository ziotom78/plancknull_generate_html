;; File utilities
;; ==============

;; This module contains a number of general-purpose functions that
;; help dealing with the (usually large) amount of files handled by
;; the program.

(module file-utils
  (map->gif
   spectrum->js
   spectrum->gif
   fits-name->gif-name
   fits-name->js-name
   dir-copy)

  (import chicken
	  scheme
	  extras
	  files
	  data-structures
	  posix
	  healpix
	  srfi-1
	  srfi-4
	  srfi-13
	  srfi-18)

  (require-extension shell)
  (require-extension filepath)
  (require-extension directory-utils)
  (require-extension cairo)
  (require-extension blas)

  ;; We need to implement the code that will convert the maps in FITS
  ;; format into GIF images that can be included in the HTML report.

  ;; This function uses the facilities provided by `healpix.scm` to
  ;; draw a map with a title and a color bar on a cairo context, which
  ;; is then saved into a 24-bit PNG file.
  (define (map->png map
		    file-name
		    bmp-width bmp-height
		    title
		    #!key
		    (component 1)
		    (color-extrema #f) ; If not `#f`, this should be a pair
		    (title-height 20)
		    (gradient-bar-height 30))
    (let* ((map-bitmap-height (- bmp-height title-height gradient-bar-height))
	   (bitmap (healpix:map->bitmap map
					bmp-width
					map-bitmap-height
					component))
	   (map-extrema (if color-extrema
			    color-extrema
			    (healpix:get-float-map-extrema map component)))
	   (cairo-surface (cairo-image-surface-create CAIRO_FORMAT_RGB24
						      bmp-width
						      bmp-height))
	   (cairo-context (cairo-create cairo-surface)))

      ;; This code fills the background of the image with white
      (cairo-rectangle cairo-context
		       0.0 0.0
		       bmp-width bmp-height)
      (cairo-set-source-rgb cairo-context
			    1.0 1.0 1.0)
      (cairo-fill cairo-context)

      ;; This code paints the title. It uses `cairo-text-extents` to
      ;; determine its width, in order to correctly center it
      ;; horizontally.
      (let ((title-font-size (* 0.9 title-height)))
	(cairo-set-font-size cairo-context
			     title-font-size)
	(let ((title-te (make-cairo-text-extents-type)))
	  (cairo-text-extents cairo-context
			      title
			      title-te)
	  (cairo-move-to cairo-context
			 (* (- bmp-width (cairo-text-extents-width title-te))
			    0.5)
			 title-font-size)
	  (cairo-set-source-rgb cairo-context
				0.0 0.0 0.0)
	  (cairo-show-text cairo-context
			   title)))

      ;; Now we plot the map. The first two `cons` specify the region
      ;; in the cairo context (i.e. the drawing area), while the last
      ;; `cons` specifies the number of rows and columns in `bitmap`
      ;; (bidimensional array of floating point values).
      (healpix:plot-bitmap-to-cairo-surface cairo-context
					    (cons 0.0 title-height)
					    (cons bmp-width map-bitmap-height)
					    map-extrema
					    bitmap
					    (cons bmp-width map-bitmap-height))

      ;; Now comes the gradient bar
      (cairo-set-font-size cairo-context 16)
      (healpix:plot-gradient-bar cairo-context
				 (cons 0.0 (+ title-height map-bitmap-height))
				 (cons bmp-width (- gradient-bar-height 1))
				 map-extrema
				 ; `\u03BC` is the greek letter "mu" in Unicode
				 "\u03BCK")
      ;; Finally, save the drawing area in a 24-bit PNG file
      (if (not (eq? CAIRO_STATUS_SUCCESS
		    (cairo-surface-write-to-png cairo-surface file-name)))
	  (raise (sprintf "Unable to write file ~a" file-name)))))

  (define (scale-map-to-muK! map)
    (for-each (lambda (vec)
		(unsafe-sscal! (f32vector-length vec)
			       1.0e+6
			       vec))
	      (cdr map)))

  ;; First we define a simple function that uses `map->png` and
  ;; ImageMagick's `convert` to produce a GIF file with the map.
  (define (create-1-gif input-fits-file-name
			output-gif-file-name
			title
			width
			component-number
			fixed-extrema?)
    (let ((output-png-file-name
	   (filepath:replace-extension output-gif-file-name ".png"))
	  (input-map (healpix:read-map-as-floats input-fits-file-name)))
      (printf "Creating a bitmap (PNG format, 24 bit) from ~a to ~a (fixed-extrema? is ~a)...\n"
	      input-fits-file-name
	      output-gif-file-name
	      fixed-extrema?)
      (scale-map-to-muK! input-map)
      (map->png input-map
		output-png-file-name
		width (+ 50 (/ width 2))
		title
		#:component component-number
		#:color-extrema (if fixed-extrema?
				    (cons -100 +100)
				    #f))
       ;; Run `convert`. Note the elegance of "run*" (from the "shell"
       ;; egg): we include the command-line switches as if they were
       ;; Scheme symbols! (We put a comma in front of variable names
       ;; like `,output-png-file-name` because we want them to be
       ;; interpreted as a Scheme expression: otherwise they would be
       ;; put as they are in the arguments to the process call.)
		
      (call-with-values
	  (lambda ()
	    (run* (convert ,output-png-file-name
			   -transparent white
			   ,output-gif-file-name)))
	(lambda (convert-return-code)
	  (if (> convert-return-code 0)
	      (abort "Error when executing \"convert\""))))
      (delete-file output-png-file-name)))

  ;; This function is used with maps that contain 3 components (I, Q,
  ;; U). It uses the `map->gif` function defined below and the
  ;; `montage` program from the ImageMagick suite to create one GIF
  ;; file containing the three maps.
  (define (create-3-gifs-and-combine-them input-fits-file-name
					  output-gif-file-name
					  title
					  width
					  fixed-extrema?)
    (let ((temp-dir (create-temporary-directory)))
      (printf "Creating 3 temporary GIF files in ~a...\n"
	      temp-dir)
      (let ((I-file-name (filepath:join-path (list temp-dir "I.gif")))
	    (Q-file-name (filepath:join-path (list temp-dir "Q.gif")))
	    (U-file-name (filepath:join-path (list temp-dir "U.gif"))))
	(for-each (lambda (file-name component-number component-name)
		    (create-1-gif input-fits-file-name
				  file-name
				  (sprintf "~a (~a component)"
					   title component-name)
				  width
				  component-number
				  fixed-extrema?))
		  (list I-file-name Q-file-name U-file-name)
		  (list 1           2           3)
		  (list "I"         "Q"         "U"))
	(display "Running 'montage' on the three GIF images...\n")
	(run (montage -tile "1x3"
		      -geometry ,width
		      ,I-file-name
		      ,Q-file-name
		      ,U-file-name
		      ,output-gif-file-name)))))

  ;; This function is simply a nice wrapper for `create-1-gif` and
  ;; `create-3-gif-and-combine-them`.
  (define (map->gif input-fits-file-name
		    output-gif-file-name
		    title
		    #!key (width 512) (overwrite? #f) (fixed-extrema? #t))
    (call/cc ; Chicken's shortcut for call-with-current-continuation
     (lambda (return)
       ;; Note that, since `map2tga` does not overwrite existing GIF files,
       ;; if the file already exists and the flag `overwrite?` is true we
       ;; first need to delete the file.
       (if (file-exists? output-gif-file-name)
	   (if overwrite?
	       (delete-file output-gif-file-name)
	       (return '())))

       ;; Create the directory that will contain the output file,
       ;; if it does not exist
       (create-pathname-directory output-gif-file-name)

       ;; If the map contains three components (I, Q, U) we need to
       ;; create three GIF files and then combine them into one.
       (if (eq? (healpix:num-of-components-in-map input-fits-file-name)
		1)
	   (create-1-gif input-fits-file-name
			 output-gif-file-name
			 title
			 width
			 1
			 fixed-extrema?)
	   (create-3-gifs-and-combine-them input-fits-file-name
					   output-gif-file-name
					   title
					   width
					   fixed-extrema?)))))

  ;; For null tests, we're usually interested in TT, EE and BB spectra
  ;; only (also, they are much easier to visualize since they're
  ;; always positive and can therefore be plotted using a log-log
  ;; scale). These spectra are the first three to appear in the FITS
  ;; file. But we must be sure the spectrum is not only in temperature
  ;; (as `take` will complain if you request more elements than
  ;; available): this is the reason of `min`.
  (define (read-TT-EE-BB file-name)
    (let ((n (healpix:num-of-components-in-spectrum file-name)))
      (take (healpix:read-spectrum file-name)
	    (min n 3))))

  ;; This function reads the spectrum/spectra in a FITS file and save
  ;; all the data into a CSV file.
  (define (spectrum->csv input-fits-file-name
			 output-csv-file-name)

    ;; We are interested in the first three power spectra (TT, EE, BB) only.
    (let* ((columns (read-TT-EE-BB input-fits-file-name))
	   (num-of-rows (f64vector-length (car columns))))
      (printf "Writing CSV file '~a'...\n"
	      output-csv-file-name)
      (with-output-to-file output-csv-file-name
	(lambda ()
	  (do ((row-num 0 (+ 1 row-num)))
	      ((>= row-num num-of-rows))
	    (let ((row-elements (map (lambda (column-vector)
				       (* 1.0e+12 ; K^2 -> muK^2
					  (f64vector-ref column-vector
							 row-num)))
				     columns)))
	      (printf "~a " (+ 1 row-num)) ; this is ell
	      (print (string-intersperse (map ->string row-elements)
					 " "))))))))

  ;; This function reads the spectrum/spectra in a FITS file and save
  ;; all the data into a JavaScript file. This file contains the
  ;; definition of two variables: `cl_data` (an array formatted for
  ;; being used with Flot) and `cl_title` (a string containing the
  ;; title of the power spectrum).
  (define (spectrum->js input-fits-file-name
			output-js-file-name
			title)
    (let* ((columns (read-TT-EE-BB input-fits-file-name))
	   (num-of-rows (f64vector-length (car columns))))
      (printf "Writing JavaScript file '~a'...\n"
	      output-js-file-name)
      (with-output-to-file output-js-file-name
	(lambda ()
	  (printf "var cl_title = \"~a\";\n" title)
	  (print "var cl_data = [")
	  (for-each (lambda (column-vector)
		      (display "    [ ")
		      (do ((row-num 0 (+ 1 row-num)))
			  ((>= row-num num-of-rows))
			(let ((v (* 1.0e+12 ; Convert K^2 to muK^2
				    (f64vector-ref column-vector
						   row-num))))
			  (if (> v 0.0)
			      (printf "      [~a, ~a],\n"
				      (+ 1 row-num)
				      v))))
			  (print "    ],"))
		    columns)
	  (print "];")))))

  ;; Produce a GIF image containing the spectrum/spectra in a FITS
  ;; file. The function invokes `gnuplot`, which must therefore be in
  ;; the `PATH`.
  (define (spectrum->gif input-fits-file-name
			 output-gif-file-name
			 title
			 #!key (width 512) (overwrite? #f))
    (call/cc ; Chicken's shortcut for call-with-current-continuation
     (lambda (return)
       ;; Note that, since `map2tga` does not overwrite existing GIF files,
       ;; if the file already exists and the flag `overwrite?` is true we
       ;; first need to delete the file.
       (if (file-exists? output-gif-file-name)
	   (if overwrite?
	       (delete-file output-gif-file-name)
	       (return '())))
       (let ((csv-file-name
	      (filepath:replace-extension output-gif-file-name ".csv")))
	 ;; First step: convert the FITS file into a CSV file (placed
	 ;; alongside the GIF image under the destination directory)
	 (spectrum->csv input-fits-file-name csv-file-name)
	 ;; Now open a output pipe to `gnuplot` and send the appropriate
	 ;; commands to plot the spectrum/spectra.
	 (with-output-to-pipe
	  "gnuplot"
	  (lambda ()
	    (printf #<<EOF
set terminal gif transparent enhanced size ~a, ~a
set output '~a'
set logscale xy
set xlabel 'l'
set ylabel 'C_l'
set title '~a'

EOF
                    width
		    (inexact->exact (round (/ width 1.4)))
		    output-gif-file-name
		    title)
	    (if (eq? (healpix:num-of-components-in-spectrum input-fits-file-name)
		     1)
		;; `then` part
		(printf "plot '~a' using 1:2 with lines title ''"
			csv-file-name)
		;; `else` part
		(begin
		  (printf #<<EOF
plot '~a' using 1:2 with lines title 'TT', \
     '~a' using 1:3 with lines title 'EE', \
     '~a' using 1:4 with lines title 'BB'

EOF
			  csv-file-name
			  csv-file-name
			  csv-file-name)))))))))

  ;; This function converts the name of a FITS file containing a map
  ;; into the name of the `.gif` file that will contain the
  ;; representation of the map shown in the report. It strips the
  ;; directory and extension parts and substitutes them using
  ;; functions from the `filepath` eggs. Note that we want the GIF
  ;; file to be in a subdirectory of the output directory instead of
  ;; being in the same directory as the input FITS file: in this was
  ;; the user can run `tar` or `zip` on it to obtain a self-contained
  ;; report. The `tail` parameter specifies an optional string to be
  ;; put just in front of the extension (this is useful when you
  ;; create many GIFs from the same FITS file).
  (define (fits-name->gif-name fits-name #!optional (name-tail ""))
    (filepath:replace-directory
     (filepath:replace-extension fits-name
				 (string-concatenate (list name-tail ".gif")))
     "images"))

  ;; This function converts the name of a FITS file containing a
  ;; spectrum into the name of the `.js` file that will contain the
  ;; JavaScript array containing the values of C_ell. It works pretty
  ;; much the same as `fits-name->gif-name.
  (define (fits-name->js-name fits-name)
    (filepath:replace-directory
     (filepath:replace-extension fits-name ".js")
     "js"))

  ;; A small set of files (CSS styles, JavaScript files) need to be
  ;; copied to the output directory, in order to make the report
  ;; self-contained. These files should be taken from the directory
  ;; where the executable has been launched.

  ;; Chicken Scheme does not provide a function to copy whole
  ;; directories. So we implement one using a nice function implemented
  ;; in `directory-utils`: `directory-fold`. It takes a function to be
  ;; applied to each file, a user-defined value that is passed to this
  ;; function (we do not use it, so we call it `unused` and set it to
  ;; `#f`) and the name of the directory.
  (define (dir-copy source dest)
    (directory-fold
     (lambda (filename unused)
       (let ((output-file (filepath:join-path (list dest filename))))
	 (format #t "Copying file ~a to ~a...\n" filename dest)
	 ;; Create the destination directory if it does not exist
	 (create-pathname-directory output-file)
	 ;; Copy the file. `file_copy` is part of Chicken Scheme
	 (file-copy (filepath:join-path (list source filename))
		    output-file
		    'overwrite)))
     #f ; This is our value for `unused`
     source)))
