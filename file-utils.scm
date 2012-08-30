;; File utilities
;; ==============

;; This module contains a number of general-purpose functions that
;; help dealing with the (usually large) amount of files handled by
;; the program.

(module file-utils
  (map->gif
   spectrum->gif
   fits-name->gif-name
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
	  srfi-13)
  (require-extension shell)
  (require-extension filepath)
  (require-extension directory-utils)

  ;; We need to implement the code that will convert the maps in FITS
  ;; format into GIF images that can be included in the HTML report. We
  ;; assume the availability of the `map2tga` program bundled with
  ;; [Healpix](http://healpix.jpl.nasa.gov/).

  ;; First we define a simple function that uses `map2tga` and
  ;; ImageMagick's `convert` to produce a GIF file with the map
  ;; (`map2gif` has a
  ;; [bug](http://sourceforge.net/tracker/?func=detail&aid=3559308&group_id=130539&atid=718128)
  ;; that prevents it from creating valid GIF images, at least in
  ;; Healpix 2.20a).
  (define (create-1-gif input-fits-file-name
			output-gif-file-name
			title
			width
			component-number)
       ;; Run `map2tga` and `convert`. Note the elegance of "run*"
       ;; (from the "shell" egg): we include the command-line switches
       ;; as if they were Scheme symbols! (We put a comma in front of
       ;; `(html:++ ...)` because we want it to be interpreted as a
       ;; Scheme expression: otherwise it would be put as it is in the
       ;; arguments to the process call.) *Note:* `run*` is defined in
       ;; the `shell` egg. When multiple commands are specified (in
       ;; this case, `map2tga` and `convert`), it returns a set of
       ;; values. These must be interpreted using `call-with-values`,
       ;; which is a standard Scheme function accepting a "producer"
       ;; and a "consumer". Refer to the [R5RS
       ;; documentation](http://wiki.call-cc.org/man/4/The%20R5RS%20standard#control-features)
       ;; for further details.
    (let ((output-tga-file-name
	   (filepath:replace-extension output-gif-file-name ".tga")))
      (call-with-values
	  (lambda ()
	    (format #t "Running map2tga and convert on ~a...\n"
		    input-fits-file-name)
	    (run* (map2tga ,input-fits-file-name
			   ,output-tga-file-name
			   -bar
			   -xsz ,width
			   -sig ,component-number
			   -title ,(string-concatenate (list "\"" title "\"")))
		  (convert ,output-tga-file-name
			   -transparent white
			   ,output-gif-file-name)))
	(lambda (map2tga-return-code convert-return-code)
	  (if (> map2tga-return-code 0)
	      (abort "Error when executing \"map2tga\""))
	  (if (> convert-return-code 0)
	      (abort "Error when executing \"convert\""))))
      (delete-file output-tga-file-name)))

  ;; This function is used with maps that contain 3 components (I, Q,
  ;; U). It uses the `map->gif` function defined below and the
  ;; `montage` program from the ImageMagick suite to create one GIF
  ;; file containing the three maps.
  (define (create-3-gifs-and-combine-them input-fits-file-name
					  output-gif-file-name
					  title
					  width)
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
				  component-number))
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
			 1)
	   (create-3-gifs-and-combine-them input-fits-file-name
					   output-gif-file-name
					   title
					   width)))))

  ;; This function reads the spectrum/spectra in a FITS file and save
  ;; all the data into a CSV file.
  (define (spectrum->csv input-fits-file-name
			 output-csv-file-name)
    (let* ((columns (healpix:read-spectrum input-fits-file-name))
	   (num-of-rows (f64vector-length (car columns))))
      (printf "Writing CSV file '~a'...\n"
	      output-csv-file-name)
      (with-output-to-file output-csv-file-name
	(lambda ()
	  (do ((row-num 0 (+ 1 row-num)))
	      ((>= row-num num-of-rows))
	    (let ((row-elements (map (lambda (column-vector)
				       (f64vector-ref column-vector
						      row-num))
				     columns)))
	      (printf "~a " (+ 1 row-num)) ; this is ell
	      (print (string-intersperse (map ->string row-elements)
					 " "))))))))

  ;; Produce a GIF image containing the spectrum/spectra in a FITS
  ;; file. The function invokes `gnuplot`, which must therefore be in
  ;; the `PATH`.
  (define (spectrum->gif input-fits-file-name
			 output-gif-file-name
			 title
			 #!key (width 512) (overwrite? #f))
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
set logscale y
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
	     (printf "plot '~a' using 1:2 with linespoints title ''"
		     csv-file-name)
	     ;; `else` part
	     (begin
	       (printf #<<EOF
plot '~a' using 1:2 with lines title 'TT', \
     '~a' using 1:3 with lines title 'EE', \
     '~a' using 1:4 with lines title 'BB', \
     '~a' using 1:3 with lines title 'TE', \
     '~a' using 1:3 with lines title 'TB', \
     '~a' using 1:3 with lines title 'EB' \

EOF
                      csv-file-name
		      csv-file-name
                      csv-file-name
		      csv-file-name
                      csv-file-name
		      csv-file-name)))))))

  ;; This function converts the name of a FITS file containing a map
  ;; into the name of the `.gif` file that will contain the
  ;; representation of the map shown in the report. It strips the
  ;; directory and extension parts and substitutes them using functions
  ;; from the `filepath` eggs. Note that we want the GIF file to be in a
  ;; subdirectory of the output directory instead of being in the same
  ;; directory as the input FITS file: in this was the user can run
  ;; `tar` or `zip` on it to obtain a self-contained report.
  (define (fits-name->gif-name fits-name)
    (filepath:replace-directory
     (filepath:replace-extension fits-name ".gif")
     "images"))

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


