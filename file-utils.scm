;; File utilities
;; ==============

;; This module contains a number of general-purpose functions that
;; help dealing with the (usually large) amount of files handled by
;; the program.

(module file-utils
  (export map2gif fits-name->gif-name)

  (import chicken scheme extras files srfi-13)
  (require-extension shell)
  (require-extension filepath)
  (require-extension directory-utils)

  ;; We need to implement the code that will convert the maps in FITS
  ;; format into GIF images that can be included in the HTML report. We
  ;; assume the availability of the `map2gif` program bundled with
  ;; [Healpix](http://healpix.jpl.nasa.gov/).

  ;; Note that, since `map2gif` does not overwrite existing GIF files,
  ;; if the file already exists and the flag `overwrite?` is true we
  ;; first need to delete the file.

  (define (map2gif input-fits-file-name
		   output-gif-file-name
		   title
		   #!key (overwrite? #f)) ; If not specified, it is #f
    (call/cc ; Chicken's shortcut for call-with-current-continuation
     (lambda (return)
       (if (file-exists? output-gif-file-name)
	   (if overwrite?
	       (delete-file output-gif-file-name)
	       (return '())))
       ;; Create the directory that will contain the output file,
       ;; if it does not exist
       (create-pathname-directory output-gif-file-name)
       ;; Run the program. Note the elegance of "run*" (from the "shell"
       ;; egg): we include the command-line switches as if they were
       ;; Scheme symbols! (We put a comma in front of `(html:++ ...)`
       ;; because we want it to be interpreted as a Scheme expression:
       ;; otherwise it would be put as it is in the arguments to the
       ;; process call.) *Note:* `run*` is defined in the `shell` egg.
       ;; When multiple commands are specified (in this case, `map2tga`
       ;; and `convert`), it returns a set of values. These must be
       ;; interpreted using `call-with-values`, which is a standard
       ;; Scheme function accepting a "producer" and a "consumer". Refer
       ;; to the [R5RS
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
			      -xsz 512
			      -title ,(string-concatenate (list "\"" title "\"")))
		     (convert ,output-tga-file-name
			      -transparent white
			      ,output-gif-file-name)))
	   (lambda (map2tga-return-code convert-return-code)
	     (if (> map2tga-return-code 0)
		 (abort "Error when executing \"map2tga\""))
	     (if (> convert-return-code 0)
		 (abort "Error when executing \"convert\""))))
	 (delete-file output-tga-file-name)))))

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


