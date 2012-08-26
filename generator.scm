;; Null test HTML report generator
;; ===============================
;;
;; Generate a HTML page containing the results of a set of null tests.
;;
;; Author: Maurizio Tomasi
;;
;; Start date: August 13th, 2012

;; Initialization
;; ==============

;; We need the `json` egg in order to load the database of null test
;; results written by the Python script. The `html-tags` and
;; `html-utils` eggs are very useful to generate HTML from code, much
;; in the same way as described in Paul Graham's 16th chapter of [ANSI
;; Common LISP](http://www.paulgraham.com/acl.html). The `shell` egg
;; is used to call `map2gif`. The eggs `filepath` and
;; `directory-utils` are used respectively to manipulate file paths
;; and to create/modify the directory structure of a file system (we
;; are going to use it to create the tree of directories that will
;; contain the HTML report). SRFI-19 implements a set of functions to
;; deal with date and times. We use it to put timestamps in the
;; report.
(require-extension json
		   html-tags
		   html-utils
		   shell
		   filepath
		   directory-utils
		   srfi-19)

(include "user-settings.scm")   (import user-settings)
(include "json-utils.scm") (import json-utils)
(include "file-utils.scm") (import file-utils)
(include "html-gen-utils.scm") (import html-gen-utils)

;(include "halfring-coupled-horn.scm") (import halfring-coupled-horn)
;(include "single-surv-coupled-horn.scm") (import single-surv-coupled-horn)

(format #t "Report will be created under the following path: ~a\n"
        (assq-ref 'output-dir user-args))
(format #t "Directory where to look for JSON files: ~a\n"
        (assq-ref 'input-dir user-args))

(define json-dictionary (read-json-dictionary (assq-ref 'input-dir user-args)))

;; Report generation
;; ===============

;; We copy the content of a number of directories into the destination
;; path using `dir-copy` (defined in `file-utils.scm`). Currently only
;; `css` is copied, but a separate directory `js` for JavaScript files
;; might be needed in the future).
(let ((source-dir (filepath:take-directory (car (argv)))))
  (for-each
   (lambda (dir-name)
     (dir-copy (filepath:join-path (list source-dir dir-name))
               (filepath:join-path (list (assq-ref 'output-dir user-args)
                                         dir-name))))
   (list "css")))

;; The "information" page
;; ----------------------

;; Write the HTML file. Function `call-with-output-file` will call the
;; `lambda` function passing as parameter a stream (the `file`
;; parameter) that can be used for writing. At the end of the
;; execution of the `lambda` function, the file will be automatically
;; closed (much like Python's `with` statement).
(write-html
 'information
 (lambda (file)
   (display (wrap-html 'information
                       "General information about this release"
                       (<p> (format #f #<<EOF
This data release of the null tests contains ~a
objects. It was generated on <i>~a</i> by user <b>~a</b>.

EOF
                                    (length json-dictionary)
                                    (date->string (current-date)
                                                  "~A ~e ~B ~Y, at ~H:~M:~S")
                                    (get-environment-variable "USER"))))
            file)
   (newline file)))

;; The "Halfring coupled horn" page
;; -------------------------------------

(write-results-page json-dictionary
		    'halfring-pair
		    "halfring_detset_map"
		    "halfring_detset_cl"
		    "Coupled horn, halfring differences")


;; The "Halfring frequency" page
;; -------------------------------------

(for-each
 (lambda (freq)
   (let* ((freq-string (->string freq))
	  (sub-dictionary (filter (lambda (obj)
				    (equal? (assq-ref 'channel obj)
					    freq-string))
				  json-dictionary)))
     (if sub-dictionary
	 (write-results-page sub-dictionary
			     (cons 'halfring-frequency freq)
			     "halfring_frequency_map"
			     "halfring_frequency_cl"
			     (sprintf "~a GHz map, halfring differences"
				      freq)))))
 (list 30 44 70))

;; The "Single survey single channel" page
;; -------------------------------------

;; This is the trickiest of the tests, as there are _lots_ of plots to
;; show. We split the plots according to the radiometer, as this is
;; the most useful representation. First we need some way to sort the
;; results according to the survey. Since this null test deal with
;; survey differences, each FITS file has _two_ surveys associated
;; with it (e.g. the JSON tag is `"surveys": [5, 4]`, which means that
;; in this test SS4 map was subtracted from SS4 map). Unfortunately,
;; the two surveys are not listed in ascending order. The code for
;; `compare-surveys` does these things: (1) orders the two surveys for
;; both objects `obj1` and `obj2` (by mean of the functions `min` and
;; `max`: this works because we only have two surveys), (2) compares
;; the first (smallest) index of the survey for `obj1` and `obj2`: if
;; they're different, use this as the sorting criterion, otherwise use
;; the second (greatest) index of the survey.
(define (compare-surveys obj1 obj2)
  (let ((ss1-pair1 (apply min (assq-ref 'surveys obj1)))
	(ss2-pair1 (apply max (assq-ref 'surveys obj1)))
	(ss1-pair2 (apply min (assq-ref 'surveys obj2)))
	(ss2-pair2 (apply max (assq-ref 'surveys obj2))))
    (if (eq? ss1-pair1 ss1-pair2)
	(< ss2-pair1 ss2-pair2)
	(< ss1-pair1 ss1-pair2))))

;; Now we call `write-results-page` repeatedly. We use the keyword
;; `fallback-comparison:` as this allows us to specify a general
;; function to be used to sort the objects in the report.
(for-each
 (lambda (rad-symbol)
   (let* ((rad-string (symbol->string rad-symbol))
	  (sub-dictionary (filter (lambda (obj)
				    (equal? (assq-ref 'channel obj)
					    rad-string))
				  json-dictionary)))
     (write-results-page sub-dictionary
			 (cons 'surv-rad rad-symbol)
			 "surveydiff_single_ch_map"
			 "surveydiff_single_ch_cl"
			 (sprintf "~a, survey differences"
				  (symbol->string rad-symbol))
			 fallback-comparison: compare-surveys)))
 ;; Using SRFI-1's `iota` and some clever use of `map` we might avoid
 ;; listing all the radiometers here. But it would be less readable.
 (list 'LFI18M 'LFI18S
       'LFI19M 'LFI19S
       'LFI20M 'LFI20S
       'LFI21M 'LFI21S
       'LFI22M 'LFI22S
       'LFI23M 'LFI23S
       'LFI24M 'LFI24S
       'LFI25M 'LFI25S
       'LFI26M 'LFI26S
       'LFI27M 'LFI27S
       'LFI28M 'LFI28S))

;; The "Single survey coupled horn" page
;; -------------------------------------

(for-each
 (lambda (horn-pair)
   (let ((sub-dictionary (filter (lambda (obj)
				   (equal? (assq-ref 'channel obj)
					   horn-pair))
				 json-dictionary)))
     (write-results-page sub-dictionary
			 (cons 'surv-pair horn-pair)
			 "surveydiff_detset_map"
			 "surveydiff_detset_cl"
			 (sprintf "Horns ~a, survey differences"
				  horn-pair)
			 fallback-comparison: compare-surveys)))
 (list "18_23" "19_22" "20_21"))

;; The "Single survey frequency map" page
;; -------------------------------------

(for-each
 (lambda (freq)
   (let* ((freq-string (->string freq))
	  (sub-dictionary (filter (lambda (obj)
				    (equal? (assq-ref 'channel obj)
					    freq-string))
				  json-dictionary)))
     (if sub-dictionary
	 (write-results-page sub-dictionary
			     (cons 'surv-frequency freq)
			     "surveydiff_frequency_map"
			     "surveydiff_frequency_cl"
			     (sprintf "~a GHz map, halfring differences"
				      freq)
			     fallback-comparison: compare-surveys))))
 (list 30 44 70))
