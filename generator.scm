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

(write-results-page json-dictionary
		    'halfring-frequency
		    "halfring_frequency_map"
		    "halfring_frequency_cl"
		    "Frequency map, halfring differences")

;; The "Single survey single channel" page
;; -------------------------------------

(write-results-page json-dictionary
		    'surv-rad
		    "surveydiff_single_ch_map"
		    "surveydiff_single_ch_cl"
		    "Single channel, survey differences")

;; The "Single survey coupled horn" page
;; -------------------------------------

(write-results-page json-dictionary
		    'surv-pair
		    "surveydiff_detset_map"
		    "surveydiff_detset_cl"
		    "Coupled horn, survey differences")

;; The "Single survey coupled horn" page
;; -------------------------------------

(write-results-page json-dictionary
		    'surv-frequency
		    "surveydiff_frequency_map"
		    "surveydiff_frequency_cl"
		    "Frequency map, survey differences")
