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

(include "single-surv-coupled-horn.scm") (import single-surv-coupled-horn)

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

;; The "Single survey coupled horn" page
;; -------------------------------------

;; Write the HTML file. We're passing only a (sorted) subset of the
;; entries in `json-dictionary` (list of JSON objects loaded from the
;; input directory), namely only those that are relevant to this page.
(write-single-survey-horn-pair-page
 (sort (filter-on-filetype json-dictionary
			   '("surveydiff_detset_map"
			     "surveydiff_detset_cl"))
       (lambda (x y)
	 (let ((channel-x (assq-ref 'channel x))
	       (channel-y (assq-ref 'channel y)))
	   (if (not channel-x)
	       (abort (sprintf "Object #1 ~a does not have the \"channel\" tag"
			       x)))
	   (if (not channel-y)
	       (abort (sprintf "Object #2 ~a does not have the \"channel\" tag"
			       y)))
	   ;; Sort the entries according to their channel
	   (string<? channel-x channel-y)))))

;; The "Half rings" page
;; ---------------------

;; (write-html
;;  'halfring-frequency
;;  (lambda (file)
;;    (let* ((cur-dict (filter-on-filetype "halfring_frequency"))
;;        (fits-file-paths (map (lambda (x)
;;                                (assq-ref 'file_name x))
;;                              cur-dict)))
