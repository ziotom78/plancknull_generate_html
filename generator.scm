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
(require-extension json)
(require-extension html-tags)
(require-extension html-utils)
(require-extension shell)
(require-extension filepath)
(require-extension directory-utils)
(require-extension srfi-19)

(include "json-utils.scm") (import json-utils)
(include "file-utils.scm") (import file-utils)

;; We're going to concatenate a number of HTML statements, so it is
;; nicer to have a shorthand for the Scheme function
;; `string-concatenate`. Note that Scheme identifiers can include
;; symbols as well, not only letters and numbers.
(define html:++ string-concatenate)

;; Command-line parsing
;; ====================

;; Here we implement the code that will read the JSON files from the
;; command-line and merge them into one big list of dictionaries
;; (a-lists).

;; If `args` is the list of command-line arguments provided to the
;; program, function `parse-command-line` analyzes them and returns an
;; associative list containing the following fields: `'output-dir` is
;; a string specifying the directory where to save the report,
;; `'input-dir` is a string containing the path where to look for JSON
;; files.
(define (parse-command-line program-name args)
  (if (not (eq? (length args) 2))
      (begin (format #t #<<EOF
                     Usage: ~a NULL_TEST_DIR OUTPUT_PATH

                     where NULL_TEST_DIR is the path to the directory containing
                     the results of the null tests to be included in the report,
                     and OUTPUT_PATH is the path where to save the files of the
                     HTML report. If OUTPUT_PATH does not exist, it will be created.

EOF
                     program-name)
             (exit 1)))
  (list (cons 'input-dir (car args))
        (cons 'output-dir (cadr args))))

;; The variable `user-args` holds all the information the user
;; specified from the command line. Note that `argv` (function, not
;; variable!) is not part of R5RS: it is a Chicken extension.
(define user-args (parse-command-line (car (argv))
                                      (cdr (argv))))

(format #t "Report will be created under the following path: ~a\n"
        (assq-ref 'output-dir user-args))
(format #t "Directory where to look for JSON files: ~a\n"
        (assq-ref 'input-dir user-args))

;; Find all the files in the null test directory and read their
;; contents, then assemble all the objects (a-lists) into a list
;; (using `extract-alists-from-json-file`). Note that `append` is not
;; a R5RS built-in (it is part of
;; [SRFI-1](http://srfi.schemers.org/srfi-1/srfi-1.html)), but Chicken
;; incorporates it. Also, `find-files` is part of the Chicken `posix`
;; library, and it accepts regular expressions to match the files (the
;; meaning of `.+\\.json$` is: a sequence of N>0 characters which ends
;; with `.json`).
(define json-dictionary
  (apply append (map extract-alists-from-json-file
                     (find-files (assq-ref 'input-dir user-args)
                                 ".+\\.json$"))))

;; The function `filter-on-filetype` filters the contents of
;; `json-dictionary` acc ording to a given file type. The use of
;; `filter` is exactly the same as in Python.
(define (filter-on-filetype file-type)
  (filter (lambda (object)
            (equal? (assq-ref 'file_type object)
                    file-type))
          json-dictionary))

;; Given a JSON object, returns the full path associated with it. To
;; understand this code, keep in mind that the path specified in each
;; JSON object is relative to the root directory where the null tests
;; were saved. The path to this root directory is passed to the
;; program through the command line (hence the reference to
;; `user-args`).
(define (abspath-from-json object)
  (filepath:join-path
   (list
    (assq-ref 'input-dir user-args)
    (assq-ref 'file_name object))))

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

;; General-purpose functions
;; -------------------------

;; We keep the names of the HTML files to be created in a alist for
;; avoiding repetitions in the code (they must be used when creating
;; the files and when producing the `<a>` links in the side menu). The
;; function `get-html-file-name` is the only access function we need
;; for this alist, so we make `html-file-name` a local definition.
(define (get-html-file-name label)
  (let ((html-file-names
         '((information . "index.html")
	   (halfring-pair . "halfring_pair.html")
           (halfring-frequency . "halfring_freq.html")
           (surv-rad . "surv_radiometer.html")
           (surv-horn . "surv_horn.html")
           (surv-pair . "surv_pair.html")
           (surv-frequency . "surv_freq.html")
           (surv-cross-freq . "surv_cross.html")
           (full-pair . "full_pair.html")
           (full-frequency . "full_freq.html")
           (full-cross-freq . "full_cross.html")
           (table-of-contents . "toc.html"))))
    (assq-ref label html-file-names)))

;; Each HTML page contains a menu on the left, whose look is specified
;; by the CSS file `css/menu.css`. Function `side-menu` is
;; responsibile for creating this menu for each HTML page. The
;; argument `page-tag` is one of the symbols recognized by
;; `get-html-file-name` (see above), and specifies for which HTML page
;; we are generating this menu. We define a `smart-<a>` function which
;; uses `get-html-file-name` to retrieve the link to be used in the
;; `<a>` tag, and it also defines the class according to the fact that
;; the entry links to its own page or not (the CSS style uses this to
;; highlight selected items in bold).
;;
;; Note how nice is to use the `html-tag` package: we are producing
;; HTML code using commands like `<a>` just in plain HTML, yet we can
;; freely call Scheme functions within it (in this case,
;; `get-html-file-name`).
(define (side-menu page-tag)
  (let ((smart-<a> (lambda (title tag)
                     (<a> href: (get-html-file-name tag)
                          class: (if (eq? tag page-tag)
                                     "selected"
                                     "unselected")
                          title))))
    (<nav> id: "nav"
           (<ul> id: "menu"
                 (<li> (smart-<a> "Information" 'information))
                 (<li> "1-h tests (half rings) &raquo;"
                       (<ul> (smart-<a> "Horn pair" 'halfring-pair))
                       (<ul> (smart-<a> "Frequency" 'halfring-frequency)))
                 (<li> "Survey tests &raquo;"
                       (<ul>
                        (<li> (smart-<a> "Single radiometer" 'surv-rad))
                        (<li> (smart-<a> "Single horn" 'surv-horn))
                        (<li> (smart-<a> "Horn pair" 'surv-pair))
                        (<li> (smart-<a> "Frequency" 'surv-frequency))
                        (<li> (smart-<a> "Cross-frequency" 'surv-cross-freq))))
                 (<li> "Full-mission tests &raquo;"
                       (<ul>
                        (<li> (smart-<a> "Horn pair" 'full-pair))
                        (<li> (smart-<a> "Frequency" 'full-frequency))
                        (<li> (smart-<a> "Cross-frequency" 'full-cross-freq))))
                 (<li> (smart-<a> "Table of contents" 'table-of-contents))))))

;; This is the name of the data release. *TODO*: make the release name
;; specifiable from the command line/configuration file
(define test-release-name "DX9")

;; The function `make-title-for-report` makes up the title for the
;; overall report, which is going to be repeated at the beginning of
;; each HTML page. It does so by prepending the name of the data
;; release to the title itself (in the parameter `string`). We want to
;; put the name of the release (e.g. `DX9`) at the beginning, so it
;; will be shown by tabbed browsers like Chrome even when a lot of
;; tabs are opened.
(define (make-title-for-report)
  (format #f "~a null tests" test-release-name))

;; The function `wrap-html` takes a string containing some HTML code
;; and encloses it in a self-contained HTML structure which comprises
;; the side menu. It returns a string.
(define (wrap-html file-tag page-title body)
  (html-page (html:++ (list
                       (<h1> (make-title-for-report))
                       (side-menu file-tag)
                       (<div> id: "body"
                              (<h2> page-title)
                              body)))
             title: page-title
             css: '("css/main.css" "css/menu.css")))

;; We are going to create a number of HTML files, and the creation of
;; each of them follows the same rules:
;;
;; 1. Determine the name of the file using `get-html-file-name`
;; 1. Ensure that the directory where this file will be written exists
;; 1. Inform the user about the pathname of the file we are going to
;;    write
;; 1. Write the file.
;;
;; Function `write-html` wraps all of this into one function. The
;; parameter `file-tag` is passed to `get-html-file-name` as it is,
;; while `write-function` is a function accepting as its unique
;; parameter the output stream to be used to write into the file.
(define (write-html file-tag write-function)
  (let ((output-file-name (filepath:join-path
                           (list (assq-ref 'output-dir user-args)
                                 (get-html-file-name file-tag)))))
    (create-pathname-directory output-file-name)
    (format #t "Writing file ~a...\n" output-file-name)
    (call-with-output-file output-file-name write-function)))

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

;; Write the HTML file. The variable `cur-dict` contains a subset of
;; all the JSON entries stored in `json-dictionary` (loaded from the
;; JSON files specified from the command line).
(write-html
 'surv-pair
 (lambda (file)
   (let ((list-of-objs (sort (filter-on-filetype "surveydiff_detset_map")
			     (lambda (x y)
			       ;; Sort the entries according to their channel
			       (string<? (assq-ref 'channel x)
					 (assq-ref 'channel y)))))

	 ;; The function `emit-HTML-index-entry-for-object is still
	 ;; part of the `let` above. It accepts a JSON object and will
	 ;; produce a link to the entry.
	 (emit-HTML-index-entry-for-object
	  (lambda (obj)
	    (let ((title (assq-ref 'title obj)))
	      (<ul> (<a> href: "#" title "\n")))))

	 ;; The function `emit-HTML-for-object` is still part of the
	 ;; `let` above. It accepts a JSON object and will produce
	 ;; HTML code to be put straight into the page.
	 (emit-HTML-for-object
	  (lambda (obj)
	    (let* ((title (assq-ref 'title obj))
		   (fits-file-name (abspath-from-json obj))
		   (gif-file-name (fits-name->gif-name fits-file-name)))
	      (format #t "Writing GIF file ~a\n" gif-file-name)
	      (map2gif fits-file-name
		       (filepath:join-path
			(list (assq-ref 'output-dir user-args)
			      gif-file-name))
		       title)
	      (<div> class: "page_section"
	       (<h4> title)
	       "\n"
	       (<img> src: gif-file-name alt: title)
	       "\n")))))

     ;; The `let` has ended, now comes the body of `lambda (file)
     ;; ...`. We simply apply `emit-HTML-for-object` to the whole list
     ;; of objects, and wrap the output into some nice HTML tags
     ;; (using `wrap-html`).
     (display (wrap-html 'surv-pair
                         "Coupled horn, survey differences"
			 (html:++ (list (<div> class: "page_index"
					       (itemize (map emit-HTML-index-entry-for-object
							  list-of-objs)))
					(string-intersperse
					 (map emit-HTML-for-object list-of-objs)
					 "\n"))))
	      file)
     (newline file))))

;; The "Half rings" page
;; ---------------------

;; (write-html
;;  'halfring-frequency
;;  (lambda (file)
;;    (let* ((cur-dict (filter-on-filetype "halfring_frequency"))
;;        (fits-file-paths (map (lambda (x)
;;                                (assq-ref 'file_name x))
;;                              cur-dict)))
