;; Null test HTML report generator
;; ===============================
;;
;; Generate a HTML page containing the results of a set of null tests.
;;
;; Author: Maurizio Tomasi
;;
;; Start date: August 13th, 2012
;;
;; How to generate the executable
;; ==============================
;;
;; The program is written in Scheme
;; [R5RS](http://www.schemers.org/Documents/Standards/R5RS/), and it
;; needs to be compiled into an executable in order to be executed.
;; There are two ways to do it: either grab a precompiled binary (easy
;; way) or compile it by yourself (hard way). In the following section
;; we'll detail the two procedures.
;;
;; Grabbing the executable (easy way)
;; ----------------------------------
;;
;; You can ask Maurizio Tomasi for a self-contained bundle. At the LFI
;; DPC and at NERSC, he keeps one in his homedir. The bundle is a
;; directory containing the executable (named `standalone_generator`)
;; and a large set of dynamic libraries needed to run the program. You
;; can move this directory anywhere in your filesystem, but do not try
;; to run it on different architectures/distributions (e.g. from a
;; RedHat system to Ubuntu, from x86 to x86_64...).
;;
;; Compiling the executable from the source (hard way)
;; ---------------------------------------------------
;;
;; To compile the source code into an executable you must have the
;; [Chicken Scheme](http://www.call-cc.org/) compiler and a few
;; open-source libraries for Chicken (called
;; [_eggs_](http://wiki.call-cc.org/eggs)). To install Chicken, you
;; can use your package manager (e.g. `sudo apt-get install
;; chicken-bin` under Ubuntu Linux) if you are a `sudo` user.
;; Otherwise, you must install it from source in your home directory.
;; Open a terminal and run the following commands (if you do not see
;; the second line fully, select the text with the mouse):
;;
;;     mkdir -p $HOME/usr $HOME/.chicken-temp
;;     curl http://code.call-cc.org/releases/current/chicken.tar.gz | tar xz -C $HOME/.chicken-temp
;;     pushd $HOME/.chicken-temp/chicken-*
;;     make PLATFORM=linux PREFIX=$HOME/usr install
;;     popd
;;     rm -rf $HOME/.chicken-temp
;;
;; (If you want, you can change `$HOME/usr` with any other directory
;; you want. It should be the place where you usually install things
;; under your home directory.) At this point, if you did not so
;; already, put the following lines at the end of your `~/.profile`
;; (assuming you're using `sh`, `bash` or `dash` as your login shell):
;;
;;     export PATH=$HOME/usr/bin:$PATH
;;     export C_INCLUDE_PATH=$HOME/usr/include:$C_INCLUDE_PATH
;;     export LIBRARY_PATH=$HOME/usr/lib:$LIBRARY_PATH
;;     export LD_LIBRARY_PATH=$HOME/usr/lib:$LD_LIBRARY_PATH
;;     export MANPATH=$HOME/usr/man:$MANPATH
;;
;; Logout and login to see the new variabiles. If you are able to run
;; `chicken -version`, then the installation of the compiler
;; completed successfully.
;;
;; Now you need some "eggs" (that is, Chicken's libraries). Move to
;; the directory where you have the source code of
;; `plancknull_generate_html` and run the following command:
;;
;;     sudo make install_eggs
;;
;; (If you've installed Chicken under your homedir, omit `sudo`). Now
;; you should be able to compile the program, simply run
;;
;;     make generator
;;
;; This will silently convert `generator.scm` into a C program, which
;; will then be compiled to a standalone executable. The `Makefile`
;; provided with the source code allows for many targets, run `make
;; TARGET` where `TARGET` is one of the following:
;;
;; * `generator` produces the executable. Be careful that this program
;; needs the Chicken runtime libraries to be accessible at runtime
;; (therefore you cannot distribute it)
;; * `deploy` produces a stand-alone executable in the directory
;; `standalone_generator`. Unlike the `generator` target, the content
;; of the directory (containing the executable `standalone_generator`
;; plus many dynamic libraries) _can_ be distributed to others, as all
;; the Chicken libraries needed by the program are included in the
;; directory. (You can think of it as Chicken's analogous to Mac OS X
;; `.app` directories.)
;; * `install_eggs` repeatedly calls `chicken-install` to install the
;; eggs required to compile the program. You might have to use `sudo`
;; (e.g. this is the case if you installed Chicken using Ubuntu's
;; `apt-get`).
;; * `documentation` runs Schematic on the source code to produce this
;; documentation (in the `docs` directory).
;; * `help` prints a summary of the available options implemented in
;; the makefile.
;;
;; How to run the program
;; ======================
;;
;; To run the program, you must have
;; [`map2tga`](http://healpix.jpl.nasa.gov/html/facilitiesnode9.htm)
;; and `convert` (part of
;; [ImageMagick](http://www.imagemagick.org/script/index.php) &mdash;
;; its fork [GraphicsMagick](http://www.graphicsmagick.org/) should be
;; ok as well) in your path. `map2tga` is provided by
;; [Healpix](http://healpix.jpl.nasa.gov/): if you work with
;; Planck/LFI, you surely have it. (Note that it is already installed
;; at the LFI DPC.)
;;
;; The program needs as input one or more JSON files containing
;; information about the products of the null tests. Typically, these
;; are produced by the
;; [`plancknull`](https://github.com/zonca/plancknull) program. You
;; can specify them from the command line:
;;
;;     $ generator NULL_TEST_DIRECTORY OUTPUT_PATH
;;
;; (If you are using the standalone executable, run
;; `standalone_generator` instead of `generator`). This will read the
;; results of the null tests from the subdirectories under
;; `NULL_TEST_DIRECTORY`, and it will create the directory
;; `OUTPUT_PATH` and populate it with the files needed for the HTML
;; report. If `OUTPUT_PATH` does not exist, it will be silently
;; created.
;;
;; How to read the source code of this program
;; ===========================================
;;
;; This page was created automatically from the program source code
;; using [`schematic`](http://wiki.call-cc.org/eggref/4/schematic), a
;; documenting tool for Chicken Scheme. Install it from the command
;; line with the command `sudo chicken-install schematic`, then
;; run
;;
;;     schematic -f markdown generator.scm
;;
;; (assuming you have
;; [`markdown`](http://en.wikipedia.org/wiki/Markdown), which can be
;; easily installed using `apt-get` under Debian/Ubuntu). This will
;; create a sub-directory `html` where you'll find the source code of
;; this very webpage.

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

;; We import a separate module which contains a number of
;; useful definitions for working with JSON modules.
(include "json-utils.scm") (import json-utils)

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

;; GIF generation
;; ==============

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

;; Report generation
;; ===============

;; Copying common files
;; --------------------
;;
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
   source))

;; Now we can iterate over the names of the directories to be copied
;; (currently only `css`, but a separate directory `js` for JavaScript
;; files might be needed in the future).
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
