;; Null test HTML report generator
;; ===============================
;;
;; Generate a HTML page containing the results of a set of null tests.
;;
;; Author: Maurizio Tomasi
;;
;; Start date: August 13th, 2012
;;
;; How to compile the program
;; ==========================
;;
;; The program is written in Scheme
;; [R5RS](http://www.schemers.org/Documents/Standards/R5RS/). It
;; requires the [Chicken Scheme](http://www.call-cc.org/) compiler and
;; a few open-source libraries (called
;; [_eggs_](http://wiki.call-cc.org/eggs)). Once you have Chicken on
;; your system (just run `sudo apt-get install chicken-bin` under
;; Debian/Ubuntu), you can install any egg you need using the command
;;
;;     sudo chicken-install <NAME>
;;
;; To compile this program, you need the following eggs:
;;
;; * [`shell`]((http://wiki.call-cc.org/eggref/4/shell) (run shell commands)
;; * [`filepath`]((http://wiki.call-cc.org/eggref/4/filepath) (operations on strings representing file paths)
;; * [`directory-utils`](http://wiki.call-cc.org/eggref/4/directory-utils) (operations on directories)
;; * [`json`](http://wiki.call-cc.org/eggref/4/json) (read/write JSON data)
;; * [`html-tags`](http://wiki.call-cc.org/eggref/4/html-tags) (generate HTML directly from Scheme code)
;; * [`html-utils`](http://wiki.call-cc.org/eggref/4/html-utils) (nice shorthands to produce complex HTML patterns)
;;
;; Use this command to build the executable:
;;
;;     csc generator.scm
;;
;; This will silently convert `generator.scm` into a C program, which
;; will then be compiled to a standalone executable. (Use the
;; `-output-file` option to save the C source file, if you need it
;; &mdash; see the [Chicken
;; documentation](http://wiki.call-cc.org/man/4/Using%20the%20compiler#distributing-compiled-c-files)
;; for instructions about how to compile this file.)
;;
;; To run the program, you must have
;; [`map2gif`](http://healpix.jpl.nasa.gov/html/facilitiesnode9.htm)
;; in your path. It is one of the programs provided by
;; [Healpix](http://healpix.jpl.nasa.gov/): if you work with
;; Planck/LFI, you surely have it. (Note that it is already installed
;; on the LFI DPC.)
;;
;; A very short introduction to Scheme
;; ===================================
;;
;; To help the reader who has never read Scheme code, I am summarizing
;; here the main characteristics of the language. This is not a Scheme
;; tutorial, just a general introduction written for people that are
;; already proficient with some other language (in the text there are
;; many references to Python). If you are not interested in reading
;; the source code of this program, you can skip this section.
;;
;; Chicken Scheme implements the R5RS standard of Scheme, a language
;; similar to LISP. Scheme is a really simple language (the language
;; and the standard library are described by a 50-page document, to
;; compare with Python 2.7, which needs 127 A4 pages for the language
;; plus 1366 pages for its huge library). This simplicity derives from
;; three facts:
;;
;; 1. The syntax is extremely simple: apart from letters and numbers,
;; the only symbols which have special meaning for the compiler are
;; `(`, `)`, `'`, `,` (rarely used) and whitespaces (plus `;`, which
;; starts a comment).
;; 2. The standard library is quite small (clearly this is not an
;; advantage, but Chicken Scheme provides a broad selection of
;; extensions, called "eggs", that mitigate this problem)
;; 3. No complex features of high-end languages are specified by the
;; standard. For instance, a typical Python program uses OOP
;; techniques, which are grounded on many non-trivial concepts (object
;; encapsulation, inheritance, abstract methods, static methods...).
;; You can easily [extend Scheme to support
;; OOP](http://community.schemewiki.org/?object-systems) through its
;; powerful macro system, but we won't do this in this program.
;;
;; Scheme is based on the concept of list, which is a series of
;; elements separated by spaces and enclosed within parentheses, like
;; `(1 2 3)`. Function calls are lists where the first element is the
;; function and the others are the parameters. E.g., to calculate the
;; sinus of 0.1 you write `(sin 0.2)`, to print a string you write
;; `(print "Hello, world!")`. By default, a list is always interpreted
;; as a function call, unless there is a `'` before the open
;; parenthesis. So `(sin 0.1)` calculates the sinus of 0.1, but `'(sin
;; 0.1)` is a list of two elements: the first is the `sin` function,
;; the second is the number 0.1. Therefore, in Scheme program and data
;; share the same representation, and you can easily convert one into
;; another (using e.g. `eval`: `(eval '(sin 0.1))` is the same as
;; `(sin 0.1)`, but in the first case you can build your list
;; programatically).
;;
;; The parenthesis syntax is used everywhere, also in mathematical
;; expressions ("infix notation", sometimes known as "reverse Polish
;; notation"). To calculate `5 * (1 + 2 + 3)` you write `(* 5 (+ 1 2
;; 3))`. (Note that both `*` and `+` are used like any other function
;; call.)
;;
;; Function and variable definition share the same syntax: you have to
;; use `define`. For instance, `(define x 1)` creates a new variable
;; which contains the integer 1, while `(define (f x) (* 2 x))`
;; defines a function `f` which accepts one parameter `x` and which
;; returns `x` doubled.
;;
;; Similarly to Python, anonymous functions can be defined using
;; `lambda`. Unlike Python, Scheme's `lambda` expressions can contain
;; any sequence of instructions.
;;
;; Loops can be implemented using either recursion (a "functional"
;; construct) or `do` (an "imperative" construct). However, for simple
;; programs like the one we are describing here, we only rely on
;; functions like `map` and `filter`, which are analogous to Python's
;; counterparts.
;;
;; To end with an example, consider this Python code:
;;
;;     print ', '.join([x.upper() for x in ("a", "b", "c")])
;;
;; which prints "A, B, C". It can be translated in Chicken Scheme:
;;
;;     (string-intersperse (map string-upcase
;;                              '("a" "b" "c"))
;;                         ", ")
;;
;; (In Scheme you usually use much more newlines than in imperative
;; programs. This helps in visualizing which arguments are parts of
;; which list, as it is easy to get confused by nested parentheses. In
;; the following of this document, you can highlight parentheses on
;; the code on the right by moving the mouse over it.)
;;
;; Thanks to its powerful macro system, Scheme (and LISP languages in
;; general) is very powerful in handling HTML. The Chicken's library
;; [`html-tags`](http://wiki.call-cc.org/eggref/4/html-tags)
;; implements HTML-like commands in Scheme, which are converted into
;; strings. The idea is that every time you have some HTML tag in the
;; form `<tag>...</tag>`, you write it as the Scheme command `(<tag>
;; ...)`, where parentheses are used to delimit the tag. This command
;; is converted into a string, that can then be printed on screen.
;; Here is an example:
;;
;;     (require-extension html-tags)
;;     (print (<h1> (format #f "The result of the sum is ~a" (+ 3 6))))
;;
;; Note that we can call Scheme functions like `format` and `+` within
;; the HTML tags. This program will print
;;
;;     <h1>The result of the sum is 9</h1>
;;
;; This differs substantially from the typical approach of using a
;; template library like Python's [Jinja2
;; library](http://jinja.pocoo.org/docs) or
;; [Django](https://www.djangoproject.com): in that case, you write a
;; HTML template interspersed with Jinja2's [internal
;; language](http://jinja.pocoo.org/docs/templates/#expressions)
;; (which, although similar, is _not_ Python: see e.g. the use of the
;; `|` operator) - the same [applies to Django as
;; well](https://docs.djangoproject.com/en/1.4/topics/templates/).
;; Thus, you have to learn a new language (other than Python and HTML)
;; to use it. In our case we instead use Scheme for everything.
;;
;; Scheme's syntax can look weird at first, but it is grounded on two
;; very simple elements: parentheses (which group elements) and white
;; spaces (which separate elements within parentheses). Compare this
;; with e.g. Python, where there are many symbols to be used in a
;; program: `()` identifies a tuple (or the parameters in a function
;; call), `[]` a list, `{}` a dictionary, `:` introduces a sub-loop or
;; a definition, `;` separates statements in the same line, etc. And
;; there are some strange quirks in the language, e.g. you have to
;; remember that a one-element list can be written as `[1]`, but for a
;; one-element tuple you must append a comma: `(1,)`.
;;
;; How to read the source code of this program
;; ===========================================
;;
;; The program source code was formatted with the aim of making it
;; look nice using
;; [`schematic`](http://wiki.call-cc.org/eggref/4/schematic), a
;; documenting tool for Chicken Scheme. Install it using
;; `chicken-install` as usual, then run
;;
;;     schematic -f markdown generator.scm
;;
;; (assuming you have
;; [`markdown`](http://en.wikipedia.org/wiki/Markdown), which can be
;; easily installed using `apt-get` under Debian/Ubuntu). This will
;; create a sub-directory `html` where you'll find the source code of
;; the program nicely formatted in a HTML file &mdash; it's much
;; easier to read!

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
;; contain the HTML report).
(require-extension json)
(require-extension html-tags)
(require-extension html-utils)
(require-extension shell)
(require-extension filepath)
(require-extension directory-utils)

;; We're going to concatenate a number of HTML statements, so it is
;; nicer to have a shorthand for the Scheme function
;; `string-concatenate`. Note that Scheme identifiers can include
;; symbols as well, not only letters and numbers.
(define html:++ string-concatenate)

;; Since JSON entries are going to be handled as _a-lists_
;; ([association
;; lists](http://en.wikipedia.org/wiki/Association_list)), we'll often
;; use a nice shorthand which is not provided by standard Scheme (note
;; however that `assq-ref` is a
;; [built-in](http://www.gnu.org/software/guile/manual/html_node/Retrieving-Alist-Entries.html)
;; in GNU Guile, but - alas! - not in Chicken Scheme).
(define (assq-ref entry a-list) (cdr (assq entry a-list)))

;; JSON parsing
;; ============

;; The `json-read` function returns a vector of pairs of the form
;; `(string . value)` (with `string` being the key). However, this is
;; not the best representation for the data. First of all, when
;; looking for a specific key it is much faster to work with symbols
;; than with strings. Also, in order to use the built-in Scheme
;; function `assq` we need to have lists, not vectors. The function
;; `json->alist` performs these two conversions, by means of
;; `string->symbol` and `vector->list` (two Scheme built-ins).
(define (json->alist dictionary)
  (map (lambda (x)
	 (cons (string->symbol (car x))
	       (cdr x)))
       (vector->list dictionary)))

;; Command-line parsing
;; ====================

;; Here we implement the code that will read the JSON files from the
;; command-line and merge them into one big list of dictionaries
;; (a-lists).

;; The names of the files to be read are taken from the command line
;; (`cdr` returns all the elements of the list but the first, since
;; this is the name of the executable file). Note that `argv`
;; (function, not variable!) is not part of R5RS: it is a Chicken
;; extension.
(define input-files (cdr (argv)))

;; Parse a file and return a list of associative lists suitable for
;; being used with functions like `assq` and `assv`.
(define (extract-alists-from-json-file file-name)
  (let ((entries (call-with-input-file file-name json-read)))
    (map json->alist entries)))

;; Read all the files specified from the command line and join the
;; a-lists returned by `extract-alists-from-json-file`. Note that
;; `append` is not a R5RS built-in (it is part of
;; [SRFI-1](http://srfi.schemers.org/srfi-1/srfi-1.html)), but Chicken
;; incorporates it.
(define dictionary
  (apply append (map extract-alists-from-json-file input-files)))

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
     ;; Run the program. Note the elegance of "run" (from the "shell" egg):
     ;; we include the command-line switches as if they were Scheme symbols!
     (run (map2gif -inp ,input-fits-file-name
		   -out ,output-gif-file-name
		   -bar .T.
		   -xsz 512
		   -ttl ,(html:++ (list "\"" title "\"")))))))

;; This function converts the name of a FITS file containing a map
;; into the name of the `.gif` file that will contain the
;; representation of the map shown in the report. It strips the
;; directory and extension parts and substitutes them using functions
;; from the `filepath` eggs. Note that we want the GIF file to be in a
;; subdirectory of the current directory instead of being in the same
;; directory as the input FITS file: in this was the user can run
;; `tar` or `zip` on it to obtain a self-contained report.
(define (fits-name->gif-name fits-name)
  (filepath:replace-directory (filepath:replace-extension fits-name ".gif")
			      "./images"))

;; HTML generation
;; ===============

;; We keep the names of the HTML files to be created in a alist for
;; avoiding repetitions in the code (they must be used when creating
;; the files and when producing the `<a>` links in the dropdown menu).
;; The function `get-html-file-name` is the only access function we
;; need for this alist, so we make `html-file-name` a local
;; definition.
(define (get-html-file-name label)
  (let ((html-file-names
	 '((information . "index.html")
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

;; The page contains a drop-down menu. Its look is specified in the
;; CSS file `dropdown_menu.css`, and its style is plagiarized from [a
;; nice tutorial available on the
;; WWW](http://dhirajkumarsingh.wordpress.com/2012/05/20/css3-animated-dropdown-menu/).
;; Note how nice is to use the `html-tag` package: we are producing
;; HTML code using commands like `<a>` just in plain HTML, yet we can
;; freely call Scheme functions within it (in this case,
;; `get-html-file-name`). This is a cleaner approach than other
;; template libraries, which usually need to define and implement some
;; ad-hoc language (e.g. the Python library
;; [Jinja2](http://jinja.pocoo.org/)).
(define dropdown-menu
  (<nav> id: "nav"
	 (<ul> id: "menu"
	       (<li> (<a> href: (get-html-file-name 'information)
			  "Information"))
	       (<li> (<a> href: "#" "1-h tests &raquo;"))
	       (<li> (<a> href: "#" "Survey tests &raquo;")
		     (<ul>
		      (<li> (<a> href: (get-html-file-name 'surv-rad)
				 "Single radiometer"))
		      (<li> (<a> href: (get-html-file-name 'surv-horn)
				 "Single horn"))
		      (<li> (<a> href: (get-html-file-name 'surv-pair)
				 "Horn pair"))
		      (<li> (<a> href: (get-html-file-name 'surv-frequency)
				 "Frequency"))
		      (<li> (<a> href: (get-html-file-name 'surv-cross-freq)
				 "Cross-frequency"))))
	       (<li> (<a> href: "#" "Full-mission tests &raquo;")
		     (<ul>
		      (<li> (<a> href: (get-html-file-name 'full-pair)
				 "Horn pair"))
		      (<li> (<a> href: (get-html-file-name 'full-frequency)
				 "Frequency"))
		      (<li> (<a> href: (get-html-file-name 'full-cross-freq)
				 "Cross-frequency"))))
	       (<li> (<a> href: (get-html-file-name 'table-of-contents)
			  "Table of contents")))))

;; The function `wrap-html` takes a string containing some HTML code
;; and encloses it in a self-contained HTML structure which comprises
;; the dropdown menu. It returns a string.
(define (wrap-html title body)
  (html-page (html:++ (list
		       (<h1> title)
		       dropdown-menu
		       body))
	     title: title
	     css: '("css/main.css" "css/dropdown_menu.css")))

;; This is the name of the data release. The function `make-title`
;; makes up the title for a page. It does so by prepending the name of
;; the data release to the title itself (in the parameter `string`).
;; We want to put the name of the release (e.g. `DX9`) at the
;; beginning, so it will be shown by tabbed browsers like Chrome even
;; when a lot of tabs are opened.
;;
;; *TODO*: make the release name specifiable from the command
;; line/configuration file
(define (make-title string)
  (let ((test-release-name "DX9"))
    (format #f "~a null tests: ~a" test-release-name string)))

;; The "information" page
;; ----------------------

;; Write the HTML file. Function `call-with-output-file` will call the
;; `lambda` function passing as parameter a stream (the `file`
;; parameter) that can be used for writing. At the end of the
;; execution of the `lambda` function, the file will be automatically
;; closed (much like Python's `with` statement).
(call-with-output-file (get-html-file-name 'information)
  (lambda (file)
    (display (wrap-html (make-title "results")
			(<p> "General information about the release"))
	     file)
    (newline file)))

;; The "Single survey coupled horn" page
;; -------------------------------------

;; Write the HTML file. The variable `cur-dict` contains a subset of
;; all the JSON entries stored in `dictionary` (loaded from the JSON
;; files specified from the command line). The use of `filter` is
;; exactly the same as in Python.
(call-with-output-file (get-html-file-name 'surv-pair)
  (lambda (file)
    (let* ((cur-dict (filter (lambda (x)
			       (equal? (assq-ref 'file_type x)
				       "single_survey_coupled_horn_map"))
			     dictionary))
	   (fits-file-paths (map (lambda (x)
				   (assq-ref 'file_path x))
				 cur-dict)))
      ;; Create the .gif files
      (for-each (lambda (test-result)
		  (let ((fits-file-name (assq-ref 'file_path test-result)))
		    (format #t "Writing to ~a\n" (fits-name->gif-name fits-file-name))
		    (map2gif fits-file-name
			     (fits-name->gif-name fits-file-name)
			     (assq-ref 'title test-result))))
		cur-dict)
      
      ;; Write links to each image
      (display (wrap-html (make-title "coupled horn, survey differences")
			  (itemize (map (lambda (fits-name)
					  (let ((gif-name (fits-name->gif-name fits-name)))
					    (<img> src: gif-name
						   alt: gif-name)))
					fits-file-paths)))
	       file)
      (newline file))))
