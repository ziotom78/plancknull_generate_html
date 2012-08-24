(module html-gen-utils
  (html:++ wrap-html write-html)

  (import chicken
	  scheme
	  extras
	  srfi-13
	  json-utils
	  user-settings)

  (require-extension directory-utils
		     filepath
		     html-tags
		     html-utils)

  ;; We're going to concatenate a number of HTML statements, so it is
  ;; nicer to have a shorthand for the Scheme function
  ;; `string-concatenate`. Note that Scheme identifiers can include
  ;; symbols as well, not only letters and numbers.
  (define html:++ string-concatenate)

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

  ;; The function `wrap-html` takes a string containing some HTML code
  ;; and encloses it in a self-contained HTML structure which
  ;; comprises the side menu. It returns a string.
  (define (wrap-html file-tag page-title body)
    (html-page (html:++ (list
			 (<h1> (make-title-for-report))
			 (side-menu file-tag)
			 (<div> id: "body"
				(<h2> page-title)
				body)))
	       title: page-title
	       css: '("css/main.css" "css/menu.css")))

  ;; We are going to create a number of HTML files, and the creation
  ;; of each of them follows the same rules:
  ;;
  ;; 1. Determine the name of the file using `get-html-file-name`
  ;; 1. Ensure that the directory where this file will be written
  ;;    exists
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
      (call-with-output-file output-file-name write-function))))
