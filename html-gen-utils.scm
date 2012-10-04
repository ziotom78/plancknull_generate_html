(module html-gen-utils
  (html:++
   wrap-html
   write-html
   emit-HTML-index-entry-for-object
   emit-HTML-for-map-object
   emit-HTML-for-object
   write-results-page)

  (import chicken
	  scheme
	  extras
	  srfi-13
	  data-structures
	  json-utils
	  file-utils
	  user-settings)

  (require-extension directory-utils
		     filepath
		     html-tags
		     html-utils)

  (define gif-exts (cons "100muK" "variable"))

  ;; We're going to concatenate a number of HTML statements, so it is
  ;; nicer to have a shorthand for the Scheme function
  ;; `string-concatenate`. (Also, our shorthand does not require a
  ;; list but a variable number of parameters.) Note that Scheme
  ;; identifiers can include symbols as well, not only letters and
  ;; numbers.
  (define (html:++ . args)
    (string-concatenate args))

  ;; This function returns the name of the HTML file to create given a
  ;; variable `label` that uniquely identifies it. Such variable can
  ;; either be a symbol or a pair, where the first element (`car`) is
  ;; a symbol and the second element is some other type whose meaning
  ;; depends on the `car`. The pair must be unique, but neither its
  ;; `car` nor its `cdr` have to.
  (define (get-html-file-name label)
    (cond ((eq? label 'information) "index.html")
	  ((eq? label 'halfring-pair) "halfring_pair.html")
	  ((eq? label 'surv-horn) "surv_horn.html")
	  ((eq? label 'surv-cross-freq) "surv_cross.html")
	  ((eq? label 'full-pair) "full_pair.html")
	  ((eq? label 'full-frequency) "full_freq.html")
	  ((eq? label 'full-cross-freq) "full_cross.html")
	  ((eq? label 'table-of-contents) "toc.html")
	  ((pair? label)
	   (let ((label-car (car label)))
	     (cond ((eq? label-car 'halfring-frequency)
		    (sprintf "halfring_frequency_~a.html"
			     (cdr label)))
		   ((eq? label-car 'surv-rad)
		    (sprintf "surv_radiometer_~a.html"
			     (cdr label)))
		   ((eq? label-car 'surv-pair)
		    (sprintf "surv_pair_~a.html"
			     (cdr label)))
		   ((eq? label-car 'surv-frequency)
		    (sprintf "surv_freq_~a.html"
			     (cdr label)))
		   ((eq? label-car 'full-pair)
		    (sprintf "full_pair_~a.html"
			     (cdr label)))
		   ((eq? label-car 'full-frequency)
		    (sprintf "full_freq_~a.html"
			     (cdr label)))
		   (else (abort "Unknown pair in get-html-file-name")))))
	  (else (abort "Unknown label in get-html-file-name"))))

  ;; This function is similar to `get-html-file-name`, but it returns
  ;; the name of the JSON file containing the data associated with the
  ;; maps/spectra shown in the HTML page
  (define (get-json-file-name label)
    (filepath:replace-extension (get-html-file-name label)
				".js"))

  ;; This is the name of the data release. *TODO*: make the release name
  ;; specifiable from the command line/configuration file
  (define test-release-name "Delta-DX9")

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
			    class: (if (equal? tag page-tag)
				       "selected"
				       "unselected")
			    title))))
      (<nav>
	     (<h1> (smart-<a> "Information" 'information) "\n")
	     (<h1> "1-h tests (half rings) &raquo;"
		   (<h2> (smart-<a> "Horn pair" 'halfring-pair))
		   (<h2> "Frequency &raquo;" #\newline
			 (<h3> (smart-<a> "30 GHz" (cons 'halfring-frequency 30)))
			 (<h3> (smart-<a> "44 GHz" (cons 'halfring-frequency 44)))
			 (<h3> (smart-<a> "70 GHz" (cons 'halfring-frequency 70)))))
	     (<h1> "Survey tests &raquo;" #\newline
		   (<h2> "Single radiometer &raquo" #\newline
			 (<h3> (smart-<a> "LFI18M" (cons 'surv-rad 'LFI18M)))
			 (<h3> (smart-<a> "LFI18S" (cons 'surv-rad 'LFI18S)))
			 (<h3> (smart-<a> "LFI19M" (cons 'surv-rad 'LFI19M)))
			 (<h3> (smart-<a> "LFI19S" (cons 'surv-rad 'LFI19S)))
			 (<h3> (smart-<a> "LFI20M" (cons 'surv-rad 'LFI20M)))
			 (<h3> (smart-<a> "LFI20S" (cons 'surv-rad 'LFI20S)))
			 (<h3> (smart-<a> "LFI21M" (cons 'surv-rad 'LFI21M)))
			 (<h3> (smart-<a> "LFI21S" (cons 'surv-rad 'LFI21S)))
			 (<h3> (smart-<a> "LFI22M" (cons 'surv-rad 'LFI22M)))
			 (<h3> (smart-<a> "LFI22S" (cons 'surv-rad 'LFI22S)))
			 (<h3> (smart-<a> "LFI23M" (cons 'surv-rad 'LFI23M)))
			 (<h3> (smart-<a> "LFI23S" (cons 'surv-rad 'LFI23S)))
			 (<h3> (smart-<a> "LFI24M" (cons 'surv-rad 'LFI24M)))
			 (<h3> (smart-<a> "LFI24S" (cons 'surv-rad 'LFI24S)))
			 (<h3> (smart-<a> "LFI25M" (cons 'surv-rad 'LFI25M)))
			 (<h3> (smart-<a> "LFI25S" (cons 'surv-rad 'LFI25S)))
			 (<h3> (smart-<a> "LFI26M" (cons 'surv-rad 'LFI26M)))
			 (<h3> (smart-<a> "LFI26S" (cons 'surv-rad 'LFI26S)))
			 (<h3> (smart-<a> "LFI27M" (cons 'surv-rad 'LFI27M)))
			 (<h3> (smart-<a> "LFI27S" (cons 'surv-rad 'LFI27S)))
			 (<h3> (smart-<a> "LFI28M" (cons 'surv-rad 'LFI28M)))
			 (<h3> (smart-<a> "LFI28S" (cons 'surv-rad 'LFI28S))))
		   ;(<h2> (smart-<a> "Single horn" 'surv-horn))
		   (<h2> "Horn pair &raquo;" #\newline
			 (<h3> (smart-<a> "#18-#23" (cons 'surv-pair "18_23")))
			 (<h3> (smart-<a> "#19-#22" (cons 'surv-pair "19_22")))
			 (<h3> (smart-<a> "#20-#21" (cons 'surv-pair "20_21"))))
		   (<h2> "Frequency &raquo;" #\newline
			 (<h3> (smart-<a> "30 GHz" (cons 'surv-frequency 30)))
			 (<h3> (smart-<a> "44 GHz" (cons 'surv-frequency 44)))
			 (<h3> (smart-<a> "70 GHz" (cons 'surv-frequency 70))))
		   (<h2> (smart-<a> "Cross-frequency" 'surv-cross-freq)))
	     (<h1> (smart-<a> "Table of contents" 'table-of-contents)))))

  ;; The function `wrap-html` takes a string containing some HTML code
  ;; and encloses it in a self-contained HTML structure which
  ;; comprises the side menu. It returns a string.
  (define (wrap-html file-tag page-title body)
    (html-page (html:++ (<script> type: "text/javascript"
				  src: "js/switch-map-images.js")
			(<script> type: "text/javascript"
				  src: "js/plot_bars.js")
			(<script> type: "text/javascript"
				  src: (get-json-file-name file-tag))
			(<h1> id: "main_title" (make-title-for-report))
			(side-menu file-tag)
			(<div> id: "page_body"
			       (<h2> page-title)
			       body)
			(<script> type: "text/javascript"
				  "window.onload = plotBars(json_object_list);\n"))
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
      (call-with-output-file output-file-name write-function)))


  ;; This function accepts a JSON object and will produce a link to
  ;; the entry. It is meant to be used e.g. with `itemize` (from the
  ;; `html-utils` egg), like in the following code:
  ;;
  ;;      (itemize (map emit-HTML-index-entry-for-object list-of-objs))
  ;;
  (define (emit-HTML-index-entry-for-object obj)
    (let ((title (assq-ref 'title obj)))
      (<ul> "\n"
	    (<canvas> id: (json-obj->div-index-id obj)
		      style: "border:1px solid #000000;"
		      width: 120
		      height: 8)
	    "\n"
	    (<a> href: (html:++ "#" (json-obj->HTML-anchor obj))
		 title "\n")
	    "\n")))

  ;; This function accepts a JSON object and will produce
  ;; HTML code to be put straight into the page.
  (define (emit-HTML-for-map-object obj)
    (let* ((title (assq-ref 'title obj))
	   (fits-file-name (abspath-from-json obj))
	   (gif-file-names
	    (list (fits-name->gif-name fits-file-name (car gif-exts))
		  (fits-name->gif-name fits-file-name (cdr gif-exts)))))
      (for-each (lambda (gif-file-name fixed-extrema?)
		  (printf "Writing GIF file ~a (fixed-extrema? is ~a)\n"
			  gif-file-name
			  fixed-extrema?)
		  (map->gif fits-file-name
			    (filepath:join-path
			     (list (assq-ref 'output-dir user-args)
				   gif-file-name))
			    title
			    fixed-extrema?: fixed-extrema?))
		gif-file-names
		(list #t #f))
      (<img> class: "map_image"
	     src: (car gif-file-names)
	     alt: title
	     onclick: (sprintf "switchMapImages(this, \"~a\", \"~a\")"
			       (car gif-file-names)
			       (cadr gif-file-names))
	     title: (assq-ref 'file_name obj))))

  ;; This function accepts a JSON object and will produce
  ;; HTML code to be put straight into the page.
  (define (emit-HTML-for-spectrum-object obj)
    (let* ((title (assq-ref 'title obj))
	   (fits-file-name (abspath-from-json obj))
	   (js-file-name (fits-name->js-name fits-file-name))
	   (gif-file-name (fits-name->gif-name fits-file-name)))
      (format #t "Writing GIF file ~a\n" gif-file-name)
      (spectrum->gif fits-file-name
		     (filepath:join-path
		      (list (assq-ref 'output-dir user-args)
			    gif-file-name))
		     title)
      (spectrum->js fits-file-name
		    (filepath:join-path
		     (list (assq-ref 'output-dir user-args)
			   js-file-name))
		    title)
      (html:++ (<script> type: "text/javascript"
			 src: js-file-name)
	       (<img> class: "cl_image"
		      src: gif-file-name
		      alt: title
		      onClick: (sprintf "window.open(\"html/cl_spectrum.html?data_file=~a\",\"~a\")"
					js-file-name
					title)
		      title: (assq-ref 'file_name obj)))))

  ;; This function accepts a JSON object and will produce
  ;; HTML code to be put straight into the page.
  (define (emit-HTML-for-object map-obj spectrum-obj)
    (<div> class: "page_section"
	   (<h4> (<a> name: (json-obj->HTML-anchor map-obj)
		      (assq-ref 'title map-obj)))
	   "\n"
	   (<table>
	    (<tr> valign: "top" align: "left"
	     (<td> (emit-HTML-for-map-object map-obj) "\n")
	     (<td> (emit-HTML-for-spectrum-object spectrum-obj) "\n")
	   "\n"))))

  ;; Sort a list of JSON objects according to the tag `key-tag`,
  ;; which should be a string. If the two elements are equal and
  ;; `fallback-comparison` is defined to be a function of two
  ;; parameters, this will be used for the comparison.
  (define (sort-obj-list obj-list key-tag #!key (fallback-comparison #f))
    (sort obj-list
	  (lambda (x y)
	    (let ((channel-x (assq-ref key-tag x))
		  (channel-y (assq-ref key-tag y)))
	      (if (and (equal? channel-x channel-y)
		       fallback-comparison)
		  (fallback-comparison x y)
		  ;; Sort the entries according to their channel
		  (string<? channel-x channel-y))))))

  ;; This function generates the main content of a page within a
  ;; number of <div> (one with class "page_index" and all the others
  ;; with "page_section").
  (define (write-results-page obj-list
			      file-tag
			      map-tag
			      cl-tag
			      title
			      #!key (fallback-comparison #f))
    (let ((sorted-list (sort-obj-list (filter-on-filetype obj-list
							  (list map-tag
								cl-tag))
				      'channel
				      fallback-comparison: fallback-comparison)))

      ;; Write the objects we're going to use in this page into a JSON
      ;; file. The latter will be included in the HTML file, so that
      ;; Javascript codes can use it.
      (let ((json-file-name (filepath:join-path
			     (list (assq-ref 'output-dir user-args)
				   (get-json-file-name file-tag)))))
	(printf "Writing JSON file ~a\n" json-file-name)
	(call-with-output-file json-file-name
	  (lambda (file)
	    (display "json_object_list = " file)
	    (write-json-dictionary file sorted-list)
	    (display ";\n" file))))

      ;; Now emit the HTML codes for the page
      (write-html
       file-tag
       (lambda (file)
	 (let ((map-objs (filter-on-filetype sorted-list map-tag))
	       (cl-objs  (filter-on-filetype sorted-list cl-tag)))
	   ;; We simply apply `emit-HTML-for-object` to the whole list
	   ;; of objects, and wrap the output into some nice HTML tags
	   ;; (using `wrap-html`).
	   (printf "Including ~a maps and ~a spectra in the page...\n"
		   (length map-objs)
		   (length cl-objs))
	   (display
	    (wrap-html file-tag
		       title
		       (html:++
			(<div> id: "results"
			       (<div> id: "page_index"
				      (itemize (map emit-HTML-index-entry-for-object
						    map-objs))
				      (<input> type: "radio"
					       name: "barchartradio"
					       id: "map_std_I"
					       onclick: "plotBars(json_object_list);"
					       checked: #t
					       "RMS of the I map")
				      (<input> type: "radio"
					       name: "barchartradio"
					       id: "map_p2p_I"
					       onclick: "plotBars(json_object_list);"
					       "Peak-to-peak in the I map")
				      (<input> type: "radio"
					       name: "barchartradio"
					       id: "removed_monopole_I"
					       onclick: "plotBars(json_object_list);"
					       "Removed monopole")
				      (<input> type: "radio"
					       name: "barchartradio"
					       id: "dipole_I"
					       onclick: "plotBars(json_object_list);"
					       "Residual dipole"))
			       (<p> "Clicking on the image of a map switches "
				    "between a fixed color-scale range (good "
				    "for comparisons/animations) and a range "
				    "taylored for the map under question "
				    "(good for checking the P-P level and for "
				    "reveal finer details).")
			       (<p> "Clicking on any of the spectral plots "
				    "opens a new window containing a larger, "
				    "interactive plot.")
			       (string-intersperse
				(map emit-HTML-for-object map-objs cl-objs)
				"\n"))))
	    file)
	   (newline file)))))))
