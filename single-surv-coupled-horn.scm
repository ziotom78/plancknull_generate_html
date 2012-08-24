(module single-surv-coupled-horn
  (write-single-survey-horn-pair-page)

  (import chicken
	  scheme
	  data-structures
	  extras
	  json-utils
	  html-gen-utils
	  user-settings
	  file-utils)
  (require-extension html-tags
		     html-utils
		     filepath)

  ;; This function accepts a JSON object and will produce a link to
  ;; the entry.
  (define (emit-HTML-index-entry-for-object obj)
    (let ((title (assq-ref 'title obj)))
      (<ul> (<a> href: "#" title "\n"))))

  ;; This function accepts a JSON object and will produce
  ;; HTML code to be put straight into the page.
  (define (emit-HTML-for-object obj)
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
	     "\n")))

  (define (write-single-survey-horn-pair-page obj-list)
    (write-html
     'surv-pair
     (lambda (file)
       (let ((map-objs (filter-on-filetype obj-list "surveydiff_detset_map"))
	     (cl-objs  (filter-on-filetype obj-list "surveydiff_detset_cl")))
	 ;; We simply apply `emit-HTML-for-object` to the whole list
	 ;; of objects, and wrap the output into some nice HTML tags
	 ;; (using `wrap-html`).
	 (printf "Including ~a maps and ~a spectra in the page...\n"
		 (length map-objs)
		 (length cl-objs))
	 (display (wrap-html 'surv-pair
			   "Coupled horn, survey differences"
			   (html:++ (list (<div> class: "page_index"
						 (itemize (map emit-HTML-index-entry-for-object
							       map-objs)))
					  (string-intersperse
					   (map emit-HTML-for-object map-objs)
					   "\n"))))
		file)
       (newline file))))))
