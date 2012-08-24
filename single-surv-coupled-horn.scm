(module single-surv-coupled-horn
  (write-single-survey-horn-pair-page)

  (import chicken
	  scheme
	  data-structures
	  extras
	  json-utils
	  html-gen-utils
	  user-settings)
  (require-extension html-tags
		     html-utils
		     filepath)

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
			   (html:++ (<div> class: "page_index"
					   (itemize (map emit-HTML-index-entry-for-object
							 map-objs)))
				    (string-intersperse
				     (map emit-HTML-for-object map-objs)
				     "\n")))
		file)
       (newline file))))))
