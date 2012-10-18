;; Module `json-utils`
;; ===================
;;
;; This module defines a few functions that are useful for
;; loading/interpreting JSON data.

(module json-utils
  ;; List of exported symbols
  (json->alist
   alist->json
   assq-ref
   assoc-ref
   extract-alists-from-json-file
   read-json-dictionary
   write-json-dictionary
   abspath-from-json
   filter-on-filetype
   json-obj->HTML-anchor
   json-obj->div-index-id)

  (import chicken
	  scheme
	  extras
	  posix
	  regex
	  srfi-1
	  user-settings)
  (require-extension json
		     filepath)

  ;; The `json-read` function returns a vector of pairs of the form
  ;; `(string . value)` (with `string` being the key). However, this
  ;; is not the best representation for the data. First of all, when
  ;; looking for a specific key it is much faster to work with symbols
  ;; than with strings. Also, in order to use the built-in Scheme
  ;; function `assq` we need to have lists, not vectors. The function
  ;; `json->alist` performs these two conversions, by means of
  ;; `string->symbol` and `vector->list` (two Scheme built-ins).
  (define (json->alist dictionary)
    (map (lambda (x)
	   (let ((key (string->symbol (car x))))
	     (cons (cond
		    ((eq? key 'map_std) 'map_std_I)
		    ((eq? key 'map_p2p) 'map_p2p_I)
		    (else key))
		   (cdr x))))
	 (vector->list dictionary)))

  ;; This function is the inverse of `json->alist`, in the sense that
  ;; `(alist->json (json->alist x))` will return `x` whenever `x` is a
  ;; valid JSON object (i.e. returned by the `json-read` function).
  (define (alist->json alist)
    (if alist
	(list->vector (map (lambda (x)
			     (cons (symbol->string (car x))
				   (cdr x)))
			   alist))
	(begin
	  (display "Something smells fishy here!")
	  #f)))

  ;; Since JSON entries are going to be handled as _a-lists_
  ;; ([association
  ;; lists](http://en.wikipedia.org/wiki/Association_list)), we'll
  ;; often use a nice shorthand which is not provided by standard
  ;; Scheme (note however that `assq-ref` is a
  ;; [built-in](http://www.gnu.org/software/guile/manual/html_node/Retrieving-Alist-Entries.html)
  ;; in GNU Guile, but - alas! - not in Chicken Scheme).
  (define (assq-ref entry a-list) 
    (let ((field (assq entry a-list)))
      (if field
	  (cdr field)
	  #f)))

  ;; The same applies for `assoc` (which uses `equal?` instead of
  ;; `eq?`).
  (define (assoc-ref entry a-list) 
    (let ((field (assoc entry a-list)))
      (if field
	  (cdr field)
	  #f)))

  ;; Parse a file and return a list of associative lists suitable for
  ;; being used with functions like `assq` and `assv`. Note that, unlike
  ;; `json-read`, this function returns a list even if the JSON file
  ;; contains just one object: this is done through the `(if (list?
  ;; ...))`.
  (define (extract-alists-from-json-file file-name)
    (format #t "Reading file ~a...\n" file-name)
    (let ((entries (call-with-input-file file-name json-read)))
      (map json->alist (if (list? entries) entries (list entries)))))

  ;; Find all the files in the null test directory and read their
  ;; contents, then assemble all the objects (a-lists) into a list
  ;; (using `extract-alists-from-json-file`). Note that `append` is
  ;; not a R5RS built-in (it is part of
  ;; [SRFI-1](http://srfi.schemers.org/srfi-1/srfi-1.html)), but
  ;; Chicken incorporates it. Also, `find-files` is part of the
  ;; Chicken `posix` library, and it accepts regular expressions to
  ;; match the files (the meaning of `.+\\.json$` is: a sequence of
  ;; N>0 characters which ends with `.json`).
  (define (read-json-dictionary input-dir)
    (apply append (map extract-alists-from-json-file
		       (find-files input-dir
				   test: ".+\\.json$"))))

  ;; This function writes a JSON representation of `list-of-alists`, a
  ;; list of a-lists, into `file` (a port already opened for output).
  (define (write-json-dictionary file list-of-alists)
    (display "[\n" file)
    (for-each (lambda (x)
		(json-write (alist->json x) file)
		(display ",\n" file))
	      list-of-alists)
    (display "]" file))

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

  ;; The function `filter-on-filetype` filters the contents of a JSON
  ;; a-list ording to one or more file types. The use of `filter` is
  ;; exactly the same as in Python.
  (define (filter-on-filetype dict file-types)
    (if (list? file-types)
	;; This is the `ten` part of the `if`. We're going recursive!
	(apply append (map (lambda (type) (filter-on-filetype dict type))
			   file-types))
	;; This is the `else` part of the `if`.
	(filter (lambda (object)
		  (equal? (assq-ref 'file_type object)
			  file-types))
		dict)))

  ;; Given a JSON object, this produces a valid HTML anchor for the
  ;; object.
  (define (json-obj->HTML-anchor obj)
    (string-substitute "[-/]" "_"
		       (assq-ref 'base_file_name obj)
		       'every-match))

  ;; Given a JSON object, this produces a valid ID to be used for the
  ;; <div> element which contains the small bar plot for this element.
  ;; Note that this algorithm must match with the one in "plotBars"
  ;; (file `js/plot_bars.js`).
  (define (json-obj->div-index-id obj)
    (string-append "div_"
		   (string-substitute "[-/]" "_"
				      (assq-ref 'base_file_name obj)
				      'every-match))))
