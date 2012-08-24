;; Module `json-utils`
;; ===================
;;
;; This module defines a few functions that are useful for
;; loading/interpreting JSON data.

(module json-utils
  ;; List of exported symbols
  (json->alist
   assq-ref
   extract-alists-from-json-file
   read-json-dictionary
   abspath-from-json
   filter-on-filetype)

  (import chicken
	  scheme
	  extras
	  posix
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
	   (cons (string->symbol (car x))
		 (cdr x)))
	 (vector->list dictionary)))

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
		       (find-files input-dir ".+\\.json$"))))


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
		dict))))
