;; Module `json-utils`
;; ===================
;;
;; This module defines a few functions that are useful for
;; loading/interpreting JSON data.

(module json-utils
  ;; List of exported symbols
  (json->alist assq-ref extract-alists-from-json-file)

  (import chicken scheme extras)
  (require-extension json)

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
      (map json->alist (if (list? entries) entries (list entries))))))
