(module user-settings
  (parse-command-line user-args)

  (import chicken scheme extras)

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
					(cdr (argv)))))
