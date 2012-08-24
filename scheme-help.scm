;; Appendix: a very short introduction to Scheme
;; =============================================
;;
;; To help the reader who has never read Scheme code, I am summarizing
;; here the main characteristics of the language. This is not a Scheme
;; tutorial, just a general introduction written for people that are
;; already proficient with some other language (in the text there are
;; many references to Python). If you are not interested in reading
;; the source code of this program, you can skip this section.
;;
;; Chicken Scheme implements the
;; [R5RS](http://www.schemers.org/Documents/Standards/R5RS/) standard
;; of Scheme, a language derived from LISP. Scheme is a really simple
;; language (the language and the standard library are described
;; together by a 50-page document: compare this with Python 2.7, which
;; needs 127 A4 pages for the language plus 1366 pages for the
;; standard library). This simplicity derives from three facts:
;;
;; 1. The syntax is extremely simple: apart from letters and numbers,
;; the only symbols which have special meaning for the compiler are
;; `(`, `)`, quote, backtick, comma (rarely used) and whitespaces
;; (plus `;`, which starts a comment).
;; 2. The standard library is quite small. Clearly this is not an
;; advantage, but Chicken Scheme provides a broad selection of
;; extensions, called "eggs", that mitigate this problem.
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
;; To end with an example, consider this quite idiomatic Python code:
;;
;;     print ", ".join([x.upper() for x in ("a", "b", "c")])
;;
;; which prints "A, B, C". It can be translated in Chicken Scheme:
;;
;;     (string-intersperse (map string-upcase
;;                              '("a" "b" "c"))
;;                         ", ")
;;
;; While Python uses many different syntactic elements (dot,
;; parentheses, brackets, the `for` and `in` keyword, explicit naming
;; of the `x` variable), everything in this Scheme snipped follows the
;; same idea of using parentheses to indicate both function calls and
;; lists.
;;
;; (In Scheme you usually use much more newlines than in imperative
;; programs. This helps in visualizing which arguments are parts of
;; which list, as it is easy to get confused by nested parentheses. In
;; the following of this document, you can highlight parentheses on
;; the code on the right by moving the mouse over it.)
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
;; HTML generation and Scheme
;; --------------------------
;;
;; Thanks to its powerful macro system, Scheme (and LISP languages in
;; general) is very good in handling HTML, see e.g. Paul Graham's 16th
;; chapter of [ANSI Common LISP](http://www.paulgraham.com/acl.html).
;; The Chicken's library
;; [`html-tags`](http://wiki.call-cc.org/eggref/4/html-tags)
;; implements HTML-like commands in Scheme, which are converted into
;; strings. (A more sophisticated approach would use one of the many
;; Chicken's [SXML](http://en.wikipedia.org/wiki/SXML) libraries.) The
;; idea is that every time you have some HTML tag in the form
;; `<tag>...</tag>`, you write it as the Scheme command `(<tag> ...)`,
;; where parentheses are used to delimit the tag. This command is
;; converted into a string, that can then be printed on screen. Here
;; is an example:
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
;; to use it. With Scheme, we're using it for everything!
