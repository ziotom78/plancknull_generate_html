# HTML report generator for plancknull

This project provides a HTML report generator for the data produced by
the program [plancknull](https://github.com/zonca/plancknull). It is
written using [Chicken Scheme](http://www.call-cc.org).

The program takes as input a [JSON](http://en.wikipedia.org/wiki/JSON)
database (usually produced by `plancknull`) which specifies the type
of tests that must be included in the report. Its output is a
self-contained HTML report that can be distributed (it does not have
external references).

## How to generate the executable

The program is written in Scheme
[R5RS](http://www.schemers.org/Documents/Standards/R5RS/), and it
needs to be compiled into an executable in order to be executed. There
are two ways to do it: either grab a precompiled binary (easy way) or
compile it by yourself (hard way). In the following section we'll
detail the two procedures.

### Grabbing the executable (easy way)

You can ask Maurizio Tomasi for a self-contained bundle. At the LFI
DPC and at NERSC, he keeps one in his homedir. The bundle is a
directory containing the executable (named `standalone_generator`) and
a large set of dynamic libraries needed to run the program. You can
move this directory anywhere in your filesystem, but do not try to run
it on different architectures/distributions (e.g. from a RedHat system
to Ubuntu, from x86 to x86_64...).

### Compiling the executable from the source (hard way)

To compile the source code into an executable you must have the
[Chicken Scheme](http://www.call-cc.org/) compiler and a few
open-source libraries for Chicken (called
[_eggs_](http://wiki.call-cc.org/eggs)). To install Chicken, you can
use your package manager (e.g. `sudo apt-get install chicken-bin`
under Ubuntu Linux) if you are a `sudo` user. Otherwise, you must
install it from source in your home directory. Open a terminal and run
the following commands:

    mkdir -p $HOME/usr $HOME/.chicken-temp
    curl http://code.call-cc.org/releases/current/chicken.tar.gz | tar xz -C $HOME/.chicken-temp
    pushd $HOME/.chicken-temp/chicken-*
    make PLATFORM=linux PREFIX=$HOME/usr install
    popd
    rm -rf $HOME/.chicken-temp

(If you want, you can change `$HOME/usr` with any other directory you
want. It should be the place where you usually install things under
your home directory.) At this point, if you did not so already, put
the following lines at the end of your `~/.profile` (assuming you're
using `sh`, `bash` or `dash` as your login shell):

    export PATH=$HOME/usr/bin:$PATH
    export C_INCLUDE_PATH=$HOME/usr/include:$C_INCLUDE_PATH
    export LIBRARY_PATH=$HOME/usr/lib:$LIBRARY_PATH
    export LD_LIBRARY_PATH=$HOME/usr/lib:$LD_LIBRARY_PATH
    export MANPATH=$HOME/usr/man:$MANPATH

Logout and login to see the new variabiles. If you are able to run
`chicken -version`, then the installation of the compiler completed
successfully.

Now you need some "eggs" (that is, Chicken's libraries). Move to the
directory where you have the source code of `plancknull_generate_html`
and run the following command:

    sudo make install_eggs

(If you've installed Chicken under your homedir, omit `sudo`). Now you
should be able to compile the program, simply run

    make generator

This will silently convert `generator.scm` into a C program, which
will then be compiled to a standalone executable. The `Makefile`
provided with the source code allows for many targets, run `make
TARGET` where `TARGET` is one of the following:

* `generator` produces the executable. Be careful that this program
needs the Chicken runtime libraries to be accessible at runtime
(therefore you cannot distribute it)
* `deploy` produces a stand-alone executable in the directory
`standalone_generator`. Unlike the `generator` target, the content of
the directory (containing the executable `standalone_generator` plus
many dynamic libraries) _can_ be distributed to others, as all the
Chicken libraries needed by the program are included in the directory.
(You can think of it as Chicken's analogous to Mac OS X `.app`
directories.)
* `install_eggs` repeatedly calls `chicken-install` to install the
eggs required to compile the program. You might have to use `sudo`
(e.g. this is the case if you installed Chicken using Ubuntu's
`apt-get`).
* `documentation` runs Schematic on the source code to produce this
documentation (in the `docs` directory).
* `help` prints a summary of the available options implemented in
the makefile.

## How to run the program

To run the program, you must have
[`map2tga`](http://healpix.jpl.nasa.gov/html/facilitiesnode9.htm) and
`convert` (part of
[ImageMagick](http://www.imagemagick.org/script/index.php) &mdash; its
fork [GraphicsMagick](http://www.graphicsmagick.org/) should be ok as
well) in your path. `map2tga` is provided by
[Healpix](http://healpix.jpl.nasa.gov/): if you work with Planck/LFI,
you surely have it. (Note that it is already installed at the LFI
DPC.)

The program needs as input one or more JSON files containing
information about the products of the null tests. Typically, these are
produced by the [`plancknull`](https://github.com/zonca/plancknull)
program. You specify the directory containing the output of
`plancknull` as the first parameter of the program, followed by the
directory where to save the report:

    $ generator NULL_TEST_DIRECTORY OUTPUT_PATH

(If you are using the standalone executable, run
`plancknull_generate_html_*` instead of `generator`). This will read
the results of the null tests from the subdirectories under
`NULL_TEST_DIRECTORY`, then it will populate the directory
`OUTPUT_PATH` with the files needed for the HTML report. If
`OUTPUT_PATH` does not exist, it will be silently created.

## How to read the source code of this program

The source code of the program can be processed using
[`schematic`](http://wiki.call-cc.org/eggref/4/schematic), a
documenting tool for Chicken Scheme, in order to have some nicely
formatted HTMLs. Install it from the command line
with the command `sudo chicken-install schematic`, then run

    schematic -f markdown generator.scm

(assuming you have
[`markdown`](http://en.wikipedia.org/wiki/Markdown), which can be
easily installed using `apt-get` under Debian/Ubuntu). This will
create a sub-directory `html` where you'll find the source code of
this very webpage.

The output of `schematic` is also available at
http://ziotom78.github.com/plancknull_generate_html/.

## Appendix: a very short introduction to Scheme

To help the reader who has never read Scheme code, I am summarizing
here the main characteristics of the language. This is not a Scheme
tutorial, just a general introduction written for people that are
already proficient with some other language (in the text there are
many references to Python). If you are not interested in reading the
source code of this program, you can skip this section.

Chicken Scheme implements the
[R5RS](http://www.schemers.org/Documents/Standards/R5RS/) standard of
Scheme, a language derived from LISP. Scheme is a really simple
language (the language and the standard library are described together
by a 50-page document: compare this with Python 2.7, which needs 127
A4 pages for the language plus 1366 pages for the standard library).
This simplicity derives from three facts:

1. The syntax is extremely simple: apart from letters and numbers, the
only symbols which have special meaning for the compiler are `(`, `)`,
quote, backtick, comma (rarely used) and whitespaces (plus `;`, which
starts a comment).
2. The standard library is quite small. Clearly this is not an
advantage, but Chicken Scheme provides a broad selection of
extensions, called "eggs", that mitigate this problem.
3. No complex features of high-end languages are specified by the
standard. For instance, a typical Python program uses OOP techniques,
which are grounded on many non-trivial concepts (object encapsulation,
inheritance, abstract methods, static methods...). You can easily
[extend Scheme to support OOP](http://community.schemewiki.org/?object-systems)
through its powerful macro system, but we won't do this in this
program.

Scheme is based on the concept of list, which is a series of elements
separated by spaces and enclosed within parentheses, like `(1 2 3)`.
Function calls are lists where the first element is the function and
the others are the parameters. E.g., to calculate the sinus of 0.1 you
write `(sin 0.2)`, to print a string you write `(print "Hello,
world!")`. By default, a list is always interpreted as a function
call, unless there is a `'` before the open parenthesis. So `(sin
0.1)` calculates the sinus of 0.1, but `'(sin 0.1)` is a list of two
elements: the first is the `sin` function, the second is the number
0.1. Therefore, in Scheme program and data share the same
representation, and you can easily convert one into another (using
e.g. `eval`: `(eval '(sin 0.1))` is the same as `(sin 0.1)`, but in
the first case you can build your list programatically).

The parenthesis syntax is used everywhere, also in mathematical
expressions ("infix notation", sometimes known as "reverse Polish
notation"). To calculate `5 * (1 + 2 + 3)` you write `(* 5 (+ 1 2
3))`. (Note that both `*` and `+` are used like any other function
call.)

Function and variable definition share the same syntax: you have to
use `define`. For instance, `(define x 1)` creates a new variable
which contains the integer 1, while `(define (f x) (* 2 x))`
defines a function `f` which accepts one parameter `x` and which
returns `x` doubled.

Similarly to Python, anonymous functions can be defined using
`lambda`. Unlike Python, Scheme's `lambda` expressions can contain any
sequence of instructions.

Loops can be implemented using either recursion (a "functional"
construct) or `do` (an "imperative" construct). However, for simple
programs like the one we are describing here, we only rely on
functions like `map` and `filter`, which are analogous to Python's
counterparts.

To end with an example, consider this quite idiomatic Python code:

    print ", ".join([x.upper() for x in ("a", "b", "c")])

which prints "A, B, C". It can be translated in Chicken Scheme:

    (string-intersperse (map string-upcase
                             '("a" "b" "c"))
                        ", ")

While Python uses many different syntactic elements (dot, parentheses,
brackets, the `for` and `in` keyword, explicit naming of the `x`
variable), everything in this Scheme snipped follows the same idea of
using parentheses to indicate both function calls and lists.

(In Scheme you usually use much more newlines than in imperative
programs. This helps in visualizing which arguments are parts of which
list, as it is easy to get confused by nested parentheses. In the
following of this document, you can highlight parentheses on the code
on the right by moving the mouse over it.)

Scheme's syntax can look weird at first, but it is grounded on two
very simple elements: parentheses (which group elements) and white
spaces (which separate elements within parentheses). Compare this with
e.g. Python, where there are many symbols to be used in a program:
`()` identifies a tuple (or the parameters in a function call), `[]` a
list, `{}` a dictionary, `:` introduces a sub-loop or a definition,
`;` separates statements in the same line, etc. And there are some
strange quirks in the language, e.g. you have to remember that a
one-element list can be written as `[1]`, but for a one-element tuple
you must append a comma: `(1,)`.

HTML generation and Scheme
--------------------------

Thanks to its powerful macro system, Scheme (and LISP languages in
general) is very good in handling HTML, see e.g. Paul Graham's 16th
chapter of [ANSI Common LISP](http://www.paulgraham.com/acl.html). The
Chicken's library
[`html-tags`](http://wiki.call-cc.org/eggref/4/html-tags) implements
HTML-like commands in Scheme, which are converted into strings. (A
more sophisticated approach would use one of the many Chicken's
[SXML](http://en.wikipedia.org/wiki/SXML) libraries.) The idea is that
every time you have some HTML tag in the form `<tag>...</tag>`, you
write it as the Scheme command `(<tag> ...)`, where parentheses are
used to delimit the tag. This command is converted into a string, that
can then be printed on screen. Here is an example:

    (require-extension html-tags)
    (print (<h1> (format #f "The result of the sum is ~a" (+ 3 6))))

Note that we can call Scheme functions like `format` and `+` within
the HTML tags. This program will print

    <h1>The result of the sum is 9</h1>

This differs substantially from the typical approach of using a
template library like Python's
[Jinja2 library](http://jinja.pocoo.org/docs) or
[Django](https://www.djangoproject.com): in that case, you write a
HTML template interspersed with Jinja2's
[internal language](http://jinja.pocoo.org/docs/templates/#expressions)
(which, although similar, is _not_ Python: see e.g. the use of the `|`
operator) - the same
[applies to Django as well](https://docs.djangoproject.com/en/1.4/topics/templates/).
Thus, you have to learn a new language (other than Python and HTML) to
use it. With Scheme, we're using it for everything!
