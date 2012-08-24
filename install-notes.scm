;; How to generate the executable
;; ==============================
;;
;; The program is written in Scheme
;; [R5RS](http://www.schemers.org/Documents/Standards/R5RS/), and it
;; needs to be compiled into an executable in order to be executed.
;; There are two ways to do it: either grab a precompiled binary (easy
;; way) or compile it by yourself (hard way). In the following section
;; we'll detail the two procedures.
;;
;; Grabbing the executable (easy way)
;; ----------------------------------
;;
;; You can ask Maurizio Tomasi for a self-contained bundle. At the LFI
;; DPC and at NERSC, he keeps one in his homedir. The bundle is a
;; directory containing the executable (named `standalone_generator`)
;; and a large set of dynamic libraries needed to run the program. You
;; can move this directory anywhere in your filesystem, but do not try
;; to run it on different architectures/distributions (e.g. from a
;; RedHat system to Ubuntu, from x86 to x86_64...).
;;
;; Compiling the executable from the source (hard way)
;; ---------------------------------------------------
;;
;; To compile the source code into an executable you must have the
;; [Chicken Scheme](http://www.call-cc.org/) compiler and a few
;; open-source libraries for Chicken (called
;; [_eggs_](http://wiki.call-cc.org/eggs)). To install Chicken, you
;; can use your package manager (e.g. `sudo apt-get install
;; chicken-bin` under Ubuntu Linux) if you are a `sudo` user.
;; Otherwise, you must install it from source in your home directory.
;; Open a terminal and run the following commands (if you do not see
;; the second line fully, select the text with the mouse):
;;
;;     mkdir -p $HOME/usr $HOME/.chicken-temp
;;     curl http://code.call-cc.org/releases/current/chicken.tar.gz | tar xz -C $HOME/.chicken-temp
;;     pushd $HOME/.chicken-temp/chicken-*
;;     make PLATFORM=linux PREFIX=$HOME/usr install
;;     popd
;;     rm -rf $HOME/.chicken-temp
;;
;; (If you want, you can change `$HOME/usr` with any other directory
;; you want. It should be the place where you usually install things
;; under your home directory.) At this point, if you did not so
;; already, put the following lines at the end of your `~/.profile`
;; (assuming you're using `sh`, `bash` or `dash` as your login shell):
;;
;;     export PATH=$HOME/usr/bin:$PATH
;;     export C_INCLUDE_PATH=$HOME/usr/include:$C_INCLUDE_PATH
;;     export LIBRARY_PATH=$HOME/usr/lib:$LIBRARY_PATH
;;     export LD_LIBRARY_PATH=$HOME/usr/lib:$LD_LIBRARY_PATH
;;     export MANPATH=$HOME/usr/man:$MANPATH
;;
;; Logout and login to see the new variabiles. If you are able to run
;; `chicken -version`, then the installation of the compiler
;; completed successfully.
;;
;; Now you need some "eggs" (that is, Chicken's libraries). Move to
;; the directory where you have the source code of
;; `plancknull_generate_html` and run the following command:
;;
;;     sudo make install_eggs
;;
;; (If you've installed Chicken under your homedir, omit `sudo`). Now
;; you should be able to compile the program, simply run
;;
;;     make generator
;;
;; This will silently convert `generator.scm` into a C program, which
;; will then be compiled to a standalone executable. The `Makefile`
;; provided with the source code allows for many targets, run `make
;; TARGET` where `TARGET` is one of the following:
;;
;; * `generator` produces the executable. Be careful that this program
;; needs the Chicken runtime libraries to be accessible at runtime
;; (therefore you cannot distribute it)
;; * `deploy` produces a stand-alone executable in the directory
;; `standalone_generator`. Unlike the `generator` target, the content
;; of the directory (containing the executable `standalone_generator`
;; plus many dynamic libraries) _can_ be distributed to others, as all
;; the Chicken libraries needed by the program are included in the
;; directory. (You can think of it as Chicken's analogous to Mac OS X
;; `.app` directories.)
;; * `install_eggs` repeatedly calls `chicken-install` to install the
;; eggs required to compile the program. You might have to use `sudo`
;; (e.g. this is the case if you installed Chicken using Ubuntu's
;; `apt-get`).
;; * `documentation` runs Schematic on the source code to produce this
;; documentation (in the `docs` directory).
;; * `help` prints a summary of the available options implemented in
;; the makefile.
;;
;; How to run the program
;; ======================
;;
;; To run the program, you must have
;; [`map2tga`](http://healpix.jpl.nasa.gov/html/facilitiesnode9.htm)
;; and `convert` (part of
;; [ImageMagick](http://www.imagemagick.org/script/index.php) &mdash;
;; its fork [GraphicsMagick](http://www.graphicsmagick.org/) should be
;; ok as well) in your path. `map2tga` is provided by
;; [Healpix](http://healpix.jpl.nasa.gov/): if you work with
;; Planck/LFI, you surely have it. (Note that it is already installed
;; at the LFI DPC.)
;;
;; The program needs as input one or more JSON files containing
;; information about the products of the null tests. Typically, these
;; are produced by the
;; [`plancknull`](https://github.com/zonca/plancknull) program. You
;; can specify them from the command line:
;;
;;     $ generator NULL_TEST_DIRECTORY OUTPUT_PATH
;;
;; (If you are using the standalone executable, run
;; `standalone_generator` instead of `generator`). This will read the
;; results of the null tests from the subdirectories under
;; `NULL_TEST_DIRECTORY`, and it will create the directory
;; `OUTPUT_PATH` and populate it with the files needed for the HTML
;; report. If `OUTPUT_PATH` does not exist, it will be silently
;; created.
;;
;; How to read the source code of this program
;; ===========================================
;;
;; This page was created automatically from the program source code
;; using [`schematic`](http://wiki.call-cc.org/eggref/4/schematic), a
;; documenting tool for Chicken Scheme. Install it from the command
;; line with the command `sudo chicken-install schematic`, then
;; run
;;
;;     schematic -f markdown generator.scm
;;
;; (assuming you have
;; [`markdown`](http://en.wikipedia.org/wiki/Markdown), which can be
;; easily installed using `apt-get` under Debian/Ubuntu). This will
;; create a sub-directory `html` where you'll find the source code of
;; this very webpage.
