Source code for `plancknull_generate_html`
==========================================

This site provides the full documentation of the program
[`plancknull_generate_html`](https://github.com/ziotom78/plancknull_generate_html),
used to produce reports of the null tests produced for the
[Planck/LFI instrument](http://www.rssd.esa.int/index.php?project=planck&page=lfi_top).

The program has been written using
[Chicken Scheme](http://www.call-cc.org/). Select one of the files
below to read the commented source code:

* [`fitsio.scm`](./fitsio.scm.html): bindings to
  [CFITSIO](http://heasarc.gsfc.nasa.gov/fitsio/).
* [`healpix.scm`](./healpix.scm.html): partial implementation of the
  Healpix pixelization. It contains the code which loads a FITS map
  and paint it on a [Cairo](http://www.cairographics.org/) surface.
* [`file-utils.scm`](./file-utils.scm.html): functions that deal with
  files (e.g. producing a GIF file from a
  [Healpix](http://healpix.jpl.nasa.gov/) map in FITS format).
* [`generator.scm`](./generator.scm.html): the main program
* [`html-gen-utils.scm`](./html-gen-utils.scm.html): functions that ease
  the production of HTML pages for the null test reports.
* [`json-utils.scm`](./json-utils.scm.html): utility functions to
  handle JSON files produced by the
  [`plancknull`](https://github.com/zonca/plancknull) program
* [`user-settings.scm`](./user-settings.scm.html): functions that read
  user's preferences (currently only from the command line).