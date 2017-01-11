# cm-fop
A small library for [C-Mera](https://github.com/kiselgra/c-mera) that provides Feature-Oriented Programming. The system was originally proposed in our [GPCE'15 paper](http://lgdv.cs.fau.de/publications/publication/Pub.2015.tech.IMMD.IMMD9.lightw/) and this repository contains an updated version of it (supporting an arbitrary number of specializations per feature).

## Test-Run & Installation
With C-Mera installed the simplest test is running our example program from source and simply loading the composition system:
	
	$ cd cm-fop/test
	$ cm cu test.load.lisp

The output should be two implementations of a simple per-element accumulation, one using OpenMP and one using CUDA (hence, `cm cu`, or `cugen`).

Now you can make the system known to your lisp environment, e.g. for `CCL` by

	$ ln -s /path/to/c-mera ~/quicklisp/local-projects/c-mera
	$ ln -s /path/to/cm-fop ~/quicklisp/local-projects/cm-fop

or for `SBCL` by

	$ ln -s /path/to/c-mera ~/common-lisp/c-mera
	$ ln -s /path/to/cm-fop ~/common-lisp/cm-fop

and then use it via asdf:

	$ cd cm-fop
	$ cm cu test/test.asdf.lisp

