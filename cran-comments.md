## parseLatex 0.4.4

* This revision is mainly at the request of CRAN, to fix
issues detected by rchk and a new gcc compiler.  
* There are still some rchk false positives, since the parser
makes use of Tomas Kalibera's PRESERVE_SV macro, and it isn't
recognized by rchk.
* It also makes \let and \def syntax a bit more flexible.
