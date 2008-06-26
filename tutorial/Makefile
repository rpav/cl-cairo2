EXAMPLEFILES=hearts.png lissajous.eps text.eps example.eps pattern.eps
RAWEXAMPLEFILES=hearts.png lissajous.pdf text.pdf example.pdf pattern.pdf
LISP=sbcl

tutorial.pdf: tutorial.dvi
	dvipdfm tutorial

tutorial.dvi: tutorial.tex $(EXAMPLEFILES)
	latex tutorial.tex

$(RAWEXAMPLEFILES): example.lisp
	$(LISP) --eval '(progn (load "example.lisp") (quit))'

%.eps: %.pdf
	pdf2ps $< $@
