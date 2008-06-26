EXAMPLEFILES=hearts.png lissajous.epsi text.epsi example.epsi pattern.epsi
RAWEXAMPLEFILES=hearts.png lissajous.ps text.ps example.ps pattern.ps
LISP=sbcl

tutorial.pdf: tutorial.dvi
	dvipdfm tutorial

tutorial.dvi: tutorial.tex $(EXAMPLEFILES)
	latex tutorial.tex

$(RAWEXAMPLEFILES): example.lisp
	$(LISP) --eval '(progn (load "example.lisp") (quit))'

%.epsi: %.ps
	ps2epsi $< $@
