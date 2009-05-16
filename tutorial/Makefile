EXAMPLEFILES=hearts.pdf lissajous.pdf text.pdf example.pdf pattern.pdf
LISP=sbcl

tutorial.pdf: tutorial.tex $(EXAMPLEFILES)
	pdflatex tutorial.tex

$(EXAMPLEFILES): example.lisp
	$(LISP) --eval '(progn (load "example.lisp") (quit))'
