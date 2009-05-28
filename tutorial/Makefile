EXAMPLEFILES=simpletext.pdf lissajous.pdf text.pdf example.pdf \
		hearts.pdf pattern.pdf simple.png
LISP=sbcl

tutorial.pdf: tutorial.tex $(EXAMPLEFILES)
	pdflatex tutorial.tex

$(EXAMPLEFILES): example.lisp
	$(LISP) --eval '(progn (load "example.lisp") (quit))'

clean:
	rm -f $(EXAMPLEFILES)
