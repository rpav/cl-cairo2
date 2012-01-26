ifndef CAIRO_INCLUDE_DIR
CAIRO_INCLUDE_DIR=./cairo-headers/1.10.2
endif

PLATFORMS := xlib quartz win32

CAIRO_INCLUDE_FILES=$(wildcard $(CAIRO_INCLUDE_DIR)/*.h)

.PHONY: all install clean

all: cl-cairo2-swig.lisp cl-cairo2-xlib-swig.lisp cl-cairo2-quartz-swig.lisp cl-cairo2-win32-swig.lisp

install: all
	for i in $(PLATFORMS); do cp cl-cairo2-$$i-swig.lisp ../$$i; done
	cp cl-cairo2-swig.lisp ..

clean:
	for i in $(PLATFORMS); do rm -f cl-cairo2-$$i-swig.lisp; done
	rm -f cl-cairo2-swig.lisp

cl-cairo2-swig.lisp: cl-cairo2.i cairo-custom-features.h $(CAIRO_INCLUDE_FILES)
	swig -cffi -I./cairo-features/core -I$(CAIRO_INCLUDE_DIR) cl-cairo2.i

cl-cairo2-xlib-swig.lisp: cl-cairo2-xlib.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I./cairo-features/xlib -I$(CAIRO_INCLUDE_DIR) cl-cairo2-xlib.i

cl-cairo2-quartz-swig.lisp: cl-cairo2-quartz.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I./cairo-features/quartz -I$(CAIRO_INCLUDE_DIR) cl-cairo2-quartz.i

cl-cairo2-win32-swig.lisp: cl-cairo2-win32.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I./cairo-features/win32 -I$(CAIRO_INCLUDE_DIR) cl-cairo2-win32.i
	sed 's/^M//' $@ >$@.tmp; mv -f $@.tmp $@
