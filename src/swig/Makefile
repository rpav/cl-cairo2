ifndef CAIRO_INCLUDE_DIR
CAIRO_INCLUDE_DIR=./cairo-headers/1.8.10
endif

CAIRO_INCLUDE_FILES=$(wildcard $(CAIRO_INCLUDE_DIR)/*.h)

all: cl-cairo2-swig.lisp cl-cairo2-xlib-swig.lisp cl-cairo2-quartz-swig.lisp cl-cairo2-win32-swig.lisp

cl-cairo2-swig.lisp: cl-cairo2.i cairo-custom-features.h $(CAIRO_INCLUDE_FILES)
	swig -cffi -I./cairo-features/core -I$(CAIRO_INCLUDE_DIR) cl-cairo2.i

cl-cairo2-xlib-swig.lisp: cl-cairo2-xlib.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I./cairo-features/xlib -I$(CAIRO_INCLUDE_DIR) cl-cairo2-xlib.i

cl-cairo2-quartz-swig.lisp: cl-cairo2-quartz.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I./cairo-features/quartz -I$(CAIRO_INCLUDE_DIR) cl-cairo2-quartz.i

cl-cairo2-win32-swig.lisp: cl-cairo2-win32.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I./cairo-features/win32 -I$(CAIRO_INCLUDE_DIR) cl-cairo2-win32.i
	sed 's/^M//' $@ >$@.tmp; mv -f $@.tmp $@
