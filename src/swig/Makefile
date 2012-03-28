-include Makefile.inc

ifndef CAIRO_INCLUDE_DIR
$(error You must run make with CAIRO_INCLUDE_DIR=/path/to/your/cairo; \
this should be a path to a cairo repository checkout which include \
all platform headers. This will be saved in Makefile.inc for future runs)
endif

PLATFORMS := xlib quartz win32 xcb

CAIRO_INCLUDE_FILES=$(wildcard $(CAIRO_INCLUDE_DIR)/*.h)

.PHONY: all install clean save-settings

all: cl-cairo2-swig.lisp cl-cairo2-xlib-swig.lisp cl-cairo2-quartz-swig.lisp \
     cl-cairo2-win32-swig.lisp cl-cairo2-xcb-swig.lisp \
     save-settings

install: all
	for i in $(PLATFORMS); do cp cl-cairo2-$$i-swig.lisp ../$$i; done
	cp cl-cairo2-swig.lisp ..

save-settings:
	@echo "# This file is generated.  All edits will be discarded." > Makefile.inc
	@echo CAIRO_INCLUDE_DIR=$(CAIRO_INCLUDE_DIR) >> Makefile.inc

clean:
	for i in $(PLATFORMS); do rm -f cl-cairo2-$$i-swig.lisp; done
	rm -f cl-cairo2-swig.lisp

cl-cairo2-swig.lisp: cl-cairo2.i cairo-custom-features.h $(CAIRO_INCLUDE_FILES)
	swig -cffi -I./cairo-features/core -I$(CAIRO_INCLUDE_DIR) cl-cairo2.i

cl-cairo2-xlib-swig.lisp: cl-cairo2-xlib.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I./cairo-features/xlib -I$(CAIRO_INCLUDE_DIR) cl-cairo2-xlib.i

cl-cairo2-xcb-swig.lisp: cl-cairo2-xcb.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I/usr/include -I./cairo-features/xcb -I$(CAIRO_INCLUDE_DIR) cl-cairo2-xcb.i

cl-cairo2-quartz-swig.lisp: cl-cairo2-quartz.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I./cairo-features/quartz -I$(CAIRO_INCLUDE_DIR) cl-cairo2-quartz.i

cl-cairo2-win32-swig.lisp: cl-cairo2-win32.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -noswig-lisp -I./cairo-features/win32 -I$(CAIRO_INCLUDE_DIR) cl-cairo2-win32.i
	sed 's/^M//' $@ >$@.tmp; mv -f $@.tmp $@
