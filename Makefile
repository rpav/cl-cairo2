ifndef CAIRO_INCLUDE_DIR
CAIRO_INCLUDE_DIR=/usr/include/cairo
endif

CAIRO_INCLUDE_FILES=$(wildcard $(CAIRO_INCLUDE_DIR)/*.h)

cl-cairo2-swig.lisp: cl-cairo2.i common.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -I$(CAIRO_INCLUDE_DIR) -DCOMMON cl-cairo2.i

cl-cairo2-x11-swig.lisp: cl-cairo2-x11.i common.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -I$(CAIRO_INCLUDE_DIR) cl-cairo2-x11.i

cl-cairo2-mac-swig.lisp: cl-cairo2-mac.i common.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -I$(CAIRO_INCLUDE_DIR) cl-cairo2-mac.i

cl-cairo2-win-swig.lisp: cl-cairo2-win.i common.i $(CAIRO_INCLUDE_FILES)
	swig -cffi -I$(CAIRO_INCLUDE_DIR) cl-cairo2-win.i
	sed 's/^M//' $@ >$@.tmp; mv -f $@.tmp $@

# test-swig.lisp: test.i
# 	swig -cffi -generate-typedef test.i

# asdf:
# 	rm -Rf /tmp/cl-cairo2-latest
# 	mkdir /tmp/cl-cairo2-latest
# 	cp * -R /tmp/cl-cairo2-latest
# 	tar -cvzf /tmp/cl-cairo2-latest.tar.gz -C /tmp cl-cairo2-latest
# 	gpg -b -a /tmp/cl-cairo2-latest.tar.gz
