%module "cl-cairo2-xcb-swig"

#define __extension__

%insert("lisphead") %{
(in-package :cl-cairo2)
%}

typedef unsigned long xcb_drawable_t;

%include "cairo-features.h"
%include "cairo-custom-features.h"
%include "cairo-min.h"
%include "cairo-xcb.h"
%include "cairo-ft.h"
