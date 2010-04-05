%module "cl-cairo2-win32-swig"

%insert("lisphead") %{
(in-package :cl-cairo2)
%}

%include "cairo-features.h"
%include "cairo-custom-features.h"
%include "cairo-min.h"
%include "cairo-win32.h"
