%module "cl-cairo2-quartz-swig"

%insert("lisphead") %{
(in-package :cl-cairo2)
%}

%include "cairo-features.h"
%include "cairo-custom-features.h"
%include "cairo-min.h"
%include "cairo-quartz.h"
