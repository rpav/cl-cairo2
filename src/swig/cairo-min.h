/*
* Macros and typdefs imported from the cairo header files
*
* This file is not a part of the cairo header files.
* This is the minimum cairo.h alternative to make swig work with
* surface specific .i files.
*/

#ifdef __cplusplus
# define CAIRO_BEGIN_DECLS extern "C" {
# define CAIRO_END_DECLS }
#else
# define CAIRO_BEGIN_DECLS
# define CAIRO_END_DECLS
#endif

#ifndef cairo_public
# if defined (_MSC_VER) && ! defined (CAIRO_WIN32_STATIC_BUILD)
# define cairo_public __declspec(dllimport)
# else
# define cairo_public
# endif
#endif

typedef enum _cairo_format {
    CAIRO_FORMAT_ARGB32,
    CAIRO_FORMAT_RGB24,
    CAIRO_FORMAT_A8,
    CAIRO_FORMAT_A1
    /* The value of 4 is reserved by a deprecated enum value.
* The next format added must have an explicit value of 5.
CAIRO_FORMAT_RGB16_565 = 4,
*/
} cairo_format_t;

typedef void* HFONT;
typedef unsigned int ATSUFontID;
