/*
 * Most cairo features required for cl-cairo2 are enabled in
 * ./cairo-features/{platform}/cairo-features.h.
 * If you want to disable some of them and/or enable additional features,
 * do it here.
 */

#if CL_CAIRO2_USING_CORE
/*
 * Enable/disable core features here.
 */
//#define CAIRO_HAS_DIRECTFB_SURFACE 1
//#define CAIRO_HAS_XCB_SURFACE 1
//#define CAIRO_HAS_QUARTZ_IMAGE_SURFACE 1
//#define CAIRO_HAS_UTF8_TO_UTF16 1
#endif

#if CL_CAIRO2_USING_XLIB
/*
 * Enable/disable xlib specific features here.
 */
#endif

#if CL_CAIRO2_USING_QUARTZ
/*
 * Enable/disable quarz specific features here.
 */
#define __LP64__ 1 // Exclude the ATSU FONT functions.
#endif

#if CL_CAIRO2_USING_WIN32
/*
 * Enable/disable win32 specific features here.
 */
#endif
