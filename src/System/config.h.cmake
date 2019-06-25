/* src/System/config.h.cmake */

/* check assertions in code */
#cmakedefine DEBUG_ASSERT 1

/* Defined if ...-style argument passing works */
#cmakedefine DOTS_OK 1

/* Enable experimental style rewriting code */
#cmakedefine EXPERIMENTAL 1

/* gs path relative to TEXMACS_PATH */
#cmakedefine GS_EXE "@GS_EXE@"

/* gs fonts */
#cmakedefine GS_FONTS "@GS_FONTS@"

/* gs lib */
#cmakedefine GS_LIB "@GS_LIB@"

/* Guile version */
#cmakedefine GUILE_A 1

/* Guile version */
#cmakedefine GUILE_B 1

/* Guile version */
#cmakedefine GUILE_C 1

/* Guile version */
#cmakedefine GUILE_D 1

#cmakedefine GUILE_NUM 0

/* Define to 1 if the system has the type `FILE'. */
#cmakedefine HAVE_FILE 1

/* Define to 1 if you have the `gettimeofday' function. */
#cmakedefine HAVE_GETTIMEOFDAY 1

/* Define to 1 if the system has the type `intptr_t'. */
#cmakedefine HAVE_INTPTR_T 1

/* Define to 1 if you have the <inttypes.h> header file. */
#cmakedefine HAVE_INTTYPES_H 1

/* Define to 1 if you have the <memory.h> header file. */
#cmakedefine HAVE_MEMORY_H 1

/* Define to 1 if you have the <pty.h> header file. */
#cmakedefine HAVE_PTY_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#cmakedefine HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#cmakedefine HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#cmakedefine HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#cmakedefine HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#cmakedefine HAVE_SYS_TYPES_H 1

/* Define to 1 if the system has the type `time_t'. */
#cmakedefine HAVE_TIME_T 1

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine HAVE_UNISTD_H 1

/* Define to 1 if you have the <util.h> header file. */
#cmakedefine HAVE_UTIL_H 1

#cmakedefine HAVE_X11_XLIB_H 1

#cmakedefine HAVE_X11_XUTIL_H 1

/* Link axel library with TeXmacs */
#cmakedefine LINKED_AXEL 1

/* Link cairo library with TeXmacs */
#cmakedefine LINKED_CAIRO 1

/* Link freetype library with TeXmacs */
#cmakedefine LINKED_FREETYPE 1

/* Link imlib2 library with TeXmacs */
#cmakedefine LINKED_IMLIB2 1

#cmakedefine LINKED_SQLITE3 1

#cmakedefine MACOSX_EXTENSIONS 1

#cmakedefine OS_MACOS 1

#cmakedefine MACOS_QT_MENU 1

#cmakedefine PDFHUMMUS_NO_TIFF 1

/* Disable fast memory allocator */
#cmakedefine NO_FAST_ALLOC 1

/* Use g++ strictly prior to g++ 3.0 */
#cmakedefine OLD_GNU_COMPILER 1

/* Define to the address where bug reports for this package should be sent. */
#cmakedefine PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#cmakedefine PACKAGE_NAME ""

/* Define to the full name and version of this package. */
#cmakedefine PACKAGE_STRING ""

/* Define to the one symbol short name of this package. */
#cmakedefine PACKAGE_TARNAME ""

#cmakedefine PACKAGE_URL ""

/* Define to the version of this package. */
#cmakedefine PACKAGE_VERSION ""

#cmakedefine PDF_RENDERER 1

#cmakedefine QTTEXMACS 1

#cmakedefine QTPIPES 1

#cmakedefine SIZEOF_VOID_P @SIZEOF_VOID_P@ 

/* Define to 1 if you have the ANSI C header files. */
#cmakedefine STDC_HEADERS 1

#cmakedefine TEXMACS_REVISION "@TEXMACS_REVISION@"

#cmakedefine TM_DYNAMIC_LINKING 1

/* Use axel library */
#cmakedefine USE_AXEL 1

/* Use cairo library */
#cmakedefine USE_CAIRO 1

/* Use freetype library */
#cmakedefine USE_FREETYPE 1

#cmakedefine USE_GS 1

/* Use iconv library */
#cmakedefine USE_ICONV 1

/* Use imlib2 library */
#cmakedefine USE_IMLIB2 1

#cmakedefine USE_SPARKLE 1

#cmakedefine USE_SQLITE3 1

#cmakedefine USE_STACK_TRACE 1

/* Define to 1 if the X Window System is missing or not being used. */
#cmakedefine X_DISPLAY_MISSING 1

/* Guile string size type */
#define guile_str_size_t @guile_str_size_t@
