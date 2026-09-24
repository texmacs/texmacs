/* src/System/config.h.cmake. Generated from config.h.cmake by CMake. */

/* Qt major version number */
#cmakedefine AC_QT_MAJOR_VERSION @AC_QT_MAJOR_VERSION@

/* The normal alignment of 'void *', in bytes. */
#cmakedefine ALIGNOF_VOID_P @ALIGNOF_VOID_P@

/* Alternative version number */
#cmakedefine ALTERNATIVE_VERSION "@ALTERNATIVE_VERSION@"

/* Enable experimental Cocoa port */
#cmakedefine AQUATEXMACS 1

/* If there is a static plugin Cocoa */
#cmakedefine CocoaPlugin 1

/* check assertions in code */
#cmakedefine DEBUG_ASSERT 1

/* debugging built */
#cmakedefine DEBUG_ON 1

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
#cmakedefine GUILE_B 1
#cmakedefine GUILE_C 1
#cmakedefine GUILE_D 1
#cmakedefine GUILE_NUM @GUILE_NUM@
#cmakedefine GUILE_HEADER_16 1
#cmakedefine GUILE_HEADER_18 1
#cmakedefine GUILE_VERSION @GUILE_VERSION@

/* Define to 1 if the system has the type 'FILE'. */
#cmakedefine HAVE_FILE 1

/* Define to 1 if you have the 'gettimeofday' function. */
#cmakedefine HAVE_GETTIMEOFDAY 1

/* Define to 1 if the system has the type 'intptr_t'. */
#cmakedefine HAVE_INTPTR_T 1

/* Define to 1 if you have the <inttypes.h> header file. */
#cmakedefine HAVE_INTTYPES_H 1

/* Define to 1 if you have the <memory.h> header file. */
#cmakedefine HAVE_MEMORY_H 1

/* Define to 1 if you have the <pty.h> header file. */
#cmakedefine HAVE_PTY_H 1

/* Define if the Qt framework is available. */
#cmakedefine HAVE_QT 1

/* Define to 1 if you have the 'snprintf' function. */
#cmakedefine HAVE_SNPRINTF 1

/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine HAVE_STDINT_H 1

/* Define to 1 if you have the <stdio.h> header file. */
#cmakedefine HAVE_STDIO_H 1

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

/* Define to 1 if the system has the type 'time_t'. */
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

/* Link GnuTLS library with TeXmacs */
#cmakedefine LINKED_GNUTLS 1

/* Link imlib2 library with TeXmacs */
#cmakedefine LINKED_IMLIB2 1

#cmakedefine LINKED_SQLITE3 1

#cmakedefine MACOSX_EXTENSIONS 1
#cmakedefine OS_MACOS 1
#cmakedefine MACOS_QT_MENU 1

#cmakedefine OS_ANDROID 1
#cmakedefine OS_CYGWIN 1
#cmakedefine OS_DARWIN 1
#cmakedefine OS_FREEBSD 1
#cmakedefine OS_GNU_LINUX 1
#cmakedefine OS_HAIKU 1
#cmakedefine OS_IRIX 1
#cmakedefine OS_MINGW 1
#cmakedefine OS_MINGW64 1
#cmakedefine OS_POWERPC_GNU_LINUX 1
#cmakedefine OS_SOLARIS 1
#cmakedefine OS_SUN 1

#cmakedefine PDFHUMMUS_NO_DCT 1
#cmakedefine PDFHUMMUS_NO_TIFF 1
#cmakedefine PDF_RENDERER 1

/* Memory allocator */
#cmakedefine NO_FAST_ALLOC 1
#cmakedefine MAX_FAST @MAX_FAST@

/* Compiler */
#cmakedefine OLD_GNU_COMPILER 1

/* Package details */
#cmakedefine PACKAGE_BUGREPORT "@PACKAGE_BUGREPORT@"
#cmakedefine PACKAGE_NAME "@PACKAGE_NAME@"
#cmakedefine PACKAGE_STRING "@PACKAGE_STRING@"
#cmakedefine PACKAGE_TARNAME "@PACKAGE_TARNAME@"
#cmakedefine PACKAGE_URL "@PACKAGE_URL@"
#cmakedefine PACKAGE_VERSION "@PACKAGE_VERSION@"

/* GUI */
#cmakedefine QTTEXMACS 1
#cmakedefine QTPIPES 1

/* Type sizes */
#cmakedefine SIZEOF_INT @SIZEOF_INT@
#cmakedefine SIZEOF_LONG @SIZEOF_LONG@
#cmakedefine SIZEOF_LONG_LONG @SIZEOF_LONG_LONG@
#cmakedefine SIZEOF_SHORT @SIZEOF_SHORT@
#cmakedefine SIZEOF_VOID_P @SIZEOF_VOID_P@

#cmakedefine STACK_SIZE @STACK_SIZE@
#cmakedefine STDC_HEADERS 1

#cmakedefine TEXMACS_FIX_1_GNUTLS 1
#cmakedefine TEXMACS_REVISION "@TEXMACS_REVISION@"
#cmakedefine TM_DYNAMIC_LINKING 1

/* Optional features and libraries */
#cmakedefine USE_ASPELL 1
#cmakedefine USE_AXEL 1
#cmakedefine USE_CAIRO 1
#cmakedefine USE_FREETYPE @USE_FREETYPE@
#cmakedefine USE_GNUTLS 1
#cmakedefine USE_GS 1
#cmakedefine USE_ICONV 1
#cmakedefine USE_IMLIB2 1
#cmakedefine USE_INTL 1
#cmakedefine USE_QTSVG 1
#cmakedefine USE_RESVG 1
#cmakedefine USE_SPARKLE 1
#cmakedefine USE_SQLITE3 1
#cmakedefine USE_STACK_TRACE 1

/* Word length and masks */
#cmakedefine WORD_LENGTH @WORD_LENGTH@
#cmakedefine WORD_LENGTH_INC @WORD_LENGTH_INC@
#cmakedefine WORD_MASK @WORD_MASK@

#cmakedefine X11TEXMACS 1
#cmakedefine X_DISPLAY_MISSING 1

/* Guile string size type */
#cmakedefine guile_str_size_t @guile_str_size_t@

/* Qt static plugins */
#cmakedefine qt_no_fontconfig 1
#cmakedefine qt_static_plugin_qgif 1
#cmakedefine qt_static_plugin_qico 1
#cmakedefine qt_static_plugin_qjpeg 1
#cmakedefine qt_static_plugin_qsvg 1
#cmakedefine qt_static_plugin_xcb 1
