# CMake file for TeXmacs
# created by Massimiliano Gubinelli 

# The name of our project is "TeXmacs". CMakeLists files in this project can 
# refer to the root source directory of the project as ${TEXMACS_SOURCE_DIR} and 
# to the root binary directory of the project as ${TEXMACS_BINARY_DIR}. 

cmake_minimum_required(VERSION 2.6)



### Project name 

PROJECT(TEXMACS CXX C) # check for C/C++ compiler

SET(PACKAGE TeXmacs)
SET(DEVEL_VERSION "1.0.7")
SET(DEVEL_RELEASE "1")
SET(STABLE_VERSION "1.0.7")
SET(STABLE_RELEASE "1")
SET(WINDOWS_VERSION "1.0.5")
SET(WINDOWS_RELEASE "1")

#INCLUDE_DIRECTORIES(${TEXMACS_SOURCE_DIR}/cmake/)
MESSAGE( ${TEXMACS_SOURCE_DIR}/cmake )

SET(CMAKE_MODULE_PATH ${TEXMACS_SOURCE_DIR}/cmake)

# Recurse into the "src" subdirectory. This does not actually 
# cause another cmake executable to run. The same process will walk through 
# the project's entire directory structure. 

ADD_SUBDIRECTORY(src)

### Include standard modules

INCLUDE(CheckFunctionExists)
INCLUDE(CheckLibraryExists)
INCLUDE(CheckIncludeFile)
INCLUDE(CheckTypeSize)
INCLUDE(FindPkgConfig)
INCLUDE(CheckCSourceCompiles)


###  Test for Guile

INCLUDE(FindGuile)
MY_FIND_GUILE(GUILE_CFLAGS GUILE_LDFLAGS)

### Check for standard functionalities

CHECK_TYPE_SIZE(FILE HAVE_SIZE)
CHECK_TYPE_SIZE(intptr_t HAVE_INTPTR_T)
CHECK_TYPE_SIZE(time_t HAVE_TIME_T)
CHECK_INCLUDE_FILE(pty.h HAVE_PTY_H)
CHECK_INCLUDE_FILE(util.h HAVE_UTIL_H)
CHECK_FUNCTION_EXISTS(gettimeofday HAVE_GETTIMEOFDAY)

INCLUDE(FindX11)

### Check for dl library

CHECK_FUNCTION_EXISTS(dlopen HAVE_DLOPEN)

IF (NOT ${HAVE_DLOPEN})
  CHECK_LIBRARY_EXISTS(dl dlopen "" HAVE_DLOPEN)
  IF (${HAVE_DLOPEN})
    SET(CONFIG_BDL "-ldl")
  ENDIF (${HAVE_DLOPEN})
ENDIF (NOT ${HAVE_DLOPEN})

IF (NOT ${HAVE_DLOPEN})
  CHECK_LIBRARY_EXISTS(dld dld_link "" HAVE_DLOPEN)
  IF (${HAVE_DLOPEN})
    SET(CONFIG_BDL "-ldl")
  ENDIF (${HAVE_DLOPEN})
ENDIF (NOT ${HAVE_DLOPEN})

IF (NOT ${HAVE_DLOPEN})
  CHECK_FUNCTION_EXISTS(shl_load HAVE_DLOPEN)
  IF (${HAVE_DLOPEN})
    SET(CONFIG_BDL "-ldl")
  ENDIF (${HAVE_DLOPEN})
ENDIF (NOT ${HAVE_DLOPEN})


IF (${HAVE_DLOPEN})
  SET(DYNAMIC_LINKING 1) # dynamic linking works
ENDIF (${HAVE_DLOPEN})

### Check for Iconv

INCLUDE(FindIconv)

#SET(ICONV_CFLAGS "-I${ICONV_INCLUDE_DIR}")
#SET(ICONV_LDFLAGS ICONV_LIBRARIES)

#MESSAGE ( ${ICONV_INCLUDE_DIR} ${ICONV_LIBRARIES})


### Check for FreeType2

INCLUDE(FindFreeType2)

# FREETYPE_CFLAGS, FREETYPE_LDFLAGS


### Check for Cairo

pkg_check_modules(CAIRO cairo)

SET(CMAKE_REQUIRED_FLAGS "${CAIRO_CFLAGS}" "${CAIRO_CFLAGS_OTHERS}" "${CAIRO_LDFLAGS}" "${CAIRO_LDFLAGS_OTHERS}")
SET(CMAKE_REQUIRED_INCLUDES "${CAIRO_INCLUDE_DIRS}" "${CAIRO_LIBRARY_DIRS}")
SET(CMAKE_REQUIRED_LIBRARIES ${CAIRO_LIBRARIES})


FILE(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/src.c"
  "#include <cairo.h>
   void main() {
    cairo_surface_t *surface;
    surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 120, 120);
   }")

TRY_COMPILE(_cairo_works
  ${CMAKE_BINARY_DIR}
  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/src.c
  COMPILE_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS}
  CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${CAIRO_CFLAGS} ${CAIRO_CFLAGS_OTHERS}
              -DINCLUDE_DIRECTORIES:STRING=${CAIRO_INCLUDE_DIRS}
              -DLINK_DIRECTORIES:STRING=${CAIRO_LIBRARY_DIRS}
              -DLINK_LIBRARIES:STRING=${CAIRO_LIBRARIES}
  OUTPUT_VARIABLE OUTPUT )

IF(_cairo_works)
  MESSAGE("cairo works")
ELSE(_cairo_works)
  MESSAGE(${OUTPUT})
ENDIF(_cairo_works)

### Generate configure headers

CONFIGURE_FILE(${TEXMACS_SOURCE_DIR}/src/System/config.h.cmake ${TEXMACS_BINARY_DIR}/src/System/config.h)


