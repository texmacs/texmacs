# CMake file for TeXmacs
# created by Massimiliano Gubinelli 

# The name of our project is "TeXmacs". CMakeLists files in this project can 
# refer to the root source directory of the project as ${TEXMACS_SOURCE_DIR} and 
# to the root binary directory of the project as ${TEXMACS_BINARY_DIR}. 


### --------------------------------------------------------------------
### Basic settings (project independent)
### --------------------------------------------------------------------

cmake_minimum_required(VERSION 2.6)

IF(NOT CMAKE_BUILD_TYPE)
 SET(CMAKE_BUILD_TYPE Debug CACHE STRING
     "Choose the type of build, options are: None Debug Release
RelWithDebInfo MinSizeRel."
     FORCE)
ENDIF(NOT CMAKE_BUILD_TYPE)

INCLUDE(CMakeDetermineSystem)


### --------------------------------------------------------------------
### Project name 
### --------------------------------------------------------------------

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





### --------------------------------------------------------------------
### Include standard modules
### --------------------------------------------------------------------

INCLUDE(CheckFunctionExists)
INCLUDE(CheckLibraryExists)
INCLUDE(CheckIncludeFile)
INCLUDE(CheckTypeSize)
INCLUDE(FindPkgConfig)
INCLUDE(CheckCSourceCompiles)


### --------------------------------------------------------------------
###  Test for Guile
### --------------------------------------------------------------------

INCLUDE(FindGuile)
MY_FIND_GUILE(GUILE_CFLAGS GUILE_LDFLAGS)

### --------------------------------------------------------------------
### Check for standard functionalities
### --------------------------------------------------------------------

CHECK_TYPE_SIZE(FILE HAVE_SIZE)
CHECK_TYPE_SIZE(intptr_t HAVE_INTPTR_T)
CHECK_TYPE_SIZE(time_t HAVE_TIME_T)
CHECK_INCLUDE_FILE(pty.h HAVE_PTY_H)
CHECK_INCLUDE_FILE(util.h HAVE_UTIL_H)
CHECK_FUNCTION_EXISTS(gettimeofday HAVE_GETTIMEOFDAY)

INCLUDE(FindX11)

### --------------------------------------------------------------------
### Check for dl library
### --------------------------------------------------------------------

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

### --------------------------------------------------------------------
### Check for Iconv
### --------------------------------------------------------------------

INCLUDE(FindIconv)

#SET(ICONV_CFLAGS "-I${ICONV_INCLUDE_DIR}")
#SET(ICONV_LDFLAGS ICONV_LIBRARIES)

#MESSAGE ( ${ICONV_INCLUDE_DIR} ${ICONV_LIBRARIES})


### --------------------------------------------------------------------
### Check for FreeType2
### --------------------------------------------------------------------

INCLUDE(FindFreeType2)

# FREETYPE_CFLAGS, FREETYPE_LDFLAGS


### --------------------------------------------------------------------
### Check for Cairo
### --------------------------------------------------------------------

pkg_check_modules(CAIRO cairo)

SET(CMAKE_REQUIRED_FLAGS ${CAIRO_CFLAGS} ${CAIRO_CFLAGS_OTHERS})
SET(CMAKE_REQUIRED_INCLUDES ${CAIRO_INCLUDE_DIRS})
SET(CMAKE_REQUIRED_LIBRARIES ${CAIRO_LIBRARIES})
SET(CMAKE_REQUIRED_LIBDIRS ${CAIRO_LIBRARY_DIRS})

IF(FALSE)
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
ENDIF(FALSE)

CHECK_C_SOURCE_COMPILES(  "#include <cairo.h>
   void main() {
    cairo_surface_t *surface;
    surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 120, 120);
   }" _cairo_works)

IF(_cairo_works)
  MESSAGE("cairo works")
ENDIF(_cairo_works)

### --------------------------------------------------------------------
### Check for Imlib2
### --------------------------------------------------------------------

IF(${WITH_IMLIB2})

  EXECUTE_PROCESS( COMMAND imlib2-config --cflags
                   RESULT_VARIABLE _imlib2_config_result
                   OUTPUT_VARIABLE TMP_IMLIB2_CFLAGS 
                   ERROR_VARIABLE TMP_IMLIB2_CFLAGS ) 

  EXECUTE_PROCESS( COMMAND imlib2-config --libs
                   RESULT_VARIABLE _imlib2_config_result
                   OUTPUT_VARIABLE TMP_IMLIB2_LDFLAGS 
                   ERROR_VARIABLE TMP_IMLIB2_LDFLAGS ) 


  CHECK_INCLUDE_FILE(Imlib2.h HAS_IMLIB2_H)

  SET(CMAKE_REQUIRED_FLAGS ${TMP_IMLIB2_CFLAGS})
#  SET(CMAKE_REQUIRED_INCLUDES ${CAIRO_INCLUDE_DIRS})
#  SET(CMAKE_REQUIRED_LIBRARIES ${CAIRO_LIBRARIES})
  SET(CMAKE_REQUIRED_LIBDIRS ${TMP_IMLIB2_LDFLAGS})

  CHECK_C_SOURCE_COMPILES(
  "#include <X11/Xlib.h>
   #include <Imlib2.h>
   #include <stdio.h>
   #include <string.h>
   int main() {
    Imlib_Image image= imlib_load_image("");
    return 0;
   }" _imlib2_works)
  
  IF(_imlib2_works)
    SET(USE_IMLIB2 1)
    SET(IMLIB2_CFLAGS ${TMP_IMLIB2_CFLAGS})
    IF(${WITH_IMLIB2} STREQUAL "linked")
      SET(IMLIB2_LDFLAGS ${TMP_IMLIB2_LDFLAGS})
      SET(LINKED_IMLIB2 1)
    ENDIF(${WITH_IMLIB2} STREQUAL "linked")
  ENDIF(_imlib2_works)
ENDIF(${WITH_IMLIB2})

IF(NOT USE_IMLIB2)
  MESSAGE(STATUS "disabling Imlib2 support")
ENDIF(NOT USE_IMLIB2)

### --------------------------------------------------------------------
### Check for Axel
### --------------------------------------------------------------------

### TODO

### --------------------------------------------------------------------
### Handle different systems case by case
### --------------------------------------------------------------------

SET(CONFIG_OS "GNU_LINUX")
SET(CONFIG_OS_SUFFIX "gnu-linux")
SET(CONFIG_CXXFLAGS "")
SET(CONFIG_CXXTEMPLATE "")
SET(CONFIG_STD_SETENV "#define STD_SETENV")
SET(CONFIG_SO "so")
SET(CONFIG_LIB_PATH "LD_LIBRARY_PATH")
SET(CONFIG_CHMOD "chmod -f")
SET(CONFIG_CXXOPTIMIZE "-O2")
SET(CONFIG_BSTATIC "-Wl,-Bstatic")
SET(CONFIG_BSHARED "-Wl,-Bdynamic")
SET(CONFIG_BFLAGS "")
SET(CONFIG_BPATH "-Wl,-rpath,")
SET(CONFIG_WORD_LENGTH "4")
SET(CONFIG_WORD_LENGTH_INC "3")
SET(CONFIG_WORD_MASK "0xfffffffc")
SET(CONFIG_MAX_FAST "260 // WORD_LENGTH more than power of 2")

CHECK_TYPE_SIZE("void*" SIZE_OF_VOIDP)

MESSAGE(STATUS "Check if we are on a 64-bits computer")
IF (${SIZE_OF_VOIDP} EQUAL 8)
  SET(CONFIG_WORD_LENGTH "8")
  SET(CONFIG_WORD_LENGTH_INC "7")
  SET(CONFIG_WORD_MASK "0xfffffffffffffff8")
  SET(CONFIG_MAX_FAST "264 // WORD_LENGTH more than power of 2")
ENDIF (${SIZE_OF_VOIDP} EQUAL 8)


IF(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  MESSAGE(STATUS "final adjustments for an Intel or AMD GNU/Linux host")
  SET(CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")

IF(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
  MESSAGE(STATUS "final adjustments for an Intel or AMD GNU/BSD host")
  SET(CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")

IF(${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")
  MESSAGE(STATUS "final adjustments for an Intel or AMS Solaris host")
  SET(CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
  SET(CONFIG_CXXOPTIMIZE "-O3")
  SET(CONFIG_BPATH "-Wl,-R,")
#  SET(X11_LDFLAGS "${X_LIBS} -lXext -lX11 -lsocket")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")

IF(${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")
  MESSAGE(STATUS "final adjustments for cygwin host")
  SET(CONFIG_OS "CYGWIN")
  SET(CONFIG_BFLAGS "-Wl,-stack,8388608")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")

## TODO FINISH


### --------------------------------------------------------------------
### Memory allocation options
### --------------------------------------------------------------------

IF(${DISABLE_FASTALLOC})
  SET(NO_FAST_ALLOC 1)
ENDIF(${DISABLE_FASTALLOC})


### --------------------------------------------------------------------
### Experimental options
### --------------------------------------------------------------------

#  SET(EXPERIMENTAL 1)


### --------------------------------------------------------------------
### GUI selection
### --------------------------------------------------------------------

IF(NOT TEXMACS_GUI)
  SET(TEXMACS_GUI "X11")
ENDIF(NOT TEXMACS_GUI)

IF(${TEXMACS_GUI} STREQUAL "QT")

  INCLUDE(FindQt4)

  IF(CMAKE_BUILD_TYPE STREQUAL "Release")
    ADD_DEFINITIONS(-DQT_NO_DEBUG)
  ENDIF(CMAKE_BUILD_TYPE STREQUAL "Release")

  SET(NO_FAST_ALLOC 1)
  SET(QTTEXMACS 1)
  SET(CONFIG_GUI "QT")
  SET(CONFIG_QT "Qt")

  MESSAGE("-- Enabling experimental  QT port")
  
ELIF(${TEXMACS_GUI} STREQUAL "Aqua")

  SET(NO_FAST_ALLOC 1)
  SET(AQUATEXMACS 1)
  SET(COCOA_CFLAGS "")
  SET(COCOA_LDFLAGS "-framework COCOA")
  SET(CONFIG_GUI "COCOA")
  SET(CONFIG_COCOA "Cocoa")

  MESSAGE("-- Enabling experimental Cocoa port")

ELSE(${TEXMACS_GUI} STREQUAL "QT")

  SET(CONFIG_X11 "X11 Widkit Ghostscript")
  SET(CONFIG_GUI "X11")
  

  MESSAGE("-- Enabling standard X11 interface")

ENDIF(${TEXMACS_GUI} STREQUAL "QT")

### --------------------------------------------------------------------
### Debugging options
### --------------------------------------------------------------------


### --------------------------------------------------------------------
### Warning options
### --------------------------------------------------------------------

### --------------------------------------------------------------------
### Optimization options
### --------------------------------------------------------------------


### --------------------------------------------------------------------
### Generate configure headers
### --------------------------------------------------------------------

CONFIGURE_FILE(${TEXMACS_SOURCE_DIR}/src/System/config.h.cmake ${TEXMACS_BINARY_DIR}/src/System/config.h)


### --------------------------------------------------------------------
### Compile sources
### --------------------------------------------------------------------

ADD_SUBDIRECTORY(src)

