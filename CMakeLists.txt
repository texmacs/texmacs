# CMake file for TeXmacs
# created by Massimiliano Gubinelli 

# The name of our project is "TEXMACS". CMakeLists files in this project can 
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

#INCLUDE_DIRECTORIES(${TEXMACS_SOURCE_DIR}/cmake/)
#MESSAGE( ${TEXMACS_SOURCE_DIR}/cmake )

SET(CMAKE_MODULE_PATH ${TEXMACS_SOURCE_DIR}/cmake)

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
###  Test for Libraries
### --------------------------------------------------------------------

FIND_PACKAGE(Guile REQUIRED)
FIND_PACKAGE(Iconv)
FIND_PACKAGE(FreeType2)
FIND_PACKAGE(Cairo)
FIND_PACKAGE(Imlib2)
#FIND_PACKAGE(Axel)





### --------------------------------------------------------------------
###  Some Guile related adjustements
### --------------------------------------------------------------------

IF(Guile_VERSION_STRING VERSION_LESS 1.6)
  SET(CONFIG_GUILE_SERIAL "A")
ELSEIF(Guile_VERSION_STRING VERSION_LESS 1.8)
  SET(CONFIG_GUILE_SERIAL "B")
ELSE(Guile_VERSION_STRING VERSION_LESS 1.8)
  SET(CONFIG_GUILE_SERIAL "C")
ENDIF(Guile_VERSION_STRING VERSION_LESS 1.6)


SET(CMAKE_REQUIRED_FLAGS "-Werror ${Guile_CFLAGS}")
SET(CMAKE_REQUIRED_INCLUDES ${Guile_INCLUDE_DIRS})
SET(CMAKE_REQUIRED_LIBRARIES ${Guile_LIBRARIES})

MESSAGE(STATUS "Checking the size_t of guile strings")

CHECK_C_SOURCE_COMPILES(  "#include <guile/gh.h>
  #include <libguile.h>
  void print_string (SCM s) {
    int len_r;
    char* r= gh_scm2newstr (s, &len_r); } int main() { return 0; }
" _guile_str_size_test)

IF(_guile_str_size_test)
  SET(guile_str_size_t int)
ELSE(_guile_str_size_test)
  SET(guile_str_size_t size_t)
ENDIF(_guile_str_size_test)

MESSAGE(STATUS "Checking whether ... arguments behave correctly")

CHECK_C_SOURCE_COMPILES(  
" #include <guile/gh.h>
  #include <libguile.h>
  typedef SCM (*FN)(...);
  static SCM identity (SCM x) { return x; }
  void declare () { gh_new_procedure ("identity", (FN) identity, 1, 0, 0); }
  int main() { return 0; }
" _guile_dots_test)

IF(_guile_dots_test)
  SET(DOTS_OK 1)
ELSE(_guile_dots_test)
  SET(DOTS_OK)
ENDIF(_guile_dots_test)

#MESSAGE(STATUS "Checking if statically linking with guile works")
##TODO


### --------------------------------------------------------------------
### GUI selection
### --------------------------------------------------------------------

SET(GUI_TYPE) # nothing or WIN32, MACOSX bundles are treated independetly

IF(NOT TEXMACS_GUI)
  SET(TEXMACS_GUI "X11" CACHE STRING "TeXmacs Gui (Qt, X11, Aqua)")
ENDIF(NOT TEXMACS_GUI)

IF(TEXMACS_GUI STREQUAL "Qt")

  FIND_PACKAGE(Qt4)
 
  IF(QT4_FOUND)
    IF(WIN32)
     SET(GUI_TYPE WIN32)
    ENDIF(WIN32)

    INCLUDE(${QT_USE_FILE})
    SET(NO_FAST_ALLOC 1)
    SET(QTTEXMACS 1)
    SET(CONFIG_GUI "QT")
    SET(CONFIG_QT "Qt")
    MESSAGE(STATUS "Enabling experimental  Qt port")
  ELSE(QT4_FOUND)
    MESSAGE(ERROR "Qt is required to compile the Qt port")
  ENDIF(QT4_FOUND)
  
ELSEIF(TEXMACS_GUI STREQUAL "Aqua")

  IF (APPLE)
    SET(GUI_TYPE MACOSX_BUNDLE)

    SET(NO_FAST_ALLOC 1)
    SET(AQUATEXMACS 1)
    SET(COCOA_CFLAGS "")
    SET(COCOA_LDFLAGS "-framework COCOA")
    SET(CONFIG_GUI "COCOA")
    SET(CONFIG_COCOA "Cocoa")

    MESSAGE(STATUS "Enabling experimental Cocoa port")

  ELSE (APPLE)
  
    MESSAGE( ERROR "Aqua GUI is not supported on this system")
    
  ENDIF (APPLE)

ELSE(TEXMACS_GUI STREQUAL "Qt")

  FIND_PACKAGE(X11)

  SET(CONFIG_X11 "X11 Widkit Ghostscript")
  SET(CONFIG_GUI "X11")
  

  MESSAGE(STATUS "Enabling standard X11 interface")

ENDIF(TEXMACS_GUI STREQUAL "Qt")

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
CONFIGURE_FILE(${TEXMACS_SOURCE_DIR}/src/System/tm_configure.hpp.cmake ${TEXMACS_BINARY_DIR}/src/System/tm_configure.hpp)

### --------------------------------------------------------------------
### Set the executable target name and other stuff
### --------------------------------------------------------------------

SET(TeXmacs_binary_name "texmacs_${TEXMACS_GUI}")

# If we are build a "Debug" version then put that on the built Application
SET (EXE_DEBUG_EXTENSION "_debug")

### --------------------------------------------------------------------
### Compile sources
### --------------------------------------------------------------------

ADD_SUBDIRECTORY(src)

### --------------------------------------------------------------------
### Complete bundle if MacOSX
### --------------------------------------------------------------------

IF (APPLE AND NOT TEXMACS_GUI STREQUAL X11)
  SET(BUNDLE_DEBUG_EXTENSION)
  IF (DEBUG)
    SET(BUNDLE_DEBUG_EXTENSION "_debug")
  ENDIF (DEBUG)  
  SET(APP_BUNDLE_NAME ${TeXmacs_binary_name}${BUNDLE_DEBUG_EXTENSION}".app")
  ADD_CUSTOM_TARGET ( bundle
            mkdir -p  ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/MacOS 
    COMMAND mkdir -p  ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/Resources
    COMMAND cp ${TEXMACS_SOURCE_DIR}/misc/bundle/Info.plist ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/Resources
    COMMAND cp ${TEXMACS_SOURCE_DIR}/misc/bundle/TeXmacs ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/MacOS
    COMMAND chmod a+x  ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/MacOS/TeXmacs
  )
ENDIF (APPLE AND NOT TEXMACS_GUI STREQUAL X11)


### --------------------------------------------------------------------
### Installation
### --------------------------------------------------------------------

