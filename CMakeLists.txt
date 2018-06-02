# CMake file for TeXmacs
# created by Massimiliano Gubinelli 

# The name of our project is "TEXMACS". CMakeLists files in this project can 
# refer to the root source directory of the project as ${TEXMACS_SOURCE_DIR} and 
# to the root binary directory of the project as ${TEXMACS_BINARY_DIR}. 


### --------------------------------------------------------------------
### Basic settings (project independent)
### --------------------------------------------------------------------

cmake_minimum_required(VERSION 3.5)

set(CMAKE_CXX_STANDARD 11)

IF(NOT CMAKE_BUILD_TYPE)
 SET(CMAKE_BUILD_TYPE Debug CACHE STRING
     "Choose the type of build, options are: None Debug Release
RelWithDebInfo MinSizeRel."
     FORCE)
ENDIF(NOT CMAKE_BUILD_TYPE)

#INCLUDE(CMakeDetermineSystem)


### --------------------------------------------------------------------
### Project name 
### --------------------------------------------------------------------

PROJECT(TEXMACS CXX C) # check for C/C++ compiler

SET(PACKAGE TeXmacs)
SET(DEVEL_VERSION "1.99.6")
SET(DEVEL_RELEASE "1")
SET(STABLE_VERSION "1.99.6")
SET(STABLE_RELEASE "1")

#INCLUDE_DIRECTORIES(${TEXMACS_SOURCE_DIR}/cmake/)
#MESSAGE( ${TEXMACS_SOURCE_DIR}/cmake )

set(CMAKE_MODULE_PATH ${TEXMACS_SOURCE_DIR}/cmake)


### --------------------------------------------------------------------
### Options
### --------------------------------------------------------------------
IF(NOT TEXMACS_GUI)
  SET(TEXMACS_GUI "Qt" CACHE STRING "TeXmacs Gui (Qt, X11, Aqua)")
ENDIF(NOT TEXMACS_GUI)

option (QTPIPES "use Qt pipes" ON)

option (USE_SQLITE3 "Use SQLite3" ON)
if (USE_SQLITE3)
    find_package (SQLite3)
    option (LINKED_SQLITE3 "Use Linked SQLite3" ON)
endif (USE_SQLITE3)

option (USE_FREETYPE "use Freetype" ON)
if (USE_FREETYPE)
    find_package (Freetype)
    option (LINKED_FREETYPE "linked Freetype" ON)
endif (USE_FREETYPE)

option (USE_CAIRO "use Cairo" OFF)
if (USE_CAIRO)
    find_package(Cairo)
    option (LINKED_CAIRO "linked Cairo" ON)
endif (USE_CAIRO)

option (USE_IMLIB2 "use Imlib2" OFF)
if (USE_IMLIB2)
    find_package(Imlib2)
    option (LINKED_IMLIB2 "linked Imlib2" ON)
endif (USE_IMLIB2)

option (USE_SPARKLE "use Sparkle" OFF)

option(PDFHUMMUS_NO_TIFF "Disable TIFF in PDF Hummus" ON)

if (APPLE)
  set(MACOSX_EXTENSIONS 1)
  set(OS_MACOS 1)
  option(MACOS_QT_MENU "Disable MacOS native menu" ON)
endif (APPLE)


### --------------------------------------------------------------------
### Include standard modules
### --------------------------------------------------------------------

include(CheckFunctionExists)
include(CheckLibraryExists)
include(CheckIncludeFile)
include(CheckTypeSize)
include(FindPkgConfig)
include(CheckCSourceCompiles)
include(FindX11)


### --------------------------------------------------------------------
### Check for standard functionalities
### --------------------------------------------------------------------
CHECK_TYPE_SIZE(FILE HAVE_FILE)
CHECK_TYPE_SIZE(intptr_t HAVE_INTPTR_T)
CHECK_TYPE_SIZE(time_t HAVE_TIME_T)
CHECK_INCLUDE_FILE(inttypes.h HAVE_INTTYPES_H)
CHECK_INCLUDE_FILE(memory.h HAVE_MEMORY_H)
CHECK_INCLUDE_FILE(stdlib.h HAVE_STDLIB_H)
CHECK_INCLUDE_FILE(strings.h HAVE_STRINGS_H)
CHECK_INCLUDE_FILE(string.h HAVE_STRING_H)
CHECK_INCLUDE_FILE(sys/stat.h HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE(unistd.h HAVE_UNISTD_H)
CHECK_INCLUDE_FILE(X11/Xlib.h HAVE_X11_XLIB_H)
CHECK_INCLUDE_FILE(X11/Xutil.h HAVE_X11_XUTIL_H)
CHECK_INCLUDE_FILE(pty.h HAVE_PTY_H)
CHECK_INCLUDE_FILE(util.h HAVE_UTIL_H)
CHECK_FUNCTION_EXISTS(gettimeofday HAVE_GETTIMEOFDAY)


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
  #SET(TM_DYNAMIC_LINKING 1) # dynamic linking works
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

### --------------------------------------------------------------------
### Package Data
### --------------------------------------------------------------------

set(PACKAGE_BUGREPORT 1)
set(PACKAGE_NAME 1)
set(PACKAGE_STRING 1)
set(PACKAGE_TARNAME 1)
set(PACKAGE_URL 1)
set(PACKAGE_VERSION 1)

### --------------------------------------------------------------------
### Memory allocation options
### --------------------------------------------------------------------

#if(${DISABLE_FASTALLOC})
#  set(NO_FAST_ALLOC 1)
#endif(${DISABLE_FASTALLOC})


### --------------------------------------------------------------------
### Experimental options
### --------------------------------------------------------------------

#  SET(EXPERIMENTAL 1)

### --------------------------------------------------------------------
###  Test for Libraries
### --------------------------------------------------------------------

find_package(Guile REQUIRED)
find_package(PNG)
find_package(Iconv)
find_package(ZLIB)
find_package(JPEG)
#FIND_PACKAGE(Axel)





### --------------------------------------------------------------------
###  Some Guile related adjustements
### --------------------------------------------------------------------
if(Guile_VERSION_STRING VERSION_LESS 1.6)
  set(CONFIG_GUILE_SERIAL "A")
  set(GUILE_A 1)
elseif(Guile_VERSION_STRING VERSION_LESS 1.8)
  set(CONFIG_GUILE_SERIAL "B")
  set(GUILE_B 1)
elseif(Guile_VERSION_STRING VERSION_LESS 2.0)
  set(CONFIG_GUILE_SERIAL "C")
  set(GUILE_C 1)
elseif(Guile_VERSION_STRING VERSION_LESS 3.0)
  set(CONFIG_GUILE_SERIAL "D")
  set(GUILE_D 1)
  set(SCM_SIZET size_t)
else(Guile_VERSION_STRING VERSION_LESS 1.6)
  message(ERROR "Not Guile serial A,B,C or D???")
endif(Guile_VERSION_STRING VERSION_LESS 1.6)


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
  void declare () { gh_new_procedure (\"identity\", (FN) identity, 1, 0, 0); }
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

if (TEXMACS_GUI STREQUAL "Qt")

  find_package (Qt5 COMPONENTS Core Gui Widgets PrintSupport REQUIRED)
  set (QT_LIBRARIES Qt5::Core Qt5::Gui Qt5::Widgets Qt5::PrintSupport)

  IF(WIN32)
   SET(GUI_TYPE WIN32)
  ENDIF(WIN32)

  include_directories(
    ${Qt5Core_INCLUDE_DIRS}
    ${Qt5Gui_INCLUDE_DIRS}
    ${Qt5Widgets_INCLUDE_DIRS}
    ${Qt5PrintSupport_INCLUDE_DIRS}
  )

  add_definitions("-DQTTEXMACS")
  set (QTTEXMACS 1)

  SET(CONFIG_GUI "QT")
  SET(CONFIG_QT "Qt")
  MESSAGE(STATUS "Enabling experimental  Qt port")
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

else (TEXMACS_GUI STREQUAL "Qt")

  find_package (X11)

  include_directories(
    ${X11_INCLUDE_DIR}
  )

  add_definitions("-DX11TEXMACS")
  set (X11TEXMACS 1)

  SET(CONFIG_X11 "X11 Widkit Ghostscript")
  SET(CONFIG_GUI "X11")
  

  MESSAGE(STATUS "Enabling standard X11 interface")

endif (TEXMACS_GUI STREQUAL "Qt")

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
set(DEBUG_ASSERT 1)
set(USE_GS 1)
set(GS_EXE /usr/bin/gs)
set(GS_FONTS ../share/ghostscript/fonts:/usr/share/fonts:)
set(GS_LIB ../share/ghostscript/9.06/lib:)
set(GUILE_NUM 1)
set(PDF_RENDERER 1)
set(SIZEOF_VOID_P 8)
set(STDC_HEADERS 1)
set(USE_STACK_TRACE 1)
configure_file(${TEXMACS_SOURCE_DIR}/src/System/config.h.cmake ${TEXMACS_BINARY_DIR}/src/System/config.h)

set(CONFIG_STD_SETENV "#define STD_SETENV")
set(tm_orig ${TEXMACS_SOURCE_DIR})
set(CONFIG_HOST_OS "linux-gnu") # TODO: should not be hard-coded
set(CONFIG_HOST_VENDOR "pc") # TODO: should not be hard-coded
set(CONFIG_HOST_CPU ${CMAKE_SYSTEM_PROCESSOR})
set(CONFIG_USER $ENV{USER})
string(TIMESTAMP CONFIG_DATE)
set(tm_devel TeXmacs-${DEVEL_VERSION})
set(tm_stable TeXmacs-${STABLE_VERSION})
set(tm_devel_release ${tm_devel}-${DEVEL_RELEASE})
set(tm_stable_release ${tm_stable}-${STABLE_RELEASE})
configure_file(${TEXMACS_SOURCE_DIR}/src/System/tm_configure.hpp.cmake ${TEXMACS_BINARY_DIR}/src/System/tm_configure.hpp)

set(prefix ${CMAKE_INSTALL_PREFIX})
set(exec_prefix ${prefix})
set(datarootdir ${prefix}/share)
set(datadir ${datarootdir})
set(tmdata ${datarootdir}/TeXmacs)
set(tmbin ${exec_prefix}/lib/texmacs/TeXmacs)

### --------------------------------------------------------------------
### Set the executable target name and other stuff
### --------------------------------------------------------------------

set(TeXmacs_binary_name "texmacs.bin")

# If we are build a "Debug" version then put that on the built Application
# SET (EXE_DEBUG_EXTENSION "_debug")

### --------------------------------------------------------------------
### Compile sources
### --------------------------------------------------------------------

add_subdirectory(src)
add_subdirectory(plugins)
add_subdirectory(misc)
add_subdirectory(TeXmacs)

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
### bin/
install(FILES ${TEXMACS_BINARY_DIR}/misc/script/texmacs
              ${TEXMACS_BINARY_DIR}/misc/scripts/fig2ps
              misc/scripts/tm_gs 
        DESTINATION bin
        PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                    GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
install(FILES  DESTINATION share/TeXmacs/bin
  PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ)
### include/
install(DIRECTORY TeXmacs/include DESTINATION ${CMAKE_INSTALL_PREFIX})
### share/
install(DIRECTORY plugins DESTINATION share/TeXmacs
  PATTERN "bin" EXCLUDE)
install(FILES TeXmacs/misc/mime/texmacs.applications DESTINATION share/application_registry)
install(FILES TeXmacs/misc/mime/texmacs.desktop DESTINATION share/applications)
install(FILES TeXmacs/misc/images/TeXmacs.svg DESTINATION share/icons/gnome/scalable/apps)
install(FILES TeXmacs/misc/images/text-texmacs.svg DESTINATION share/icons/gnome/scalable/mimetypes)
install(FILES TeXmacs/misc/mime/texmacs.xml DESTINATION share/mime/packages)
install(FILES TeXmacs/misc/pixmaps/TeXmacs.xpm DESTINATION share/pixmaps)
