# CMake file for TeXmacs
# created by Massimiliano Gubinelli 
# maintained by Darcy Shen

# The name of our project is "TEXMACS". CMakeLists files in this project can 
# refer to the root source directory of the project as ${TEXMACS_SOURCE_DIR} and 
# to the root binary directory of the project as ${TEXMACS_BINARY_DIR}. 


### --------------------------------------------------------------------
### Basic settings (project independent)
### --------------------------------------------------------------------

cmake_minimum_required (VERSION 3.5)

set (CMAKE_CXX_STANDARD 11)
set (CMAKE_AUTOMOC ON)
set (CMAKE_INCLUDE_CURRENT_DIR ON)
set (CMAKE_CXX_FLAGS "-Wno-deprecated-register")

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Debug CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
    FORCE)
endif (NOT CMAKE_BUILD_TYPE)

#INCLUDE(CMakeDetermineSystem)

# Configure CCache if available
find_program (CCACHE_FOUND ccache)
if (CCACHE_FOUND)
  set_property (GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
  set_property (GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
endif (CCACHE_FOUND)


### --------------------------------------------------------------------
### Project name 
### --------------------------------------------------------------------

project (TEXMACS CXX C) # check for C/C++ compiler

set (PACKAGE TeXmacs)
set (CMAKE_MODULE_PATH ${TEXMACS_SOURCE_DIR}/cmake)
set (DEVEL_VERSION "1.99.10")
set (DEVEL_RELEASE "1")
set (STABLE_VERSION "1.99.10")
set (STABLE_RELEASE "1")
set (TEXMACS_REVISION "1.99.10")


### --------------------------------------------------------------------
### Options
### --------------------------------------------------------------------
if (NOT TEXMACS_GUI)
  if (APPLE)
    set (TEXMACS_GUI "Qt4" CACHE STRING "TeXmacs Gui (Qt, X11, Aqua)")
  else (APPLE)
    set (TEXMACS_GUI "Qt" CACHE STRING "TeXmacs Gui (Qt, X11, Aqua)")
  endif (APPLE)
endif (NOT TEXMACS_GUI)

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

option (PDFHUMMUS_NO_TIFF "Disable TIFF in PDF Hummus" ON)

option (USE_GMP "use GMP" ON)
if (USE_GMP)
  find_package(GMP)
endif (USE_GMP)

if (APPLE)
  set (MACOSX_EXTENSIONS 1)
  set (OS_MACOS 1)
  option (MACOS_QT_MENU "Disable MacOS native menu" OFF)
endif (APPLE)


### --------------------------------------------------------------------
### Include standard modules
### --------------------------------------------------------------------

include (CheckFunctionExists)
include (CheckLibraryExists)
include (CheckIncludeFile)
include (CheckTypeSize)
include (FindPkgConfig)
include (CheckCSourceCompiles)
include (FindX11)


### --------------------------------------------------------------------
### Check for standard functionalities
### --------------------------------------------------------------------
check_type_size (FILE HAVE_FILE)
check_type_size (intptr_t HAVE_INTPTR_T)
check_type_size (time_t HAVE_TIME_T)
check_include_file (inttypes.h HAVE_INTTYPES_H)
check_include_file (memory.h HAVE_MEMORY_H)
check_include_file (stdlib.h HAVE_STDLIB_H)
check_include_file (strings.h HAVE_STRINGS_H)
check_include_file (string.h HAVE_STRING_H)
check_include_file (sys/stat.h HAVE_SYS_STAT_H)
check_include_file (unistd.h HAVE_UNISTD_H)
check_include_file (X11/Xlib.h HAVE_X11_XLIB_H)
check_include_file (X11/Xutil.h HAVE_X11_XUTIL_H)
check_include_file (pty.h HAVE_PTY_H)
check_include_file (util.h HAVE_UTIL_H)
check_function_exists (gettimeofday HAVE_GETTIMEOFDAY)


### --------------------------------------------------------------------
### Check for dl library
### --------------------------------------------------------------------

check_function_exists (dlopen HAVE_DLOPEN)

if (NOT ${HAVE_DLOPEN})
  check_library_exists (dl dlopen "" HAVE_DLOPEN)
  if (${HAVE_DLOPEN})
    set (CONFIG_BDL "-ldl")
  endif (${HAVE_DLOPEN})
endif (NOT ${HAVE_DLOPEN})

if (NOT ${HAVE_DLOPEN})
  check_library_exists (dld dld_link "" HAVE_DLOPEN)
  if (${HAVE_DLOPEN})
    set (CONFIG_BDL "-ldl")
  endif (${HAVE_DLOPEN})
endif (NOT ${HAVE_DLOPEN})

if (NOT ${HAVE_DLOPEN})
  check_function_exists (shl_load HAVE_DLOPEN)
  if (${HAVE_DLOPEN})
    set (CONFIG_BDL "-ldl")
  endif (${HAVE_DLOPEN})
endif (NOT ${HAVE_DLOPEN})


if (${HAVE_DLOPEN})
  #SET(TM_DYNAMIC_LINKING 1) # dynamic linking works
endif (${HAVE_DLOPEN})

### --------------------------------------------------------------------
### Handle different systems case by case
### --------------------------------------------------------------------

set (CONFIG_OS "GNU_LINUX")
set (CONFIG_OS_SUFFIX "gnu-linux")
set (CONFIG_CXXTEMPLATE "")
set (CONFIG_STD_SETENV "#define STD_SETENV")
set (CONFIG_SO "so")
set (CONFIG_LIB_PATH "LD_LIBRARY_PATH")
set (CONFIG_CHMOD "chmod -f")
set (CONFIG_CXXOPTIMIZE "-O2")
set (CONFIG_BSTATIC "-Wl,-Bstatic")
set (CONFIG_BSHARED "-Wl,-Bdynamic")
set (CONFIG_BFLAGS "")
set (CONFIG_BPATH "-Wl,-rpath,")
set (CONFIG_WORD_LENGTH "4")
set (CONFIG_WORD_LENGTH_INC "3")
set (CONFIG_WORD_MASK "0xfffffffc")
set (CONFIG_MAX_FAST "260 // WORD_LENGTH more than power of 2")

check_type_size ("void*" SIZE_OF_VOIDP)

message (STATUS "Check if we are on a 64-bits computer")
if (${SIZE_OF_VOIDP} EQUAL 8)
  set (CONFIG_WORD_LENGTH "8")
  set (CONFIG_WORD_LENGTH_INC "7")
  set (CONFIG_WORD_MASK "0xfffffffffffffff8")
  set (CONFIG_MAX_FAST "264 // WORD_LENGTH more than power of 2")
endif (${SIZE_OF_VOIDP} EQUAL 8)


if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  message (STATUS "final adjustments for an Intel or AMD GNU/Linux host")
  set (CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
endif (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")

if (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
  message (STATUS "final adjustments for an Intel or AMD GNU/BSD host")
  set (CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
endif (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")

if (${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")
  message (STATUS "final adjustments for an Intel or AMS Solaris host")
  set (CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
  set (CONFIG_CXXOPTIMIZE "-O3")
  set (CONFIG_BPATH "-Wl,-R,")
#  SET(X11_LDFLAGS "${X_LIBS} -lXext -lX11 -lsocket")
endif (${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")

if (${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")
  message (STATUS "final adjustments for cygwin host")
  set (CONFIG_OS "CYGWIN")
  set (CONFIG_BFLAGS "-Wl,-stack,8388608")
endif (${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")

### --------------------------------------------------------------------
### Package Data
### --------------------------------------------------------------------

set (PACKAGE_BUGREPORT 1)
set (PACKAGE_NAME 1)
set (PACKAGE_STRING 1)
set (PACKAGE_TARNAME 1)
set (PACKAGE_URL 1)
set (PACKAGE_VERSION 1)

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

find_package (Guile REQUIRED)
find_package (PNG)
find_package (Iconv)
find_package (ZLIB)
find_package (JPEG)
#FIND_PACKAGE(Axel)

### --------------------------------------------------------------------
###  Some Guile related adjustements
### --------------------------------------------------------------------
if (Guile_VERSION_STRING VERSION_LESS 1.6)
  set (CONFIG_GUILE_SERIAL "A")
  set (GUILE_A 1)
elseif (Guile_VERSION_STRING VERSION_LESS 1.8)
  set (CONFIG_GUILE_SERIAL "B")
  set (GUILE_B 1)
elseif (Guile_VERSION_STRING VERSION_LESS 2.0)
  set (CONFIG_GUILE_SERIAL "C")
  set (GUILE_C 1)
elseif (Guile_VERSION_STRING VERSION_LESS 3.0)
  set (CONFIG_GUILE_SERIAL "D")
  set (GUILE_D 1)
  set (SCM_SIZET size_t)
else (Guile_VERSION_STRING VERSION_LESS 1.6)
  message (ERROR "Not Guile serial A,B,C or D???")
endif (Guile_VERSION_STRING VERSION_LESS 1.6)


set (CMAKE_REQUIRED_FLAGS "-Werror ${Guile_CFLAGS}")
set (CMAKE_REQUIRED_INCLUDES ${Guile_INCLUDE_DIRS})
set (CMAKE_REQUIRED_LIBRARIES ${Guile_LIBRARIES})

message (STATUS "Checking the size_t of guile strings")

check_c_source_compiles ("
#include <guile/gh.h>
#include <libguile.h>
void print_string (SCM s) {
  int len_r;
  char* r= gh_scm2newstr (s, &len_r);
}
int main() { return 0; }
" _guile_str_size_test)

if (_guile_str_size_test)
  set (guile_str_size_t int)
else (_guile_str_size_test)
  set (guile_str_size_t size_t)
endif (_guile_str_size_test)

message (STATUS "Checking whether ... arguments behave correctly")

check_c_source_compiles ("
#include <guile/gh.h>
#include <libguile.h>
typedef SCM (*FN)(...);
static SCM identity (SCM x) { return x; }
void declare () { gh_new_procedure (\"identity\", (FN) identity, 1, 0, 0); }
int main() { return 0; }
" _guile_dots_test)

if (_guile_dots_test)
  set (DOTS_OK 1)
else (_guile_dots_test)
  set (DOTS_OK)
endif (_guile_dots_test)

### --------------------------------------------------------------------
### Generate configure headers
### --------------------------------------------------------------------
set (DEBUG_ASSERT 1)
set (USE_GS 1)
set (GS_EXE /usr/bin/gs)
set (GS_FONTS ../share/ghostscript/fonts:/usr/share/fonts:)
set (GS_LIB ../share/ghostscript/9.06/lib:)
set (GUILE_NUM 1)
set (PDF_RENDERER 1)
set (SIZEOF_VOID_P 8)
set (STDC_HEADERS 1)
set (USE_STACK_TRACE 1)

set (CONFIG_STD_SETENV "#define STD_SETENV")
set (tm_orig ${TEXMACS_SOURCE_DIR})
set (CONFIG_HOST_OS "linux-gnu") # TODO: should not be hard-coded
set (CONFIG_HOST_VENDOR "pc") # TODO: should not be hard-coded
set (CONFIG_HOST_CPU ${CMAKE_SYSTEM_PROCESSOR})
set (CONFIG_USER $ENV{USER})
string (TIMESTAMP CONFIG_DATE)
set (tm_devel TeXmacs-${DEVEL_VERSION})
set (tm_stable TeXmacs-${STABLE_VERSION})
set (tm_devel_release ${tm_devel}-${DEVEL_RELEASE})
set (tm_stable_release ${tm_stable}-${STABLE_RELEASE})

set (prefix ${CMAKE_INSTALL_PREFIX})
set (exec_prefix ${prefix})
set (datarootdir ${prefix}/share)
set (datadir ${datarootdir})
set (tmdata ${datarootdir}/TeXmacs)
set (tmbin ${exec_prefix}/lib/texmacs/TeXmacs)

configure_file (${TEXMACS_SOURCE_DIR}/src/System/config.h.cmake ${TEXMACS_BINARY_DIR}/src/System/config.h)
configure_file (${TEXMACS_SOURCE_DIR}/src/System/tm_configure.hpp.cmake ${TEXMACS_BINARY_DIR}/src/System/tm_configure.hpp)

set (CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -include ${TEXMACS_BINARY_DIR}/src/System/config.h")

### --------------------------------------------------------------------
### the include dirs
### --------------------------------------------------------------------
set (TeXmacs_Include_Dirs
  ${TEXMACS_SOURCE_DIR}/src/Data/Convert
  ${TEXMACS_SOURCE_DIR}/src/Data/Document
  ${TEXMACS_SOURCE_DIR}/src/Data/Drd
  ${TEXMACS_SOURCE_DIR}/src/Data/History
  ${TEXMACS_SOURCE_DIR}/src/Data/Observers
  ${TEXMACS_SOURCE_DIR}/src/Data/String
  ${TEXMACS_SOURCE_DIR}/src/Data/Tree
  ${TEXMACS_SOURCE_DIR}/src/Edit
  ${TEXMACS_SOURCE_DIR}/src/Edit/Editor
  ${TEXMACS_SOURCE_DIR}/src/Edit/Interface
  ${TEXMACS_SOURCE_DIR}/src/Edit/Modify
  ${TEXMACS_SOURCE_DIR}/src/Edit/Process
  ${TEXMACS_SOURCE_DIR}/src/Edit/Replace
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Bitmap_fonts
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Colors
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Fonts
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Gui
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Handwriting
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Mathematics
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Pictures
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Renderer
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Spacial
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Types
  ${TEXMACS_SOURCE_DIR}/src/Kernel/Abstractions
  ${TEXMACS_SOURCE_DIR}/src/Kernel/Containers
  ${TEXMACS_SOURCE_DIR}/src/Kernel/Types
  ${TEXMACS_SOURCE_DIR}/src/Plugins
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Pdf/LibAesgm
  ${TEXMACS_SOURCE_DIR}/src/Scheme
  ${TEXMACS_SOURCE_DIR}/src/Scheme/Guile
  ${TEXMACS_SOURCE_DIR}/src/Scheme/Scheme
  ${TEXMACS_SOURCE_DIR}/src/System
  ${TEXMACS_SOURCE_DIR}/src/System/Boot
  ${TEXMACS_SOURCE_DIR}/src/System/Classes
  ${TEXMACS_SOURCE_DIR}/src/System/Files
  ${TEXMACS_SOURCE_DIR}/src/System/Language
  ${TEXMACS_SOURCE_DIR}/src/System/Link
  ${TEXMACS_SOURCE_DIR}/src/System/Misc
  ${TEXMACS_SOURCE_DIR}/src/Texmacs
  ${TEXMACS_SOURCE_DIR}/src/Texmacs/Data
  ${TEXMACS_SOURCE_DIR}/src/Typeset
  ${TEXMACS_SOURCE_DIR}/src/Typeset/Bridge
  ${TEXMACS_SOURCE_DIR}/src/Typeset/Concat
  ${TEXMACS_SOURCE_DIR}/src/Typeset/Page
  ${TEXMACS_SOURCE_DIR}/TeXmacs/include 
  ${TEXMACS_BINARY_DIR}/src/System/ 
)

set (TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs}
  ${Guile_INCLUDE_DIRS} ${FREETYPE_INCLUDE_DIRS} ${Cairo_INCLUDE_DIRS}
  ${IMLIB2_INCLUDE_DIR} ${GMP_INCLUDES}
)

### --------------------------------------------------------------------
### the main sources
### --------------------------------------------------------------------
file (GLOB_RECURSE TeXmacs_Base_SRCS
  "${TEXMACS_SOURCE_DIR}/src/Data/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Edit/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Graphics/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Kernel/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Scheme/Scheme/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Scheme/Guile/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/System/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Texmacs/Data/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Texmacs/Server/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Texmacs/Window/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Typeset/*.cpp"
)

file (GLOB_RECURSE TeXmacs_Std_Plugins_SRCS
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Axel/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Bibtex/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Cairo/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Database/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Freetype/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Pdf/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Pdf/*.c"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Ghostscript/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Imlib2/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Ispell/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Metafont/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/LaTeX_Preview/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Mplayer/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Openssl/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Unix/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Sqlite3/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Updater/*.cpp"
)

file (GLOB_RECURSE TeXmacs_Style_SRCS
  "${TEXMACS_SOURCE_DIR}/src/Style/*.cpp"
)

set (TeXmacs_All_SRCS ${TeXmacs_Base_SRCS}  ${TeXmacs_Std_Plugins_SRCS})

### --------------------------------------------------------------------
### GUI Interfaces
### --------------------------------------------------------------------

file (GLOB_RECURSE TeXmacs_Widkit_SRCS "${TEXMACS_SOURCE_DIR}/src/Plugins/Widkit/*.cpp")
file (GLOB_RECURSE TeXmacs_X11_SRCS "${TEXMACS_SOURCE_DIR}/src/Plugins/X11/*.cpp")
file (GLOB_RECURSE TeXmacs_Qt_SRCS "${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/*.cpp")

set (TeXmacs_Qt_Moc_HDRS
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMApplication.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMFileDialog.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMGuiHelper.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMInteractiveInputHelper.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMInteractivePrompt.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMMenuHelper.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMPipeLink.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMPrintDialog.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMPrinterSettings.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMScrollView.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMSockets.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMStyle.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMTreeModel.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMWidget.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMWindow.hpp
)

set (TeXmacs_Cocoa_SRCS
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_dialogues.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_gui.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_menu.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_renderer.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_utilities.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_widget.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/mac_utilities.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/TMButtonsController.m
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/TMView.mm
)

set (TeXmacs_MacOS_SRCS
  ${TEXMACS_SOURCE_DIR}/src/Plugins/MacOS/HIDRemote.m
  ${TEXMACS_SOURCE_DIR}/src/Plugins/MacOS/mac_images.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/MacOS/mac_spellservice.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/MacOS/mac_utilities.mm
)

### --------------------------------------------------------------------
### Determine TeXmacs_Libraries
### --------------------------------------------------------------------

set (TeXmacs_Libraries
  ${Guile_LIBRARIES}
  ${ZLIB_LIBRARIES}
  ${JPEG_LIBRARIES}
  ${PNG_LIBRARIES}
  -lpthread
)

if (LINKED_CAIRO)
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${CAIRO_LIBRARIES})
endif (LINKED_CAIRO)

if (LINKED_SQLITE3)
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${SQLITE3_LIBRARIES})
endif (LINKED_SQLITE3)

if (LINKED_FREETYPE)
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${FREETYPE_LIBRARIES})
endif (LINKED_FREETYPE)


### --------------------------------------------------------------------
### GUI selection
### --------------------------------------------------------------------

set (GUI_TYPE) # nothing or WIN32, MACOSX bundles are treated independetly

if (TEXMACS_GUI MATCHES "Qt.*")

  if (TEXMACS_GUI STREQUAL "Qt4")
    find_package (Qt4 REQUIRED)
    set (TeXmacs_Include_Dirs
      ${TeXmacs_Include_Dirs}
      ${QT_INCLUDE_DIR}
    )
    include (${QT_USE_FILE})
  else (TEXMACS_GUI STREQUAL "Qt4")
    find_package (Qt5 COMPONENTS Core Gui Widgets PrintSupport REQUIRED)
    set (QT_LIBRARIES Qt5::Core Qt5::Gui Qt5::Widgets Qt5::PrintSupport)
    set (TeXmacs_Include_Dirs
      ${TeXmacs_Include_Dirs}
      ${Qt5Core_INCLUDE_DIRS}
      ${Qt5Gui_INCLUDE_DIRS}
      ${Qt5Widgets_INCLUDE_DIRS}
      ${Qt5PrintSupport_INCLUDE_DIRS}
    )
    if (Qt5_POSITION_INDEPENDENT_CODE)
      set (CMAKE_POSITION_INDEPENDENT_CODE ON)
    endif (Qt5_POSITION_INDEPENDENT_CODE)
  endif (TEXMACS_GUI STREQUAL "Qt4")

  add_definitions (${QT_DEFINITIONS})
  add_definitions ("-DQTTEXMACS")

  if (CMAKE_BUILD_TYPE STREQUAL "Release")
    add_definitions ("-DQT_NO_DEBUG")
  endif (CMAKE_BUILD_TYPE STREQUAL "Release")

  set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_Qt_SRCS} ${TeXmacs_Qt_Moc_HDRS})
  set (TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs} ${QT_INCLUDES})
  set (TeXmacs_Libraries ${TeXmacs_Libraries}  ${QT_LIBRARIES})

  if (WIN32)
    set (GUI_TYPE WIN32)
  endif (WIN32)

  set (QTTEXMACS 1)
  set (CONFIG_GUI "QT")
  set (CONFIG_QT "Qt")
  message (STATUS "Enabling experimental  Qt port")
elseif (TEXMACS_GUI STREQUAL "Aqua")
  find_library (COCOA_LIBRARY Cocoa)
  find_path (COCOA_INCLUDE_DIR Cocoa/Cocoa.h)
  set (TeXmacs_Include_Dirs
    ${TeXmacs_Include_Dirs}
    ${COCOA_INCLUDE_DIR}
  )
  set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_Cocoa_SRCS})
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${COCOA_LIBRARY})

  mark_as_advanced(COCOA_LIBRARY)
  mark_as_advanced(COCOA_INCLUDE_DIR)
else (TEXMACS_GUI STREQUAL "Qt")
  find_package (X11)

  set (TeXmacs_Include_Dirs
    ${TeXmacs_Include_Dirs}
    ${X11_INCLUDE_DIR}
  )
  set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_Widkit_SRCS} ${TeXmacs_X11_SRCS})
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${X11_LIBRARIES})

  add_definitions ("-DX11TEXMACS")
  set (X11TEXMACS 1)

  set (CONFIG_X11 "X11 Widkit Ghostscript")
  set (CONFIG_GUI "X11")

  message (STATUS "Enabling standard X11 interface")
endif (TEXMACS_GUI MATCHES "Qt.*")

if (APPLE AND TEXMACS_GUI STREQUAL "X11")
  set (TeXmacs_MacOS_SRCS
    ${TeXmacs_MacOS_SRCS}
    ${TEXMACS_SOURCE_DIR}/src/Plugins/MacOS/mac_app.mm
  )
endif (APPLE AND TEXMACS_GUI STREQUAL "X11")

if (APPLE AND USE_SPARKLE)
  set (TeXmacs_MacOS_SRCS
    ${TeXmacs_MacOS_SRCS}
    ${TEXMACS_SOURCE_DIR}/src/Plugins/Updater/tm_sparkle.mm
  )
endif (APPLE AND USE_SPARKLE)

if (APPLE)
  set (TeXmacs_Libraries
    ${TeXmacs_Libraries}
    "-framework ApplicationServices"
    "-framework CoreFoundation"
    "-framework Security"
    "-framework Carbon"
    "-framework AppKit"
    "-framework Cocoa"
    "-framework IOKit"
    "-framework CoreGraphics"
    "-framework CoreServices"
    "-framework CoreText"
    "-framework Foundation"
    "-framework ImageIO"
  )
  set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_MacOS_SRCS})

  set (GUI_TYPE MACOSX_BUNDLE)
  set (NO_FAST_ALLOC 1)
  set (AQUATEXMACS 1)
  set (COCOA_CFLAGS "")
  set (COCOA_LDFLAGS "-framework COCOA")
  set (CONFIG_GUI "COCOA")
  set (CONFIG_COCOA "Cocoa")

  message (STATUS "Enabling experimental Cocoa port")
else (APPLE)
endif (APPLE)

include_directories (${TeXmacs_Include_Dirs})

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
### Set the executable target name and other stuff
### --------------------------------------------------------------------

if (APPLE)
  set (TeXmacs_binary_name "TeXmacs")
else (APPLE)
  set (TeXmacs_binary_name "texmacs.bin")
endif (APPLE)

### --------------------------------------------------------------------
### Compile sources
### --------------------------------------------------------------------

add_subdirectory (src)
add_subdirectory (plugins)
add_subdirectory (misc)
add_subdirectory (TeXmacs)

### --------------------------------------------------------------------
### Installation
### --------------------------------------------------------------------

if (APPLE)
  install (FILES packages/macos/TeXmacs.icns DESTINATION ${CMAKE_INSTALL_PREFIX})
  install (FILES packages/macos/TeXmacs-document.icns DESTINATION ${CMAKE_INSTALL_PREFIX})
  install (DIRECTORY src/Plugins/Cocoa/English.lproj DESTINATION ${CMAKE_INSTALL_PREFIX})
else (APPLE)
endif (APPLE)

install (FILES ${TEXMACS_BINARY_DIR}/misc/script/texmacs
              ${TEXMACS_BINARY_DIR}/misc/scripts/fig2ps
              misc/scripts/tm_gs 
        DESTINATION bin
        PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                    GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
install (FILES  DESTINATION share/TeXmacs/bin
  PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ)
### include/
install (DIRECTORY TeXmacs/include DESTINATION ${CMAKE_INSTALL_PREFIX})
### share/
install (DIRECTORY plugins DESTINATION share/TeXmacs PATTERN "bin" EXCLUDE)
install (FILES TeXmacs/misc/mime/texmacs.applications DESTINATION share/application_registry)
install (FILES TeXmacs/misc/mime/texmacs.desktop DESTINATION share/applications)
install (FILES TeXmacs/misc/images/TeXmacs.svg DESTINATION share/icons/gnome/scalable/apps)
install (FILES TeXmacs/misc/images/text-texmacs.svg DESTINATION share/icons/gnome/scalable/mimetypes)
install (FILES TeXmacs/misc/mime/texmacs.xml DESTINATION share/mime/packages)
install (FILES TeXmacs/misc/pixmaps/TeXmacs.xpm DESTINATION share/pixmaps)

### --------------------------------------------------------------------
### Testing
### --------------------------------------------------------------------
include (CTest)
set (GTEST_ROOT "${TEXMACS_SOURCE_DIR}/3rdparty/benchmark/googletest/googletest")
set (GTEST_INCLUDE_DIR ${GTEST_ROOT}/include)
set (GTEST_LIBRARIES gtest gtest_main)
set (GTEST_MAIN_LIBRARY gtest_main)
set (GTEST_LIBRARY gtest)

add_subdirectory (3rdparty/benchmark EXCLUDE_FROM_ALL)
add_subdirectory (${GTEST_ROOT} EXCLUDE_FROM_ALL)
find_package (GTest REQUIRED)

enable_testing ()

add_subdirectory (tests)
add_subdirectory (benchmark)
### ---------------------------------------------------------------------
### VSCode Support
### ---------------------------------------------------------------------
foreach (DIR ${TeXmacs_Include_Dirs})
  set (VSCODE_INCLUDE_DIRS "${VSCODE_INCLUDE_DIRS}\"${DIR}\",\n                ")
endforeach (DIR ${TeXmacs_Include_Dirs})

configure_file (${TEXMACS_SOURCE_DIR}/misc/vscode/c_cpp_properties.json.in
  ${TEXMACS_SOURCE_DIR}/.vscode/c_cpp_properties.json
  @ONLY
)
