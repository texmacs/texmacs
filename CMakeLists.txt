###############################################################################
#
# MODULE      : CMakeLists.txt
# DESCRIPTION : CMake file for TeXmacs
# COPYRIGHT   : (C) 2008-2009  Massimiliano Gubinelli
#               (C) 2017-2020  Darcy Shen
#               (C) 2026       Liza Belos
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
###############################################################################

cmake_minimum_required (VERSION 3.16)

set (CMAKE_CXX_STANDARD 17)
set (CMAKE_CXX_STANDARD_REQUIRED ON)
set (CMAKE_C_STANDARD 99)
set (CMAKE_C_STANDARD_REQUIRED ON)
set (CMAKE_INCLUDE_CURRENT_DIR ON)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Release CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
    FORCE)
endif (NOT CMAKE_BUILD_TYPE)

# Configure CCache if available
find_program (CCACHE_FOUND ccache)
if (CCACHE_FOUND)
  set_property (GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
  set_property (GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
endif (CCACHE_FOUND)

### --------------------------------------------------------------------
### Project name 
### --------------------------------------------------------------------

project (TEXMACS CXX C)

if (WIN32)
  enable_language (RC)
endif ()

set (PACKAGE TeXmacs)
set (PACKAGE_NAME "TeXmacs")
set (PACKAGE_TARNAME "TeXmacs")
set (PACKAGE_BUGREPORT "https://savannah.gnu.org/bugs/?group=texmacs")
set (PACKAGE_URL "https://www.texmacs.org")

set (VERSION_MAJOR "2")
set (VERSION_MINOR "1")
set (VERSION_BUILD "5")
set (TEXMACS_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BUILD})
set (PACKAGE_VERSION "${TEXMACS_VERSION}")
set (PACKAGE_STRING "TeXmacs ${TEXMACS_VERSION}")

set (DEVEL_VERSION ${TEXMACS_VERSION})
set (DEVEL_RELEASE "1")
set (STABLE_VERSION ${TEXMACS_VERSION})
set (STABLE_RELEASE "1")

if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs/SVNREV")
  file (READ "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs/SVNREV" ALTERNATIVE_VERSION)
  string (STRIP "${ALTERNATIVE_VERSION}" ALTERNATIVE_VERSION)
elseif (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/SVNREV")
  file (READ "${CMAKE_CURRENT_SOURCE_DIR}/SVNREV" SVNREV_RAW)
  string (STRIP "${SVNREV_RAW}" SVNREV_RAW)
  string (REGEX REPLACE "^.*:" "" SVNINT "${SVNREV_RAW}")
  string (REGEX REPLACE "[^0-9].*$" "" SVNINT "${SVNINT}")
  set (ALTERNATIVE_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${SVNINT}")
else ()
  set (ALTERNATIVE_VERSION "${TEXMACS_VERSION}")
endif ()

if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/SVNREV")
  file (READ "${CMAKE_CURRENT_SOURCE_DIR}/SVNREV" TEXMACS_REVISION)
  string (STRIP "${TEXMACS_REVISION}" TEXMACS_REVISION)
else ()
  set (TEXMACS_REVISION "${TEXMACS_VERSION}")
endif ()

# Check for in-source vs out-of-source build
if (CMAKE_CURRENT_BINARY_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  set (IS_IN_SOURCE_BUILD TRUE)
  set (TEXMACS_TARGET_DIR "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs")
  message (WARNING
    "\n*************************************************************************\n"
    " ATTENTION : Vous compilez directement dans le dossier source !\n"
    " Des fichiers générés seront placés dans l'arborescence source.\n"
    " Il est fortement recommandé d'utiliser un dossier build séparé :\n"
    "     cmake -B build -G Ninja\n"
    "     cmake --build build\n"
    "*************************************************************************\n"
  )
  # Ensure TeXmacs structure matching autoconf in source directory
  file (WRITE "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs/SVNREV" "${ALTERNATIVE_VERSION}\n")
  file (MAKE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs/bin")
  file (MAKE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs/lib")
  file (MAKE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs/examples/plugins/dynlink/lib")
  file (GLOB _EXAMPLE_PLUGIN_DIRS LIST_DIRECTORIES true "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs/examples/plugins/*")
  foreach(_DIR ${_EXAMPLE_PLUGIN_DIRS})
    if (IS_DIRECTORY "${_DIR}")
      file (MAKE_DIRECTORY "${_DIR}/bin")
    endif ()
  endforeach ()
else ()
  set (IS_IN_SOURCE_BUILD FALSE)
  set (TEXMACS_TARGET_DIR "${CMAKE_CURRENT_BINARY_DIR}/TeXmacs")
  # Reproduce TeXmacs directory structure strictly in build directory
  file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
  file (WRITE "${CMAKE_CURRENT_BINARY_DIR}/TeXmacs/SVNREV" "${ALTERNATIVE_VERSION}\n")
  file (MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/TeXmacs/bin")
  file (MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/TeXmacs/lib")
  file (MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/TeXmacs/examples/plugins/dynlink/lib")
  file (GLOB _EXAMPLE_PLUGIN_DIRS LIST_DIRECTORIES true "${CMAKE_CURRENT_BINARY_DIR}/TeXmacs/examples/plugins/*")
  foreach(_DIR ${_EXAMPLE_PLUGIN_DIRS})
    if (IS_DIRECTORY "${_DIR}")
      file (MAKE_DIRECTORY "${_DIR}/bin")
    endif ()
  endforeach ()

  add_custom_target (deploy_texmacs_to_build ALL
    COMMAND ${CMAKE_COMMAND} -E copy_directory
      "${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs"
      "${CMAKE_CURRENT_BINARY_DIR}/TeXmacs"
    COMMENT "Synchronizing TeXmacs directory into ${CMAKE_CURRENT_BINARY_DIR}/TeXmacs"
  )
endif ()

# Add local prefix path if available
if (DEFINED ENV{WORKING_DIR_WIN})
  list (APPEND CMAKE_PREFIX_PATH "$ENV{WORKING_DIR_WIN}/local")
  link_directories ("$ENV{WORKING_DIR_WIN}/local/lib")
elseif (DEFINED ENV{WORKING_DIR})
  list (APPEND CMAKE_PREFIX_PATH "$ENV{WORKING_DIR}/local")
  link_directories ("$ENV{WORKING_DIR}/local/lib")
endif ()
if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../../local")
  get_filename_component (_LOCAL_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../local" ABSOLUTE)
  list (APPEND CMAKE_PREFIX_PATH "${_LOCAL_DIR}")
  link_directories ("${_LOCAL_DIR}/lib")
endif ()

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

### --------------------------------------------------------------------
### Include standard modules
### --------------------------------------------------------------------

include (CheckFunctionExists)
include (CheckLibraryExists)
include (CheckIncludeFile)
include (CheckTypeSize)
include (FindPkgConfig)

### --------------------------------------------------------------------
### OS & Platform Settings
### --------------------------------------------------------------------

check_type_size ("void*" SIZEOF_VOID_P)
check_type_size ("int" SIZEOF_INT)
check_type_size ("long" SIZEOF_LONG)
check_type_size ("long long" SIZEOF_LONG_LONG)
check_type_size ("short" SIZEOF_SHORT)
set (ALIGNOF_VOID_P ${SIZEOF_VOID_P})

if (${SIZEOF_VOID_P} EQUAL 8)
  set (WORD_LENGTH "8")
  set (WORD_LENGTH_INC "7")
  set (WORD_MASK "0xfffffffffffffff8")
  set (MAX_FAST "264")
else ()
  set (WORD_LENGTH "4")
  set (WORD_LENGTH_INC "3")
  set (WORD_MASK "0xfffffffc")
  set (MAX_FAST "260")
endif ()

if (WIN32)
  set (CONFIG_OS "MINGW")
  set (OS_MINGW 1)
  if (${SIZEOF_VOID_P} EQUAL 8)
    set (OS_MINGW64 1)
    set (CONFIG_OS_COMPAT "Windows64")
  else ()
    set (CONFIG_OS_COMPAT "Windows")
  endif ()
elseif (APPLE)
  set (CONFIG_OS "MACOS")
  set (OS_MACOS 1)
  set (MACOSX_EXTENSIONS 1)
  set (CONFIG_OS_COMPAT "Unix")
else ()
  set (CONFIG_OS "GNU_LINUX")
  set (OS_GNU_LINUX 1)
  set (CONFIG_OS_COMPAT "Unix")
endif ()

check_type_size (FILE HAVE_FILE)
check_type_size (intptr_t HAVE_INTPTR_T)
check_type_size (time_t HAVE_TIME_T)
check_include_file (inttypes.h HAVE_INTTYPES_H)
check_include_file (memory.h HAVE_MEMORY_H)
check_include_file (stdlib.h HAVE_STDLIB_H)
check_include_file (strings.h HAVE_STRINGS_H)
check_include_file (string.h HAVE_STRING_H)
check_include_file (sys/stat.h HAVE_SYS_STAT_H)
check_include_file (sys/types.h HAVE_SYS_TYPES_H)
check_include_file (unistd.h HAVE_UNISTD_H)
check_include_file (pty.h HAVE_PTY_H)
check_include_file (util.h HAVE_UTIL_H)
check_function_exists (gettimeofday HAVE_GETTIMEOFDAY)
check_function_exists (snprintf HAVE_SNPRINTF)

set (STDC_HEADERS 1)
set (DEBUG_ASSERT 1)

### --------------------------------------------------------------------
### Guile integration (embedded tm-guile188)
### --------------------------------------------------------------------

set (SCHEME_IMPL "embedded18" CACHE STRING "Implementation of Scheme to compile for.")

if (SCHEME_IMPL STREQUAL "embedded18" OR SCHEME_IMPL STREQUAL "tm-guile188")
  message (STATUS "Using embedded Guile from tm-guile188")
  add_subdirectory (tm-guile188)
  set (GUILE_C 1)
  set (GUILE_NUM 1)
  set (GUILE_VERSION "1.8")
  set (guile_str_size_t "size_t")
  set (Guile_LIBRARIES guile)
  set (Guile_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/tm-guile188
    ${CMAKE_CURRENT_SOURCE_DIR}/tm-guile188/libguile
    ${CMAKE_CURRENT_SOURCE_DIR}/tm-guile188/srfi
    ${CMAKE_CURRENT_BINARY_DIR}/tm-guile188
    ${CMAKE_CURRENT_BINARY_DIR}/tm-guile188/libguile
    ${GUILE_BUILD_DIR}/include
    ${GUILE_BUILD_DIR}/include/libguile
  )
else ()
  pkg_search_module (Guile REQUIRED guile-1.8 guile-3.0 guile-2.2 guile-2.0)
  if (Guile_VERSION VERSION_LESS 2.0)
    set (GUILE_C 1)
  else ()
    set (GUILE_D 1)
  endif ()
  set (GUILE_NUM 1)
  set (GUILE_VERSION "${Guile_VERSION}")
  set (guile_str_size_t "size_t")
endif ()

### --------------------------------------------------------------------
### GUI & Qt Selection
### --------------------------------------------------------------------

set (TEXMACS_GUI "Qt" CACHE STRING "TeXmacs Gui (Qt, Qt6, Qt5, Qt4, Aqua, X11)")
option (QTPIPES "use Qt pipes" ON)

if (TEXMACS_GUI MATCHES "Qt.*")
  set (QT_NO_ENTRYPOINT TRUE)
  if (TEXMACS_GUI STREQUAL "Qt4")
    find_package (Qt4 REQUIRED)
    set (AC_QT_MAJOR_VERSION 4)
    set (QT_LIBRARIES ${QT_LIBRARIES})
    include (${QT_USE_FILE})
  elseif (TEXMACS_GUI STREQUAL "Qt5")
    find_package (Qt5 COMPONENTS Core Gui Widgets PrintSupport Svg Network REQUIRED)
    set (AC_QT_MAJOR_VERSION 5)
    set (QT_LIBRARIES Qt5::Core Qt5::Gui Qt5::Widgets Qt5::PrintSupport Qt5::Svg Qt5::Network)
  else ()
    # Try finding Qt6 first, otherwise fall back to Qt5
    find_package (Qt6 COMPONENTS Core Gui Widgets PrintSupport Svg Network QUIET)
    if (Qt6_FOUND)
      message (STATUS "Found Qt6: ${Qt6_VERSION}")
      set (AC_QT_MAJOR_VERSION 6)
      set (QT_LIBRARIES Qt6::Core Qt6::Gui Qt6::Widgets Qt6::PrintSupport Qt6::Svg Qt6::Network)
    else ()
      find_package (Qt5 COMPONENTS Core Gui Widgets PrintSupport Svg Network REQUIRED)
      set (AC_QT_MAJOR_VERSION 5)
      set (QT_LIBRARIES Qt5::Core Qt5::Gui Qt5::Widgets Qt5::PrintSupport Qt5::Svg Qt5::Network)
    endif ()
  endif ()

  set (QTTEXMACS 1)
  set (USE_QTSVG 1)
  set (CONFIG_GUI "QT")
  set (CONFIG_QT "Qt")
  set (QT_PLUGIN_DIR "Qt")
endif ()

### --------------------------------------------------------------------
### External Libraries
### --------------------------------------------------------------------

# Resvg (SVG rendering)
option (USE_RESVG "Use resvg library for SVG rendering" ON)
if (USE_RESVG)
  find_path (RESVG_INCLUDE_DIR resvg.h
    PATHS
      "$ENV{WORKING_DIR_WIN}/local/include"
      "${_LOCAL_DIR}/include"
      "${WORKING_DIR}/local/include"
  )
  find_library (RESVG_LIBRARY NAMES resvg libresvg
    PATHS
      "$ENV{WORKING_DIR_WIN}/local/lib"
      "${_LOCAL_DIR}/lib"
      "${WORKING_DIR}/local/lib"
  )
  if (RESVG_INCLUDE_DIR AND RESVG_LIBRARY)
    set (USE_RESVG 1)
    if (WIN32)
      set (RESVG_EXTRA_LIBS ws2_32 userenv bcrypt ntdll)
    elseif (APPLE)
      set (RESVG_EXTRA_LIBS "-framework Security" "-framework CoreFoundation")
    else ()
      set (RESVG_EXTRA_LIBS pthread dl m)
    endif ()
    set (RESVG_LIBRARIES ${RESVG_LIBRARY} ${RESVG_EXTRA_LIBS})
    message (STATUS "Found resvg: ${RESVG_LIBRARY}")
  else ()
    set (USE_RESVG 0)
    message (STATUS "resvg not found, disabling USE_RESVG")
  endif ()
endif ()

# Aspell
option (USE_ASPELL "Use aspell library" ON)
if (USE_ASPELL)
  find_path (ASPELL_INCLUDE_DIR aspell.h
    PATHS
      "$ENV{WORKING_DIR_WIN}/local/include"
      "${_LOCAL_DIR}/include"
      "${WORKING_DIR}/local/include"
  )
  find_library (ASPELL_LIBRARY NAMES aspell libaspell
    PATHS
      "$ENV{WORKING_DIR_WIN}/local/lib"
      "${_LOCAL_DIR}/lib"
      "${WORKING_DIR}/local/lib"
  )
  find_library (INTL_LIBRARY NAMES intl libintl)
  if (ASPELL_INCLUDE_DIR AND ASPELL_LIBRARY)
    set (USE_ASPELL 1)
    set (USE_INTL 1)
    set (ASPELL_LIBRARIES ${ASPELL_LIBRARY})
    if (INTL_LIBRARY)
      list (APPEND ASPELL_LIBRARIES ${INTL_LIBRARY})
    endif ()
    message (STATUS "Found aspell: ${ASPELL_LIBRARY}")
  else ()
    set (USE_ASPELL 0)
  endif ()
endif ()

# Freetype
option (USE_FREETYPE "use Freetype" ON)
if (USE_FREETYPE)
  find_package (Freetype REQUIRED)
  set (USE_FREETYPE 3)
  set (LINKED_FREETYPE 1)
endif ()

# PNG, ZLIB, Iconv
find_package (PNG REQUIRED)

find_package (ZLIB REQUIRED)

find_package (Iconv)
if (Iconv_FOUND)
  set (USE_ICONV 1)
endif ()

# GnuTLS
option (USE_GNUTLS "use GnuTLS" ON)
if (USE_GNUTLS)
  pkg_check_modules (GNUTLS QUIET gnutls hogweed nettle)
  if (NOT GNUTLS_FOUND)
    find_package (GnuTLS REQUIRED)
  endif ()
  set (USE_GNUTLS 1)
  set (LINKED_GNUTLS 1)
  set (TEXMACS_FIX_1_GNUTLS 1)
  if (WIN32)
    set (GNUTLS_EXTRA_LIBS dbghelp Secur32 shell32 ole32 advapi32 secur32 pthread)
  endif ()
endif ()

# SQLite3
option (USE_SQLITE3 "Use SQLite3" ON)
if (USE_SQLITE3)
  find_package (SQLite3)
  if (SQLITE3_FOUND OR SQLite3_FOUND)
    set (LINKED_SQLITE3 1)
  endif ()
endif ()

# Ghostscript
set (USE_GS 1)
if (WIN32)
  set (GS_EXE "bin/gs.exe")
  set (GS_FONTS "share/ghostscript/fonts")
  set (GS_LIB "share/ghostscript/lib")
else ()
  set (GS_EXE "/usr/bin/gs")
  set (GS_FONTS "share/ghostscript/fonts")
  set (GS_LIB "share/ghostscript/lib")
endif ()

# PDF Hummus native renderer
set (PDF_RENDERER 1)
set (PDFHUMMUS_NO_TIFF 1)
set (PDFHUMMUS_NO_DCT 1)

### --------------------------------------------------------------------
### Generate configure headers
### --------------------------------------------------------------------

set (CONFIG_STD_SETENV "#define STD_SETENV")
set (tm_orig "${CMAKE_CURRENT_SOURCE_DIR}")

if (WIN32)
  set (CONFIG_HOST_OS "mingw32")
  set (CONFIG_HOST_VENDOR "w64")
else ()
  set (CONFIG_HOST_OS "${CMAKE_SYSTEM_NAME}")
  set (CONFIG_HOST_VENDOR "pc")
endif ()

set (CONFIG_HOST_CPU "${CMAKE_SYSTEM_PROCESSOR}")

if (DEFINED ENV{USER})
  set (CONFIG_USER "$ENV{USER}")
elseif (DEFINED ENV{USERNAME})
  set (CONFIG_USER "$ENV{USERNAME}")
else ()
  set (CONFIG_USER "user")
endif ()

string (TIMESTAMP CONFIG_DATE)
set (tm_devel "TeXmacs-${TEXMACS_VERSION}")
set (tm_stable "TeXmacs-${TEXMACS_VERSION}")
set (tm_devel_release "${tm_devel}-1")
set (tm_stable_release "${tm_stable}-1")

configure_file (${CMAKE_CURRENT_SOURCE_DIR}/src/System/config.h.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/src/System/config.h)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/src/System/tm_configure.hpp.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/src/System/tm_configure.hpp)

### --------------------------------------------------------------------
### Source Files
### --------------------------------------------------------------------

file (GLOB_RECURSE TeXmacs_Data_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Data/*.cpp")
file (GLOB_RECURSE TeXmacs_Edit_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Edit/*.cpp")
file (GLOB_RECURSE TeXmacs_Graphics_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/*.cpp")
file (GLOB_RECURSE TeXmacs_Kernel_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Kernel/*.cpp")
file (GLOB_RECURSE TeXmacs_Scheme_SRCS
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Scheme/Scheme/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Scheme/Guile/*.cpp"
)
option (ENABLE_EXPERIMENTAL "Enable experimental style rewriting code" OFF)
if (ENABLE_EXPERIMENTAL)
  file (GLOB_RECURSE TeXmacs_Style_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Style/*.cpp")
  add_compile_definitions (EXPERIMENTAL=1)
else ()
  set (TeXmacs_Style_SRCS "")
endif ()
file (GLOB_RECURSE TeXmacs_System_Base_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/System/*.cpp")
set (TeXmacs_System_C_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/System/Misc/fakedlopen.c")
file (GLOB_RECURSE TeXmacs_Typeset_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Typeset/*.cpp")
file (GLOB_RECURSE TeXmacs_Texmacs_SRCS
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Texmacs/Data/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Texmacs/Server/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Texmacs/Window/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Texmacs/Texmacs/texmacs.cpp"
)

# Standard plugins
file (GLOB_RECURSE TeXmacs_Std_Plugins_SRCS
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Bibtex/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Database/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Freetype/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Gnutls/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Pdf/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Ghostscript/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Ispell/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Metafont/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/LaTeX_Preview/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Openssl/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Updater/*.cpp"
)
file (GLOB TeXmacs_Pdf_C_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Pdf/LibAesgm/*.c")

if (USE_RESVG)
  list (APPEND TeXmacs_Std_Plugins_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Resvg/resvg.cpp")
endif ()
if (USE_SQLITE3)
  file (GLOB_RECURSE TeXmacs_Sqlite_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Sqlite3/*.cpp")
  list (APPEND TeXmacs_Std_Plugins_SRCS ${TeXmacs_Sqlite_SRCS})
endif ()

# GUI Sources
file (GLOB TeXmacs_Qt_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Qt/*.cpp")
file (GLOB TeXmacs_Qt_HDRS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Qt/*.hpp")

# System OS Sources
if (WIN32)
  if (CMAKE_SIZEOF_VOID_P EQUAL 8)
    file (GLOB TeXmacs_OS_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Windows64/*.cpp")
    list (REMOVE_ITEM TeXmacs_OS_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Windows64/windows64_entrypoint.cpp")
  else ()
    file (GLOB TeXmacs_OS_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Windows/*.cpp")
    list (REMOVE_ITEM TeXmacs_OS_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Windows/windows32_entrypoint.cpp")
  endif ()
elseif (APPLE)
  # Apple/Cocoa
else ()
  file (GLOB TeXmacs_OS_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Unix/*.cpp")
  list (REMOVE_ITEM TeXmacs_OS_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Unix/unix_entrypoint.cpp")
endif ()

set (TeXmacs_All_SRCS
  ${TeXmacs_Data_SRCS}
  ${TeXmacs_Edit_SRCS}
  ${TeXmacs_Graphics_SRCS}
  ${TeXmacs_Kernel_SRCS}
  ${TeXmacs_Scheme_SRCS}
  ${TeXmacs_Style_SRCS}
  ${TeXmacs_System_Base_SRCS}
  ${TeXmacs_System_C_SRCS}
  ${TeXmacs_Typeset_SRCS}
  ${TeXmacs_Texmacs_SRCS}
  ${TeXmacs_Std_Plugins_SRCS}
  ${TeXmacs_Pdf_C_SRCS}
  ${TeXmacs_Qt_SRCS}
  ${TeXmacs_Qt_HDRS}
  ${TeXmacs_OS_SRCS}
)

### --------------------------------------------------------------------
### Include Directories
### --------------------------------------------------------------------

set (TeXmacs_Include_Dirs
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Data/Convert
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Data/Document
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Data/Drd
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Data/History
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Data/Observers
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Data/Parser
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Data/String
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Data/Tree
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Edit
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Edit/Editor
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Edit/Interface
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Edit/Modify
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Edit/Process
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Edit/Replace
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Bitmap_fonts
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Colors
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Fonts
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Gui
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Handwriting
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Mathematics
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Pictures
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Renderer
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Spacial
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Graphics/Types
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Kernel/Abstractions
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Kernel/Containers
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Kernel/Types
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Pdf/LibAesgm
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Gnutls
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Resvg
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Qt
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Scheme
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Scheme/Guile
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Scheme/Scheme
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Scheme/Glue
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Style
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Style/Environment
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Style/Evaluate
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Style/Memorizer
  ${CMAKE_CURRENT_SOURCE_DIR}/src/System
  ${CMAKE_CURRENT_SOURCE_DIR}/src/System/Boot
  ${CMAKE_CURRENT_SOURCE_DIR}/src/System/Classes
  ${CMAKE_CURRENT_SOURCE_DIR}/src/System/Files
  ${CMAKE_CURRENT_SOURCE_DIR}/src/System/Language
  ${CMAKE_CURRENT_SOURCE_DIR}/src/System/Link
  ${CMAKE_CURRENT_SOURCE_DIR}/src/System/Misc
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Texmacs
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Texmacs/Data
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Texmacs/Server
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Texmacs/Window
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Typeset
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Typeset/Bridge
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Typeset/Concat
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Typeset/Page
  ${CMAKE_CURRENT_SOURCE_DIR}/TeXmacs/include
  ${CMAKE_CURRENT_BINARY_DIR}/src/System
  ${Guile_INCLUDE_DIRS}
  ${FREETYPE_INCLUDE_DIRS}
  ${PNG_INCLUDE_DIRS}
  ${GNUTLS_INCLUDE_DIRS}
)

if (WIN32)
  if (CMAKE_SIZEOF_VOID_P EQUAL 8)
    list (APPEND TeXmacs_Include_Dirs ${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Windows64)
  else ()
    list (APPEND TeXmacs_Include_Dirs ${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Windows ${CMAKE_CURRENT_SOURCE_DIR}/src/Plugins/Windows/nowide)
  endif ()
  list (APPEND TeXmacs_Include_Dirs ${CMAKE_CURRENT_SOURCE_DIR}/packages/windows)
endif ()

if (RESVG_INCLUDE_DIR)
  list (APPEND TeXmacs_Include_Dirs ${RESVG_INCLUDE_DIR})
endif ()
if (ASPELL_INCLUDE_DIR)
  list (APPEND TeXmacs_Include_Dirs ${ASPELL_INCLUDE_DIR})
endif ()
if (SQLITE3_INCLUDE_DIRS)
  list (APPEND TeXmacs_Include_Dirs ${SQLITE3_INCLUDE_DIRS})
elseif (SQLite3_INCLUDE_DIRS)
  list (APPEND TeXmacs_Include_Dirs ${SQLite3_INCLUDE_DIRS})
endif ()

### --------------------------------------------------------------------
### Libraries List
### --------------------------------------------------------------------

set (TeXmacs_Libraries
  ${Guile_LIBRARIES}
  ${QT_LIBRARIES}
  ${ZLIB_LIBRARIES}
  ${FREETYPE_LIBRARIES}
  ${PNG_LIBRARIES}
  ${GNUTLS_LIBRARIES}
  ${GNUTLS_EXTRA_LIBS}
)

if (USE_RESVG)
  list (APPEND TeXmacs_Libraries ${RESVG_LIBRARIES})
endif ()
if (USE_ASPELL)
  list (APPEND TeXmacs_Libraries ${ASPELL_LIBRARIES})
endif ()
if (USE_ICONV AND Iconv_LIBRARIES)
  list (APPEND TeXmacs_Libraries ${Iconv_LIBRARIES})
endif ()
if (USE_SQLITE3)
  if (SQLITE3_LIBRARIES)
    list (APPEND TeXmacs_Libraries ${SQLITE3_LIBRARIES})
  elseif (SQLite3_LIBRARIES)
    list (APPEND TeXmacs_Libraries ${SQLite3_LIBRARIES})
  endif ()
endif ()
if (WIN32)
  list (APPEND TeXmacs_Libraries
    dbghelp Secur32 shell32 ole32 advapi32 secur32 dwmapi ws2_32 userenv bcrypt ntdll pthread
  )
endif ()

### --------------------------------------------------------------------
### Subdirectories
### --------------------------------------------------------------------

add_subdirectory (src)
add_subdirectory (plugins)
add_subdirectory (misc)
add_subdirectory (TeXmacs)

### --------------------------------------------------------------------
### Installation
### --------------------------------------------------------------------

if (WIN32)
  install (DIRECTORY plugins DESTINATION ${CMAKE_INSTALL_PREFIX}
    FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ
    PATTERN "bin" EXCLUDE
    PATTERN "CMakeLists.txt" EXCLUDE
    PATTERN ".gitignore" EXCLUDE)
else (WIN32)
  install (DIRECTORY plugins DESTINATION share/TeXmacs
    FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ
    PATTERN "bin" EXCLUDE
    PATTERN "CMakeLists.txt" EXCLUDE
    PATTERN ".gitignore" EXCLUDE)
endif (WIN32)

install (FILES TeXmacs/misc/mime/texmacs.desktop DESTINATION share/applications)
install (FILES TeXmacs/misc/images/texmacs.svg RENAME TeXmacs.svg
  DESTINATION share/icons/hicolor/scalable/apps)
install (FILES TeXmacs/misc/images/text-x-texmacs.svg
  DESTINATION share/icons/hicolor/scalable/mimetypes)
install (FILES TeXmacs/misc/mime/texmacs.xml DESTINATION share/mime/packages)
install (FILES TeXmacs/misc/pixmaps/TeXmacs.xpm DESTINATION share/pixmaps)
foreach (size IN ITEMS 16 22 24 32 48 64 128 256 512)
  install (FILES TeXmacs/misc/images/texmacs-${size}.png RENAME TeXmacs.png
    DESTINATION share/icons/hicolor/${size}x${size}/apps/)
endforeach ()
