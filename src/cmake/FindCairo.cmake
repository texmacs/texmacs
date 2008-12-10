# from http://zi.fi/cmake/Modules/FindCairo.cmake
# modified

# - Try to find Cairo
# Once done, this will define
#
#  Cairo_FOUND - system has Cairo
#  Cairo_INCLUDE_DIRS - the Cairo include directories
#  Cairo_LIBRARIES - link these to use Cairo

include(LibFindMacros)
include(CheckCSourceCompiles)

# Dependencies
libfind_package(Cairo Freetype)

# Use pkg-config to get hints about paths
libfind_pkg_check_modules(Cairo_PKGCONF cairo)

# Include dir
find_path(Cairo_INCLUDE_DIR
  NAMES cairo.h
  PATHS ${Cairo_PKGCONF_INCLUDE_DIRS}
  PATH_SUFFIXES cairo
)

# Finally the library itself
find_library(Cairo_LIBRARY
  NAMES cairo
  PATHS ${Cairo_PKGCONF_LIBRARY_DIRS}
)

# Set the include dir variables and the libraries and let libfind_process do the rest.
# NOTE: Singular variables for this library, plural for libraries this this lib depends on.
set(Cairo_PROCESS_INCLUDES Cairo_INCLUDE_DIR Freetype_INCLUDE_DIRS)
set(Cairo_PROCESS_LIBS Cairo_LIBRARY Freetype_LIBRARIES)
libfind_process(Cairo)

IF (Cairo_FOUND)

#### check if cairo works

SET(CMAKE_REQUIRED_FLAGS ${CAIRO_CFLAGS} ${CAIRO_CFLAGS_OTHERS})
SET(CMAKE_REQUIRED_INCLUDES ${Cairo_INCLUDE_DIRS})
SET(CMAKE_REQUIRED_LIBRARIES ${Cairo_LIBRARIES})
#SET(CMAKE_REQUIRED_LIBDIRS ${CAIRO_LIBRARY_DIRS})


CHECK_C_SOURCE_COMPILES(  "#include <cairo.h>
   void main() {
    cairo_surface_t *surface;
    surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 120, 120);
   }" _cairo_works)

IF(_cairo_works)
  MESSAGE(STATUS "Cairo works")
ELSE(_cairo_works)
  MESSAGE(WARNING "Cairo is not working properly: disabling")  
  SET(Cairo_FOUND NO)
ENDIF(_cairo_works)

ENDIF (Cairo_FOUND)
