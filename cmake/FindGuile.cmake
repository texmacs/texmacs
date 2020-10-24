# - Locate the GNU Guile library
# Once done, this will define
#
#  Guile_FOUND - system has Freetype
#  Guile_INCLUDE_DIRS - the Freetype include directories
#  Guile_LIBRARIES - link these to use Freetype
#  Guile_VERSION_STRING - version of Guile

include (LibFindMacros)

libfind_pkg_check_modules (Guile guile-1.8)

set (Guile_VERSION_STRING 1.8.8)

string (REPLACE "-l" " " _guile_lib_list "${Guile_LIBRARIES}" )
string (REPLACE "-L" " " _guile_libdirs ${Guile_LIBRARY_DIRS} "")

foreach (i ${_guile_lib_list})
  string (STRIP ${i} i)
  if (i)
    if (NOT _guile_flag_library_${i}) # avoid copies
      find_library(_guile_tmp_library_${i}
        NAMES ${i}
        PATHS ${_guile_libdirs}
      )
      message (STATUS ">>>>>>>>>" ${_guile_tmp_library_${i}})
      if (_guile_tmp_library_${i})
        set (_guile_flag_library_${i})
        set (_guile_libraries ${_guile_libraries} ${_guile_tmp_library_${i}})
      endif (_guile_tmp_library_${i})
    endif (NOT _guile_flag_library_${i})
  endif (i)
endforeach (i)

set (Guile_LIBRARIES ${_guile_libraries})