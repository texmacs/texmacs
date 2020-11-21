###############################################################################
# MODULE     : CMake file for various plugins
# COPYRIGHT  : (C) 2018-2020  Darcy Shen
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

if (NOT WIN32)
  add_subdirectory (axiom)
  add_subdirectory (coq)
  add_subdirectory (maple)
  add_subdirectory (r)
  add_subdirectory (shell)
endif (NOT WIN32)


# Install the binaries for plugin
file(GLOB PLUGIN_DIRS LIST_DIRECTORIES true *)
foreach(PLUGIN ${PLUGIN_DIRS})
  if (EXISTS ${PLUGIN}/bin/)
    file(RELATIVE_PATH PLUGIN_NAME ${TEXMACS_SOURCE_DIR}/plugins ${PLUGIN})
    if (WIN32)
      install(DIRECTORY ${PLUGIN}/bin/ DESTINATION plugins/${PLUGIN_NAME}/bin
        PATTERN "*"
        PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
    else (WIN32)
      install(DIRECTORY ${PLUGIN}/bin/ DESTINATION share/TeXmacs/plugins/${PLUGIN_NAME}/bin
        PATTERN "*"
        PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
    endif (WIN32)
  endif(EXISTS ${PLUGIN}/bin/)
endforeach(PLUGIN)
