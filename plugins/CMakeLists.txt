###############################################################################
# MODULE     : CMake file for various plugins
# COPYRIGHT  : (C) 2018-2019  Darcy Shen
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

add_subdirectory(axiom)
add_subdirectory(coq)
add_subdirectory(maple)
add_subdirectory(r)
add_subdirectory(shell)

### lib/
if (APPLE)
else (APPLE)
  file(GLOB PLUGIN_DIRS *)
  foreach(PLUGIN ${PLUGIN_DIRS})
    if (EXISTS ${PLUGIN}/bin/)
      install(DIRECTORY ${PLUGIN}/bin/ DESTINATION lib/texmacs/TeXmacs/bin
        PATTERN "*"
        PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
    endif(EXISTS ${PLUGIN}/bin/)
  endforeach(PLUGIN)
endif (APPLE)
