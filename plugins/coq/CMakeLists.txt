###############################################################################
# MODULE     : CMake file for Coq plugin
# COPYRIGHT  : (C) 2018-2019  Darcy Shen
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

include_directories (${TEXMACS_BINARY_DIR}/src/System/)

set (coq_SRCS src/pico_xml.c src/tm_coq.c)

add_executable (tm_coq ${coq_SRCS})

# target_link_libraries(tm_r -lutil)

install(TARGETS tm_coq DESTINATION share/TeXmacs/plugins/coq/bin)

