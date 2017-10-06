include_directories (${TEXMACS_BINARY_DIR}/src/System/)

set (r_SRCS src/tm_r.c)

add_executable (tm_r ${r_SRCS})

target_link_libraries(tm_r -lutil)

install(TARGETS tm_r DESTINATION share/TeXmacs/plugins/r/bin)
