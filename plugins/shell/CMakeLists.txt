include_directories (${TEXMACS_BINARY_DIR}/src/System/)

set (shell_SRCS src/tm_shell.cpp)

add_executable (tm_shell ${shell_SRCS})

target_link_libraries(tm_shell -lutil)

install(TARGETS tm_shell DESTINATION share/TeXmacs/plugins/shell/bin)
