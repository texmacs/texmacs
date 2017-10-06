set (axiom_SRCS src/tm_axiom.c)

add_executable (tm_axiom ${axiom_SRCS})

install(TARGETS tm_axiom DESTINATION share/TeXmacs/plugins/axiom/bin)
