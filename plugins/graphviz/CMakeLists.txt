set (graphviz_SRCS src/tm_graphviz.c)

add_executable (tm_graphviz ${graphviz_SRCS})

install(TARGETS tm_graphviz DESTINATION share/TeXmacs/plugins/graphviz/bin)
