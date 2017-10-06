set (maple_SRCS src/tm_maple_5.cpp)

add_executable (tm_maple_5 ${maple_SRCS})

install(TARGETS tm_maple_5 DESTINATION share/TeXmacs/plugins/maple/bin)
