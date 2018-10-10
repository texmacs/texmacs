
add_library(texmacs_body STATIC ${TeXmacs_All_SRCS})

add_executable (${TeXmacs_binary_name}
  ./Texmacs/Texmacs/texmacs.cpp
)

target_link_libraries (${TeXmacs_binary_name}
  texmacs_body
  ${TeXmacs_Libraries}
)

#--------------------------------------------------------------------------------
#-- Set the Debug and Release names for the libraries
SET_TARGET_PROPERTIES(${TeXmacs_binary_name} 
    PROPERTIES
    DEBUG_OUTPUT_NAME ${TeXmacs_binary_name}${EXE_DEBUG_EXTENSION}
    RELEASE_OUTPUT_NAME ${TeXmacs_binary_name}
    RUNTIME_OUTPUT_DIRECTORY ${TEXMACS_BINARY_DIR}
)

install(TARGETS ${TeXmacs_binary_name} DESTINATION lib/texmacs/TeXmacs/bin)
