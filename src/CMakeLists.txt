
add_library(texmacs_body STATIC ${TeXmacs_All_SRCS})

add_executable (${TeXmacs_binary_name}
  ./Texmacs/Texmacs/texmacs.cpp
)

target_link_libraries (${TeXmacs_binary_name}
  texmacs_body
  ${TeXmacs_Libraries}
)

if (APPLE)
  set(MACOSX_BUNDLE_DISPLAY_NAME TeXmacs)
  set_target_properties (${TeXmacs_binary_name}
    PROPERTIES
    MACOSX_BUNDLE TRUE
    MACOSX_BUNDLE_INFO_PLIST ${TEXMACS_SOURCE_DIR}/packages/macos/Info.plist.in
    DEBUG_OUTPUT_NAME ${TeXmacs_binary_name}${EXE_DEBUG_EXTENSION}
    RELEASE_OUTPUT_NAME ${TeXmacs_binary_name}
    RUNTIME_OUTPUT_DIRECTORY ${TEXMACS_BINARY_DIR}
  )
else (APPLE)
  set_target_properties (${TeXmacs_binary_name}
    PROPERTIES
    DEBUG_OUTPUT_NAME ${TeXmacs_binary_name}${EXE_DEBUG_EXTENSION}
    RELEASE_OUTPUT_NAME ${TeXmacs_binary_name}
    RUNTIME_OUTPUT_DIRECTORY ${TEXMACS_BINARY_DIR}
  )
  install (TARGETS ${TeXmacs_binary_name} DESTINATION lib/texmacs/TeXmacs/bin)
endif (APPLE)

