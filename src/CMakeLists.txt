### --------------------------------------------------------------------
### TeXmacs source build
### --------------------------------------------------------------------

# Shared body of TeXmacs object files
add_library (texmacs_body OBJECT ${TeXmacs_All_SRCS})

target_include_directories (texmacs_body PUBLIC
  ${TeXmacs_Include_Dirs}
)

target_compile_options (texmacs_body PRIVATE
  $<$<COMPILE_LANGUAGE:C,CXX>:-include>
  $<$<COMPILE_LANGUAGE:C,CXX>:${CMAKE_BINARY_DIR}/src/System/config.h>
)

target_link_libraries (texmacs_body PUBLIC
  ${TeXmacs_Libraries}
)

set_target_properties (texmacs_body PROPERTIES AUTOMOC ON)

if (WIN32)
  set_source_files_properties (
    "${TEXMACS_SOURCE_DIR}/packages/windows/resource.rc"
    PROPERTIES COMPILE_FLAGS "-I\"${TEXMACS_SOURCE_DIR}/packages/windows\""
  )

  if (CMAKE_SIZEOF_VOID_P EQUAL 8)
    set (TEXMACS_ENTRYPOINT_SRC "${TEXMACS_SOURCE_DIR}/src/Plugins/Windows64/windows64_entrypoint.cpp")
  else ()
    set (TEXMACS_ENTRYPOINT_SRC "${TEXMACS_SOURCE_DIR}/src/Plugins/Windows/windows32_entrypoint.cpp")
  endif ()

  set (TEXMACS_RC_SRC "${TEXMACS_SOURCE_DIR}/packages/windows/resource.rc")

  # texmacs.exe
  add_executable (texmacs WIN32
    $<TARGET_OBJECTS:texmacs_body>
    ${TEXMACS_ENTRYPOINT_SRC}
    ${TEXMACS_RC_SRC}
  )

  target_include_directories (texmacs PRIVATE ${TeXmacs_Include_Dirs})
  target_compile_options (texmacs PRIVATE
    $<$<COMPILE_LANGUAGE:C,CXX>:-include>
    $<$<COMPILE_LANGUAGE:C,CXX>:${CMAKE_BINARY_DIR}/src/System/config.h>
    -UQT_NEEDS_QMAIN
  )
  target_link_libraries (texmacs PRIVATE ${TeXmacs_Libraries})
  target_link_options (texmacs PRIVATE "-Wl,--stack,16777216")

  # texmacs-open.exe
  add_executable (texmacs-open WIN32
    $<TARGET_OBJECTS:texmacs_body>
    "${TEXMACS_SOURCE_DIR}/src/Launcher/texmacs_open_main.cpp"
    ${TEXMACS_RC_SRC}
  )

  target_include_directories (texmacs-open PRIVATE ${TeXmacs_Include_Dirs})
  target_compile_options (texmacs-open PRIVATE
    $<$<COMPILE_LANGUAGE:C,CXX>:-include>
    $<$<COMPILE_LANGUAGE:C,CXX>:${CMAKE_BINARY_DIR}/src/System/config.h>
    -UQT_NEEDS_QMAIN
  )
  target_link_libraries (texmacs-open PRIVATE ${TeXmacs_Libraries})
  target_link_options (texmacs-open PRIVATE "-Wl,--stack,16777216")

  set_target_properties (texmacs texmacs-open PROPERTIES
    QT_NO_ENTRYPOINT TRUE
    RUNTIME_OUTPUT_DIRECTORY "${TEXMACS_TARGET_DIR}/bin"
    PDB_OUTPUT_DIRECTORY "${TEXMACS_TARGET_DIR}/bin"
  )

  if (TARGET deploy_texmacs_to_build)
    add_dependencies (texmacs deploy_texmacs_to_build)
    add_dependencies (texmacs-open deploy_texmacs_to_build)
  endif ()

  if (IS_IN_SOURCE_BUILD)
    add_custom_command (TARGET texmacs POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:texmacs>"
        "${TEXMACS_TARGET_DIR}/bin/texmacs.bin"
      COMMENT "Copying texmacs.exe -> texmacs.bin in TeXmacs/bin"
    )

    add_custom_command (TARGET texmacs-open POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:texmacs-open>"
        "${TEXMACS_TARGET_DIR}/bin/texmacs-open.bin"
      COMMENT "Copying texmacs-open.exe -> texmacs-open.bin in TeXmacs/bin"
    )
  else ()
    add_custom_command (TARGET texmacs POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:texmacs>"
        "${CMAKE_BINARY_DIR}/TeXmacs/bin/texmacs.bin"
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:texmacs>"
        "${CMAKE_BINARY_DIR}/texmacs.exe"
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:texmacs>"
        "${CMAKE_BINARY_DIR}/bin/texmacs.exe"
      COMMENT "Copying texmacs.exe -> texmacs.bin in build/TeXmacs/bin and build/texmacs.exe"
    )

    add_custom_command (TARGET texmacs-open POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:texmacs-open>"
        "${CMAKE_BINARY_DIR}/TeXmacs/bin/texmacs-open.bin"
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:texmacs-open>"
        "${CMAKE_BINARY_DIR}/texmacs-open.exe"
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:texmacs-open>"
        "${CMAKE_BINARY_DIR}/bin/texmacs-open.exe"
      COMMENT "Copying texmacs-open.exe -> texmacs-open.bin in build/TeXmacs/bin and build/texmacs-open.exe"
    )
  endif ()

  install (TARGETS texmacs texmacs-open DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)

elseif (APPLE)
  set (TeXmacs_binary_name "TeXmacs")
  add_executable (${TeXmacs_binary_name} MACOSX_BUNDLE
    $<TARGET_OBJECTS:texmacs_body>
  )
  target_include_directories (${TeXmacs_binary_name} PRIVATE ${TeXmacs_Include_Dirs})
  target_compile_options (${TeXmacs_binary_name} PRIVATE
    $<$<COMPILE_LANGUAGE:C,CXX>:-include>
    $<$<COMPILE_LANGUAGE:C,CXX>:${CMAKE_BINARY_DIR}/src/System/config.h>
  )
  target_link_libraries (${TeXmacs_binary_name} PRIVATE ${TeXmacs_Libraries})
  set_target_properties (${TeXmacs_binary_name} PROPERTIES
    MACOSX_BUNDLE TRUE
    MACOSX_BUNDLE_INFO_PLIST ${TEXMACS_SOURCE_DIR}/packages/macos/Info.plist.in
    DEBUG_OUTPUT_NAME ${TeXmacs_binary_name}${EXE_DEBUG_EXTENSION}
    RELEASE_OUTPUT_NAME ${TeXmacs_binary_name}
    RUNTIME_OUTPUT_DIRECTORY ${TEXMACS_BINARY_DIR}
  )
else ()
  set (TeXmacs_binary_name "texmacs.bin")
  add_executable (${TeXmacs_binary_name}
    $<TARGET_OBJECTS:texmacs_body>
    "${TEXMACS_SOURCE_DIR}/src/Plugins/Unix/unix_entrypoint.cpp"
  )
  target_include_directories (${TeXmacs_binary_name} PRIVATE ${TeXmacs_Include_Dirs})
  target_compile_options (${TeXmacs_binary_name} PRIVATE
    $<$<COMPILE_LANGUAGE:C,CXX>:-include>
    $<$<COMPILE_LANGUAGE:C,CXX>:${CMAKE_BINARY_DIR}/src/System/config.h>
  )
  target_link_libraries (${TeXmacs_binary_name} PRIVATE ${TeXmacs_Libraries})
  set_target_properties (${TeXmacs_binary_name} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${TEXMACS_TARGET_DIR}/bin"
  )
  install (TARGETS ${TeXmacs_binary_name} DESTINATION ${tmbin}/bin)
endif ()
