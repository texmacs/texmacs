configure_file(scripts/texmacs.in ${TEXMACS_BINARY_DIR}/misc/script/texmacs @ONLY)
configure_file(scripts/fig2ps.in ${TEXMACS_BINARY_DIR}/misc/scripts/fig2ps @ONLY)
configure_file(man/texmacs.1.in ${TEXMACS_SOURCE_DIR}/misc/man/texmacs.1 @ONLY)

### share/man
set(TOOLS "texmacs" "fig2ps")
set(MAN_SECTION "1")
foreach(TOOL ${TOOLS})
  set(TOOL_MANFILE ${CMAKE_CURRENT_SOURCE_DIR}/man/${TOOL}.${MAN_SECTION})
  set(TOOL_MANFILE_GZ ${CMAKE_CURRENT_BINARY_DIR}/${TOOL}.${MAN_SECTION}.gz)
  add_custom_command(
    OUTPUT ${TOOL_MANFILE_GZ}
    COMMAND gzip -c ${TOOL_MANFILE} > ${TOOL_MANFILE_GZ}
    MAIN_DEPENDENCY ${TOOL_MANFILE}
    COMMENT "Building ${TOOL} man page"
    VERBATIM
  )
  add_custom_target(${TOOL}.${MAN_SECTION} ALL DEPENDS ${TOOL_MANFILE_GZ})
  install(FILES ${TOOL_MANFILE_GZ} DESTINATION share/man/man${MAN_SECTION})
endforeach(TOOL)
