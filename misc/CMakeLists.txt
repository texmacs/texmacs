set(prefix "${CMAKE_INSTALL_PREFIX}")
set(exec_prefix "${prefix}")
set(datarootdir "${prefix}/share")
set(datadir "${datarootdir}")
set(tmdata "\${datarootdir}/TeXmacs")
set(tmbin "\${exec_prefix}/libexec/TeXmacs")
if (APPLE)
  set(CONFIG_LIB_PATH "DYLD_LIBRARY_PATH")
else ()
  set(CONFIG_LIB_PATH "LD_LIBRARY_PATH")
endif ()

configure_file(scripts/texmacs.in ${CMAKE_CURRENT_BINARY_DIR}/scripts/texmacs @ONLY)
configure_file(scripts/fig2ps.in ${CMAKE_CURRENT_BINARY_DIR}/scripts/fig2ps @ONLY)

if (IS_IN_SOURCE_BUILD)
  set (MAN_TARGET_FILE "${CMAKE_CURRENT_SOURCE_DIR}/man/texmacs.1")
else ()
  set (MAN_TARGET_FILE "${CMAKE_CURRENT_BINARY_DIR}/man/texmacs.1")
endif ()
configure_file(man/texmacs.1.in ${MAN_TARGET_FILE} @ONLY)

# Deploy to TeXmacs/bin layout matching autoconf
file(MAKE_DIRECTORY "${TEXMACS_TARGET_DIR}/bin")
configure_file(scripts/texmacs.in ${TEXMACS_TARGET_DIR}/bin/texmacs @ONLY)
configure_file(scripts/fig2ps.in ${TEXMACS_TARGET_DIR}/bin/fig2ps @ONLY)
if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/scripts/tm_gs")
  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/scripts/tm_gs" DESTINATION "${TEXMACS_TARGET_DIR}/bin")
endif ()
if (EXISTS "${TEXMACS_TARGET_DIR}/bin/texmacs")
  file(CHMOD "${TEXMACS_TARGET_DIR}/bin/texmacs"
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif ()
if (EXISTS "${TEXMACS_TARGET_DIR}/bin/fig2ps")
  file(CHMOD "${TEXMACS_TARGET_DIR}/bin/fig2ps"
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif ()
if (EXISTS "${TEXMACS_TARGET_DIR}/bin/tm_gs")
  file(CHMOD "${TEXMACS_TARGET_DIR}/bin/tm_gs"
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif ()

if (NOT WIN32)
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
endif ()
