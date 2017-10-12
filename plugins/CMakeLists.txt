add_subdirectory(axiom)
add_subdirectory(graphviz)
add_subdirectory(maple)
add_subdirectory(r)
add_subdirectory(shell)

### lib/
file(GLOB PLUGIN_DIRS *)
foreach(PLUGIN ${PLUGIN_DIRS})
  if (EXISTS ${PLUGIN}/bin/)
    install(DIRECTORY ${PLUGIN}/bin/ DESTINATION lib/texmacs/TeXmacs/bin
      PATTERN "*"
      PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
  endif(EXISTS ${PLUGIN}/bin/)
endforeach(PLUGIN)

