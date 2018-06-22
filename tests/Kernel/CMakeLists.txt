file(GLOB_RECURSE TEST_SRC_FILES "*.cpp")

# from list of files we'll create tests test_name.cpp -> test_name
foreach (_test_file ${TEST_SRC_FILES})
  get_filename_component (_test_name ${_test_file} NAME_WE)
  add_executable (${_test_name}
    ${TeXmacs_All_SRCS}
    ${_test_file}
  )
  target_link_libraries (${_test_name}
    ${GTEST_LIBRARY}
    ${GTEST_MAIN_LIBRARY}
    ${TeXmacs_Libraries}
  )
  add_test (${_test_name} ${_test_name})
  set_tests_properties (${_test_name} PROPERTIES TIMEOUT 5)
endforeach ()
