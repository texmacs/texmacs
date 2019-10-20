set (BENCHMARK_ENABLE_TESTING OFF CACHE BOOL "Suppressing benchmark's tests" FORCE)
add_subdirectory (gbench EXCLUDE_FROM_ALL)
include_directories (gbench/include)

file (GLOB_RECURSE BENCH_SRC_FILES "Kernel/*.cpp")
# from list of files we'll create tests test_name.cpp -> test_name
foreach (_bench_file ${BENCH_SRC_FILES})
  get_filename_component (_bench_name ${_bench_file} NAME_WE)
  add_executable (${_bench_name}
    ${_bench_file}
  )
  target_link_libraries (${_bench_name}
    texmacs_body
    benchmark_main
    ${TeXmacs_Libraries}
  )
endforeach ()
