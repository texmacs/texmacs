# Configure WebAssembly build settings
# taken from https://github.com/forderud/QtWasm

# For some reason, the build settings need to be provided through the linker.
# Most flags are configured to match Qt qmake
# See EMCC_COMMON_LFLAGS in https://github.com/qt/qtbase/blob/dev/mkspecs/wasm-emscripten/qmake.conf

# Activate Embind C/C++ bindings
# https://emscripten.org/docs/porting/connecting_cpp_and_javascript/embind.html
add_link_options(--bind) 

# Activate WebGL 2 (in addition to WebGL 1)
# https://emscripten.org/docs/porting/multimedia_and_graphics/OpenGL-support.html#webgl-friendly-subset-of-opengl-es-2-0-3-0
add_link_options("SHELL:-s USE_WEBGL2=1")
add_link_options(
  "$<$<CONFIG:DEBUG>:SHELL:-s GL_DEBUG=1>"
)

# Emulate missing OpenGL ES2/ES3 features
# https://emscripten.org/docs/porting/multimedia_and_graphics/OpenGL-support.html#opengl-es-2-0-3-0-emulation
add_link_options("SHELL:-s FULL_ES2=1")
#add_link_options("SHELL:-s FULL_ES3=1")

# Enable demangling of C++ stack traces
# https://emscripten.org/docs/porting/Debugging.html
add_link_options("SHELL:-s DEMANGLE_SUPPORT=1")

# Run static dtors at teardown
# https://emscripten.org/docs/getting_started/FAQ.html#what-does-exiting-the-runtime-mean-why-don-t-atexit-s-run
add_link_options("SHELL:-s EXIT_RUNTIME=1")

# Allows amount of memory used to change 
# https://emscripten.org/docs/optimizing/Optimizing-Code.html#memory-growth
add_link_options("SHELL:-s ALLOW_MEMORY_GROWTH=1")
#add_link_options("SHELL:-s MAXIMUM_MEMORY=1GB") # required when combining USE_PTHREADS with ALLOW_MEMORY_GROWTH

# Enable C++ exception catching
# https://emscripten.org/docs/optimizing/Optimizing-Code.html#c-exceptions
add_compile_options(-fexceptions)
add_link_options("SHELL:-s DISABLE_EXCEPTION_CATCHING=0")

# Export UTF16ToString,stringToUTF16
# Required by https://codereview.qt-project.org/c/qt/qtbase/+/286997 (since Qt 5.14)
add_link_options("SHELL:-s EXTRA_EXPORTED_RUNTIME_METHODS=[UTF16ToString,stringToUTF16]")

# Enable Fetch API
# https://emscripten.org/docs/api_reference/fetch.html
add_link_options("SHELL:-s FETCH=1")

# Generate HTML file for each executable
#SET(CMAKE_EXECUTABLE_SUFFIX ".html")

# Also search for packages beneath filesystem root (in addition to /emsdk_portable/sdk/system)
list(APPEND CMAKE_FIND_ROOT_PATH "/")

# Link to missing Qt libraries.
# Temporary solution until Qt ship with proper CMake support for WebAssembly.
function(link_qt_static target)
    # copy in Qt HTML/JS launch files
    set(APPNAME ${target})
    configure_file("${_qt5Core_install_prefix}/plugins/platforms/wasm_shell.html"
                   "${target}.html")
    configure_file("${_qt5Core_install_prefix}/plugins/platforms/qtloader.js"
                   qtloader.js COPYONLY)
    configure_file("${_qt5Core_install_prefix}/plugins/platforms/qtlogo.svg"
                   qtlogo.svg COPYONLY)
endfunction()
