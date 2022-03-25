# from https://github.com/xiangxw/mupdf-qt/blob/dev/cmake/FindMuPDF.cmake

# You can use following variables
#   MuPDF_FOUND
#   MuPDF_INCLUDE_DIRS
#   MuPDF_LIBRARIES

# Build type
if (NOT CMAKE_BUILD_TYPE)
    set (MuPDF_BUILD_TYPE "Debug")
elseif (${CMAKE_BUILD_TYPE} STREQUAL "Debug")
    set (MuPDF_BUILD_TYPE "Debug")
elseif (${CMAKE_BUILD_TYPE} STREQUAL "Release")
    set (MuPDF_BUILD_TYPE "Release")
else ()
    message (FATAL_ERROR "CMAKE_BUILD_TYPE not supported: " ${CMAKE_BUILD_TYPE})
endif ()
message (STATUS "Finding MuPDF with build type: " ${MuPDF_BUILD_TYPE} "...")

# Find MuPDF source directory
set (MuPDF_SOURCE_NAME "mupdf")
set (MuPDF_SOURCE_DIR ${CMAKE_SOURCE_DIR}/${MuPDF_SOURCE_NAME})
if (NOT EXISTS ${MuPDF_SOURCE_DIR})
    message (FATAL_ERROR "MuPDF source code not found!")
endif ()

# Find include directory
set (MuPDF_INCLUDE_DIRS ${MuPDF_SOURCE_DIR}/include)

# Find libraries
if (MSVC) # TODO set different path for different build type
    set (MuPDF_LIBRARY_COMPONENTS libmupdf libthirdparty)
    if (${MuPDF_BUILD_TYPE} STREQUAL "Debug")
        set (MuPDF_LIBRARY_PATH ${MuPDF_SOURCE_DIR}/platform/win32/Debug)
        
    elseif (${MuPDF_BUILD_TYPE} STREQUAL "Release")
        set (MuPDF_LIBRARY_PATH ${MuPDF_SOURCE_DIR}/platform/win32/Release)
    endif ()
else ()
    set (MuPDF_LIBRARY_COMPONENTS mupdf mujs jbig2dec jpeg openjpeg z)
    if (NOT UNIX) # Use provided freetype library
        set (MuPDF_LIBRARY_COMPONENTS ${MuPDF_LIBRARY_COMPONENTS} freetype)
    endif ()
    if (${MuPDF_BUILD_TYPE} STREQUAL "Debug")
        set (MuPDF_LIBRARY_PATH ${MuPDF_SOURCE_DIR}/build/debug)
    elseif (${MuPDF_BUILD_TYPE} STREQUAL "Release")
        set (MuPDF_LIBRARY_PATH ${MuPDF_SOURCE_DIR}/build/release)
    endif ()
endif ()
foreach (MuPDF_LIBRARY_COMPONENT ${MuPDF_LIBRARY_COMPONENTS})
    find_library (${MuPDF_LIBRARY_COMPONENT}_LIB
        ${MuPDF_LIBRARY_COMPONENT}
        PATHS ${MuPDF_LIBRARY_PATH}
        NO_DEFAULT_PATH)
    if (NOT ${MuPDF_LIBRARY_COMPONENT}_LIB)
        message (FATAL_ERROR "Library " ${MuPDF_LIBRARY_COMPONENT} " not found in " ${MuPDF_LIBRARY_PATH})
    endif ()
    set (MuPDF_LIBRARIES ${MuPDF_LIBRARIES} ${${MuPDF_LIBRARY_COMPONENT}_LIB})
endforeach () 
if (UNIX)
    # Use system freetype library in Linux, it's more compatiable with Qt library
    find_package (Freetype REQUIRED)
    # Use system crypto library
    find_package (PkgConfig REQUIRED)
    pkg_check_modules (CRYPTO REQUIRED libcrypto)
    set (MuPDF_LIBRARIES ${MuPDF_LIBRARIES} ${FREETYPE_LIBRARIES} ${CRYPTO_LIBRARIES} -lm )
endif ()

# Other
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (MuPDF MuPDF_INCLUDE_DIRS MuPDF_LIBRARIES)
mark_as_advanced (MuPDF_INCLUDE_DIRS MuPDF_LIBRARIES)
