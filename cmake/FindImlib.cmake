# 
# This module finds if IMLIB is available and determines where the
# include files and libraries are. 
# On Unix/Linux it relies on the output of imlib-config.
# This code sets the following variables:
# 
#
#
# IMLIB_FOUND       = system has IMLIB lib
#
# IMLIB_LIBRARIES   = full path to the libraries
#    on Unix/Linux with additional linker flags from "imlib-config --libs"
# 
# CMAKE_IMLIB_CXX_FLAGS  = Unix compiler flags for IMLIB, essentially "`imlib-config --cxxflags`"
#
# IMLIB_INCLUDE_DIR      = where to find headers 
#
# IMLIB_LINK_DIRECTORIES = link directories, useful for rpath on Unix
#
# 
# author Jan Woetzel and Jan-Friso Evers
# www.mip.informatik.uni-kiel.de/~jw

IF(WIN32)
  MESSAGE("FindIMLIB.cmake: IMLIB not (yet) supported on WIN32")
    SET(IMLIB_FOUND OFF	)

  
ELSE(WIN32)
  IF(UNIX) 
    SET(IMLIB_CONFIG_PREFER_PATH "$ENV{IMLIB_HOME}/bin" CACHE STRING "preferred path to imlib")
    FIND_PROGRAM(IMLIB_CONFIG imlib-config
      ${IMLIB_CONFIG_PREFER_PATH}
      /usr/bin/
      /opt/gnome/bin/
      )

    IF (IMLIB_CONFIG) 
      # OK, found imlib-config. 
      # set CXXFLAGS to be fed into CXX_FLAGS by the user:
      SET(IMLIB_CXX_FLAGS "`${IMLIB_CONFIG} --cflags`")
      
      # set INCLUDE_DIRS to prefix+include
      EXEC_PROGRAM(${IMLIB_CONFIG}
	ARGS --prefix
	OUTPUT_VARIABLE IMLIB_PREFIX)
      SET(IMLIB_INCLUDE_DIR ${IMLIB_PREFIX}/include CACHE STRING INTERNAL)

      # extract link dirs for rpath  
      EXEC_PROGRAM(${IMLIB_CONFIG}
	ARGS --libs
	OUTPUT_VARIABLE IMLIB_CONFIG_LIBS )
      
      # set link libraries and link flags
      #SET(IMLIB_LIBRARIES "`${IMLIB_CONFIG} --libs`")
      SET(IMLIB_LIBRARIES ${IMLIB_CONFIG_LIBS})
      
      # split off the link dirs (for rpath)
      # use regular expression to match wildcard equivalent "-L*<endchar>"
      # with <endchar> is a space or a semicolon
      STRING(REGEX MATCHALL "[-][L]([^ ;])+" 
	IMLIB_LINK_DIRECTORIES_WITH_PREFIX 
	"${IMLIB_CONFIG_LIBS}" )
      #MESSAGE("DBG  IMLIB_LINK_DIRECTORIES_WITH_PREFIX=${IMLIB_LINK_DIRECTORIES_WITH_PREFIX}")
      
      # remove prefix -L because we need the pure directory for LINK_DIRECTORIES
      # replace -L by ; because the separator seems to be lost otherwise (bug or feature?)
      IF (IMLIB_LINK_DIRECTORIES_WITH_PREFIX)
	STRING(REGEX REPLACE "[-][L]" ";" IMLIB_LINK_DIRECTORIES ${IMLIB_LINK_DIRECTORIES_WITH_PREFIX} )
	#MESSAGE("DBG  IMLIB_LINK_DIRECTORIES=${IMLIB_LINK_DIRECTORIES}")
      ENDIF (IMLIB_LINK_DIRECTORIES_WITH_PREFIX)
      
      # replace space separated string by semicolon separated vector to make 
      # it work with LINK_DIRECTORIES
      SEPARATE_ARGUMENTS(IMLIB_LINK_DIRECTORIES)
      
      MARK_AS_ADVANCED(
	IMLIB_CXX_FLAGS
	IMLIB_INCLUDE_DIR
	IMLIB_LIBRARIES
	IMLIB_LINK_DIRECTORIES
	IMLIB_CONFIG_PREFER_PATH 
	IMLIB_CONFIG
	)

    ELSE(IMLIB_CONFIG)
      MESSAGE( "FindIMLIB.cmake: imlib-config not found. Please set it manually. IMLIB_CONFIG=${IMLIB_CONFIG}")
    ENDIF(IMLIB_CONFIG)

  ENDIF(UNIX)
ENDIF(WIN32)


IF(IMLIB_LIBRARIES)
  IF(IMLIB_INCLUDE_DIR OR IMLIB_CXX_FLAGS)

    SET(IMLIB_FOUND 1)

  ENDIF(IMLIB_INCLUDE_DIR OR IMLIB_CXX_FLAGS)
ENDIF(IMLIB_LIBRARIES)

# make FIND_PACKAGE case sensitive compatible
SET(Imlib_FOUND       ${IMLIB_FOUND})
SET(Imlib_LIBRARIES   ${IMLIB_LIBRARIES})
SET(Imlib_INCLUDE_DIR ${IMLIB_INCLUDE_DIR})
SET(Imlib_CXX_FLAGS   ${IMLIB_CXX_FLAGS})
SET(Imlib_LINK_DIRECTORIES ${IMLIB_LINK_DIRECTORIES})