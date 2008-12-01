# - Locate the GNU Guile library

include(CheckIncludeFile)
include(CheckLibraryExists)

MACRO(FIND_GUILE GUILE_LDFLAGS)
  CHECK_INCLUDE_FILE("libguile.h" HAS_LIBGUILE_H)
  if(HAS_LIBGUILE_H)
    # Do we have -lguile
    CHECK_LIBRARY_EXISTS(guile scm_init_guile "" HAS_LIB_GUILE)
    if(HAS_LIB_GUILE)
      set(GUILE_LDFLAGS -lguile)
    endif(HAS_LIB_GUILE)
  endif(HAS_LIBGUILE_H)

  if(NOT HAS_LIB_GUILE)
    message(FATAL_ERROR "guile library not found")
  endif(NOT HAS_LIB_GUILE)
ENDMACRO(FIND_GUILE)

MACRO(MY_FIND_GUILE GUILE_CFLAGS GUILE_LDFLAGS)
  EXECUTE_PROCESS( COMMAND guile-config link
                   RESULT_VARIABLE _guile_config_result
                   OUTPUT_VARIABLE _guile_config_output 
                   ERROR_VARIABLE _guile_config_output ) 

  IF(_guile_config_result)
    MESSAGE(WARNING " cannot find guile-config; is Guile installed?")
  ENDIF(_guile_config_result)
 

  EXECUTE_PROCESS( COMMAND guile-config compile
                   RESULT_VARIABLE _guile_config_result
                   OUTPUT_VARIABLE GUILE_ORIGINAL_CFLAGS 
                   ERROR_VARIABLE GUILE_ORIGINAL_CFLAGS ) 


  SET(GUILE_CFLAGS "${GUILE_ORIGINAL_CFLAGS}")
  SET(GUILE_VARIANT_CFLAGS  "${GUILE_ORIGINAL_CFLAGS} ${GUILE_ORIGINAL_CFLAGS}/guile ${GUILE_ORIGINAL_CFLAGS}/libguile")


  EXECUTE_PROCESS( COMMAND guile-config link
                   RESULT_VARIABLE _guile_config_result
                   OUTPUT_VARIABLE GUILE_LDFLAGS 
                   ERROR_VARIABLE GUILE_LDFLAGS ) 

  EXECUTE_PROCESS( COMMAND guile-config info libdir
                   RESULT_VARIABLE _guile_config_result
                   OUTPUT_VARIABLE _guile_libdir 
                   ERROR_VARIABLE _guile_libdir ) 


  SET(GUILE_VARIANT_LDFLAGS "-L${_guile_libdir} -lguile -lreadline -ltermcap")


  EXECUTE_PROCESS( COMMAND guile-config --version
                   RESULT_VARIABLE _guile_config_result
                   OUTPUT_VARIABLE _guile_version 
                   ERROR_VARIABLE _guile_version ) 

  STRING( REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" QTVERSION "${_guile_version}")

  IF(QTVERSION VERSION_LESS 1.6)
    SET(CONFIG_GUILE_SERIAL A)
  ELSEIF(QTVERSION VERSION_LESS "1.8")
    SET(CONFIG_GUILE_SERIAL B)
  ELSE(QTVERSION VERSION_LESS 1.8)
    SET(CONFIG_GUILE_SERIAL C)
  ENDIF(QTVERSION VERSION_LESS 1.6)
  

#  AC_SUBST(GUILE_CFLAGS)
#  AC_SUBST(GUILE_LDFLAGS)
ENDMACRO(MY_FIND_GUILE)
