# - Try to find Imlib2


MACRO(FIND_IMLIB2 IMLIB2_CFLAGS IMLIB2_LDFLAGS)


  EXECUTE_PROCESS( COMMAND imlib2-config --cflags
                   RESULT_VARIABLE _imlib2_config_result
                   OUTPUT_VARIABLE IMLIB2_CFLAGS 
                   ERROR_VARIABLE IMLIB2_CFLAGS ) 

  EXECUTE_PROCESS( COMMAND imlib2-config --libs
                   RESULT_VARIABLE _imlib2_config_result
                   OUTPUT_VARIABLE IMLIB2_LDFLAGS 
                   ERROR_VARIABLE IMLIB2_LDFLAGS ) 




ENDMACRO(FIND_IMLIB2)
