# aclocal.m4 - Local adaptation and libs
# Denis RAUX LIX 2015
# due to compatibilty problem with regex in bash before 3.2, 
# alway echo pattern string


m4_include([misc/m4/m4-ax_check_compile_flag.m4])
m4_include([misc/m4/m4-ax_append_flag.m4])
m4_include([misc/m4/m4-ax_save_flags.m4])
m4_include([misc/m4/m4-ax_restore_flags.m4])
m4_include([misc/m4/x11.m4])
m4_include([misc/m4/qt.m4])
m4_include([misc/m4/guile.m4])
m4_include([misc/m4/sparkle.m4])
m4_include([misc/m4/imlib2.m4])
m4_include([misc/m4/sql.m4])
m4_include([misc/m4/axel.m4])
m4_include([misc/m4/cairo.m4])
m4_include([misc/m4/gs.m4])
m4_include([misc/m4/hummus.m4])
m4_include([misc/m4/iconv.m4])
m4_include([misc/m4/freetype.m4])
m4_include([misc/m4/dlopen.m4])

#-------------------------------------------------------------------
# General functions
#-------------------------------------------------------------------
AC_DEFUN([LC_SUBST],[ 
  AC_SUBST([$1_CXX],[$$1_CXXFLAGS])
  AC_SUBST([$1_CPP],[$$1_CPPFLAGS])])

#remove leading and trailing
AC_DEFUN([LC_STRIP],[
	@<:@@<:@ "$$1" =~ [$(echo "^@{:@.*[^[:space:]])[[:space:]]+$"]) @:>@@:>@ && $1=${BASH_REMATCH[[1]]}
	@<:@@<:@ "$$1" =~ [$(echo "^[[:space:]]+([^[:space:]].*)$"]) @:>@@:>@ && $1=${BASH_REMATCH[[1]]}
# transform \\ in \
	$1=${$1//\/\//\/}
]) 

# remove unwanted space in well know flags
AC_DEFUN([LC_NORMALIZE_FLAG], [
  LC_STRIP([$1])
	if @<:@@<:@ "$$1" =~ [$(echo "^-(l|L|D)[[:space:]]+([^[:space:]].*)$"]) @:>@@:>@
	then $1=-${BASH_REMATCH[[1]]}${BASH_REMATCH[[2]]}
	fi
])


# append flag ($1) to ($2)
# authorize some duplicate according to the pattern
AC_DEFUN([LC_APPEND_FLAG],[
	if @<:@@<:@ "$1" =~ $(echo "^[[:space:]]*-Wl,.+") @:>@@:>@
	then $2="$$2 $1"
	else AX_APPEND_FLAG([$1],[$2])
	fi
])

#############################################################################
# generic pop flag $2 from $1 line $3 line flag separator (right most element)
AC_DEFUN([_LC_POP_FLAG],[
  LC_STRIP([$1])
	$2=${$1@%:@@%:@*$3} # get flag
	if test "$$2" = "$$1"
	then $1=
	else $2="$3$$2"; $1=${$1%$$2} # remove flag
	fi
	LC_NORMALIZE_FLAG([$2])
])

# generic get flag $2 from $1 line $3 line flag separator (left most element)
AC_DEFUN([_LC_GET_FLAG],[
  LC_STRIP([$1])
	$2=${$1%%$3*} # get flag
	$1=${$1@%:@*$$2} # remove flag
	LC_NORMALIZE_FLAG([$2])
])

#get flag $2 from $1 line (GET:left most element POP:right most)
AC_DEFUN([LC_GET_FLAG],[_LC_GET_FLAG([$1],[$2],[ -])])
AC_DEFUN([LC_POP_FLAG],[_LC_POP_FLAG([$1],[$2],[ -])])

#pop flag $2 from $1 line (GET:left most element POP:right most)
# we can have file names with no -l before
# line have been normalize  space is the separator
AC_DEFUN([LC_GET_FLAG_LIBS],[_LC_GET_FLAG([$1],[$2],[ ])])
AC_DEFUN([LC_POP_FLAG_LIBS],[_LC_POP_FLAG([$1],[$2],[ ])])
#############################################################################

#############################################################################
# wrapping AC_?_IFELSE for erro smg
AC_DEFUN([LC_X_IFELSE],[
	AC_MSG_CHECKING([$2] linking)
	AC_$1_IFELSE([$3], [ AC_MSG_RESULT(yes) $4
	],[ AC_MSG_RESULT(no) $5
	])])
AC_DEFUN([LC_RUN_IFELSE],[LC_X_IFELSE([RUN],[$1],[$2],[$3],[$4])])
AC_DEFUN([LC_LINK_IFELSE],[LC_X_IFELSE([LINK],[$1],[$2],[$3],[$4])])
AC_DEFUN([LC_COMPILE_IFELSE],[LC_X_IFELSE([COMPILE],[$1],[$2],[$3],[$4])])
#############################################################################

m4_define([lib_ext],[dylib])
AC_DEFUN([lc_lib_name],[m4_tolower($1)])

# Trivial compile flags setting for the LIBRARY ($1) in rep ($2)
AC_DEFUN([LC_SET_TRIVIAL_FLAGS],[
if [[ "$2" ]]
then
	LC_SCATTER_FLAGS([-I$2/include -L$2/lib -l[]lc_lib_name([$1])],[$1])
else
	LC_SCATTER_FLAGS([-l[]lc_lib_name([$1])],[$1])
fi
])
	

#############################################################################
# We use function here to avoid 'm4 local variable' conflict for embedded calls
# Needed to define functions at main Level
m4_define([f_lc_functions_list],[ f_lc_scatter_flags  ])

#############################################################################
# prepend a anonymous list into library FLAGS
# LC_APPEND_FLAG is used to avoid duplicate flag
# $1 anonymous list $2 destination
AC_DEFUN([LC_PREPEND_LIST],[
	[$0]_list="$1"
  #pop the old list
	while @<:@ "$$2" @:>@
  do  LC_POP_FLAG_LIBS([$2],[[$0]_flag])
    LC_APPEND_FLAG([$[$0]_flag],[[$0]_nlist])
	done
  # append new list
  while @<:@ "$[$0]_list" @:>@
  do	LC_POP_FLAG_LIBS([[$0]_list],[[$0]_flag])
    LC_APPEND_FLAG([$[$0]_flag],[[$0]_nlist])
	done
  $2=
  while @<:@ "$[$0]_nlist" @:>@
  do	LC_GET_FLAG_LIBS([[$0]_nlist],[[$0]_flag])
	    $2="$[$0]_flag $$2"
	done
	unset ${![$0]_*}
])

#############################################################################
# merge a anonymous list into library FLAGS
# $1 list $2 destination
AC_DEFUN([LC_MERGE_FLAGS],[
  [$0]_list="$1"
	if @<:@@<:@ "$2" =~ $(echo "(^|_)LIBS") @:>@@:>@
	then LC_PREPEND_LIST([$1],[$2])
	else  while @<:@ "$[$0]_list" @:>@
        do	LC_GET_FLAG([[$0]_list],[[$0]_flag])
            LC_APPEND_FLAG([$[$0]_flag],[$2])
	      done
	fi
  unset ${![$0]_*}
])

#############################################################################

# Groups flags definition
m4_define([superseded_flags],[[CFLAGS],[CXXFLAGS],[CPPFLAGS]])
m4_define([merged_flags],[[LDFLAGS],[LIBS]])
m4_define([all_flags],[superseded_flags, merged_flags])

# generic transfert flag $1 within the superseded list $2 and the merged list $3
AC_DEFUN([_LC_TRANSFERT_FLAGS],
 	[m4_foreach([_tmp1], [$2], [ if @<:@ "$$1_[]_tmp1" @:>@; then _tmp1=$$1_[]_tmp1;fi;])]
 	[m4_foreach([_tmp1], [$3], [LC_MERGE_FLAGS([$$1_[]_tmp1], [_tmp1])])]
)

# set compile flags from the LIBRARY ($1) flags into standard flags
# in order to test static linking set the -static if needed
AC_DEFUN([LC_SET_FLAGS],[
  _LC_TRANSFERT_FLAGS([$1],[superseded_flags],[merged_flags])
  if test -n "$LNSTATIC"
  then LC_APPEND_FLAG([-static], [CFLAGS])
       LC_APPEND_FLAG([-static], [CXXFLAGS])
  fi
])

# merge the LIBRARY ($1) flags into general compile flags
# The libraries in ($2)  should be remove and read with the right linking option 
# The remaining libs are merged in general compile flags with dynamic option
AC_DEFUN([LC_COMBINE_FLAGS],[
  for [$0]_lib in $2
  do  STRIP_ARG([$1_LIBS],[-l$[$0]_lib])
      [$0]_libs="$[$0]_libs -l$[$0]_lib"
  done
  if test -n "$$1_LIBS"
  then  LC_MERGE_FLAGS([$LNDYNAMIC],[$1_LIBS])
  fi
  if test -n "$2"
  then  LC_MERGE_FLAGS([$LNSTATIC $[$0]_libs],[$1_LIBS])
  fi
  _LC_TRANSFERT_FLAGS([$1],[],[merged_flags])
  unset ${![$0]_*}
])

# supersede the $2 empty flags  by $1 flags 
AC_DEFUN([LC_SET_EMPTY_FLAGS],[
 	m4_foreach([_tmp1], [all_flags], [$2_[]_tmp1=${$2_[]_tmp1:-${$1_[]_tmp1}};])])

# raw copy flags from $1 to $2
 AC_DEFUN([LC_COPY_FLAGS],[
  	m4_foreach([_tmp1], [all_flags], [$2_[]_tmp1="${$1_[]_tmp1}";])])

# Delete the LIBRARY ($1) flags
AC_DEFUN([LC_CLEAR_FLAGS],[
  m4_ifblank([$1],[m4_define([_tmp2])],[m4_define([_tmp2],[_])])
  m4_foreach([_tmp1], [all_flags],[unset $1[]_tmp2[]_tmp1;])])

#############################################################################
# build the lib name with a underscore if needed
m4_define([lc_libname],[m4_ifblank([$1],[$2],[$1_$2])])
#dispatch compil flags list $1 in a LIBRARY ($2) flags
#
AC_DEFUN([LC_SCATTER_FLAGS],[
  [$0]_list="$1" 
	
  while test -n "$[$0]_list" ; 
	do
	  LC_GET_FLAG([[$0]_list], [[$0]_flag])
		LC_STRIP([$0]_flag)
	  case "$[$0]_flag" in
	    -l*@:}@ LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[LIBS])]);;
	    -L*|-framework*@:}@ LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[LDFLAGS])]);;
	    -I*|-U*|-D*@:}@ LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[CPPFLAGS])]);;
      -F*@:}@ LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[CPPFLAGS])]) LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[LDFLAGS])]);;
	    -Wl,*@:}@ AC_MSG_WARN(Flag $[$0]_flag dropped for lib $2);;
	    -*@:}@ 
	      AX_CHECK_COMPILE_FLAG($[$0]_flag,[LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[CXXFLAGS])])],[AC_MSG_WARN(Flag $[$0]_flag dropped)],[],[]);;
	    *@:}@ AC_MSG_WARN(Flags $[$0]_flag NOT managed);;
	  esac
  done
  unset ${![$0]_*}
])

#############################################################################

# Dump flags
 AC_DEFUN([_LC_DUMP_FLAGS], [
 	m4_foreach([_tmp1], [[CFLAGS], [CXXFLAGS], [CPPFLAGS], [LIBS], [LDFLAGS]], 
 	[AC_MSG_NOTICE([$1[]_tmp1:$$1[]_tmp1])
 	])
 ])
 
AC_DEFUN([LC_DUMP_FLAGS], [m4_ifblank([$1],[_LC_DUMP_FLAGS],[_LC_DUMP_FLAGS([$1_])])])

# get arg value from a args list
# $1 args list $2 arg $3 arg value
AC_DEFUN([LC_GET_ARG_VALUE], [
		if @<:@@<:@ $$1 =~ $(echo "(^|[[:space:]]+)$2(=?|[[:space:]]*)(.*)$") @:>@@:>@
		then $3=${BASH_REMATCH[[3]]%% -*}
		else unset $3
		fi
])
	
# remove all  occurrence of arg from a args list
# $1 args list to clean $2 arg to remove
AC_DEFUN([STRIP_ARG], [
  unset [$0]_memo
  if test "$$1" = "$2"
  then $1=
  else  while test "$$1" != "$[$0]_memo"
      do [$0]_memo="$$1"
        $1="${$1/@%:@$2[[[:space:]]]/}" # at the beginning
        $1="${$1/%[[[:space:]]]$2/}"    # at the end
        $1="${$1//[[[:space:]]]$2[[[:space:]]]/ }"  # all in the middle no consecutive  
      done
    fi
  $1=$(echo $$1)  # wipe extra blank if any
  unset ${![$0]_*}
])
	
# remove multiple occurrences of arg from a args list
# $1 args list to clean $2 args to remove
AC_DEFUN([STRIP_ALL_ARGS], [
	unset [$0]_list
	for [$0]_arg in "$2"	
	do  while @<:@@<:@ $$1 != $[$0]_list @:>@@:>@
		  do [$0]_list=$$1
		     STRIP_ARG([$1],[$[$0]_arg])
	    done
	done
	unset ${![$0]_*}
])
	
   



#-------------------------------------------------------------------
# Support for stack traces
#-------------------------------------------------------------------

AC_DEFUN([AC_CPLUSPLUS_STACK],[
  AC_MSG_CHECKING(for C++ stack backtrace support)
  AC_RUN_IFELSE([AC_LANG_PROGRAM([
#include <stdio.h>
#include <stdlib.h>
#include <execinfo.h>
#include <cxxabi.h>
], [
    FILE* out= stderr;
    unsigned int max_frames= 63;
    fprintf(out, "stack trace:\n");
    void* addrlist[[max_frames+1]];
    int addrlen = backtrace(addrlist, sizeof(addrlist) / sizeof(void*));
    if (addrlen == 0) return 1;
    char** symbollist = backtrace_symbols(addrlist, addrlen);
    size_t funcnamesize = 256;
    char* funcname = (char*)malloc(funcnamesize);
    for (int i = 1; i < addrlen; i++) {
        char *begin_name = 0, *begin_offset = 0, *end_offset = 0;
        for (char *p = symbollist[[i]]; *p; ++p) {
            if (*p == '(')
                begin_name = p;
            else if (*p == '+')
                begin_offset = p;
            else if (*p == ')' && begin_offset) {
                end_offset = p;
                break;
            }
        }
        if (begin_name && begin_offset && end_offset
            && begin_name < begin_offset)
        {
            *begin_name++ = '\0';
            *begin_offset++ = '\0';
            *end_offset = '\0';
            int status;
            char* ret = abi::__cxa_demangle(begin_name,
                                            funcname, &funcnamesize, &status);
            if (status == 0) {
                funcname = ret;
                fprintf(out, "  %s : %s+%s\n",
                        symbollist[[i]], funcname, begin_offset);
            }
            else {
                fprintf(out, "  %s : %s()+%s\n",
                        symbollist[[i]], begin_name, begin_offset);
            }
        }
        else fprintf(out, "  %s\n", symbollist[[i]]);
    }
    free(funcname);
    free(symbollist);
    return 0;
  ])],[
    AC_MSG_RESULT(yes)
    AC_DEFINE(USE_STACK_TRACE, 1, [Use C++ stack backtraces])
  ],[
    AC_MSG_RESULT(no)
  ],[
    AC_MSG_RESULT(no)
  ])
])

