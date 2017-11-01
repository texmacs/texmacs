
#--------------------------------------------------------------------
#
# MODULE      : m4_lc_flag_utils.m4
# DESCRIPTION : Various utilities for manipulating groups of flags
# COPYRIGHT   : (C) 2016, 2017  Denis Raux
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

# NOTE: due to compatibilty problem with regex in bash before 3.2,
# always rely on 'echo' when using pattern strings

------------------------------------------------------------------
# General functions
#-------------------------------------------------------------------
# Create local variable based on those located in config.h
AC_DEFUN([LC_DEFINE],[
  typeset $1="$2"
  AC_DEFINE($1,$2,$3)
])

# checking function without extra display before result
m4_define([LS_SAVE_DESC],[3])
AC_DEFUN([LC_MSG_CHECKING],[
  AC_MSG_CHECKING([$1])
  exec LS_SAVE_DESC>&AS_MESSAGE_FD AS_MESSAGE_FD>/dev/null
])
AC_DEFUN([LC_MSG_RESULT],[
  exec AS_MESSAGE_FD>&LS_SAVE_DESC
  AC_MSG_RESULT([$1])
])

AC_DEFUN([LC_SUBST],[ 
  AC_SUBST([$1_CXX],[$$1_CXXFLAGS])
  AC_SUBST([$1_CPP],[$$1_CPPFLAGS])
  if @<:@@<:@ $enable_dumpflags =~ $(echo "(^|@<:@@<:@:space:@:>@@:>@)($1|ALL)($|@<:@@<:@:space:@:>@@:>@)") @:>@@:>@
  then LC_DUMP_FLAGS([$1])
  fi
# adaptation layer remove when finished
  AC_SUBST([$1_CFLAGS],["$$1_CXXFLAGS $$1_CPPFLAGS"])
  AC_SUBST([$1_LDFLAGS],[""])
])

# remove unwanted space in well know flags
AC_DEFUN([LC_NORMALIZE_FLAG], [
  $1=$(echo $$1)    # normalize whitespaces
  $1=${$1//\/\//\/}   # normalize path separator
  $1=${$1//\'}   # remove quote

  if @<:@@<:@ "$$1" =~ [$(echo "^-(l|L|D)[[:space:]]+([^[:space:]].*)$"]) @:>@@:>@
  then $1=-${BASH_REMATCH[[1]]}${BASH_REMATCH[[2]]}
  fi
])


# append flag ($1) to ($2)
# authorize some duplicate according to the pattern
AC_DEFUN([LC_APPEND_FLAG],[
  if @<:@@<:@ "$1" =~ $(echo '^-Wl,') @:>@@:>@
  then $2+=" $1"
  elif @<:@@<:@ "$1" =~ $(echo '^-l') @:>@@:>@
  then STRIP_ALL_ARGS([$2],[$1])
       $2+=" $1"
  else
    if @<:@@<:@ "$$2" =~ $(echo "(^|@<:@@<:@:space:@:>@@:>@)$1($|@<:@@<:@:space:@:>@@:>@)") @:>@@:>@
    then AC_MSG_WARN(Drop duplicate flag $1)
    else $2+=" $1"
    fi
  fi
])

# Generic tools for set manipulation

# Transform a comma separated set to a blank separated one
m4_define([unlist],[m4_map_sep([m4_unquote],[ ],[$@])])
m4_define([lib_powerset],[m4_dquote(m4_map_sep([unlist],[,],[$1]))])

# deal with quotted set
define([q_car],[m4_car(m4_unquote([$1]))])
define([q_cdr],[m4_cdr(m4_unquote([$1]))])
# give the dimension of the quoted list first item 
define([q_count],[m4_count(m4_unquote(q_car([$1])))])
# give the maximal dimension of s set
m4_define([get_dim],[m4_ifnblank([$1], 
  [m4_max(q_count([$1]),get_dim(q_cdr([$1])))], [0])])

# if $1 == $2 then $3 else $4
m4_define([lc_a_eq],[m4_if([0],m4_cmp([$1],[$2]),[$3],[$4])])

# sort a set list according to the set dimension
m4_define([_order_lst], [lc_a_eq([0],[$3],[],[m4_ifnblank([$1],
[lc_a_eq(q_count([$1]),$3,
[_order_lst(q_cdr([$1]),[$2],[$3]),q_car([$1])],
[_order_lst(q_cdr([$1]),[$2],[$3])])],
[_order_lst([$2],[$2],m4_decr([$3]))])])])
m4_define([order_lst],
[m4_define([rlst], [m4_reverse(m4_unquote([$1]))])dnl
m4_cdr(_order_lst([rlst],[rlst],get_dim([$1])))])

# power set construction for libs combinaison
#lc_power_set([A,B,C]) gives[A],[A,B],[A,B,C],[A,C],[B],[B,C],[C]
#
m4_define([lc_list_add],[[[$1,$2]]])
m4_define([prodset],[m4_quote([[$1]],m4_foreach(ms,[$2],[lc_list_add($1,m4_quote(ms)),])[$2])])
m4_define([lc_power_set],[dnl
m4_cond(m4_count($1),1,m4_dquote([$1]),[prodset(m4_car($1),lc_power_set(m4_cdr($1)))])])

# return a sort libraries list 
m4_define([lc_slib_powerset],[lib_powerset(order_lst(lc_power_set([$1])))])


#############################################################################
# generic get flag $2 from $1 line $3 line flag separator
AC_DEFUN([LC_GETPOP_FLAG],[
  m4_ifblank([$3], [[$0]_sepa=-], [[$0]_sepa=[$3]])
  [$0]_1=" $(echo $$1)" # remove unnecessary spaces and add a heading space 
  $1=${[$0]_1@&t@GETPOP $[$0]_sepa*} # get heading
  if test "$$1" = "$[$0]_1"
  then $1=${$1@%:@}; unset $2     # flag not found
  else 
    [$0]_tail=${[$0]_1@%:@$$1 } # strip heading
    $2=${[$0]_tail%% -*}        # get flag and data
    if test "$$2" = "$[$0]_tail"  # is there someting after the flag
    then $1=$(echo $$1)
    else $1=$(echo $$1${[$0]_tail@%:@$$2})
    fi
    LC_NORMALIZE_FLAG([$2])
    if [[ $$0_sepa != - ]]    # just return the value
    then $2=${$2@%:@$[$0]_sepa}
    fi
  fi
  
  unset ${![$0]_*}
])

# generic get flag $2 from $1 line $3 line flag separator (left most element)
m4_define([LC_GET_FLAG],[m4_define([GETPOP],[%%]) LC_GETPOP_FLAG([$1],[$2],[$3])])
# generic get flag $2 from $1 line $3 line flag separator (right most element)
m4_define([LC_POP_FLAG],[m4_define([GETPOP],[%]) LC_GETPOP_FLAG([$1],[$2],[$3])])

#############################################################################

#############################################################################
# wrapping AC_?_IFELSE for erro smg
AC_DEFUN([LC_X_IFELSE],[
  AC_MSG_CHECKING([$2] linking)
  AC_$1_IFELSE([$3], [ 
    AC_MSG_RESULT(yes) 
    $4
  ],[ 
    AC_MSG_RESULT(no) 
    $5
  ])])
AC_DEFUN([LC_RUN_IFELSE],[LC_X_IFELSE([RUN],[$1],[$2],[$3],[$4],[$5])])
AC_DEFUN([LC_LINK_IFELSE],[LC_X_IFELSE([LINK],[$1],[$2],[$3],[$4],[$5])])
AC_DEFUN([LC_COMPILE_IFELSE],[LC_X_IFELSE([COMPILE],[$1],[$2],[$3],[$4],[$5])])
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
# prepend a anonymous list into library FLAGS
# LC_APPEND_FLAG is used to avoid duplicate flag
# $1 anonymous list $2 destination
AC_DEFUN([LC_PREPEND_LIST],[
  [$0]_nlist="$1"
  #pop the old list
  LC_GET_FLAG([$2],[$0_flag])
  while @<:@@<:@ $[$0]_flag @:>@@:>@
  do  LC_APPEND_FLAG([$$0_flag],[$0_nlist])
      LC_GET_FLAG([$2],[$0_flag])
    
  done
  $2="$[$0]_nlist"
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
        do  LC_GET_FLAG([$0_list],[$0_flag])
            LC_APPEND_FLAG([$$0_flag],[$2])
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
  [m4_foreach([_tmp1], [$2], [ 
    _tmp1="$BASE_[]_tmp1 $$1_[]_tmp1"
  ])]
  [m4_foreach([_tmp1], [$3], [LC_MERGE_FLAGS([$$1_[]_tmp1], [_tmp1])])]
)

# set compile flags from the LIBRARY ($1) flags into standard flags
# in order to test static linking set the -static if needed
AC_DEFUN([LC_SET_FLAGS],[
  _LC_TRANSFERT_FLAGS([$1],[superseded_flags,[LIBS]],[LDFLAGS])
])

# merge the LIBRARY ($1) flags into general compile flags
AC_DEFUN([LC_COMBINE_FLAGS],[
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
# It is an improved version of AC_CHECK_LIB
# Can find a suitable combinaison of extra LIBS and return it in $6
AC_DEFUN([_LC_CHECK_LIB],[
  m4_ifblank([$5],[$4],[
    unset ac_cv_lib_$1_$2
# For system variable in $1
    unset ac_cv_lib_$1___$2

    AC_MSG_NOTICE([check $1 with extra libs m4_car($5)])
    [$0]_libs=$LIBS
    LIBS+=" m4_car($5)"
    AC_CHECK_LIB([$1],[$2],[
      LIBS=$[$0]_libs
      m4_ifnblank([$6],[$6="m4_car($5)"])
      $3
    ],[
      LIBS=$[$0]_libs
      _LC_CHECK_LIB([$1],[$2],[$3],[$4],m4_cdr($5),[$6])
    ])
    unset ${![$0]_*}
  ])
])

AC_DEFUN([LC_CHECK_LIB],[
  AC_CHECK_LIB([$1],[$2],[$3],[
    _LC_CHECK_LIB([$1],[$2],[$3],[$4],lc_slib_powerset([$5]),[$6])
  ])
])

#############################################################################
# build the lib name with a underscore if needed
m4_define([lc_libname],[m4_ifblank([$1],[$2],[$1_$2])])

# mainly used to parse aguments returned by individuals configure maodule m4
# dispatch compil flags list $1 in a LIBRARY ($2) flags
# in case of libs will search for dependancies according the build mode
#
AC_DEFUN([LC_SCATTER_FLAGS],[
  [$0]_list="$1" 
  
  LC_GET_FLAG([$0_list], [$0_flag])
  while test -n "$[$0]_flag"
  do
    case "$[$0]_flag" in
      -l*@:}@ LC_APPEND_FLAG([$[$0]_flag],[[$0]_LIBS]);;
      -L*|-framework*@:}@ LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[LDFLAGS])]);;
      -I*|-U*|-D*@:}@ LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[CPPFLAGS])]);;
      -F*@:}@ LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[CPPFLAGS])]);;
      -Wl,-F*@:}@ LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[LDFLAGS])]);;
      -Wl,*@:}@ AC_MSG_WARN(Flag $[$0]_flag dropped for lib $2);;
      -*@:}@ 
        AX_CHECK_COMPILE_FLAG($[$0]_flag,[
          LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[CXXFLAGS])])
          LC_APPEND_FLAG([$[$0]_flag],[lc_libname([$2],[CFLAGS])])
        ],[AC_MSG_WARN(Flag $[$0]_flag dropped)],[],[]);;
      *@:}@ AC_MSG_WARN(Flags $[$0]_flag NOT managed);;
    esac
    LC_GET_FLAG([$0_list], [$0_flag])
  done
  LC_PREPEND_LIST([$[$0]_LIBS],[lc_libname([$2],[LIBS])])
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
    if @<:@@<:@ "$$1" =~ $(echo "(^|@<:@@<:@:space:@:>@@:>@+)$2(=?|@<:@@<:@:space:@:>@@:>@*)(.*)$") @:>@@:>@
    then $3=${BASH_REMATCH[[3]]%% -*}
         $3=${$3%% }  # strip tailing blanks
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

AC_ARG_ENABLE(dumpflags,
  AS_HELP_STRING([--enable-dumpflags@<:@=no@:>@], 
  [list of flags to dump (i.e QT ICONV GUILE ...)]),[],[])
