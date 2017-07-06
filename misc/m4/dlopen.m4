
#--------------------------------------------------------------------
#
# MODULE      : dlopen.m4
# DESCRIPTION : Checks for dlopen in standard or dl library
# COPYRIGHT   : (C) 2016 Joris van der Hoeven, Denis RAUX
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------


#--------------------------------------------------------------------
# we are looking in sequence for dlopen or dld_link or shl_load in
# standard libs or libdl or libdld
#--------------------------------------------------------------------

m4_define([dyn_link_ok],[AC_DEFINE(TM_DYNAMIC_LINKING, [$1], [Dynamic linking function name])])

AC_DEFUN([LC_DLOPEN],[
  AC_SEARCH_LIBS(dlopen, libdl libdld, [dyn_link_ok(dlopen)],[
    AC_SEARCH_LIBS(dld_link, libdl libdld, [dyn_link_ok(dld_link)],[
      AC_SEARCH_LIBS(shl_load, libdl libdld, [dyn_link_ok(shl_load)],[
        AC_MSG_WARN([dynamic linking using dlopen will not work])
      ])
    ])
  ])
])
