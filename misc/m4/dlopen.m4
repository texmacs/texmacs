
#--------------------------------------------------------------------
# Checks for dlopen in standard or dl library
# we are looking in sequence for dlopen or dld_link or shl_load in
# standard libs or libdl or libdld
#--------------------------------------------------------------------

m4_define([dyn_link_ok],[AC_DEFINE(TM_DYNAMIC_LINKING, [$1], [Dynamic linking function name])])

AC_DEFUN([LC_DLOPEN],[
  AC_SEARCH_LIBS(dlopen, libdl libdld, [dyn_link_ok(dlopen)],[
    AC_SEARCH_LIBS(dld_link, libdl libdld, [dyn_link_ok(dld_link)],[
      AC_SEARCH_LIBS(shl_load, libdl libdld, [dyn_link_ok(shl_load)],[
        echo configure: warning: dynamic linking using dlopen will not work
      ])
    ])
  ])
])
