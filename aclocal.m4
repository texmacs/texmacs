
#--------------------------------------------------------------------
#
# MODULE      : aclocal.m4
# DESCRIPTION : TeXmacs m4 macros
# COPYRIGHT   : (C) 2016, 2017  Denis Raux, Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

m4_include([misc/m4/m4-ax_check_compile_flag.m4])
m4_include([misc/m4/m4-ax_append_flag.m4])
m4_include([misc/m4/m4-ax_save_flags.m4])
m4_include([misc/m4/m4-ax_restore_flags.m4])
m4_include([misc/m4/m4-lc_flag_utils.m4])
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
m4_include([misc/m4/tm_subversion.m4])
m4_include([misc/m4/tm_platform.m4])
m4_include([misc/m4/tm_debug.m4])
m4_include([misc/m4/tm_optimize.m4])
m4_include([misc/m4/tm_gui.m4])
m4_include([misc/m4/tm_macos.m4])
m4_include([misc/m4/tm_windows.m4])
m4_include([misc/m4/tm_fastalloc.m4])
m4_include([misc/m4/tm_install.m4])

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
