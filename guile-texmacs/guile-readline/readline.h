#ifndef READLINEH
#define READLINEH

/*	Copyright (C) 1997, 1999, 2000, 2006 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 *
 */

/* SCM_RL_API is a macro prepended to all function and data definitions
   which should be exported or imported in the resulting dynamic link
   library in the Win32 port. */

#if defined (SCM_RL_IMPORT)
# define SCM_RL_API __declspec (dllimport) extern
#elif defined (SCM_RL_EXPORT) || defined (DLL_EXPORT)
# define SCM_RL_API __declspec (dllexport) extern
#else
# define SCM_RL_API extern
#endif

#include "libguile/__scm.h"

SCM_RL_API scm_t_option scm_readline_opts[];

#define SCM_HISTORY_FILE_P     scm_readline_opts[0].val
#define SCM_HISTORY_LENGTH     scm_readline_opts[1].val
#define SCM_READLINE_BOUNCE_PARENS scm_readline_opts[2].val
#define SCM_N_READLINE_OPTIONS 3

SCM_RL_API SCM scm_readline_options (SCM setting);
SCM_RL_API void scm_readline_init_ports (SCM inp, SCM outp);
SCM_RL_API SCM scm_readline (SCM txt, SCM inp, SCM outp, SCM read_hook);
SCM_RL_API SCM scm_add_history (SCM txt);
SCM_RL_API SCM scm_clear_history (void);
SCM_RL_API SCM scm_read_history (SCM file);
SCM_RL_API SCM scm_write_history (SCM file);
SCM_RL_API SCM scm_filename_completion_function (SCM text, SCM continuep);
SCM_RL_API void scm_init_readline (void);

#ifndef HAVE_RL_CLEANUP_AFTER_SIGNAL
void rl_cleanup_after_signal ();
void rl_free_line_state ();
#endif

#endif

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
