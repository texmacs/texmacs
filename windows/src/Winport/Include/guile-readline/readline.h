#ifndef READLINEH
#define READLINEH

/*	Copyright (C) 1997, 1999, 2000 Free Software Foundation, Inc.
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
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 */

#include "libguile/__scm.h"

#if __GUILERL_EXPORT__
# define GUILERL_API __declspec (dllexport)
#elif __GUILERL_IMPORT__
# define GUILERL_API __declspec (dllimport)
#else
# define GUILERL_API
#endif

extern scm_option scm_readline_opts[];

#define SCM_HISTORY_FILE_P     scm_readline_opts[0].val
#define SCM_HISTORY_LENGTH     scm_readline_opts[1].val
#define SCM_READLINE_BOUNCE_PARENS scm_readline_opts[2].val
#define SCM_N_READLINE_OPTIONS 3

GUILERL_API extern SCM scm_readline_options (SCM setting);
GUILERL_API extern void scm_readline_init_ports (SCM inp, SCM outp);
GUILERL_API extern SCM scm_readline (SCM txt, SCM inp, SCM outp, SCM read_hook);
GUILERL_API extern SCM scm_add_history (SCM txt);
GUILERL_API extern SCM scm_read_history (SCM file);
GUILERL_API extern SCM scm_write_history (SCM file);
GUILERL_API extern SCM scm_filename_completion_function (SCM text, SCM continuep);
GUILERL_API extern void scm_init_readline (void);

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
