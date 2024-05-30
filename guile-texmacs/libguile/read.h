/* classes: h_files */

#ifndef SCM_READ_H
#define SCM_READ_H

/* Copyright (C) 1995,1996,2000, 2006 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



#include "libguile/__scm.h"

#include "libguile/options.h"


/* SCM_LINE_INCREMENTORS are the characters which cause the line count to
 * be incremented for the purposes of error reporting.  This feature
 * is only used for scheme code loaded from files.
 *
 * SCM_WHITE_SPACES are other characters which should be treated like spaces
 * in programs.
 */

#define SCM_LINE_INCREMENTORS  '\n'

#ifdef MSDOS
# define SCM_SINGLE_SPACES  ' ':case '\r':case '\f': case 26
#else
# define SCM_SINGLE_SPACES  ' ':case '\r':case '\f'
#endif

#define SCM_WHITE_SPACES  SCM_SINGLE_SPACES: case '\t'

SCM_API scm_t_option scm_read_opts[];

#define SCM_COPY_SOURCE_P      scm_read_opts[0].val
#define SCM_RECORD_POSITIONS_P scm_read_opts[1].val
#define SCM_CASE_INSENSITIVE_P scm_read_opts[2].val
#define SCM_KEYWORD_STYLE      scm_read_opts[3].val
#if SCM_ENABLE_ELISP
#define SCM_ELISP_VECTORS_P    scm_read_opts[4].val
#define SCM_ESCAPED_PARENS_P   scm_read_opts[5].val
#define SCM_N_READ_OPTIONS 6
#else
#define SCM_N_READ_OPTIONS 4
#endif



SCM_API SCM scm_sym_dot;

SCM_API SCM scm_read_options (SCM setting);
SCM_API SCM scm_read (SCM port);
SCM_API size_t scm_read_token (int ic, SCM * tok_buf, SCM port, int weird);
SCM_API SCM scm_read_hash_extend (SCM chr, SCM proc);

SCM_API void scm_i_input_error (const char *func, SCM port,
				const char *message, SCM arg)
  SCM_NORETURN;

SCM_API void scm_init_read (void);

#endif  /* SCM_READ_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
