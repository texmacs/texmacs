/* classes: h_files */

#ifndef MODULESH
#define MODULESH
/*	Copyright (C) 1998, 2000 Free Software Foundation, Inc.
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
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


#include "libguile/__scm.h"



GUILE_API extern SCM scm_the_root_module (void);
GUILE_API extern SCM scm_selected_module (void);
GUILE_API extern SCM scm_select_module (SCM module);
GUILE_API extern SCM scm_make_module (SCM name);
GUILE_API extern SCM scm_ensure_user_module (SCM name);
GUILE_API extern SCM scm_module_lookup_closure (SCM module);
GUILE_API extern SCM scm_resolve_module (SCM name);
GUILE_API extern SCM scm_load_scheme_module (SCM name);
GUILE_API extern SCM scm_env_top_level (SCM env);
GUILE_API extern SCM scm_top_level_env (SCM thunk);
GUILE_API extern SCM scm_system_module_env_p (SCM env);
GUILE_API extern SCM scm_standard_eval_closure (SCM module);
GUILE_API extern void scm_init_modules (void);
GUILE_API extern void scm_post_boot_init_modules (void);

#endif  /* MODULESH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
