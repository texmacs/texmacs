/*	Copyright (C) 1996, 1998, 2000 Free Software Foundation, Inc.
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


#ifndef LIBGUILE_DYNL_H
#define LIBGUILE_DYNL_H

#include "libguile/__scm.h"



GUILE_API void scm_register_module_xxx (char *module_name, void *init_func);
GUILE_API SCM scm_registered_modules (void);
GUILE_API SCM scm_clear_registered_modules (void);

GUILE_API SCM scm_dynamic_link (SCM fname);
GUILE_API SCM scm_dynamic_unlink (SCM dobj);
GUILE_API SCM scm_dynamic_object_p (SCM obj);
GUILE_API SCM scm_dynamic_func (SCM symb, SCM dobj);
GUILE_API SCM scm_dynamic_call (SCM symb, SCM dobj);
GUILE_API SCM scm_dynamic_args_call (SCM symb, SCM dobj, SCM args);

GUILE_API void scm_init_dynamic_linking (void);

#endif  /* LIBGUILE_DYNL_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
