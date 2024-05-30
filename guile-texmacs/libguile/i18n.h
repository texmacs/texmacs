/* classes: h_files */

#ifndef SCM_I18N_H
#define SCM_I18N_H

/* Copyright (C) 2004, 2006 Free Software Foundation, Inc.
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

SCM_API SCM scm_gettext (SCM msgid, SCM domainname, SCM category);
SCM_API SCM scm_ngettext (SCM msgid, SCM msgid_plural, SCM n, SCM domainname, SCM category);
SCM_API SCM scm_textdomain (SCM domainname);
SCM_API SCM scm_bindtextdomain (SCM domainname, SCM directory);
SCM_API SCM scm_bind_textdomain_codeset (SCM domainname, SCM encoding);

SCM_API int scm_i_to_lc_category (SCM category, int allow_lc_all);

SCM_API void scm_init_i18n (void);

#endif  /* SCM_I18N_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
