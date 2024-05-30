/* srfi-4.c --- Homogeneous numeric vector datatypes.
 *
 * 	Copyright (C) 2001, 2004, 2006, 2008 Free Software Foundation, Inc.
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

/* This file is now empty since all its procedures are now in the
   core.  We keep the libguile-srfi-srfi-4.so library around anyway
   since people might still be linking with it.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <srfi/srfi-4.h>

void scm_init_srfi_4_no_clash_with_libguile (void);

void
scm_init_srfi_4_no_clash_with_libguile (void)
{
}

/* End of srfi-4.c.  */
