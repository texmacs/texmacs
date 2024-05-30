#ifndef SCM_LIBGUILE_H
#define SCM_LIBGUILE_H

/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002, 2003, 2004, 2006, 2008 Free Software Foundation, Inc.
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


/* This needs to be included outside of the extern "C" block.
 */
#include <gmp.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "libguile/__scm.h"
#include "libguile/alist.h"
#include "libguile/arbiters.h"
#include "libguile/async.h"
#include "libguile/boolean.h"
#include "libguile/chars.h"
#include "libguile/continuations.h"
#include "libguile/dynl.h"
#include "libguile/dynwind.h"
#include "libguile/eq.h"
#include "libguile/error.h"
#include "libguile/eval.h"
#include "libguile/evalext.h"
#include "libguile/extensions.h"
#include "libguile/feature.h"
#include "libguile/filesys.h"
#include "libguile/fluids.h"
#include "libguile/fports.h"
#include "libguile/futures.h"
#include "libguile/gc.h"
#include "libguile/gdbint.h"
#include "libguile/goops.h"
#include "libguile/gsubr.h"
#include "libguile/guardians.h"
#include "libguile/hash.h"
#include "libguile/hashtab.h"
#include "libguile/init.h"
#include "libguile/ioext.h"
#include "libguile/rdelim.h"
#include "libguile/rw.h"
#include "libguile/keywords.h"
#include "libguile/list.h"
#include "libguile/load.h"
#include "libguile/macros.h"
#include "libguile/mallocs.h"
#include "libguile/modules.h"
#include "libguile/net_db.h"
#include "libguile/numbers.h"
#include "libguile/objects.h"
#include "libguile/objprop.h"
#include "libguile/options.h"
#include "libguile/pairs.h"
#include "libguile/ports.h"
#include "libguile/posix.h"
#include "libguile/print.h"
#include "libguile/procprop.h"
#include "libguile/properties.h"
#include "libguile/procs.h"
#include "libguile/ramap.h"
#include "libguile/random.h"
#include "libguile/read.h"
#include "libguile/root.h"
#include "libguile/scmsigs.h"
#include "libguile/script.h"
#include "libguile/simpos.h"
#include "libguile/smob.h"
#include "libguile/snarf.h"
#include "libguile/socket.h"
#include "libguile/sort.h"
#include "libguile/srcprop.h"
#include "libguile/stackchk.h"
#include "libguile/stime.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"
#include "libguile/srfi-14.h"
#include "libguile/strorder.h"
#include "libguile/strports.h"
#include "libguile/struct.h"
#include "libguile/symbols.h"
#include "libguile/tags.h"
#include "libguile/throw.h"
#include "libguile/unif.h"
#include "libguile/validate.h"
#include "libguile/values.h"
#include "libguile/variable.h"
#include "libguile/vectors.h"
#include "libguile/srfi-4.h"
#include "libguile/version.h"
#include "libguile/vports.h"
#include "libguile/weaks.h"
#include "libguile/backtrace.h"
#include "libguile/debug.h"
#include "libguile/stacks.h"
#include "libguile/threads.h"
#include "libguile/inline.h"

#include "libguile/discouraged.h"
#include "libguile/deprecated.h"

#ifdef __cplusplus
}
#endif



#endif  /* SCM_LIBGUILE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
