/* classes: h_files */

#ifndef FPORTSH
#define FPORTSH
/*	Copyright (C) 1995,1996,1997,1998,1999, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"

#include "libguile/ports.h"



/* struct allocated for each buffered FPORT.  */
struct scm_fport {
  int fdes;			/* file descriptor.  */
};

#define SCM_FSTREAM(x) ((struct scm_fport *) SCM_STREAM (x))
#define SCM_FPORT_FDES(x) (SCM_FSTREAM (x)->fdes)

#define SCM_FPORTP(x) (!SCM_IMP (x) && (SCM_TYP16S (x) == scm_tc7_port))
#define SCM_OPFPORTP(x) (!SCM_IMP (x) && (((0xfeff | SCM_OPN) & SCM_CELL_WORD_0 (x)) == (scm_tc7_port | SCM_OPN)))
#define SCM_OPINFPORTP(x) (!SCM_IMP (x) && (((0xfeff | SCM_OPN | SCM_RDNG) & SCM_CELL_WORD_0 (x)) == (scm_tc7_port | SCM_OPN | SCM_RDNG)))
#define SCM_OPOUTFPORTP(x) (!SCM_IMP(x) && (((0xfeff | SCM_OPN | SCM_WRTNG) & SCM_CELL_WORD_0 (x)) == (scm_tc7_port | SCM_OPN | SCM_WRTNG)))

/* test whether fdes supports random access.  */
#define SCM_FDES_RANDOM_P(fdes) ((lseek (fdes, 0, SEEK_CUR) == -1) ? 0 : 1)


GUILE_API extern SCM scm_setbuf0 (SCM port);
GUILE_API extern SCM scm_setvbuf (SCM port, SCM mode, SCM size);
GUILE_API extern void scm_setfileno (FILE *fs, int fd);
GUILE_API extern void scm_evict_ports (int fd);
GUILE_API extern SCM scm_open_file (SCM filename, SCM modes);
GUILE_API extern SCM scm_fdes_to_port (int fdes, char *mode, SCM name);
GUILE_API extern void scm_init_fports (void);

#endif  /* FPORTSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
