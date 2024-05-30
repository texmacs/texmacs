/* classes: h_files */

#ifndef SCM_FPORTS_H
#define SCM_FPORTS_H

/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2006 Free Software Foundation, Inc.
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

#include "libguile/ports.h"



/* struct allocated for each buffered FPORT.  */
typedef struct scm_t_fport {
  int fdes;			/* file descriptor.  */
} scm_t_fport;

SCM_API scm_t_bits scm_tc16_fport;

#define SCM_FSTREAM(x) ((scm_t_fport *) SCM_STREAM (x))
#define SCM_FPORT_FDES(x) (SCM_FSTREAM (x)->fdes)

#define SCM_FPORTP(x) (!SCM_IMP (x) && (SCM_TYP16 (x) == scm_tc16_fport))
#define SCM_OPFPORTP(x) (SCM_FPORTP (x) && (SCM_CELL_WORD_0 (x) & SCM_OPN))
#define SCM_OPINFPORTP(x) (SCM_OPFPORTP (x) && (SCM_CELL_WORD_0 (x) & SCM_RDNG))
#define SCM_OPOUTFPORTP(x) (SCM_OPFPORTP (x) && (SCM_CELL_WORD_0 (x) & SCM_WRTNG))

/* test whether fdes supports random access.  */
#define SCM_FDES_RANDOM_P(fdes) ((lseek (fdes, 0, SEEK_CUR) == -1) ? 0 : 1)


SCM_API SCM scm_setbuf0 (SCM port);
SCM_API SCM scm_setvbuf (SCM port, SCM mode, SCM size);
SCM_API void scm_evict_ports (int fd);
SCM_API SCM scm_open_file (SCM filename, SCM modes);
SCM_API SCM scm_fdes_to_port (int fdes, char *mode, SCM name);
SCM_API SCM scm_file_port_p (SCM obj);
SCM_API void scm_init_fports (void);

/* internal functions */

SCM_API SCM scm_i_fdes_to_port (int fdes, long mode_bits, SCM name);
SCM_API int scm_i_fport_truncate (SCM, SCM);
SCM_API SCM scm_i_fport_seek (SCM, SCM, int);


#endif  /* SCM_FPORTS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
