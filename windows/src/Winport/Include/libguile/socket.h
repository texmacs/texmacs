/* classes: h_files */

#ifndef SOCKETH
#define SOCKETH
/*	Copyright (C) 1995, 1996, 1997, 2000 Free Software Foundation, Inc.
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



GUILE_API extern SCM scm_htons (SCM in);
GUILE_API extern SCM scm_ntohs (SCM in);
GUILE_API extern SCM scm_htonl (SCM in);
GUILE_API extern SCM scm_ntohl (SCM in);
GUILE_API extern SCM scm_socket (SCM family, SCM style, SCM proto);
GUILE_API extern SCM scm_socketpair (SCM family, SCM style, SCM proto);
GUILE_API extern SCM scm_getsockopt (SCM sfd, SCM level, SCM optname);
GUILE_API extern SCM scm_setsockopt (SCM sfd, SCM level, SCM optname, SCM value);
GUILE_API extern SCM scm_shutdown (SCM sfd, SCM how);
GUILE_API extern SCM scm_connect (SCM sockfd, SCM fam, SCM address, SCM args);
GUILE_API extern SCM scm_bind (SCM sockfd, SCM fam, SCM address, SCM args);
GUILE_API extern SCM scm_listen (SCM sfd, SCM backlog);
GUILE_API extern SCM scm_accept (SCM sockfd);
GUILE_API extern SCM scm_getsockname (SCM sockfd);
GUILE_API extern SCM scm_getpeername (SCM sockfd);
GUILE_API extern SCM scm_recv (SCM sockfd, SCM buff_or_size, SCM flags);
GUILE_API extern SCM scm_send (SCM sockfd, SCM message, SCM flags);
GUILE_API extern SCM scm_recvfrom (SCM sockfd, SCM buff_or_size, SCM flags, SCM offset, SCM length);
GUILE_API extern SCM scm_sendto (SCM sockfd, SCM message, SCM fam, SCM address, SCM args_and_flags);
GUILE_API extern void scm_init_socket (void);

#endif  /* SOCKETH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
