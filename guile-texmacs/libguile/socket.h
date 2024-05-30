/* classes: h_files */

#ifndef SCM_SOCKET_H
#define SCM_SOCKET_H

/* Copyright (C) 1995,1996,1997,2000,2001, 2004, 2005, 2006 Free Software Foundation, Inc.
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



SCM_API SCM scm_htons (SCM in);
SCM_API SCM scm_ntohs (SCM in);
SCM_API SCM scm_htonl (SCM in);
SCM_API SCM scm_ntohl (SCM in);
SCM_API SCM scm_inet_aton (SCM address);
SCM_API SCM scm_inet_ntoa (SCM inetid);
SCM_API SCM scm_inet_netof (SCM address);
SCM_API SCM scm_lnaof (SCM address);
SCM_API SCM scm_inet_makeaddr (SCM net, SCM lna);
SCM_API SCM scm_inet_pton (SCM family, SCM address);
SCM_API SCM scm_inet_ntop (SCM family, SCM address);
SCM_API SCM scm_socket (SCM family, SCM style, SCM proto);
SCM_API SCM scm_socketpair (SCM family, SCM style, SCM proto);
SCM_API SCM scm_getsockopt (SCM sfd, SCM level, SCM optname);
SCM_API SCM scm_setsockopt (SCM sfd, SCM level, SCM optname, SCM value);
SCM_API SCM scm_shutdown (SCM sfd, SCM how);
SCM_API SCM scm_connect (SCM sockfd, SCM fam, SCM address, SCM args);
SCM_API SCM scm_bind (SCM sockfd, SCM fam, SCM address, SCM args);
SCM_API SCM scm_listen (SCM sfd, SCM backlog);
SCM_API SCM scm_accept (SCM sockfd);
SCM_API SCM scm_getsockname (SCM sockfd);
SCM_API SCM scm_getpeername (SCM sockfd);
SCM_API SCM scm_recv (SCM sockfd, SCM buff_or_size, SCM flags);
SCM_API SCM scm_send (SCM sockfd, SCM message, SCM flags);
SCM_API SCM scm_recvfrom (SCM sockfd, SCM buff_or_size, SCM flags, SCM offset, SCM length);
SCM_API SCM scm_sendto (SCM sockfd, SCM message, SCM fam, SCM address, SCM args_and_flags);
SCM_API void scm_init_socket (void);

/* Wrapping/unwrapping address objects.  */
struct sockaddr;
SCM_API SCM scm_from_sockaddr (const struct sockaddr *address,
			       unsigned addr_size);
SCM_API struct sockaddr *scm_to_sockaddr (SCM address, size_t *adress_size);
SCM_API struct sockaddr *scm_c_make_socket_address (SCM family, SCM address,
						    SCM args,
						    size_t *address_size);
SCM_API SCM scm_make_socket_address (SCM family, SCM address, SCM args);

#endif  /* SCM_SOCKET_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
