/* classes: h_files */

#ifndef SCM_WIN32_UNAME_H
#define SCM_WIN32_UNAME_H

/* Copyright (C) 2001, 2006 Free Software Foundation, Inc.
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

#define _UTSNAME_LENGTH 65
#define _UTSNAME_NODENAME_LENGTH _UTSNAME_LENGTH
#define _UTSNAME_DOMAIN_LENGTH _UTSNAME_LENGTH

/* Structure describing the system and machine.  */
struct utsname
{
  /* Name of the implementation of the operating system.  */
  char sysname[_UTSNAME_LENGTH];

  /* Name of this node on the network.  */
  char nodename[_UTSNAME_NODENAME_LENGTH];

  /* Current release level of this implementation.  */
  char release[_UTSNAME_LENGTH];

  /* Current version level of this release.  */
  char version[_UTSNAME_LENGTH];

  /* Name of the hardware type the system is running on.  */
  char machine[_UTSNAME_LENGTH];

  /* Name of the domain of this node on the network.  */
  char domainname[_UTSNAME_DOMAIN_LENGTH];
};

int uname (struct utsname * uts);

#endif /* SCM_WIN32_UNAME_H */
