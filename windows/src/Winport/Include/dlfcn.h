/******************************************************************************
* MODULE     : dlfcn.h
* DESCRIPTION: Windows version of POSIX dlfcn.h
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef DLFCN_H
#define DLFCN_H

/*dynamic linking....easy to port these system calls*/
#define RTLD_LAZY	1
#define RTLD_NOW	2
#define RTLD_GLOBAL	3
#define RTLD_LOCAL	4

void *dlopen(const char *file, int mode);
void *dlsym(void *handle, const char *name);
char *dlerror(void);
void dlclose(void *handle);

#endif