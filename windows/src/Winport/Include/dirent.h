/******************************************************************************
* MODULE     : dirent.h
* DESCRIPTION: Windows version of POSIX dirent.h
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef DIRENT_H
#define DIRENT_H

#include <sys/_types.h>

#ifndef _WINBASE_

#define MAX_PATH 260

typedef struct _FILETIME {
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
} FILETIME;

typedef struct _WIN32_FIND_DATA {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    CHAR   cFileName[ MAX_PATH ];
    CHAR   cAlternateFileName[ 14 ];
} WIN32_FIND_DATA;

#endif

struct dirent { 
	long d_ino; /* unique file number */
	off_t d_off; /*offset of this file within the director*/
	unsigned short d_reclen; /*length of this entry*/
	char d_name[MAX_PATH]; /*name of the entry*/
}; 

typedef struct DIR_{
	bool isOpen;
	bool findDataValid;
	HANDLE findHandle;
	int position;
	WIN32_FIND_DATA findData;
	char d_name[MAX_PATH];
}DIR;

int            closedir(DIR *);
DIR           *opendir(const char *);
struct dirent *readdir(DIR *);
int            readdir_r(DIR *, struct dirent *, struct dirent **);
void           rewinddir(DIR *);
void           seekdir(DIR *, long int);
long int       telldir(DIR *);

#endif