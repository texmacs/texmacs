/******************************************************************************
* MODULE     : sysmisc.h
* DESCRIPTION: Windows version of various POSIX function
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef SYSMISC_H
#define SYSMISC_H

#include <sys/_types.h>
#include <sys/socket.h>

typedef int pid_t;

typedef enum OS_VERSION_{
	OS_VERSION_UNSUPPORTED = 0,
	OS_VERSION_WIN_95,
	OS_VERSION_WIN_98,
	OS_VERSION_WIN_ME,
	OS_VERSION_WIN_XP,
	OS_VERSION_WIN_2000,
	OS_VERSION_WIN_SERVER_2003,
	OS_VERSION_WIN_NT351,
	OS_VERSION_WIN_NT40,
}OS_VERSION;

int _system(char *command);
int gettimeofday(struct timeval *tp, void *tzp);
int setenv(const char *name, const char *value, int rewrite);
char* getenv(char *name);
char* GetWorkingDirectory(); 
char* ExpandEnvString(char *toExpand);
OS_VERSION GetOsVersion();
int _system(char *command);
void ConvertPathing(char *toConvert);
bool SysMiscInitialize();

int select(int n,fd_set *readfds,fd_set *writefds,
       fd_set *exceptfds, timeval *timeout);
int execve(const char *filename,char *const argv [],
		   char *const envp[]);
int pipe(int *fildes);
int fork();
int close(int fildes);
size_t read(int d, void *buf, size_t nbytes);
int wait(int *status);
size_t write(int d, const void *buf, size_t nbytes);
int kill(pid_t pid, int sig);
int fcntl(int fd, int cmd, long arg);

#define srandom srand
#define random rand

#endif