/******************************************************************************
* MODULE     : spawn.hpp
* DESCRIPTION: external command handling
* COPYRIGHT  : (C) 2015  Gregoire LECERF, Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <fcntl.h>
#include <io.h>
#include <process.h>
#include <string>
//#include "string.hpp"

using namespace std;

__stdcall unsigned bkgread(void *);

class Channel {
public:
	friend unsigned int bkgread(void *);
	enum Direction {CHOUT=0,CHIN=1};
	Channel(int s= 2048):sz(s) {origin= -1; saved= -1; fd= -1;toBeClosed= -1; str= NULL; tid=0; }
	void Init (int fd, Direction d);
	void Init (Direction d);
	void redirect ();
	int getPipe() const { return(toBeClosed); }
	void read(std::string *str);
	int write(void * data, int lenght) {return (_write(fd, data, lenght));}
	void close() { if(fd>=0) { _close (fd); fd= -1;}}
	void closeUnused() { if(toBeClosed>=0) {_close (toBeClosed); toBeClosed= -1;} restore ();} 
	int wait();
	~Channel() { closeUnused(); close(); }
	const int sz;
private:
	enum Direction otherDirection (Direction t) { return(t==CHOUT?CHIN:CHOUT); }
	int toBeClosed;
	int origin;
	int saved;
	int fd;
	std::string *str;
	uintptr_t tid;
	void restore() { if(saved>=0) { _dup2(saved, origin); _close(saved); saved= -1; }}
};


class spawn_system {
public:
	spawn_system(Channel *channel[], char *name, const char *const *args);
	spawn_system(array<Channel> channel, char *name, const char *const *args);
	int getpid() { return(pid); }
	int wait();
	bool isRunning() { return(pid?true:false); }
private:
	intptr_t pid;
};
