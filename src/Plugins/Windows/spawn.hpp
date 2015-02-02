#include <fcntl.h>
#include <io.h>
#include <process.h>

using namespace std;

class Channel {
public:
	enum Direction {CHOUT=0,CHIN=1};
	Channel(int s= 2048):sz(s) {saved= -1; fd= -1;toBeClosed= 0;}
	void Init (int fd, Direction d);
	void Init (Direction d);
	int getPipe() const { return(toBeClosed); }
	int eofread(void * data, int lenght) { return(_eof(fd)==1?0:_read(fd, data, lenght)); }
	int read(void * data, int lenght) {return( _read(fd, data, lenght));}
	int write(void * data, int lenght) {return(_write(fd, data, lenght));}
	void close() { if(fd>=0) { _close(fd); fd= -1; }}
	void closeUnused() { if(toBeClosed>0) { _close(toBeClosed); toBeClosed = -1; };restore();} 
	~Channel() { restore(); closeUnused(); close(); }
	const int sz;
private:
	enum Direction otherDirection (Direction t) { return(t==CHOUT?CHIN:CHOUT); }
	int toBeClosed;
	int origin;
	int saved;
	int fd;
	void restore() { if(saved>=0) { _dup2(saved, origin); saved= -1; }}
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
