#include <errno.h>
#include "array.hpp"
#include "spawn.hpp"

void Channel::Init (Direction d) {
	int pp[2];
	if(_pipe (pp, sz,_O_BINARY) == 0) {
		fd= pp[d];
		toBeClosed=  pp[otherDirection(d)];
	} else fd= toBeClosed= 0;
}

void Channel::Init (int _fd, Direction d) {
	int pp[2];
	origin= _fd; saved= _dup(_fd);
	if(_pipe (pp, sz, O_NOINHERIT|_O_BINARY) == 0) {
		fd= pp[d];
		toBeClosed= pp[otherDirection(d)];
		_dup2(toBeClosed, _fd);
	} else { fd= -1; toBeClosed= -1; saved= -1;}
}

spawn_system::spawn_system (Channel *channel[], char *name, const char *const *args) {
	pid=_spawnvp(_P_NOWAIT,name, args);
	for(Channel **ch=channel; *ch; ++ch) (*channel)->closeUnused(); 
}
spawn_system::spawn_system(array<Channel> channel, char *name, const char *const *args) {
	pid=_spawnvp(_P_NOWAIT,name, args);
	for(int i=0; i < N(channel); ++i) channel[i].closeUnused();
}
int spawn_system::wait() {
	int ret;
	if(pid > 0) return(_cwait(&ret, pid, 0)==-1?errno:ret);
	else return(EINVAL);
}
