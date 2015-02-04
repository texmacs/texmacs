/******************************************************************************
* MODULE     : spawn.cpp
* DESCRIPTION: external command handling
* COPYRIGHT  : (C) 2015 Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <errno.h>
#include "array.hpp"
#include "spawn.hpp"
#include <windows.h>

	void Channel::Init (Direction d) {
	int pp[2];
	if(_pipe (pp, sz,O_NOINHERIT|_O_BINARY) == 0) {
		int pr[2];
		if(_pipe (pr, sz,_O_BINARY) == 0) {
			_close (pr[d]);
			_dup2 (pp[otherDirection(d)], pr[otherDirection(d)]);
			_close (pp[otherDirection(d)]);
			fd= pp[d];
			toBeClosed=  pr[otherDirection(d)];
		} else _close (pp[d]);_close (pp[otherDirection(d)]);
	};
}

void Channel::Init (int _fd, Direction d) {
	int pp[2];
	origin= _fd;
	if(_pipe (pp, sz, O_NOINHERIT|_O_BINARY) == 0) {
		fd= pp[d];
		toBeClosed= pp[otherDirection(d)];
	} else origin= -1;
}

void Channel::redirect() {
	if(origin < 0) return;
	saved= _dup (origin); 
	_dup2 (toBeClosed, origin);
}

void Channel::read(std::string *_str) {
	str=_str;
	tid= _beginthreadex (NULL, 0, bkgread, this, 0, NULL);
}

int Channel::wait() {
	int ret;
	ret= WaitForSingleObject ((HANDLE)tid, 1000); 
	CloseHandle((HANDLE)tid);
	tid= -1;
	return(ret);
}


 unsigned bkgread(void *thatv) {
	char buf[1024];int cnt;
	Channel *that= (Channel *)thatv;
	do {
		cnt= _read (that->fd, buf, sizeof(buf));
		if(cnt > 0) *(that->str)+= basic_string<char>(buf, cnt);
		else if(cnt == 0) that->close ();
	} while(cnt > 0);
	return (cnt);
}
spawn_system::spawn_system (Channel *channel[], char *name, const char *const *args) {
	for(Channel **ch=channel; *ch; ++ch) (*channel)->redirect(); 
	pid=_spawnvp (_P_NOWAIT,name, args);
	for(Channel **ch=channel; *ch; ++ch) (*channel)->closeUnused(); 
}
spawn_system::spawn_system (array<Channel> channel, char *name, const char *const *args) {
	for(int i=0; i < N(channel); ++i) channel[i].redirect();
	pid=_spawnvp (_P_NOWAIT,name, args);
	for(int i=0; i < N(channel); ++i) channel[i].closeUnused();
}
int spawn_system::wait() {
	int ret;
	if(pid > 0) return (_cwait(&ret, pid, 0)==-1?errno:ret);
	else return (EINVAL);
}
