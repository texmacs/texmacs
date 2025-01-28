
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
#include <windows.h>
#include "array.hpp"
#include "windows64_spawn.hpp"
#include "windows64_system.hpp"

Channel::Channel (int s):sz(s) {
 origin= -1; saved= -1; fd= -1;toBeClosed= -1; str= NULL; tid= 0; 
}

void
Channel::Init (Direction d) {
  int pp[2];
  if (_pipe (pp, sz,O_NOINHERIT|_O_BINARY) == 0) {
    int pr[2];
    if (_pipe (pr, sz,_O_BINARY) == 0) {
      _close (pr[d]);
      _dup2 (pp[otherDirection(d)], pr[otherDirection(d)]);
      _close (pp[otherDirection(d)]);
      fd= pp[d];
      toBeClosed=  pr[otherDirection(d)];
    } else _close (pp[d]);_close (pp[otherDirection(d)]);
  }
}

void
Channel::Init (int _fd, Direction d) {
  int pp[2];
  origin= _fd;
  if (_pipe (pp, sz, O_NOINHERIT|_O_BINARY) == 0) {
    fd= pp[d];
    toBeClosed= pp[otherDirection(d)];
  } else origin= -1;
}

void
Channel::redirect () {
  if (origin < 0) return;
  saved= _dup (origin); 
  _dup2 (toBeClosed, origin);
}

void
Channel::read (std::string *_str) {
  str=_str;
  tid= _beginthreadex (NULL, 0, bkgread, this, 0, NULL);
}

void 
Channel::close () { 
  if(fd>=0) { _close (fd); fd= -1; }
}

void 
Channel::closeUnused () {
  if(toBeClosed>=0) { _close (toBeClosed); toBeClosed= -1;} 
  restore (); 
} 

void
Channel::wait () {
  if (tid && WaitForSingleObject ((HANDLE)tid, 5000) == 0 &&
    CloseHandle ((HANDLE)tid)) tid= 0;
}
  
void
Channel::restore() {
 if(saved>=0) { _dup2 (saved, origin); _close (saved); saved= -1; }
}


Channel::~Channel () { 
  closeUnused(); 
  close(); 
  if (tid) {
    TerminateThread ((HANDLE)tid, -99); 
    CloseHandle ((HANDLE)tid); 
  }
}

unsigned
bkgread (void *thatv) {
  char buf[1024];int cnt;
  Channel *that= (Channel *)thatv;
  do {
    cnt= _read (that->fd, buf, sizeof(buf));
    if (cnt > 0) *(that->str)+= std::basic_string<char> (buf, cnt);
    else if (cnt == 0) that->close ();
  } while (cnt > 0);
  return (cnt);
}

spawn_system::spawn_system (::array<Channel> &ch, string name, ::array< ::string> args):channel(ch) {
  for (int i=0; i < N(channel); ++i) channel[i].redirect();
  pid=texmacs_spawnvp (_P_NOWAIT,name, args);
  for (int i=0; i < N(channel); ++i) channel[i].closeUnused();
}

int
spawn_system::wait() {
  int ret;
  if(pid > 0) ret= _cwait(&ret, pid, 0)==-1?errno:ret;
  else ret= EINVAL;
  for (int i=0; i < N(channel); ++i) channel[i].wait();
  return (ret);
}
