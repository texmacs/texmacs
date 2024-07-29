
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
#include "string.hpp"

__stdcall unsigned bkgread(void *);

class Channel {
public:
  friend unsigned int bkgread(void *);
  enum Direction {CHOUT=0,CHIN=1};
  Channel (int s= 2048);
  void Init (int fd, Direction d);
  void Init (Direction d);
  void redirect ();
  inline int getPipe() const { return (toBeClosed); }
  void read (std::string *str);
  inline int write (void * data, int lenght) { 
    return (_write(fd, data, lenght)); }
  void close ();
  void closeUnused ();
  void wait();
  ~Channel();
  const int sz;
private:
  inline enum Direction otherDirection (Direction t) { 
    return(t==CHOUT?CHIN:CHOUT); }
  int toBeClosed;
  int origin;
  int saved;
  int fd;
  std::string *str;
  uintptr_t tid;
  void restore();
};


class spawn_system {
public:
  spawn_system(array<Channel> &ch, string name, array<string> args);
  inline int getpid() { return (pid); }
  int wait();
  inline bool isRunning() { return (pid?true:false); }
private:
  intptr_t pid;
  array<Channel> &channel;
};
