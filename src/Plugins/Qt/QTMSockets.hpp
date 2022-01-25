
/******************************************************************************
* MODULE     : QTMSockets.hpp
* DESCRIPTION: QT TeXmacs sockets manager - Header
* COPYRIGHT  : (C) 2015 Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdint.h>
#include <QApplication>
#include <QObject>
#include <QThread>
#include <QSocketNotifier>

#include "hashset.hpp"
#include "string.hpp"
#include "tm_link.hpp"

#ifndef OS_MINGW

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define SOCKADDR_IN sockaddr_in
#define SOCKADDR_IN6 sockaddr_in6
#define SOCKADDR sockaddr
#define SOCKADDR_STORAGE sockaddr_storage

#else

namespace wsoc {
#include <winsock2.h>
#include <ws2tcpip.h>
}
typedef uint32_t in_addr_t;
typedef int socklen_t;
#define SOCKADDR_IN wsoc::sockaddr_in
#define SOCKADDR_IN6 wsoc::sockaddr_in6
#define SOCKADDR wsoc::sockaddr
#define SOCKADDR_STORAGE wsoc::sockaddr_storage

#endif

// Common structures fors sockets

extern unsigned qtmsocket_debug_count;

string debug_io_string (string s);

#define DBG_IO(a) \
  if (DEBUG_IO) debug_io << "TeXmacs " \
	<< qtmsocket_debug_count++ << "] " << a << "\n"

#define DBG_IOS(a,s) \
   if(N(s)) DBG_IO (a << debug_io_string (s))

enum state { ST_OK, ST_WSA, ST_SOCKET, ST_FCNTL, ST_BIND,
	     ST_LISTEN, ST_CONNECTION, ST_GETHOST, ST_NOTIF,
	     ST_VOID, ST_HALTED, ST_CLOSED };

class socket_basic {
public:
  bool alive () { return st == ST_OK; }
protected:
  socket_basic(void);
  ~socket_basic();
  int sock;
  int err;
  enum state st;
private:
  static int count;
#ifdef OS_MINGW
  static wsoc::WSADATA wsadata;
#endif
};

// Socket for clients

class socket_link: public QObject, public socket_basic, public tm_link_rep {
  Q_OBJECT

public:
  socket_link (int s, SOCKADDR_STORAGE* addr);
  socket_link (string host, unsigned short port=6561);
  ~socket_link ();
  string  start ();
  void    write (string s, int channel=LINK_OUT);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt () {}
  void    stop ();
  bool    alive () { return socket_basic::alive(); }
  int     getid () { return id; }

public slots:
  void data_set_ready (int);
  void ready_to_send (int);
signals:
  void disconnection (socket_link* clt);
private :
  int id;
  string inbuf;
  string outbuf;
  QSocketNotifier *qsnr,*qsnw;
  SOCKADDR_STORAGE add; 
};

// Socket for servers

class socket_server: public QObject, public socket_basic {
  Q_OBJECT

public:
  socket_server (string host, unsigned short port=6561);
  ~socket_server();
  string read (int clt);
  void write (int clt, string s);
  enum state st;
  int err;
  int srv_count() { return N(clts); }
public slots:
  void connection (int);
  void disconnection (socket_link* clt);
private :
  socket_link* find_client (int id);
  hashset<pointer> clts;
  QSocketNotifier *qsnc;
};
