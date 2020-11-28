
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

#define NMSPC(a) a
#define SOCKADDR_IN sockaddr_in
#define SOCKADDR sockaddr
#else
namespace wsoc {
#include <winsock2.h>
#include <ws2tcpip.h>
}
#define NMSPC(a) wsoc::a
typedef ushort u_short;
typedef ulong u_long;
typedef uint32_t in_addr_t;
typedef int  socklen_t;
#define SOCKADDR_IN wsoc::sockaddr_in
#define SOCKADDR wsoc::sockaddr
#endif

extern unsigned dbg_cnt;
string debug_io_string (string s);
#define DBG_IO(a) \
  if (DEBUG_IO) debug_io << "TeXmacs-" << dbg_cnt++<<"] " << a << "\n"
#define DBG_IOS(a,s) if(N(s)) DBG_IO (a << debug_io_string (s))

enum state { ST_OK, ST_WSA, ST_SOCKET, ST_FCNTL, ST_BIND,
	     ST_LISTEN, ST_CONNECTION, ST_GETHOST, ST_NOTIF,
	     ST_VOID, ST_HALTED, ST_CLOSED };
typedef int IdClt;

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
  
class socket_link: public QObject, public socket_basic, public tm_link_rep {
  Q_OBJECT

public:
  socket_link (int s,struct SOCKADDR_IN *addr);
  socket_link (string host, u_short port=6561);
  ~socket_link ();
  string  start ();
  void    write (string s, int channel=LINK_OUT);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt () {}
  void    stop ();
  bool    alive () { return socket_basic::alive(); }
  IdClt   getid () { return id; }

public slots:
  void data_set_ready (int);
  void ready_to_send (int);
signals:
  void disconnection (class socket_link * clt);
private :
  static IdClt id;
  string inbuf;
  string outbuf;
  QSocketNotifier *qsnr,*qsnw;
  struct SOCKADDR_IN add; 
};


class socket_server: public QObject, public socket_basic {
  Q_OBJECT

public:
  socket_server(in_addr_t add, u_short port=6561);
  ~socket_server();
  string read (IdClt clt);
  void write (IdClt clt, string s);
  enum state st;
  int err;
  int srv_count() { return N(clts); }
public slots:
  void connection (int);
  void disconnection (socket_link* clt);
private :
  socket_link* find_client (IdClt id);
  hashset<pointer> clts;
  QSocketNotifier *qsnc;
};
