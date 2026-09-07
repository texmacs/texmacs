
/******************************************************************************
* MODULE     : request_link.cpp
* DESCRIPTION: TeXmacs links by http post
* COPYRIGHT  : (C) 2026  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "basic.hpp"
#include "tm_link.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "tm_timer.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include "convert.hpp"
#include "web_files.hpp"

hashset<pointer> request_link_set;
void request_callback (void *obj, void *info);

/******************************************************************************
* The request_link class
******************************************************************************/

struct request_link_rep: tm_link_rep {
  string name;          // name of the plugin
  int    status;        // negative= error, null= EOF, positive means data
  string outbuf;        // pending output from plugin
  string errbuf;        // pending errors from plugin
  bool   kill;          // the request can be cancelled
  
public:
  request_link_rep (string name);
  ~request_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();

  void    feed (int channel);
};

request_link_rep::request_link_rep (string name2): name (name2) {
  request_link_set->insert ((pointer) this);
  status = 1;
  outbuf = "";
  errbuf = "";
  kill   = false;
  alive  = false;
}

request_link_rep::~request_link_rep () {
  stop ();
  request_link_set->remove ((pointer) this);
}

tm_link
make_request_link (string name) {
  return tm_new<request_link_rep> (name);
}

void
close_all_requests () {
  iterator<pointer> it= iterate (request_link_set);
  while (it->busy()) {
    request_link_rep* con= (request_link_rep*) it->next();
    if (con->alive) {
      // kill actual request (or wait that it dies)
      con->alive= false;
      con->kill= true;
    }
  }
}

void
process_all_requests () {
  iterator<pointer> it= iterate (request_link_set);
  while (it->busy()) {
    request_link_rep* con= (request_link_rep*) it->next();
    //cout << con->name << " ~> " << (con->alive? "true": "false") << "\n";
    if (con->alive)
      con->apply_command ();
  }
}

/******************************************************************************
* Routines for request_links
******************************************************************************/

string
request_link_rep::start () {
  status= 1;
  outbuf= "";
  errbuf= "";
  kill= false;
  return "request";
}

static bool
eval_request (tree t, int& status,
	      string& outbuf, string& errbuf, bool& kill) {
  // cout << "eval_request, " << t << LF;
  if (is_compound (t, "http_post", 3) && is_atomic (t[0])
      && is_tuple (t[1])) {
    string url= t[0]->label;
    tree data= t[2];
    array<string> headers;
    for (int i= 0; i < N(t[1]); i++)
      if (is_atomic (t[1][i])) headers << t[1][i]->label;
    return async_http_post_json (url, headers, data,
				 status, outbuf, errbuf, kill);
  }
  io_error << "request_link, unexpected request: " << t << LF;
  return true;
}

void
request_link_rep::write (string s, int channel) {
  // cout << "Write[" << name << "] " << s << "\n";
  if (alive || (channel != LINK_IN)) return;
  string cmd= as_string (call ("connection-request", name, "default", s));
  // cout << "Command[" << name << "," << s << "] = " << cmd << "\n";
  if (cmd == "") {
    status= 0; outbuf= ""; errbuf= ""; kill= false;
    alive= false;
    return;
  }
  tree t= scheme_to_tree (cmd);
  if (DEBUG_IO) debug_io << "Requesting '" << t << "'\n";
  status= 1; outbuf= ""; errbuf= ""; kill= false;
  alive= !eval_request (t, status, outbuf, errbuf, kill);
}

void
request_link_rep::feed (int channel) {
  // cout << "Feed " << channel << "\n";
  if ((!alive) || ((channel != LINK_OUT) && (channel != LINK_ERR))) return;
  if (status < 0)
    io_error << "Read failed for '" << name << "'\n";
  if (status <= 0) 
    alive= false;
}

string&
request_link_rep::watch (int channel) {
  // cout << "Watch " << channel << "\n";
  static string empty_string= "";
  if (channel == LINK_OUT) return outbuf;
  else if (channel == LINK_ERR) return errbuf;
  else return empty_string;
}

string
request_link_rep::read (int channel) {
  // cout << "Read " << channel << "\n";
  if (channel == LINK_OUT) {
    if (alive) {
      // cout << "\n--- partial output ---\n" << outbuf << "\n";
      return "";
    }
    string r= outbuf;
    outbuf= "";
    return r;
  }
  else if (channel == LINK_ERR) {
    string r= errbuf;
    errbuf= "";
    return r;
  }
  else return string ("");
}

void
request_link_rep::listen (int msecs) {
  (void) msecs;
  if (!alive) return;
  feed (LINK_OUT);
  feed (LINK_IN);
}

void
request_link_rep::interrupt () {
  if (!alive) return;
  alive= false;
  kill= true;
}

void
request_link_rep::stop () {
  if (!alive) return;
  alive= false;    
  kill= true;
}

/******************************************************************************
* Call back for new information on pipe
******************************************************************************/

void request_callback (void *obj, void *info) {
  (void) info;
  request_link_rep* con= (request_link_rep*) obj;  
  if (!is_nil (con->feed_cmd)) {
    // cout << "request_callback applies" << LF;
    con->feed_cmd->apply (); // call the data processor
  }
}
