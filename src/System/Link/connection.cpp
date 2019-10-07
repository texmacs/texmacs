
/******************************************************************************
* MODULE     : connection.cpp
* DESCRIPTION: TeXmacs connections
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* When the underlying link of a connection is "alive",
* then the status of the connection is either WAITING_FOR_OUTPUT
* (when waiting for output from the plugin) or WAITING_FOR_INPUT.
* If the underlying link is "dead", then the status is either
* CONNECTION_DEAD (usually) or CONNECTION_DYING (if we are still
* waiting for some residual output from the plugin).
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "connect.hpp"
#include "socket_notifier.hpp"
#include "iterator.hpp"
#include "convert.hpp"
#include "scheme.hpp"
#include "resource.hpp"
#include "Generic/input.hpp"
#include "gui.hpp"

static tree connection_retrieve (string name, string session);

/******************************************************************************
* The connection resource
******************************************************************************/

RESOURCE(connection);
struct connection_rep: rep<connection> {
  string  name;          // name of the pipe type
  string  session;       // name of the session
  tm_link ln;            // the underlying link
  int     status;        // status of the connection
  int     prev_status;   // last notified status
  bool    forced_eval;   // forced input evaluation without call backs
  texmacs_input tm_in;   // texmacs input handler for data from child
  texmacs_input tm_err;  // texmacs input handler for errors from child

public:
  connection_rep (string name, string session, tm_link ln);
  string start (bool again);
  void   write (string s);
  void   read (int channel);
  void   stop ();
  void   interrupt ();
  void   listen ();
};
RESOURCE_CODE(connection);

/******************************************************************************
* Routines for connections
******************************************************************************/

static void 
connection_callback (void *obj, void* info) {
  (void) info;
  //cout << "connection callback " << obj << LF;
  connection_rep *con = (connection_rep*) obj;
  con->listen ();
}


connection_rep::connection_rep (string name2, string session2, tm_link ln2):
  rep<connection> (name2 * "-" * session2),
  name (name2), session (session2), ln (ln2),
  status (CONNECTION_DEAD), prev_status (CONNECTION_DEAD),
  forced_eval (false),
  tm_in ("output"), tm_err ("error") {}

string
connection_rep::start (bool again) {
  string message;
  if (ln->alive) {
    message= "Continuation of '" * name * "' session";
    status = WAITING_FOR_INPUT;
  }
  else {
    message= ln->start ();
    tm_in  = texmacs_input ("output");
    tm_err = texmacs_input ("error");
    status = WAITING_FOR_OUTPUT;
    if (again && (message == "ok")) {
      beep ();
      (void) connection_retrieve (name, session);
    }
  }
  tm_in ->bof ();
  tm_err->bof ();
  ln->set_command (command (connection_callback, this));
  return message;
}

void
connection_rep::write (string s) {
  ln->write (s, LINK_IN);
  tm_in ->bof ();
  tm_err->bof ();
  status= WAITING_FOR_OUTPUT;
}

void
connection_rep::read (int channel) {
  if (channel == LINK_OUT) {
    string s= ln->read (LINK_OUT);
    int i, n= N(s);
    for (i=0; i<n; i++)
      if (tm_in->put (s[i])) {
        status= WAITING_FOR_INPUT;
        if (DEBUG_IO) debug_io << LF << HRULE;
      }
  }
  else if (channel == LINK_ERR) {
    string s= ln->read (LINK_ERR);
    int i, n= N(s);
    for (i=0; i<n; i++)
      (void) tm_err->put (s[i]);
  }
  if (!ln->alive) {
    tm_in ->eof ();
    tm_err->eof ();
    status= CONNECTION_DEAD;
  }
}

void
connection_rep::stop () {
  if (ln->alive) {
    ln->stop ();
    tm_in ->eof ();
    tm_err->eof ();
    if (status == WAITING_FOR_OUTPUT)
      status= CONNECTION_DYING;
  }
}

void
connection_rep::interrupt () {
  if (ln->alive) {
    ln->interrupt ();
    if (status == WAITING_FOR_OUTPUT)
      status= CONNECTION_DYING;
  }
}

/******************************************************************************
* Handle output from extern applications
******************************************************************************/

void
connection_notify (connection con, string ch, tree t) {
  if (t == "") return;
  call ("connection-notify",
        object (con->name),
        object (con->session),
        object (ch),
        object (t));
}

void
connection_notify_status (connection con) {
  int status=
    (con->status == CONNECTION_DYING? WAITING_FOR_OUTPUT: con->status);
  if (status == con->prev_status) return;
  call ("connection-notify-status",
        object (con->name),
        object (con->session),
        object (status));
  con->prev_status= status;
}

void
connection_rep::listen () {
  if (forced_eval) return;
  connection_notify_status (this);
  if (status != CONNECTION_DEAD) {
    read (LINK_ERR);
    connection_notify (this, "error", tm_err->get ("error"));
    read (LINK_OUT);
    connection_notify (this, "output", tm_in->get ("output"));
    connection_notify (this, "prompt", tm_in->get ("prompt"));
    connection_notify (this, "input", tm_in->get ("input"));
    tree t= connection_handlers (name);
    int i, n= N(t);
    for (i=0; i<n; i++) {
      tree doc= tm_in->get (t[i][0]->label);
      if (doc != "") call (t[i][1]->label, doc);
      doc= tm_err->get (t[i][0]->label);
      if (doc != "") call (t[i][1]->label, doc);
    }
  }
  connection_notify_status (this);  
}

/******************************************************************************
* Connection type information
******************************************************************************/

bool
connection_declared (string name) {
  return as_bool (call ("connection-defined?", name));
}

tree
connection_info (string name, string session) {
  return stree_to_tree (call ("connection-info", name, session));
}

tree
connection_handlers (string name) {
  static hashmap<string,tree> handlers (tuple ());
  if (!handlers->contains (name))
    handlers (name)= stree_to_tree (call ("connection-get-handlers", name));
  return handlers[name];
}

/******************************************************************************
* First part of interface (using a specific connection)
******************************************************************************/

string
connection_start (string name, string session, bool again) {
  // cout << "Start " << name << ", " << session << "\n";
  if (!connection_declared (name))
    return "Error: connection " * name * " has not been declared";

  connection con= connection (name * "-" * session);
  if (is_nil (con)) {
    if (DEBUG_VERBOSE)
      debug_io << "Starting session '" << session << "'\n";
    tree t= connection_info (name, session);
    if (is_tuple (t, "pipe", 1)) {
      tm_link ln= make_pipe_link (t[1]->label);
      con= tm_new<connection_rep> (name, session, ln);
    }
#ifndef QTTEXMACS
    else if (is_tuple (t, "socket", 2)) {
      tm_link ln= make_socket_link (t[1]->label, as_int (t[2]->label));
      con= tm_new<connection_rep> (name, session, ln);
    }
#endif
    else if (is_tuple (t, "dynlink", 3)) {
      tm_link ln=
        make_dynamic_link (t[1]->label, t[2]->label, t[3]->label, session);
      con= tm_new<connection_rep> (name, session, ln);
    }
  }

  return con->start (again);
}

void
connection_write (string name, string session, string s) {
  // cout << "Write " << name << ", " << session << ", " << s << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return;
  con->write (s);
}

void
connection_write (string name, string session, tree t) {
  // cout << "Write " << name << ", " << session << ", " << t << "\n";
  string s= as_string (call ("plugin-serialize", name, tree_to_stree (t)));
  connection_write (name, session, s);
}

tree
connection_read (string name, string session, string channel) {
  // cout << "Read " << name << ", " << session << ", " << channel << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return "";
  con->read (LINK_ERR);
  tree t= con->tm_err->get (channel);
  if (t == "") {
    con->read (LINK_OUT);
    t= con->tm_in->get (channel);
  }
  // cout << "Result " << t << "\n";
  return t;
}

void
connection_interrupt (string name, string session) {
  // cout << "Interrupt " << name << ", " << session << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return;
  con->interrupt ();
  con->listen ();
}

void
connection_stop (string name, string session) {
  // cout << "Stop " << name << ", " << session << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return;
  con->stop ();
  con->listen ();
}

int
connection_status (string name, string session) {
  // cout << "Status " << name << ", " << session << " -> ";
  connection con= connection (name * "-" * session);
  if ((!is_nil (con)) && (con->status == CONNECTION_DYING))
    return WAITING_FOR_OUTPUT;
  if (is_nil (con) || (!con->ln->alive)) return CONNECTION_DEAD;
  // cout << con->ln->status << "\n";
  return con->status;
}

/******************************************************************************
* Evaluation interface (using a specific connection)
******************************************************************************/

static connection
connection_get (string name, string session) {
  connection con= connection (name * "-" * session);
  if (is_nil (con)) {
    if (connection_start (name, session, true) != "ok") return con;
    con= connection (name * "-" * session);
  }
  return con;
}

static tree
connection_retrieve (string name, string session) {
  // cout << "Retrieve " << name << ", " << session << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return "";
  tree doc (DOCUMENT);
  while (true) {
    con->forced_eval= true;
#ifndef QTTEXMACS
    perform_select ();
#endif
    con->forced_eval= false;
    tree next= connection_read (name, session);
    if (next == "");
    else if (is_document (next)) doc << A (next);
    else doc << next;
    if (con->status == WAITING_FOR_INPUT) break;
  }
  if (N(doc) == 0) return "";
  // cout << "Retrieved " << doc << "\n";
  return doc;
}

tree
connection_eval (string name, string session, tree t) {
  // cout << "Evaluating " << name << ", " << session << ", " << t << LF;
  connection con= connection_get (name, session);
  if (is_nil (con)) return "";
  connection_write (name, session, t);
  return connection_retrieve (name, session);
}

tree
connection_eval (string name, string session, string s) {
  // cout << "Evaluating " << name << ", " << session << ", " << s << LF;
  connection con= connection_get (name, session);
  if (is_nil (con)) return "";
  connection_write (name, session, s);
  return connection_retrieve (name, session);
}

tree
connection_cmd (string name, string session, string cmd) {
  // cout << "Command " << name << ", " << session << ", " << cmd << LF;
  string s= as_string (call ("format-command", name, cmd));
  tree r= connection_eval (name, session, s);
  if (is_func (r, DOCUMENT, 1)) r= r[0];
  return r;
}
