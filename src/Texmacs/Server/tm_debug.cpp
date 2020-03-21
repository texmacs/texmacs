
/******************************************************************************
* MODULE     : tm_debug.cpp
* DESCRIPTION: Debugging facilities
* COPYRIGHT  : (C) 2011  Joris van der Hoeven
*              (C) 2008  Timo Bingmann from http://idlebox.net
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_server.hpp"
#include "file.hpp"
#include "tm_link.hpp"
#include "sys_utils.hpp"

bool rescue_mode= false;

/******************************************************************************
* Status reports
******************************************************************************/

string
get_system_information () {
  string r;
  r << "System information:\n";
  r << "  TeXmacs version  : "
    << TEXMACS_VERSION << "\n";
  r << "  Built by         : "
    << BUILD_USER << "\n";
  r << "  Building date    : "
    << BUILD_DATE << "\n";
  r << "  Operating system : "
    << HOST_OS << "\n";
  r << "  Vendor           : "
    << HOST_VENDOR << "\n";
  r << "  Processor        : "
    << HOST_CPU << "\n";
  r << "  Crash date       : "
    << var_eval_system ("date") << "\n";
  return r;
}

string
path_as_string (path p) {
  if (is_nil (p)) return "[]";
  string r= "[ ";
  r << as_string (p->item);
  p= p->next;
  while (!is_nil (p)) {
    r << ", " << as_string (p->item);
    p= p->next;
  }
  r << " ]";
  return r;
}

string
get_editor_status_report () {
  string r;

  if (!is_server_started ()) {
    r << "TeXmacs server not yet started";
    return r;
  }

  // If an error happens too early then there is no current view
  // and get_current_editor() will raise an exception leading to
  // an infinite loop. So we stop before.
  
  if (!has_current_view()) {
    r << "TeXmacs has not a current view";
    return r;
  }
  
  server sv= get_server ();
  r << "Editor status:\n";
  editor ed= get_current_editor ();
  path start_p, end_p;
  ed->get_selection (start_p, end_p);
  r << "  Root path          : "
    << path_as_string (ed->rp) << "\n"
    << "  Current path       : "
    << path_as_string (ed->the_path ()) << "\n"
    << "  Shifted path       : "
    << path_as_string (ed->the_shifted_path ()) << "\n"
    << "  Physical selection : "
    << path_as_string (start_p) << " -- "
    << path_as_string (end_p) << "\n";
  if (start_p != end_p) {
    selection sel;
    ed->selection_get (sel);
    r << "  Logical selection  : "
      << path_as_string (sel->start) << " -- "
      << path_as_string (sel->end) << "\n";
  }
  return r;
}

void
tree_report (string& s, tree t, path p, int indent) {
  for (int i=0; i<indent; i++) s << " ";
  if (is_atomic (t)) {
    s << raw_quote (t->label);
    s << " -- " << path_as_string (p) << "\n";
  }
  else {
    s << as_string (L(t));
    s << " -- " << path_as_string (p) << "\n";
    for (int i=0; i<N(t); i++)
      tree_report (s, t[i], p * i, indent+2);
  }
}

string
tree_report (tree t, path p) {
  string s;
  tree_report (s, t, p, 0);
  return s;
}

/******************************************************************************
* Crash management
******************************************************************************/

string
get_crash_report (const char* msg) {
  string r;
  r << "Error message:\n  " << msg << "\n"
    << "\n" << get_system_information ()
//<< "\n" << get_editor_status_report ()
    << "\n" << get_stacktrace ();
  return r;
}

void
tm_failure (const char* msg) {
  if (rescue_mode) {
    fprintf (stderr, "\nTeXmacs] Fatal unrecoverable error, %s\n", msg);
#ifdef DEBUG_ASSERT
    return;
#endif
    exit (1);
  }
  rescue_mode= true;
  cerr << "\nTeXmacs] Fatal error, " << msg << "\n";

  //cerr << "Saving crash report...\n";
  string report= get_crash_report (msg);
  url dir ("$TEXMACS_HOME_PATH/system/crash");
  url err= url_numbered (dir, "crash_report_", "");
  if (!save_string (err, report))
    cerr << "TeXmacs] Crash report saved in " << err << "\n";
  else
    cerr << "TeXmacs] Crash report could not be saved in "
         << err << "\n"
         << "TeXmacs] Dumping report below\n\n"
         << report << "\n";

  //cerr << "Saving current buffer...\n";
  server sv= get_server ();
  editor ed= get_current_editor ();
  string buf= tree_report (subtree (the_et, ed->rp), ed->rp);
  url buf_err= glue (err, "_tree");
  if (!save_string (buf_err, buf))
    cerr << "TeXmacs] Current buffer report saved in " << buf_err << "\n";
  else
    cerr << "TeXmacs] Current buffer report could not be saved in "
         << buf_err << "\n"
         << "TeXmacs] Dumping report below\n\n"
         << buf << "\n";

  //cerr << "Autosaving...\n";
  call ("autosave-all");
  //cerr << "Closing pipes...\n";
  close_all_pipes ();
  call ("quit-TeXmacs-scheme");
  clear_pending_commands ();
  //exit (1);
}
