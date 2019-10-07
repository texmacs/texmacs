
/******************************************************************************
* MODULE     : dyn_link.cpp
* DESCRIPTION: Dynamic linking of extern routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "dyn_link.hpp"
#include "url.hpp"
#include "hashmap.hpp"
#ifndef OS_MINGW
#include <dlfcn.h>
#endif
#include <TeXmacs.h>

static hashmap<string,pointer> dyn_linked (NULL);

/******************************************************************************
* Linking symbols from dynamic libraries
******************************************************************************/

string
symbol_install (string lib, string symb, pointer& f) {
#if (defined (TM_DYNAMIC_LINKING) && !defined(OS_MINGW))
  // f becomes NULL in case of failure
  // status message returned
  string out;

  if (!dyn_linked->contains (lib)) {
    url name= resolve (url ("$LD_LIBRARY_PATH", lib));
    if (is_none (name)) out= "Library '" * lib * "' not found";
    else {
      lib= concretize (name);
      c_string _lib (lib);
      dyn_linked (lib)= TM_DYNAMIC_LINKING (_lib, RTLD_LAZY);
      if (dyn_linked [lib] == NULL) {
        const char *err = dlerror();
        if (err != NULL) out= string ((char *) err);
      }
    }
  }

  pointer handle= dyn_linked [lib];
  if (handle) {
    c_string _symb (symb);
    string tag= lib * ":" * symb;
    if (!dyn_linked->contains (tag))
      dyn_linked (tag)= dlsym (handle, _symb);
    f= dyn_linked [tag];
    if (f != NULL) out= "Dynamically linked symbol '" * symb * "'";
    else out= "Can not find symbol '" * symb * "' in  '" * lib * "'";
  }
  else {
    f= NULL;
    if (out == "") out= "Couldn't find dynamic library '" * lib * "'";
  }

  if (DEBUG_AUTO) debug_automatic << out << "\n";
  return out;
#else
  return "Dynamic linking not implemented";
#endif
}

string
symbols_install (string lib, string* symb, pointer* f, int n) {
#ifndef OS_MINGW
  int i;
  for (i=0; i<n; i++) f[i]= NULL;
  for (i=0; i<n; i++) {
    string message= symbol_install (lib, symb[i], f[i]);
    if (f[i] == NULL) return message;
  }
  return "Symbols installed for library '" * lib * "'";
#else
  return "Dynamic linking not implemented";
#endif
}

/******************************************************************************
* Dynamic links
******************************************************************************/

dyn_link_rep::dyn_link_rep (string l, string s, string i, string ses):
  lib (l), symbol (s), init (i), routs (NULL), session (ses)
{
  alive= false;
}

dyn_link_rep::~dyn_link_rep () {
  // FIXME: should we 'unlink' the package?
}

tm_link
make_dynamic_link (string lib, string symb, string init, string session) {
  return tm_new<dyn_link_rep> (lib, symb, init, session);
}

static TeXmacs_exports_1 TeXmacs= {
  const_cast<char*> ("TeXmacs communication protocol 1"),
  const_cast<char*> ("TeXmacs " TEXMACS_VERSION),
};

string
dyn_link_rep::start () {
#ifndef OS_MINGW
  string name= lib * ":" * symbol * "-package";
  if (dyn_linked->contains (name))
    routs= dyn_linked [name];
  if (routs != NULL)
    return "continuation of#'" * lib * "'";
  if (DEBUG_AUTO)
    debug_automatic << "Installing dynamic link '" << lib << "'\n";

  string message= symbol_install (lib, symbol, routs);
  if (routs != NULL) {
    dyn_linked (name)= routs;
    package_exports_1* pack= (package_exports_1*) routs;
    c_string _init (init);
    char* _errors= NULL;
    char* _message= pack->install (&TeXmacs, _init, &_errors);
    if (_errors != NULL) {
      routs= NULL;
      ret= "Error: " * string (_errors);
    }
    else {
      ret= string (_message == NULL? ((char*) ""): _message);
      alive= true;
    }
    return ret;
  }
  else return message;
#else
  return "Error: dynamic linking not implemented";
#endif
}

void
dyn_link_rep::write (string s, int channel) {
#ifndef OS_MINGW
  if ((!alive) || (channel != LINK_IN)) return;
  if (routs==NULL) {
    failed_error << "Library= " << lib << "\n";
    FAILED ("library not installed");
  }
  package_exports_1* pack= (package_exports_1*) routs;

  c_string _session (session);
  c_string _s (s);
  char* _errors= NULL;
  char* _r= pack->evaluate (_s, _session, &_errors);
  ret= string (_r==NULL? (_errors==NULL? ((char*) "Error"): _errors): _r);
  if (!is_nil (this->feed_cmd)) this->feed_cmd->apply ();
#endif
}

string&
dyn_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) return ret;
  else return empty_string;
}

string
dyn_link_rep::read (int channel) {
  if (channel == LINK_OUT) {
    string r= ret;
    ret= "";
    return r;
  }
  else return "";
}

void dyn_link_rep::listen (int msecs) { (void) msecs; }
void dyn_link_rep::interrupt () {}
void dyn_link_rep::stop () {}
