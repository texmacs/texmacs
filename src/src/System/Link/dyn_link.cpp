
/******************************************************************************
* MODULE     : dyn_link.cpp
* DESCRIPTION: Dynamic linking of extern routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "dyn_link.hpp"
#include "url.hpp"
#include "hashmap.hpp"
#include <dlfcn.h>
#include <TeXmacs.h>

static hashmap<string,pointer> dyn_linked (NULL);

/******************************************************************************
* Linking symbols from dynamic libraries
******************************************************************************/

string
symbol_install (string lib, string symb, pointer& f) {
  // f becomes NULL in case of failure
  // status message returned
  string out;

  if (!dyn_linked->contains (lib)) {
    url name= resolve (url ("$LD_LIBRARY_PATH", lib));
    if (is_none (name)) out= "Library '" * lib * "' not found";
    else {
      lib= concretize (name);
      char* _lib = as_charp (lib);
      dyn_linked (lib)= dlopen (_lib, RTLD_LAZY);
      if (dyn_linked [lib] == NULL) {
	const char *err = dlerror();
	if (err != NULL) out= string ((char *) err);
      }
      delete[] _lib;
    }
  }

  pointer handle= dyn_linked [lib];
  if (handle) {
    char* _symb= as_charp (symb);
    string tag= lib * ":" * symb;
    if (!dyn_linked->contains (tag))
      dyn_linked (tag)= dlsym (handle, _symb);
    f= dyn_linked [tag];
    if (f != NULL) out= "Dynamically linked symbol '" * symb * "'";
    else out= "Can not find symbol '" * symb * "' in  '" * lib * "'";
    delete[] _symb;
  }
  else {
    f= NULL;
    if (out == "") out= "Couldn't find dynamic library '" * lib * "'";
  }

  if (DEBUG_AUTO) cout << "TeXmacs] " << out << "\n";
  return out;
}

string
symbols_install (string lib, string* symb, pointer* f, int n) {
  int i;
  for (i=0; i<n; i++) f[i]= NULL;
  for (i=0; i<n; i++) {
    string message= symbol_install (lib, symb[i], f[i]);
    if (f[i] == NULL) return message;
  }
  return "Symbols installed for library '" * lib * "'";
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
  return new dyn_link_rep (lib, symb, init, session);
}

static TeXmacs_exports_1 TeXmacs= {
  "TeXmacs communication protocol 1",
  "TeXmacs " TEXMACS_VERSION,
};

string
dyn_link_rep::start () {
  string name= lib * ":" * symbol * "-package";
  if (dyn_linked->contains (name))
    routs= dyn_linked [name];
  if (routs != NULL)
    return "continuation of#'" * lib * "'";
  if (DEBUG_AUTO)
    cout << "TeXmacs] Installing dynamic link '" << lib << "'\n";

  string message= symbol_install (lib, symbol, routs);
  if (routs != NULL) {
    dyn_linked (name)= routs;
    package_exports_1* pack= (package_exports_1*) routs;
    char* _init  = as_charp (init);
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
    delete[] _init;
    return ret;
  }
  else return message;
}

void
dyn_link_rep::write (string s, int channel) {
  if ((!alive) || (channel != LINK_IN)) return;
  if (routs==NULL)
    fatal_error ("'" * lib * "' not installed", "dyn_link_rep::write");
  package_exports_1* pack= (package_exports_1*) routs;

  char* _session= as_charp (session);
  char* _s= as_charp (s);
  char* _errors= NULL;
  char* _r= pack->evaluate (_s, _session, &_errors);
  ret= string (_r==NULL? (_errors==NULL? ((char*) "Error"): _errors): _r);
  delete[] _s;
  delete[] _session;
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
