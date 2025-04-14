
/******************************************************************************
* MODULE     : tm_ostream.cpp
* DESCRIPTION: Output stream class
* COPYRIGHT  : (C) 2009-2013  David MICHEL, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_ostream.hpp"
#include "tree.hpp"

#if defined (OS_MINGW64)
#include "Windows64/windows64_system.hpp"
#elif defined (OS_MINGW)
#include "Windows/windows32_system.hpp"
#elif defined (OS_ANDROID)
#include "Android/android_system.hpp"
#else
#include "Unix/unix_system.hpp"
#endif

#ifndef HAVE_SNPRINTF
#warning "security issue: snprintf not available, potential buffer overflows"
#endif

/******************************************************************************
* Routines for abstract base class
******************************************************************************/

tm_ostream_rep::tm_ostream_rep (): ref_count (0) {}
tm_ostream_rep::~tm_ostream_rep () {}
void tm_ostream_rep::flush () {}
void tm_ostream_rep::clear () {}
bool tm_ostream_rep::is_writable () const { return false; }
void tm_ostream_rep::write (const char*, size_t n) { (void) n; }
void tm_ostream_rep::write (tree t) { (void) t; }

/******************************************************************************
* Standard streams
******************************************************************************/

class std_ostream_rep: public tm_ostream_rep {
  FILE *file;
  bool is_w;
  bool is_mine;

public:
  std_ostream_rep ();
  std_ostream_rep (char*);
  std_ostream_rep (FILE*);
  ~std_ostream_rep ();

  bool is_writable () const;
  void write (const char* s, size_t n);
  void flush ();
};

std_ostream_rep::std_ostream_rep ():
  file (0), is_w (false), is_mine (false)
{
  is_w= true;
  is_mine= true;
}

std_ostream_rep::std_ostream_rep (char* fn):
  file (0), is_w (false), is_mine (false)
{
  file= fopen (fn, "w");
  if (file) {
    is_w= true;
    is_mine= true;
  }
  else {
    is_w= false;
    is_mine= false;
  }
}

std_ostream_rep::std_ostream_rep (FILE* f) :
  file (0), is_w (false), is_mine (false)
{
  file= f;
  if (file) is_w= true;
  else is_w= false;
  is_mine= false;
}

std_ostream_rep::~std_ostream_rep () {
  if (file && is_mine) fclose (file);
}

bool
std_ostream_rep::is_writable () const {
  return is_w;
}

void
std_ostream_rep::write (const char* s, size_t n) {
  if (!file || !is_w) {
    return;
  }
  if (n == 0) {
    return;
  }
  ssize_t written= texmacs_fwrite(s, n, file);
  if (written < 0 || ((size_t) written) != n) {
    is_w = false;
    return;
  }
  const char* c= s;
  while (*c != 0 && *c != '\n') ++c;
  if (*c == '\n') flush ();
}

void
std_ostream_rep::flush () {
  if (file && is_w) fflush (file);
}

/******************************************************************************
* String streams
******************************************************************************/

class string_ostream_rep: public tm_ostream_rep {
public:
  string* buf;

public:
  string_ostream_rep (string* buf);
  ~string_ostream_rep ();

  bool is_writable () const;
  void write (const char* s, size_t n);
};

string_ostream_rep::string_ostream_rep (string* buf2): buf (buf2) {}
string_ostream_rep::~string_ostream_rep () {}
bool string_ostream_rep::is_writable () const { return true; }
void string_ostream_rep::write (const char* s, size_t n) { (*buf) << string (s,n); }


tm_ostream
string_ostream (string& buf) {
  return (tm_ostream_rep*) tm_new<string_ostream_rep> (&buf);
}

/******************************************************************************
* Buffered streams
******************************************************************************/

class buffered_ostream_rep: public tm_ostream_rep {
public:
  tm_ostream_rep* master;
  string buf;

public:
  buffered_ostream_rep (tm_ostream_rep* master);
  ~buffered_ostream_rep ();

  bool is_writable () const;
  void write (const char* s, size_t n);
};

buffered_ostream_rep::buffered_ostream_rep (tm_ostream_rep* master2):
  master (master2) {}

buffered_ostream_rep::~buffered_ostream_rep () {}

bool
buffered_ostream_rep::is_writable () const {
  return true;
}

void
buffered_ostream_rep::write (const char* s, size_t n) {
  buf << string (s, n);
}

/******************************************************************************
* Streams for debugging purposes
******************************************************************************/

class debug_ostream_rep: public tm_ostream_rep {
public:
  string channel;

public:
  debug_ostream_rep (string channel);
  ~debug_ostream_rep ();

  bool is_writable () const;
  void write (const char* s, size_t n);
  void write (tree t);
  void clear ();
};

debug_ostream_rep::debug_ostream_rep (string channel2): channel (channel2) {}
debug_ostream_rep::~debug_ostream_rep () {}

bool
debug_ostream_rep::is_writable () const {
  return true;
}

void
debug_ostream_rep::clear () {
  clear_debug_messages (channel);
}

void
debug_ostream_rep::write (const char* s, size_t n) {
  debug_message (channel, string (s, n));
}

void
debug_ostream_rep::write (tree t) {
  debug_formatted (channel, t);
}

tm_ostream
debug_ostream (string channel) {
  return (tm_ostream_rep*) tm_new<debug_ostream_rep> (channel);
}

/******************************************************************************
* Abstract user interface
******************************************************************************/

tm_ostream::tm_ostream ():
  rep (tm_new<std_ostream_rep> ()) { INC_COUNT (this->rep); }
tm_ostream::tm_ostream (char* s):
  rep (tm_new<std_ostream_rep> (s)) { INC_COUNT (this->rep); }
tm_ostream::tm_ostream (FILE* f):
  rep (tm_new<std_ostream_rep> (f)) { INC_COUNT (this->rep); }
tm_ostream::tm_ostream (const tm_ostream& x):
  rep(x.rep) { INC_COUNT (this->rep); }
tm_ostream::tm_ostream (tm_ostream_rep* rep2): rep(rep2) {
  INC_COUNT (this->rep); }
tm_ostream::~tm_ostream () {
  DEC_COUNT (this->rep); }
tm_ostream_rep* tm_ostream::operator -> () {
  return rep; }
tm_ostream& tm_ostream::operator = (tm_ostream x) {
  INC_COUNT (x.rep); DEC_COUNT (this->rep);
  this->rep=x.rep; return *this; }
bool tm_ostream::operator == (tm_ostream& out) {
  return (&out == this); }

void
tm_ostream::clear () {
  rep->clear ();
}

void
tm_ostream::flush () {
  rep->flush ();
}

void
tm_ostream::buffer () {
  rep= tm_new<buffered_ostream_rep> (rep);
}

string
tm_ostream::unbuffer () {
  buffered_ostream_rep* ptr= (buffered_ostream_rep*) rep;
  rep= ptr->master;
  string r= ptr->buf;
  tm_delete<buffered_ostream_rep> (ptr);
  return r;
}

void
tm_ostream::redirect (tm_ostream x) {
  INC_COUNT (x.rep);
  DEC_COUNT (this->rep);
  this->rep= x.rep;
}

/******************************************************************************
* Print methods for standard types
******************************************************************************/

tm_ostream&
tm_ostream::operator << (bool b) {
  if (b) rep->write ("true");
  else rep->write ("false");
  return *this;
}

tm_ostream&
tm_ostream::operator << (char c) {
  static char _buf[8];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 8, "%c", c);
#else
  int n = sprintf (_buf, "%c", c);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (short sh) {
  static char _buf[32];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 32, "%hd", sh);
#else
  int n = sprintf (_buf, "%hd", sh);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned short ush) {
  static char _buf[32];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 32, "%hu", ush);
#else
  int n = sprintf (_buf, "%hu", ush);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (int i) {
  static char _buf[64];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 64, "%d", i);
#else
  int n = sprintf (_buf, "%d", i);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned int ui) {
  static char _buf[64];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 64, "%u", ui);
#else
  int n = sprintf (_buf, "%u", ui);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (long l) {
  static char _buf[64];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 64, "%ld", l);
#else
  int n = sprintf (_buf, "%ld", l);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned long ul) {
  static char _buf[64];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 64, "%lu", ul);
#else
  int n = sprintf (_buf, "%lu", ul);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (long long ll) {
  static char _buf[64];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 64, "%lld", ll);
#else
  int n = sprintf (_buf, "%lld", ll);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned long long ull) {
  static char _buf[64];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 64, "%llu", ull);
#else
  int n = sprintf (_buf, "%llu", ull);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (float f) {
  static char _buf[32];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 32, "%g", f);
#else
  int n = sprintf (_buf, "%g", f);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (double d) {
  static char _buf[64];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 64, "%g", d);
#else
  int n = sprintf (_buf, "%g", d);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (long double ld) {
  static char _buf[128];
#ifdef HAVE_SNPRINTF
  int n = snprintf (_buf, 128, "%Lg", ld);
#else
  int n = sprintf (_buf, "%Lg", ld);
#endif
  rep->write (_buf, n);
  return *this;
}

tm_ostream&
tm_ostream::operator << (formatted f) {
  rep->write (f.rep);
  return *this;
}

/******************************************************************************
* Standard output streams
******************************************************************************/

tm_ostream  tm_ostream::private_cout (stdout);
tm_ostream  tm_ostream::private_cerr (stderr);

tm_ostream& cout= tm_ostream::private_cout;
tm_ostream& cerr= tm_ostream::private_cerr;

tm_ostream std_error       = debug_ostream ("std-error");
tm_ostream failed_error    = debug_ostream ("failed-error");
tm_ostream boot_error      = debug_ostream ("boot-error");
tm_ostream qt_error        = debug_ostream ("qt-error");
tm_ostream widkit_error    = debug_ostream ("widkit-error");
tm_ostream aqua_error      = debug_ostream ("aqua-error");
tm_ostream font_error      = debug_ostream ("font-error");
tm_ostream convert_error   = debug_ostream ("convert-error");
tm_ostream bibtex_error    = debug_ostream ("bibtex-error");
tm_ostream io_error        = debug_ostream ("io-error");

tm_ostream std_warning     = debug_ostream ("std-warning");
tm_ostream convert_warning = debug_ostream ("convert-warning");
tm_ostream typeset_warning = debug_ostream ("typeset-warning");
tm_ostream io_warning      = debug_ostream ("io-warning");
tm_ostream widkit_warning  = debug_ostream ("widkit-warning");
tm_ostream bibtex_warning  = debug_ostream ("bibtex-warning");

tm_ostream debug_std       = debug_ostream ("debug-std");
tm_ostream debug_qt        = debug_ostream ("debug-qt");
tm_ostream debug_aqua      = debug_ostream ("debug-aqua");
tm_ostream debug_widgets   = debug_ostream ("debug-widgets");
tm_ostream debug_fonts     = debug_ostream ("debug-fonts");
tm_ostream debug_convert   = debug_ostream ("debug-convert");
tm_ostream debug_typeset   = debug_ostream ("debug-typeset");
tm_ostream debug_edit      = debug_ostream ("debug-edit");
tm_ostream debug_packrat   = debug_ostream ("debug-packrat");
tm_ostream debug_history   = debug_ostream ("debug-history");
tm_ostream debug_keyboard  = debug_ostream ("debug-keyboard");
tm_ostream debug_automatic = debug_ostream ("debug-automatic");
tm_ostream debug_boot      = debug_ostream ("debug-boot");
tm_ostream debug_events    = debug_ostream ("debug-events");
tm_ostream debug_shell     = debug_ostream ("debug-shell");
tm_ostream debug_io        = debug_ostream ("debug-io");
tm_ostream debug_spell     = debug_ostream ("debug-spell");
tm_ostream debug_updater   = debug_ostream ("debug-updater");

tm_ostream std_bench       = debug_ostream ("std-bench");
