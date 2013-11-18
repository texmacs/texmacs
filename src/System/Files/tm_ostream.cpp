
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

/******************************************************************************
* Routines for abstract base class
******************************************************************************/

tm_ostream_rep::tm_ostream_rep (): ref_count (0) {}
tm_ostream_rep::~tm_ostream_rep () {}
void tm_ostream_rep::flush () {}
bool tm_ostream_rep::is_writable () const { return false; }
void tm_ostream_rep::write (const char*) {}

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
  void write (const char*);
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
std_ostream_rep::write (const char* s) {
  if (file && is_w) {
    if (0 <= fprintf (file, "%s", s)) {
      const char* c= s;
      while (*c != 0 && *c != '\n') ++c;
      if (*c == '\n') flush ();
    }
    else is_w= false;
  }    
}

void
std_ostream_rep::flush () {
  if (file && is_w) fflush (file);
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
  void write (const char*);
};

buffered_ostream_rep::buffered_ostream_rep (tm_ostream_rep* master2):
  master (master2) {}

buffered_ostream_rep::~buffered_ostream_rep () {}

bool
buffered_ostream_rep::is_writable () const {
  return true;
}

void
buffered_ostream_rep::write (const char* s) {
  buf << s;
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
  void write (const char*);
};

debug_ostream_rep::debug_ostream_rep (string channel2): channel (channel2) {}
debug_ostream_rep::~debug_ostream_rep () {}

bool
debug_ostream_rep::is_writable () const {
  return true;
}

void
debug_ostream_rep::write (const char* s) {
  debug_message (channel, s);
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
  sprintf (_buf, "%c", c);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (short sh) {
  static char _buf[32];
  sprintf (_buf, "%hd", sh);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned short ush) {
  static char _buf[32];
  sprintf (_buf, "%hu", ush);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (int i) {
  static char _buf[64];
  sprintf (_buf, "%d", i);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned int ui) {
  static char _buf[64];
  sprintf (_buf, "%u", ui);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (long l) {
  static char _buf[64];
  sprintf (_buf, "%ld", l);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned long ul) {
  static char _buf[64];
  sprintf (_buf, "%lu", ul);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (long long ll) {
  static char _buf[64];
  sprintf (_buf, "%lld", ll);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned long long ull) {
  static char _buf[64];
  sprintf (_buf, "%llu", ull);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (float f) {
  static char _buf[32];
  sprintf (_buf, "%g", f);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (double d) {
  static char _buf[64];
  sprintf (_buf, "%g", d);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (long double ld) {
  static char _buf[128];
  sprintf (_buf, "%Lg", ld);
  rep->write (_buf);
  return *this;
}

tm_ostream&
tm_ostream::operator << (const char* s) {
  rep->write (s);
  return *this;
}

/******************************************************************************
* Standard output streams
******************************************************************************/

tm_ostream  tm_ostream::private_cout (stdout);
tm_ostream  tm_ostream::private_cerr (stderr);

tm_ostream& tm_ostream::cout= private_cout;
tm_ostream& tm_ostream::cerr= private_cerr;

tm_ostream qt_error        = debug_ostream ("qt-error");
tm_ostream font_error      = debug_ostream ("font-error");
tm_ostream conversion_error= debug_ostream ("conversion-error");
tm_ostream other_error     = debug_ostream ("other-error");

tm_ostream debug_qt        = debug_ostream ("debug-qt");
tm_ostream debug_font      = debug_ostream ("debug-font");
tm_ostream debug_convert   = debug_ostream ("debug-convert");
tm_ostream debug_typeset   = debug_ostream ("debug-typeset");
tm_ostream debug_edit      = debug_ostream ("debug-edit");
tm_ostream debug_io        = debug_ostream ("debug-io");
tm_ostream debug_other     = debug_ostream ("debug-other");
