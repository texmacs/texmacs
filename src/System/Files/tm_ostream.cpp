
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
#include "string.hpp"

/******************************************************************************
* Constructors and default implementations
******************************************************************************/

tm_ostream_rep::tm_ostream_rep (): ref_count (0) {}
tm_ostream_rep::~tm_ostream_rep () {}

/******************************************************************************
* Standard streams
******************************************************************************/

class std_ostream_rep: public tm_ostream_rep {
  FILE *file;
  string *buf;
  bool is_w;
  bool is_mine;
  bool is_buf;

public:
  std_ostream_rep ();
  std_ostream_rep (char*);
  std_ostream_rep (FILE*);
  ~std_ostream_rep ();

  bool open ();
  bool open (char*);
  bool open (FILE*);
  bool is_writable () const;
  bool is_buffered () const;
  void flush ();
  void close ();
  void buffer ();
  string unbuffer ();

  void print (bool);
  void print (char);
  void print (short);
  void print (unsigned short);
  void print (int);
  void print (unsigned int);
  void print (long);
  void print (unsigned long);
  void print (long long int);
  void print (unsigned long long int);
  void print (float);
  void print (double);
  void print (long double);
  void print (const char*);
};

/******************************************************************************
* Standard constructors
******************************************************************************/

std_ostream_rep::std_ostream_rep () :
  file (0), is_w (false), is_mine (false), is_buf (false) {
  buf= tm_new<string> ();
}

std_ostream_rep::std_ostream_rep (char* fn) :
  file (0), is_w (false), is_mine (false), is_buf (false) {
  buf= tm_new<string> ();
  open (fn);
}

std_ostream_rep::std_ostream_rep (FILE* f) :
  file (0), is_w (false), is_mine (false), is_buf (false) {
  buf= tm_new<string> ();
  open (f);
}

std_ostream_rep::~std_ostream_rep () {
  if (file && is_mine) fclose (file);
  tm_delete (buf);
}

/******************************************************************************
* Basic methods
******************************************************************************/

bool
std_ostream_rep::open () {
  if (file && is_mine) fclose (file);
  file= 0;
  *buf= "";
  is_w= true;
  is_mine= true;
  is_buf= false;
  return is_w;
}

bool
std_ostream_rep::open (char* fn) {
  if (file && is_mine) fclose (file);
  file= fopen (fn, "w");
  *buf= "";
  is_buf= false;
  if (file) {
    is_w= true;
    is_mine= true;
  } else {
    is_w= false;
    is_mine= false;
  }
  return is_w;
}

bool
std_ostream_rep::open (FILE* f) {
  if (file && is_mine) fclose (file);
  file= f;
  *buf= "";
  is_buf= false;
  if (file) is_w= true;
  else is_w= false;
  is_mine= false;
  return is_w;
}

bool
std_ostream_rep::is_writable () const {
  return is_w;
}

bool
std_ostream_rep::is_buffered () const {
  return is_buf;
}

void
std_ostream_rep::flush () {
  if (file && is_w) fflush (file);
}
  
void
std_ostream_rep::close () {
  if (file && is_mine) fclose (file);
  file= 0;
  *buf= "";
  is_w= false;
  is_mine= false;
  is_buf= false;
}

void
std_ostream_rep::buffer () {
  is_buf= true;
  *buf= "";
}

string
std_ostream_rep::unbuffer () {
  string res= *buf;
  *buf= "";
  is_buf= false;
  return res;
}

/******************************************************************************
* Printing instances of standard types
******************************************************************************/

void
std_ostream_rep::print (bool b) {
  if (is_buf) {
    if (b) *buf << "true";
    else *buf << "false";
  }
  else if (file && is_w) {
    if (b) {
      if (0 > fprintf (file, "%s", "true")) is_w= false;
    } else {
      if (0 > fprintf (file, "%s", "false")) is_w= false;
    }
  }
}

void
std_ostream_rep::print (char c) {
  if (is_buf) {
    char _buf[8];
    sprintf (_buf, "%c", c);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%c", c)) is_w= false;
}

void
std_ostream_rep::print (short sh) {
  if (is_buf) {
    char _buf[32];
    sprintf (_buf, "%hd", sh);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%hd", sh)) is_w= false;
}

void
std_ostream_rep::print (unsigned short ush) {
  if (is_buf) {
    char _buf[32];
    sprintf (_buf, "%hu", ush);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%hu", ush)) is_w= false;
}

void
std_ostream_rep::print (int i) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%d", i);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%d", i)) is_w= false;
}

void
std_ostream_rep::print (unsigned int ui) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%u", ui);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%u", ui)) is_w= false;
}

void
std_ostream_rep::print (long l) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%ld", l);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%ld", l)) is_w= false;
}

void
std_ostream_rep::print (unsigned long ul) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%lu", ul);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%lu", ul)) is_w= false;
}

void
std_ostream_rep::print (long long int l) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%lld", l);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%lld", l)) is_w= false;
}

void
std_ostream_rep::print (unsigned long long int ul) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%llu", ul);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%llu", ul)) is_w= false;
}

void
std_ostream_rep::print (float f) {
  if (is_buf) {
    char _buf[32];
    sprintf (_buf, "%g", f);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%g", f)) is_w= false;
}

void
std_ostream_rep::print (double d) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%g", d);
    *buf << _buf;
  }
  else 
  if (file && is_w)
    if (0 > fprintf (file, "%g", d)) is_w= false;
}

void
std_ostream_rep::print (long double ld) {
  if (is_buf) {
    char _buf[128];
    sprintf (_buf, "%Lg", ld);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%Lg", ld)) is_w= false;
}

void
std_ostream_rep::print (const char* s) {
  if (is_buf) *buf << s;
  else if (file && is_w) {
    if (0 <= fprintf (file, "%s", s)) {
      const char* c= s;
      while (*c != 0 && *c != '\n') ++c;
      if (*c == '\n') flush ();
    } else {
      is_w= false;
    }
  }    
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
void tm_ostream::flush () {
  rep->flush (); }

tm_ostream& tm_ostream::operator << (bool b) {
  rep->print (b); return *this; }
tm_ostream& tm_ostream::operator << (char c) {
  rep->print (c); return *this; }
tm_ostream& tm_ostream::operator << (short i) {
  rep->print (i); return *this; }
tm_ostream& tm_ostream::operator << (unsigned short i) {
  rep->print (i); return *this; }
tm_ostream& tm_ostream::operator << (int i) {
  rep->print (i); return *this; }
tm_ostream& tm_ostream::operator << (unsigned int i) {
  rep->print (i); return *this; }
tm_ostream& tm_ostream::operator << (long i) {
  rep->print (i); return *this; }
tm_ostream& tm_ostream::operator << (unsigned long i) {
  rep->print (i); return *this; }
tm_ostream& tm_ostream::operator << (long long i) {
  rep->print (i); return *this; }
tm_ostream& tm_ostream::operator << (unsigned long long i) {
  rep->print (i); return *this; }
tm_ostream& tm_ostream::operator << (float x) {
  rep->print (x); return *this; }
tm_ostream& tm_ostream::operator << (double x) {
  rep->print (x); return *this; }
tm_ostream& tm_ostream::operator << (long double x) {
  rep->print (x); return *this; }
tm_ostream& tm_ostream::operator << (const char* s) {
  rep->print (s); return *this; }

/******************************************************************************
* Standard output streams
******************************************************************************/

tm_ostream  tm_ostream::private_cout (stdout);
tm_ostream  tm_ostream::private_cerr (stderr);

tm_ostream& tm_ostream::cout= private_cout;
tm_ostream& tm_ostream::cerr= private_cerr;
