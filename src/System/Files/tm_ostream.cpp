
/******************************************************************************
* MODULE     : tm_ostream.cpp
* DESCRIPTION: Output stream class
* COPYRIGHT  : (C) 2009  David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_ostream.hpp"
#include "string.hpp"

tm_ostream
tm_ostream::private_cout (stdout);

tm_ostream
tm_ostream::private_cerr (stderr);

tm_ostream::tm_ostream (const tm_ostream& out) { // private
  file= out.file;
  is_w= out.is_w;
  is_buf= out.is_buf;
  *buf= *(out.buf);
  is_mine= false;
}

tm_ostream&
tm_ostream::operator= (const tm_ostream& out) { // private
  if (this != &out) {
    file= out.file;
    is_w= out.is_w;
    is_buf= out.is_buf;
    *buf= *(out.buf);
    is_mine= false;
  }
  return *this;
}

tm_ostream&
tm_ostream::cout= private_cout;

tm_ostream&
tm_ostream::cerr= private_cerr;

tm_ostream::tm_ostream () :
  file (0), is_w (false), is_mine (false), is_buf (false) {
  buf= tm_new<string> ();
}

tm_ostream::tm_ostream (char* fn) :
  file (0), is_w (false), is_mine (false), is_buf (false) {
  buf= tm_new<string> ();
  open (fn);
}

tm_ostream::tm_ostream (FILE* f) :
  file (0), is_w (false), is_mine (false), is_buf (false) {
  buf= tm_new<string> ();
  open (f);
}

tm_ostream::~tm_ostream () {
  if (file && is_mine) fclose (file);
  tm_delete (buf);
}

bool
tm_ostream::open () {
  if (file && is_mine) fclose (file);
  file= 0;
  *buf= "";
  is_w= true;
  is_mine= true;
  is_buf= false;
  return is_w;
}

/*
bool
tm_ostream::open (url u) {
  if (file) fclose (file);
  char* _u= as_charp (concretize (u));
  file= fopen (_u, "w");
  tm_delete_array (_u);
  if (file) is_w= true;
  else is_w= false;
  return is_w;
}
*/ 

bool
tm_ostream::open (char* fn) {
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
tm_ostream::open (FILE* f) {
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
tm_ostream::is_writable () const {
  return is_w;
}

bool
tm_ostream::is_buffered () const {
  return is_buf;
}

void
tm_ostream::flush () {
  if (file && is_w) fflush (file);
}
  
void
tm_ostream::close () {
  if (file && is_mine) fclose (file);
  file= 0;
  *buf= "";
  is_w= false;
  is_mine= false;
  is_buf= false;
}

void
tm_ostream::buffer () {
  is_buf= true;
  *buf= "";
}

string
tm_ostream::unbuffer () {
  string res= *buf;
  *buf= "";
  is_buf= false;
  return res;
}

tm_ostream&
tm_ostream::operator << (bool b) {
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
  return *this;
}

tm_ostream&
tm_ostream::operator << (char c) {
  if (is_buf) {
    char _buf[8];
    sprintf (_buf, "%c", c);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%c", c)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (short sh) {
  if (is_buf) {
    char _buf[32];
    sprintf (_buf, "%hd", sh);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%hd", sh)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned short ush) {
  if (is_buf) {
    char _buf[32];
    sprintf (_buf, "%hu", ush);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%hu", ush)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (int i) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%d", i);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%d", i)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned int ui) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%u", ui);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%u", ui)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (long l) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%ld", l);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%ld", l)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned long ul) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%lu", ul);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%lu", ul)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (long long int l) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%lld", l);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%lld", l)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned long long int ul) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%llu", ul);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%llu", ul)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (float f) {
  if (is_buf) {
    char _buf[32];
    sprintf (_buf, "%g", f);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%g", f)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (double d) {
  if (is_buf) {
    char _buf[64];
    sprintf (_buf, "%g", d);
    *buf << _buf;
  }
  else 
  if (file && is_w)
    if (0 > fprintf (file, "%g", d)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (long double ld) {
  if (is_buf) {
    char _buf[128];
    sprintf (_buf, "%Lg", ld);
    *buf << _buf;
  }
  else if (file && is_w)
    if (0 > fprintf (file, "%Lg", ld)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (const char* s) {
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
  return *this;
}

bool
tm_ostream::operator == (tm_ostream& out) {
  return (&out == this);
}

