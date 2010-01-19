
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

tm_ostream
tm_ostream::private_cout (stdout);

tm_ostream
tm_ostream::private_cerr (stderr);

tm_ostream::tm_ostream (const tm_ostream& out) { // private
  file= out.file;
  is_w= out.is_w;
  is_mine= false;
}

tm_ostream&
tm_ostream::operator= (const tm_ostream& out) { // private
  if (this != &out) {
    file= out.file;
    is_w= out.is_w;
    is_mine= false;
  } else {
    return *this;
  }
}

tm_ostream&
tm_ostream::cout= private_cout;

tm_ostream&
tm_ostream::cerr= private_cerr;

tm_ostream::tm_ostream () : file (0), is_w (false), is_mine (false) {}

tm_ostream::tm_ostream (char* fn) : file (0), is_w (false), is_mine (false) {
  open (fn);
}

tm_ostream::tm_ostream (FILE* f) : file (0), is_w (false), is_mine (false) {
  open (f);
}

tm_ostream::~tm_ostream () {
  if (file && is_mine) fclose (file);
}

bool
tm_ostream::open () {
  if (file && is_mine) fclose (file);
  file= 0;
  is_w= true;
  is_mine= true;
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
  if (file) is_w= true;
  else is_w= false;
  is_mine= false;
  return is_w;
}

bool
tm_ostream::is_writable () const {
  return is_w;
}

void
tm_ostream::flush () {
  if (file && is_w) fflush (file);
}
  
void
tm_ostream::close () {
  if (file && is_mine) fclose (file);
  file= 0;
  is_w= false;
  is_mine= false;
}

tm_ostream&
tm_ostream::operator << (bool b) {
  if (file && is_w) {
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
  if (file && is_w)
    if (0 > fprintf (file, "%c", c)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (short sh) {
  if (file && is_w)
    if (0 > fprintf (file, "%hd", sh)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned short ush) {
  if (file && is_w)
    if (0 > fprintf (file, "%hu", ush)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (int i) {
  if (file && is_w)
    if (0 > fprintf (file, "%d", i)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned int ui) {
  if (file && is_w)
    if (0 > fprintf (file, "%u", ui)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (long l) {
  if (file && is_w)
    if (0 > fprintf (file, "%ld", l)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (unsigned long ul) {
  if (file && is_w)
    if (0 > fprintf (file, "%lu", ul)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (float f) {
  if (file && is_w)
    if (0 > fprintf (file, "%g", f)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (double d) {
  if (file && is_w)
    if (0 > fprintf (file, "%g", d)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (long double ld) {
  if (file && is_w)
    if (0 > fprintf (file, "%Lg", ld)) is_w= false;
  return *this;
}

tm_ostream&
tm_ostream::operator << (const char* s) {
  if (file && is_w) {
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

