
/******************************************************************************
* MODULE     : tm_ostream.hpp
* DESCRIPTION: Output stream class
* COPYRIGHT  : (C) 2009  David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef OUT_STREAM_HPP
#define OUT_STREAM_HPP

//#include "url.hpp"
#include <cstdio>
class string;

class tm_ostream {
  FILE *file;
  string *buf;
  bool is_w;
  bool is_mine;
  bool is_buf;

  static tm_ostream private_cout;
  static tm_ostream private_cerr;

  tm_ostream (const tm_ostream&);
  tm_ostream& operator= (const tm_ostream&);

public:
  static tm_ostream& cout;
  static tm_ostream& cerr;

  tm_ostream ();
  tm_ostream (char*);
  tm_ostream (FILE*);
  ~tm_ostream ();

  bool open ();
//  bool open (url);
  bool open (char*);
  bool open (FILE*);
  bool is_writable () const;
  bool is_buffered () const;
  void flush ();
  void close ();
  void buffer ();
  string unbuffer ();

  tm_ostream& operator << (bool);
  tm_ostream& operator << (char);
  tm_ostream& operator << (short);
  tm_ostream& operator << (unsigned short);
  tm_ostream& operator << (int);
  tm_ostream& operator << (unsigned int);
  tm_ostream& operator << (long);
  tm_ostream& operator << (unsigned long);
  tm_ostream& operator << (long long int);
  tm_ostream& operator << (unsigned long long int);
  tm_ostream& operator << (float);
  tm_ostream& operator << (double);
  tm_ostream& operator << (long double);
  tm_ostream& operator << (const char*);

  bool operator == (tm_ostream&);
};

extern tm_ostream& cout;
extern tm_ostream& cerr;

#endif // defined OUT_STREAM_HPP
