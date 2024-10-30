
/******************************************************************************
* MODULE     : tm_ostream.hpp
* DESCRIPTION: Output stream class
* COPYRIGHT  : (C) 2009-2013  David MICHEL, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef OUT_STREAM_HPP
#define OUT_STREAM_HPP

//#include "url.hpp"
#include <cstdio>
#if __cplusplus >= 201703L
#include <string> // for std::char_traits<char>::length
#else
#include <cstring> // for strlen
#endif
class string;
class tm_ostream;
class formatted;
class tree;

class tm_ostream_rep {
  int ref_count;

public:
  tm_ostream_rep ();
  virtual ~tm_ostream_rep ();

  virtual bool is_writable () const;
  virtual void write (const char* s, size_t n);
  virtual void write (tree);
  virtual void flush ();
  virtual void clear ();

  friend class tm_ostream;
};

class tm_ostream {
public:
  tm_ostream_rep *rep;

public:
  static tm_ostream private_cout;
  static tm_ostream private_cerr;
  static tm_ostream& cout;
  static tm_ostream& cerr;

public:
  tm_ostream ();
  tm_ostream (char*);
  tm_ostream (FILE*);
  tm_ostream (const tm_ostream&);
  tm_ostream (tm_ostream_rep*);
  ~tm_ostream ();
  tm_ostream_rep* operator -> ();
  tm_ostream& operator = (tm_ostream x);
  bool operator == (tm_ostream&);

  void clear ();
  void flush ();
  void buffer ();
  string unbuffer ();
  void redirect (tm_ostream x);
  
#if __cplusplus >= 201703L
  inline tm_ostream& operator << (const char* s) {
    /*
    td::char_traits<char>::length(s) is a constexpr in C++17
    if the string is a literal, the length is known at compile time
    if the string is not a literal, the length is computed at runtime
    */
    rep->write (s, std::char_traits<char>::length(s));
    return *this;
  }
#else
  inline tm_ostream& operator << (const char* s) {
    rep->write (s, strlen(s));
    return *this;
  }
#endif

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
  tm_ostream& operator << (formatted);
};

extern tm_ostream& cout;
extern tm_ostream& cerr;

extern tm_ostream std_error;
extern tm_ostream failed_error;
extern tm_ostream boot_error;
extern tm_ostream widkit_error;
extern tm_ostream qt_error;
extern tm_ostream font_error;
extern tm_ostream convert_error;
extern tm_ostream io_error;
extern tm_ostream bibtex_error;

extern tm_ostream std_warning;
extern tm_ostream convert_warning;
extern tm_ostream typeset_warning;
extern tm_ostream io_warning;
extern tm_ostream widkit_warning;
extern tm_ostream bibtex_warning;

extern tm_ostream debug_std;
extern tm_ostream debug_qt;
extern tm_ostream debug_aqua;
extern tm_ostream debug_widgets;
extern tm_ostream debug_fonts;
extern tm_ostream debug_convert;
extern tm_ostream debug_typeset;
extern tm_ostream debug_edit;
extern tm_ostream debug_packrat;
extern tm_ostream debug_history;
extern tm_ostream debug_keyboard;
extern tm_ostream debug_automatic;
extern tm_ostream debug_boot;
extern tm_ostream debug_events;
extern tm_ostream debug_shell;
extern tm_ostream debug_io;
extern tm_ostream debug_spell;
extern tm_ostream debug_updater;

extern tm_ostream std_bench;

tm_ostream string_ostream (string& buf);

#endif // defined OUT_STREAM_HPP
