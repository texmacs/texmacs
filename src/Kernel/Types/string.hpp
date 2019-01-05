
/******************************************************************************
* MODULE     : string.hpp
* DESCRIPTION: Fixed size strings with reference counting and
*              pointer copying. Zero-characters are allowed in strings.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef STRING_H
#define STRING_H
#include "basic.hpp"

class string;
class string_rep: concrete_struct {
  int n;
  char* a;

public:
  inline string_rep (): n(0), a(NULL) {}
         string_rep (int n);
  inline ~string_rep () { if (n!=0) tm_delete_array (a); }
  void resize (int n);

  friend class string;
  friend inline int N (string a);
};

class string {
  CONCRETE(string);
  inline string (): rep (tm_new<string_rep> ()) {}
  inline string (int n): rep (tm_new<string_rep> (n)) {}
  string (char c);
  string (char c, int n);
  string (const char *s);
  string (const char *s, int n);
  inline char& operator [] (int i) { return rep->a[i]; }
  bool operator == (const char* s);
  bool operator != (const char* s);
  bool operator == (string s);
  bool operator != (string s);
  string operator () (int start, int end);
};
CONCRETE_CODE(string);

extern inline int N (string a) { return a->n; }
string   copy (string a);
tm_ostream& operator << (tm_ostream& out, string a);
string&  operator << (string& a, char);
string&  operator << (string& a, string b);
string   operator * (const char* a, string b);
string   operator * (string a, string b);
string   operator * (string a, const char* b);
bool     operator < (string a, string b);
bool     operator <= (string a, string b);
int      hash (string s);

bool     as_bool   (string s);
int      as_int    (string s);
long int as_long_int (string s);
double   as_double (string s);
char*    as_charp  (string s);
string   as_string_bool (bool f);
string   as_string (int i);
string   as_string (unsigned int i);
string   as_string (long int i);
string   as_string (long long int i);
string   as_string (unsigned long int i);
string   as_string (double x);
string   as_string (const char* s);
bool     is_bool   (string s);
bool     is_int    (string s);
bool     is_double (string s);
bool     is_charp  (string s);

bool  is_quoted (string s);
bool  is_id     (string s);

void  set_wait_handler (void (*) (string, string, int));
void  system_wait (string message, string argument= "", int level= 0);

/******************************************************************************
* C-style strings with automatic memory management
******************************************************************************/

class c_string;
class c_string_rep: concrete_struct {
  char* value;
  
private:
  inline c_string_rep (c_string_rep &): concrete_struct () {}
    // disable copy constructor
  inline c_string_rep& operator=(c_string_rep&) { return *this; }
    // disable assignment
  
public:
  inline c_string_rep (char* v = NULL): value (v) {}
  inline ~c_string_rep () { if (value != NULL) tm_delete_array (value); }
  friend class c_string;
};

class c_string {
  CONCRETE(c_string);
public:
  inline c_string ():
    rep (tm_new<c_string_rep> ()) {}
  inline c_string (int len):
    rep (tm_new<c_string_rep> (tm_new_array<char> (len))) {}
  inline c_string (string s):
    rep (tm_new<c_string_rep> (as_charp (s))) {}
  inline operator char* () const { return rep->value; }
};
CONCRETE_CODE(c_string);

#endif // defined STRING_H
