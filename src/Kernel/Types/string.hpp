
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
bool     operator <= (string a, string b);
int      hash (string s);

bool   as_bool   (string s);
int    as_int    (string s);
double as_double (string s);
string as_string_bool (bool f);
string as_string (int i);
string as_string (unsigned int i);
string as_string (long int i);
string as_string (long long int i);
string as_string (unsigned long int i);
string as_string (double x);
string as_string (const char* s);
bool   is_bool   (string s);
bool   is_int    (string s);
bool   is_double (string s);
bool   is_charp  (string s);

bool   is_quoted (string s);
bool   is_id     (string s);

void  set_info_handler (void (*) (string, string, int));
void  set_wait_handler (void (*) (string, string, int));
void  set_warning_handler (void (*) (string, string, int));
void  set_error_handler (void (*) (string, string, int));
void  system_info (string message, string argument= "", int level= 0);
void  system_wait (string message, string argument= "", int level= 0);
void  system_warning (string message, string argument= "", int level= 0);
void  system_error (string message, string argument= "", int level= 0);

/******************************************************************************
* C-style strings with automatic memory management
******************************************************************************/

// c_string provides automatic memory management for C-style strings
// it is implemented with the standard reference counting mechanism

// we provide automatic casting to char* for convenience
// this imposes not to provide a public constructor from char*,
// otherwise bugs can appear since a sequence of conversions like
// c_string -> char* -> c_string results into two C strings pointing
// to the same memory area, which will then be deallocated twice.
// managing an area has to be explicitly required via manage()

// release is the inverse of manage.

class c_string;
class c_string_rep: concrete_struct {
  char* value;
  
private:
  c_string_rep (c_string_rep &): concrete_struct () {}; // disable copy constructor
  c_string_rep& operator=(c_string_rep&) {}; // disable assignment
  
public:
  c_string_rep (char* v = NULL): value (v) {}
  ~c_string_rep () { if (value) tm_delete_array (value); }
  friend class c_string;
};

class c_string {
  CONCRETE(c_string);
protected:
  c_string (char* v): rep (tm_new<c_string_rep> (v)) {}
public:
  c_string (): rep (tm_new<c_string_rep> ()) {}
  operator char* () const { return rep->value; }
  char* release() { char *ptr= rep->value; rep->value= NULL; return ptr; }
  friend c_string manage (char* value);
};
CONCRETE_CODE(c_string);

inline c_string manage (char* value) { return c_string (value); }
c_string as_charp (string s);

#endif // defined STRING_H
