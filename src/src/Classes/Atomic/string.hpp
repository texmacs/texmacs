
/******************************************************************************
* MODULE     : string.hpp
* DESCRIPTION: Fixed size strings with reference counting and
*              pointer copying. Zero-characters are allowed in strings.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef STRING_H
#define STRING_H
#include "basic.hpp"

class string_rep: concrete_struct {
  int n;
  char* a;

public:
  inline string_rep (): n(0), a(NULL) {}
         string_rep (int n);
  inline ~string_rep () { if (n!=0) delete[] a; }
  void resize (int n);

  friend class string;
  friend inline int N (string a);
};

class string {
  CONCRETE(string);
  inline string (): rep (new string_rep()) {}
  inline string (int n): rep (new string_rep (n)) {}
  string (char c);
  string (char *s);
  string (char *s, int n);
  inline char& operator [] (int i) { return rep->a[i]; }
  bool operator == (char* s);
  bool operator != (char* s);
  bool operator == (string s);
  bool operator != (string s);
  string operator () (int start, int end);
};
CONCRETE_CODE(string);

extern inline int N (string a) { return a->n; }
string   copy (string a);
ostream& operator << (ostream& out, string a);
string&  operator << (string& a, char);
string&  operator << (string& a, string b);
string   operator * (char* a, string b);
string   operator * (string a, string b);
string   operator * (string a, char* b);
bool     operator <= (string a, string b);
int      hash (string s);

bool   as_bool   (string s);
int    as_int    (string s);
double as_double (string s);
char*  as_charp  (string s);
string as_string_bool (bool f);
string as_string (int i);
string as_string (double x);
string as_string (char* s);
bool   is_bool   (string s);
bool   is_int    (string s);
bool   is_double (string s);
bool   is_charp  (string s);

string quote     (string s);
string unquote   (string s);
bool   is_quoted (string s);
bool   is_id     (string s);

void  fatal_error (string message= "unknown", string routine= "unknown",
		   string file= "");

void  set_info_handler (void (*) (string, string, int));
void  set_wait_handler (void (*) (string, string, int));
void  set_warning_handler (void (*) (string, string, int));
void  set_error_handler (void (*) (string, string, int));
void  system_info (string message, string argument= "", int level= 0);
void  system_wait (string message, string argument= "", int level= 0);
void  system_warning (string message, string argument= "", int level= 0);
void  system_error (string message, string argument= "", int level= 0);

#endif // defined STRING_H
