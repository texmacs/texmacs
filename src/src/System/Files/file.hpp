
/******************************************************************************
* MODULE     : file.hpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef FILE_H
#define FILE_H
#include "url.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"

bool load_string (url file_name, string& s, bool fatal);
bool save_string (url file_name, string s, bool fatal=false);

bool is_of_type (url name, string filter);
bool is_regular (url name);
bool is_directory (url name);
bool is_symbolic_link (url name);
bool is_newer (url which, url than);
int  last_modified (url u, bool cache_flag= true);
url  url_temp (string suffix= "");
url  url_scratch (string prefix= "no_name_", string postfix= ".tm", int i=1);
bool is_scratch (url u);

array<string> read_directory (url name, bool& error_flag);

inline string sys_concretize (url u1) {
  return escape_sh (concretize (u1)); }

inline void system (string which, url u1) {
  system (which * " " * sys_concretize (u1)); }
inline void system (string which, url u1, url u2) {
  system (which * " " * sys_concretize (u1) * " " * sys_concretize (u2)); }
inline void system (string which, url u1, const char* post) {
  system (which * " " * sys_concretize (u1) * " " * post); }
inline void system (string which, url u1, const char* sep, url u2) {
  system (which * " " * sys_concretize (u1) * " " * sep *
	          " " * sys_concretize (u2)); }
inline string eval_system (string which, url u1) {
  return eval_system (which * " " * concretize (u1)); }
inline string eval_system (string which, url u1, url u2) {
  return eval_system (which * " " * concretize (u1) * " " * concretize (u2)); }

void move (url from, url to);
void copy (url from, url to);
void remove (url what);
void mkdir (url dir);
void ps2pdf (url u1, url u2);

int search_score (url u, array<string> a);

#endif // defined FILE_H
