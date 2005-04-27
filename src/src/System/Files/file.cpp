
/******************************************************************************
* MODULE     : file.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "file.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "timer.hpp"
#include "merge_sort.hpp"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#ifdef OS_WIN32
#include <sys/misc.h>
#include <sys/_stat.h>
#include <X11/Xlib.h>
#else
#include <sys/stat.h>
#endif
#include <sys/types.h>

/******************************************************************************
* New style loading and saving
******************************************************************************/

bool
load_string (url u, string& s, bool fatal) {
  // cout << "Load " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r);
  bool err= !is_rooted_name (r);
  if (!err) {
    bench_start ("load file");
    string name= concretize (r);
    char* _name= as_charp (name);
#ifdef OS_WIN32
    FILE* fin= _fopen (_name, "rb");
#else
    FILE* fin= fopen (_name, "r");
#endif
    if (fin == NULL) err= true;
    int size= 0;
    if (!err) {
      if (fseek (fin, 0L, SEEK_END) < 0) err= true;
      else {
	size = ftell (fin);
	if (size<0) err= true;
      }
    }
    if (!err) {
      rewind(fin);
      s->resize (size);
      int read= fread (&(s[0]), 1, size, fin);
      if (read < size) s->resize (read);
      fclose (fin);
    }
    delete[] _name;
    bench_cumul ("load file");
  }
  if (err && fatal)
    fatal_error (as_string (u) * " not readable", "load_string");
  return err;
}

/*
#include <fstream>
bool
load_string (url u, string& s, bool fatal) {
  string name= concretize (u);
  char* _name= as_charp (name);
  bool err= !is_rooted_name (u);
  if (!err) {
    cout << "opening " << name << "\n";
    ifstream fin (_name);
    cout << "opened\n";
    if (!fin) err= true;
    if (!err) {
      char c;
      while (fin.get (c)) s << c;
      if (!fin.eof ()) err= true;
    }
  }
  cout << "read " << s << "\n";
  if (err && fatal)
    fatal_error (as_string (u) * " not readable", "load_string");
  delete[] _name;
  return err;
}
*/

bool
save_string (url u, string s, bool fatal) {
  // cout << "Save " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r, "");
  bool err= !is_rooted_name (r);
  if (!err) {
    string name= concretize (r);
    char* _name= as_charp (name);
#ifdef OS_WIN32
    FILE* fout= _fopen (_name, "wb");
#else
    FILE* fout= fopen (_name, "w");
#endif
    if (fout == NULL) err= true;
    if (!err) {
      int i, n= N(s);
      for (i=0; i<n; i++)
	fputc (s[i], fout);
      fclose (fout);
    }
    delete[] _name;
  }
  if (err && fatal)
    fatal_error (as_string (u) * " not writeable", "save_string");
  return err;
}

/******************************************************************************
* Getting attributes of a file
******************************************************************************/

static bool
get_attributes (url name, struct stat* buf, bool link_flag=false) {
  // cout << "Stat " << name << LF;
  bench_start ("stat");
  bool flag;
  char* temp= as_charp (concretize (name));
#ifdef OS_WIN32
  flag= _stat (temp, buf);
#else
  flag= stat (temp, buf);
#endif
  (void) link_flag;
  // FIXME: configure should test whether lstat works
  // flag= (link_flag? lstat (temp, buf): stat (temp, buf));
  delete[] temp;
  bench_cumul ("stat");
  return flag;
}

bool
is_of_type (url name, string filter) {
  if (filter == "") return true;
#ifdef OS_WIN32
  if ((filter == "x") && (suffix(name) != "exe") && (suffix(name) != "bat"))
    name = glue (name, ".exe");
#endif
  int i, n= N(filter);
  bool preserve_links= false;
  for (i=0; i<n; i++)
    preserve_links= preserve_links || (filter[i] == 'l');
  struct stat buf;
  if (get_attributes (name, &buf, preserve_links)) return false;
  for (i=0; i<n; i++)
    switch (filter[i]) {
      // FIXME: should check user id and group id for r, w and x
    case 'f':
      if (!S_ISREG (buf.st_mode)) return false;
      break;
    case 'd':
      if (!S_ISDIR (buf.st_mode)) return false;
      break;
    case 'l':
      if (!S_ISLNK (buf.st_mode)) return false;
      break;
    case 'r':
      if ((buf.st_mode & (S_IRUSR | S_IRGRP | S_IROTH)) == 0) return false;
      break;
    case 'w':
      if ((buf.st_mode & (S_IWUSR | S_IWGRP | S_IWOTH)) == 0) return false;
      break;
    case 'x':
#ifdef OS_WIN32
      if (suffix(name) == "bat") break;
#endif
      if ((buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) == 0) return false;
      break;
    }
  return true;
}

bool is_regular (url name) { return is_of_type (name, "f"); }
bool is_directory (url name) { return is_of_type (name, "d"); }
bool is_symbolic_link (url name) { return is_of_type (name, "l"); }

bool
is_newer (url which, url than) {
  struct stat which_stat;
  struct stat than_stat;
  if (get_attributes (which, &which_stat, true)) return false;
  if (get_attributes (than , &than_stat , true)) return false;
  return which_stat.st_mtime > than_stat.st_mtime;
}

url
url_temp (string suffix) {
  static bool initialized= false;
  if (!initialized) {
    srandom ((int) texmacs_time ());
    initialized= true;
  }

  int rnd= random ();
  string name= "tmp_" * as_string (rnd) * suffix;
  url u ("$TEXMACS_HOME_PATH/system/tmp", name);
  if (exists (u)) return url_temp (suffix);
  return u;
}

/******************************************************************************
* Reading directories
******************************************************************************/

array<string>
read_directory (url u, bool& error_flag) {
  // cout << "Directory " << u << LF;
  u= resolve (u, "dr");
  if (is_none (u)) return array<string> ();
  string name= concretize (u);
  bench_start ("read directory");

  DIR* dp;
  char* temp= as_charp (name);
  dp= opendir (temp);
  delete[] temp;
  error_flag= (dp==NULL);
  if (error_flag) return array<string> ();

  array<string> dir;
  struct dirent* ep;
  while (true) {
    ep= readdir (dp);
    if (ep==NULL) break;
    dir << string (ep->d_name);
  }
  (void) closedir (dp);
  merge_sort (dir);

  bench_cumul ("read directory");
  return dir;
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void
ps2pdf (url u1, url u2) {
#ifdef OS_WIN32
  char *_u1, *_u2;
  _u1 = as_charp (concretize (u1));
  _u2 = as_charp (concretize (u2));
  XPs2Pdf (_u1, _u2);
  delete [] _u1;
  delete [] _u2;
#else
  system ("ps2pdf", u1, u2);
#endif
}
