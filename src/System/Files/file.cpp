
/******************************************************************************
* MODULE     : file.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "timer.hpp"
#include "merge_sort.hpp"
#include "data_cache.hpp"
#include "web_files.hpp"
#include "Scheme/object.hpp"

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

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_images.h"
#endif

/******************************************************************************
* New style loading and saving
******************************************************************************/

bool
load_string (url u, string& s, bool fatal) {
  // cout << "Load " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r);
  // cout << "Resolved " << r << LF;
  bool err= !is_rooted_name (r);
  if (!err) {
    string name= concretize (r);
    // cout << "Concrete :" << name << LF;
    // File contents in cache?
    bool file_flag= do_cache_file (name);
    bool doc_flag= do_cache_doc (name);
    string cache_type= doc_flag? string ("doc_cache"): string ("file_cache");
    if (doc_flag) cache_load ("doc_cache");
    if (is_cached (cache_type, name) && is_up_to_date (url_parent (r))) {
      s= cache_get (cache_type, name) -> label;
      return false;
    }
    // End caching

    bench_start ("load file");
    char* _name= as_charp (name);
    // cout << "OPEN :" << _name << LF;
#ifdef OS_WIN32
    FILE* fin= _fopen (_name, "rb");
#else
#ifdef __MINGW32__
    FILE* fin= fopen (_name, "rb");
#else
	FILE* fin= fopen (_name, "r");
#endif
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
    tm_delete_array (_name);
    bench_cumul ("load file");

    // Cache file contents
    if (!err && N(s) <= 10000)
      if (file_flag || doc_flag)
	cache_set (cache_type, name, s);
    // End caching
  }
  if (err && fatal) {
    cerr << "File name= " << as_string (u) << "\n";
    FAILED ("file not readable");
  }
  return err;
}

bool
save_string (url u, string s, bool fatal) {
  if (is_rooted_tmfs (u)) {
    bool err= save_to_server (u, s);
    if (err && fatal) {
      cerr << "File name= " << as_string (u) << "\n";
      FAILED ("file not writeable");
    }
    return err;
  }

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
    tm_delete_array (_name);

    // Cache file contents
    bool file_flag= do_cache_file (name);
    bool doc_flag= do_cache_doc (name);
    string cache_type= doc_flag? string ("doc_cache"): string ("file_cache");
    if (!err && N(s) <= 10000)
      if (file_flag || doc_flag)
	cache_set (cache_type, name, s);
    (bool) is_up_to_date (url_parent (r), true);
    // End caching
  }

  if (err && fatal) {
    cerr << "File name= " << as_string (u) << "\n";
    FAILED ("file not writeable");
  }
  return err;
}

/******************************************************************************
* Getting attributes of a file
******************************************************************************/

static bool
get_attributes (url name, struct stat* buf,
		bool link_flag=false, bool cache_flag= true)
{
  // cout << "Stat " << name << LF;
  string name_s= concretize (name);

  // Stat result in cache?
  if (cache_flag &&
      is_cached ("stat_cache.scm", name_s) &&
      is_up_to_date (url_parent (name)))
    {
      string r= cache_get ("stat_cache.scm", name_s) -> label;
      if (r == "#f") return true;
      buf->st_mode= ((unsigned int) as_int (r));
      return false;
    }
  // End caching

  bench_start ("stat");
  bool flag;
  char* temp= as_charp (name_s);
#ifdef OS_WIN32
  flag= _stat (temp, buf);
#else
  flag= stat (temp, buf);
#endif
  (void) link_flag;
  // FIXME: configure should test whether lstat works
  // flag= (link_flag? lstat (temp, buf): stat (temp, buf));
  tm_delete_array (temp);
  bench_cumul ("stat");

  // Cache stat results
  if (cache_flag) {
    if (flag) {
      if (do_cache_stat_fail (name_s))
	cache_set ("stat_cache.scm", name_s, "#f");
    }
    else {
      if (do_cache_stat (name_s))
	cache_set ("stat_cache.scm", name_s, as_string ((int) buf->st_mode));
    }
  }
  // End caching

  return flag;
}

bool
is_of_type (url name, string filter) {
  if (filter == "") return true;
  int i, n= N(filter);

  // Files from the web
  if (is_rooted_web (name)) {
    // cout << "  try " << name << "\n";
    url from_web= get_from_web (name);
    // cout << "  --> " << from_web << "\n";
    if (is_none (from_web)) return false;
    for (i=0; i<n; i++)
      switch (filter[i]) {
      case 'd': return false;
      case 'l': return false;
      case 'w': return false;
      case 'x': return false;
      }
    return true;
  }

  // Files from a remote server
  if (is_rooted_tmfs (name)) {
    for (i=0; i<n; i++)
      switch (filter[i]) {
      case 'd': return false;
      case 'l': return false;
      case 'r':
	if (!as_bool (call ("tmfs-permission?", name, "read")))
	  return false;
	break;
      case 'w':
	if (!as_bool (call ("tmfs-permission?", name, "write")))
	  return false;
	break;
      case 'x': return false;
      }
    return true;
  }

  // Files from the ramdisk
  if (is_ramdisc (name))
    return true;

  // Normal files
#ifdef OS_WIN32
  if ((filter == "x") && (suffix(name) != "exe") && (suffix(name) != "bat"))
    name = glue (name, ".exe");
#endif
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
#ifdef __MINGW32__
      return false;
#else
      if (!S_ISLNK (buf.st_mode)) return false;
#endif
      break;
    case 'r':
#ifndef __MINGW32__
      if ((buf.st_mode & (S_IRUSR | S_IRGRP | S_IROTH)) == 0) return false;
#else
      if ((buf.st_mode & 292) == 0) return false;
#endif
      break;
    case 'w':
#ifndef __MINGW32__
      if ((buf.st_mode & (S_IWUSR | S_IWGRP | S_IWOTH)) == 0) return false;
#else
      if ((buf.st_mode & 146) == 0) return false;
#endif
      break;
    case 'x':
#ifdef OS_WIN32
      if (suffix(name) == "bat") break;
#endif
#ifndef __MINGW32__
      if ((buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) == 0) return false;
#else
      if ((buf.st_mode & 73) == 0) return false;
#endif
      break;
    }
  return true;
}

bool is_regular (url name) { return is_of_type (name, "f"); }
bool is_directory (url name) { return is_of_type (name, "d"); }
bool is_symbolic_link (url name) { return is_of_type (name, "l"); }

int
last_modified (url u, bool cache_flag) {
  struct stat u_stat;
  if (get_attributes (u, &u_stat, true, cache_flag)) return 0;
  return u_stat.st_mtime;
}

bool
is_newer (url which, url than) {
  struct stat which_stat;
  struct stat than_stat;
  // FIXME: why was this? 
  if (is_cached ("stat_cache.scm", concretize (which))) return false;
  if (is_cached ("stat_cache.scm", concretize (than))) return false;
  // end FIXME
  if (get_attributes (which, &which_stat, true)) return false;
  if (get_attributes (than , &than_stat , true)) return false;
  return which_stat.st_mtime > than_stat.st_mtime;
}

url
url_temp (string suffix) {
#ifdef __MINGW32__
  int rnd= texmacs_time ();
#else
  static bool initialized= false;
  if (!initialized) {
    srandom ((int) texmacs_time ());
    initialized= true;
  }
  int rnd= random ();
#endif
  string name= "tmp_" * as_string (rnd) * suffix;
  url u ("$TEXMACS_HOME_PATH/system/tmp", name);
  if (exists (u)) return url_temp (suffix);
  return u;
}

url
url_scratch (string prefix, string postfix, int i) {
  url dir ("$TEXMACS_HOME_PATH/texts/scratch");
  if (!exists (dir)) mkdir (dir);
  for (; true; i++) {
    url name= dir * (prefix * as_string (i) * postfix);
    if (!exists (name)) return name;
  }
  return dir * (prefix * "x" * postfix);
}

bool
is_scratch (url u) {
  return head (u) == url ("$TEXMACS_HOME_PATH/texts/scratch");
}

/******************************************************************************
* Reading directories
******************************************************************************/

static array<string>
cache_dir_get (string dir) {
  tree t= cache_get ("dir_cache.scm", dir);
  array<string> a (N(t));
  for (int i=0; i<N(t); i++) a[i]= t[i]->label;
  return a;
}

static void
cache_dir_set (string dir, array<string> a) {
  tree t (TUPLE, N(a));
  for (int i=0; i<N(a); i++) t[i]= a[i];
  cache_set ("dir_cache.scm", dir, t);
}

array<string>
read_directory (url u, bool& error_flag) {
 //  cout << "Directory " << u << LF;
  u= resolve (u, "dr");
  if (is_none (u)) return array<string> ();
  string name= concretize (u);

  // Directory contents in cache?
  if (is_cached ("dir_cache.scm", name) && is_up_to_date (u))
    return cache_dir_get (name);
  bench_start ("read directory");
  // End caching

  DIR* dp;
  char* temp= as_charp (name);
  dp= opendir (temp);
  tm_delete_array (temp);
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

  // Caching of directory contents
  bench_cumul ("read directory");
  if (do_cache_dir (name))
    cache_dir_set (name, dir);
  // End caching

  return dir;
}

/******************************************************************************
* Searching text in the documentation
******************************************************************************/

static array<int>
search (string what, string in) {
  int i= 0, n= N(what);
  array<int> matches;
  if (n == 0) return matches;
  while (true) {
    int pos= search_forwards (what, i, in);
    if (pos == -1) return matches;
    matches << pos;
    i= pos+1;
  }
}

static bool
precedes (string in, int pos, string what) {
  return pos >= N(what) && in (pos-N(what), pos) == what;
}

static int
compute_score (string what, string in, int pos, string suf) {
  int score= 1;
  if (pos > 0 && !is_iso_alpha (in [pos-1]))
    if (pos + N(what) + 1 < N(in) && !is_iso_alpha (in [pos+N(what)]))
      score *= 10;
  if (suf == "tm") {
    if (precedes (in, pos, "<")) score= 0;
    else if (precedes (in, pos, "<\\")) score= 0;
    else if (precedes (in, pos, "<|")) score= 0;
    else if (precedes (in, pos, "</")) score= 0;
    else if (precedes (in, pos, "compound|")) score= 0;
    else if (precedes (in, pos, "<name|")) score *= 10;
    else if (precedes (in, pos, "<tmstyle|")) score *= 10;
    else if (precedes (in, pos, "<tmdtd|")) score *= 10;
    else if (precedes (in, pos, "<explain-macro|")) score *= 10;
    else if (precedes (in, pos, "<var-val|")) score *= 10;
  }
  else if (suf == "scm") {
    if (precedes (in, pos, "define ")) score *= 10;
    else if (precedes (in, pos, "define-public ")) score *= 10;
    else if (precedes (in, pos, "define (")) score *= 10;
    else if (precedes (in, pos, "define-public (")) score *= 10;
    else if (precedes (in, pos, "define-macro ")) score *= 10;
    else if (precedes (in, pos, "define-public-macro ")) score *= 10;
    else if (precedes (in, pos, "define-macro (")) score *= 10;
    else if (precedes (in, pos, "define-public-macro (")) score *= 10;
  }
  return score;
}

static int
compute_score (string what, string in, array<int> pos, string suf) {
  int score= 0, i= 0, n= N(pos);
  for (i=0; i<n; i++)
    score += compute_score (what, in, pos[i], suf);
  return score;
}

int
search_score (url u, array<string> a) {
  string in, suf= suffix (u);
  if (load_string (u, in, false)) return 0;
  in= locase_all (in);
  int i, score= 1, n= N(a);
  for (i=0; i<n; i++) {
    string what= locase_all (a[i]);
    array<int> pos= search (what, in);
    score *= compute_score (what, in, pos, suf);
    if (score == 0) return 0;
    if (score > 1000000) score= 1000000;
  }
  return score;
}

/******************************************************************************
* Commands on files
******************************************************************************/

void
move (url u1, url u2) {
  char *_u1, *_u2;
  _u1 = as_charp (concretize (u1));
  _u2 = as_charp (concretize (u2));
  (void) rename (_u1, _u2);
  tm_delete_array (_u1);
  tm_delete_array (_u2);
}

void
copy (url u1, url u2) {
  string s;
  if (!load_string (u1, s, false))
    (void) save_string (u2, s, false);
}

void
remove (url u) {
  u= expand (complete (u));
  if (is_none (u));
  else if (is_or (u)) {
    remove (u[1]);
    remove (u[2]);
  }
  else {
    char *_u= as_charp (concretize (u));
    (void) ::remove (_u);
    tm_delete_array (_u);
  }
}

void
mkdir (url u) {
#if defined (HAVE_SYS_TYPES_H) && defined (HAVE_SYS_STAT_H)
  if (exists (u)) return;
  char *_u= as_charp (concretize (u));
#if defined(__MINGW__) || defined(__MINGW32__)
  (void) ::mkdir (_u);
#else
  (void) ::mkdir (_u, S_IRWXU + S_IRGRP + S_IROTH);
#endif
  tm_delete_array (_u);
#else
#if defined(__MINGW__) || defined(__MINGW32__)
  system ("mkdir", u);
#else
  system ("mkdir -p", u);
#endif
#endif
}

void
change_mode (url u, int mode) {
#if defined (HAVE_SYS_TYPES_H) && defined (HAVE_SYS_STAT_H)
  char *_u= as_charp (concretize (u));
  (void) ::chmod (_u, mode);
  tm_delete_array (_u);
#else
  string m0= as_string ((mode >> 9) & 7);
  string m1= as_string ((mode >> 6) & 7);
  string m2= as_string ((mode >> 3) & 7);
  string m3= as_string (mode & 7);
  system ("chmod -f " * m0 * m1 * m2 * m3, u);
#endif
}

void
ps2pdf (url u1, url u2) {
#ifdef OS_WIN32
  char *_u1, *_u2;
  _u1 = as_charp (concretize (u1));
  _u2 = as_charp (concretize (u2));
  XPs2Pdf (_u1, _u2);
  tm_delete_array (_u1);
  tm_delete_array (_u2);
#else
#ifdef MACOSX_EXTENSIONS
  mac_ps_to_pdf (u1, u2);
#else
  system ("ps2pdf", u1, u2);
#endif
#endif
}
