
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
#include "tm_timer.hpp"
#include "merge_sort.hpp"
#include "data_cache.hpp"
#include "web_files.hpp"
#include "scheme.hpp"
#include "convert.hpp"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>  // strerror
#if defined (OS_MINGW)
#include "Windows/win-utf8-compat.hpp"
#else
#include <dirent.h>
#define struct_stat struct stat
#endif

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
    bool currently_cached= is_cached (cache_type, name);
    if (currently_cached && is_up_to_date (url_parent (r))) {
      s= cache_get (cache_type, name) -> label;
      return false;
    }
    // End caching

    bench_start ("load file");
    c_string _name (name);
    // cout << "OPEN :" << _name << LF;
#ifdef OS_MINGW
    FILE* fin= fopen (_name, "rb");
#else
    FILE* fin= fopen (_name, "r");
    int fd= -1;
    if (fin != NULL) {
      fd= fileno (fin);
      if (flock (fd, LOCK_SH) == -1) {
        fclose (fin);
        fin= NULL;
      }
    }
#endif
    if (fin == NULL) {
      err= true;
      if (!occurs ("system", name))
        std_warning << "Load error for " << name << ", "
                    << strerror(errno) << "\n";
    }
    int size= 0;
    if (!err) {
      if (fseek (fin, 0L, SEEK_END) < 0) err= true;
      else {
	size= ftell (fin);
	if (size<0) err= true;
      }
      if (err) {
        std_warning << "Seek failed for " << as_string (u) << "\n";
#ifdef OS_MINGW
#else
        flock (fd, LOCK_UN);
#endif
        fclose (fin);
      }
    }
    if (!err) {
      rewind (fin);
      s->resize (size);
      int read= fread (&(s[0]), 1, size, fin);
      if (read < size) s->resize (read);
#ifdef OS_MINGW
#else
      flock (fd, LOCK_UN);
#endif
      fclose (fin);
    }
    bench_cumul ("load file");

    // Cache file contents
    if (!err && (N(s) <= 10000 || currently_cached))
      if (file_flag || doc_flag)
	cache_set (cache_type, name, s);
    // End caching
  }
  if (err && fatal) {
    failed_error << "File name= " << as_string (u) << "\n";
    FAILED ("file not readable");
  }
  return err;
}

bool
save_string (url u, string s, bool fatal) {
  if (is_rooted_tmfs (u)) {
    bool err= save_to_server (u, s);
    if (err && fatal) {
      failed_error << "File name= " << as_string (u) << "\n";
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
    {
      c_string _name (name);
#ifdef OS_MINGW
      FILE* fout= fopen (_name, "wb");
#else
      FILE* fout= fopen (_name, "r+");
      bool rw= (fout != NULL);
      if (!rw) fout= fopen (_name, "w");
      int fd= -1;
      if (fout != NULL) {
        fd= fileno (fout);
        if (flock (fd, LOCK_EX) == -1) {
          fclose (fout);
          fout= NULL;
        }
        else if (rw) ftruncate (fd, 0);
      }
#endif
      if (fout == NULL) {
        err= true;
        std_warning << "Save error for " << name << ", "
                    << strerror(errno) << "\n";
      }
      if (!err) {
        int i, n= N(s);
        for (i=0; i<n; i++)
          fputc (s[i], fout);
#ifdef OS_MINGW
#else
        flock (fd, LOCK_UN);
#endif
        fclose (fout);
      }
    }
    // Cache file contents
    bool file_flag= do_cache_file (name);
    bool doc_flag= do_cache_doc (name);
    string cache_type= doc_flag? string ("doc_cache"): string ("file_cache");
    if (!err && N(s) <= 10000)
      if (file_flag || doc_flag)
	cache_set (cache_type, name, s);
    declare_out_of_date (url_parent (r));
    // End caching
  }

  if (err && fatal) {
    failed_error << "File name= " << as_string (u) << "\n";
    FAILED ("file not writeable");
  }
  return err;
}

bool
append_string (url u, string s, bool fatal) {
  if (is_rooted_tmfs (u)) FAILED ("file not appendable");

  // cout << "Save " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r, "");
  bool err= !is_rooted_name (r);
  if (!err) {
    string name= concretize (r);
    {
      c_string _name (name);
#ifdef OS_MINGW
      FILE* fout= fopen (_name, "ab");
#else
      FILE* fout= fopen (_name, "a");
      int fd= -1;
      if (fout != NULL) {
        fd= fileno (fout);
        if (flock (fd, LOCK_EX) == -1) {
          fclose (fout);
          fout= NULL;
        }
      }
#endif
      if (fout == NULL) {
        err= true;
        std_warning << "Append error for " << name << ", "
                    << strerror(errno) << "\n";
      }
      if (!err) {
        int i, n= N(s);
        for (i=0; i<n; i++)
          fputc (s[i], fout);
#ifdef OS_MINGW
#else
        flock (fd, LOCK_UN);
#endif
        fclose (fout);
      }
    }
    // Cache file contents
    declare_out_of_date (url_parent (r));
    // End caching
  }

  if (err && fatal) {
    failed_error << "File name= " << as_string (u) << "\n";
    FAILED ("file not appendable");
  }
  return err;
}

/******************************************************************************
* Getting attributes of a file
******************************************************************************/

static bool
get_attributes (url name, struct_stat* buf,
		bool link_flag=false, bool cache_flag= true)
{
  // cout << "Stat " << name << LF;
  string name_s= concretize (name);

  // Stat result in cache?
  if (cache_flag &&
      is_cached ("stat_cache.scm", name_s) &&
      is_up_to_date (url_parent (name)))
    {
      tree r= cache_get ("stat_cache.scm", name_s);
      // cout << "Cache : " << r << LF;
      if (r == "#f") return true;
      if ((is_compound(r)) && (N(r)==3)) {
        buf->st_mode = ((unsigned int) as_int (r[0]));
        buf->st_mtime= ((unsigned int) as_int (r[1]));
        buf->st_size = ((unsigned int) as_int (r[2]));
        return false;
      } 
      std_warning << "Inconsistent value in stat_cache.scm for key "
                  << name_s << LF;
      std_warning << "The current value is " << r << LF;
      std_warning << "I'm resetting this key" << LF;
      // continue and recache, the current value is inconsistent. 
    }
  // End caching

  //cout << "No cache" << LF;

  bench_start ("stat");
  bool flag;
  c_string temp (name_s);
  flag= stat (temp, buf);
  (void) link_flag;
  // FIXME: configure should test whether lstat works
  // flag= (link_flag? lstat (temp, buf): stat (temp, buf));
  bench_cumul ("stat");

  // Cache stat results
  if (cache_flag) {
    if (flag) {
      if (do_cache_stat_fail (name_s))
	cache_set ("stat_cache.scm", name_s, "#f");
    }
    else {
      if (do_cache_stat (name_s)) {
        string s1= as_string ((int) buf->st_mode);
        string s2= as_string ((int) buf->st_mtime);
        string s3= as_string ((int) buf->st_size);
	cache_set ("stat_cache.scm", name_s, tree (TUPLE, s1, s2, s3));
      }
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
#ifdef OS_MINGW
  string suf;
  if (filter == "x") {
    suf= suffix(name);
    if ((suf != "exe") && (suf != "bat") && (suf != "com")) {
      name = glue (name, ".exe");
      suf = "exe";
    }
  }
#endif
  bool preserve_links= false;
  for (i=0; i<n; i++)
    preserve_links= preserve_links || (filter[i] == 'l');
  struct_stat buf;
  bool err= get_attributes (name, &buf, preserve_links);
  for (i=0; i<n; i++)
    switch (filter[i]) {
      // FIXME: should check user id and group id for r, w and x
    case 'f':
      if (err || !S_ISREG (buf.st_mode)) return false;
      break;
    case 'd':
      if (err || !S_ISDIR (buf.st_mode)) return false;
      break;
    case 'l':
      if (err || !S_ISLNK (buf.st_mode)) return false;
      break;
    case 'r':
      if (err) return false;
      if ((buf.st_mode & (S_IRUSR | S_IRGRP | S_IROTH)) == 0) return false;
      break;
    case 'w':
      if (err) return false;
      if ((buf.st_mode & (S_IWUSR | S_IWGRP | S_IWOTH)) == 0) return false;
      break;
    case 'x':
      if (err) return false;
#ifdef OS_MINGW
      if (((suf == "exe") || (suf == "com") || (suf == "bat")) && (buf.st_mode & S_IRUSR)) return true;
#endif
      if ((buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) == 0) return false;
      break;
    }
  return true;
}

bool is_regular (url name) { return is_of_type (name, "f"); }
bool is_directory (url name) { return is_of_type (name, "d"); }
bool is_symbolic_link (url name) { return is_of_type (name, "l"); }

int
file_size (url u) {
  if (is_rooted_web (u)) return -1;
  if (is_rooted_tmfs (u)) return -1;
  struct_stat u_stat;
  if (get_attributes (u, &u_stat, true)) return -1;
  return u_stat.st_size;
}

int
last_modified (url u, bool cache_flag) {
  if (is_rooted_web (u))
    return - (int) (((unsigned int) (-1)) >> 1);
  if (is_rooted_tmfs (u))
    return - (int) (((unsigned int) (-1)) >> 1);
  struct_stat u_stat;
  if (get_attributes (u, &u_stat, true, cache_flag))
    return - (int) (((unsigned int) (-1)) >> 1);
  return u_stat.st_mtime;
}

bool
is_newer (url which, url than) {
  struct_stat which_stat;
  struct_stat than_stat;
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
#ifdef OS_MINGW
  unsigned int rnd= raw_time ();
#else
  static bool initialized= false;
  if (!initialized) {
    srandom ((int) raw_time ());
    initialized= true;
  }
  unsigned int rnd= random ();
#endif
  string name= "tmp_" * as_string (rnd) * suffix;
  url u= url_temp_dir () * url (name);
  if (exists (u)) return url_temp (suffix);
  return u;
}

url
url_numbered (url dir, string prefix, string postfix, int i) {
  if (!exists (dir)) mkdir (dir);
  for (; true; i++) {
    url name= dir * (prefix * as_string (i) * postfix);
    if (!exists (name)) return name;
  }
  return dir * (prefix * "x" * postfix);
}

url
url_scratch (string prefix, string postfix, int i) {
  url dir ("$TEXMACS_HOME_PATH/texts/scratch");
  return url_numbered (dir, prefix, postfix, i);
}

bool
is_scratch (url u) {
  return head (u) == url ("$TEXMACS_HOME_PATH/texts/scratch");
}

string
file_format (url u) {
  if (is_rooted_tmfs (u))
    return as_string (call ("tmfs-format", object (u)));
  else return suffix_to_format (suffix (u));
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
  // cout << "Directory " << u << LF;
  u= resolve (u, "dr");
  if (is_none (u)) return array<string> ();
  string name= concretize (u);

  // Directory contents in cache?
  if (is_cached ("dir_cache.scm", name) && is_up_to_date (u))
    return cache_dir_get (name);
  bench_start ("read directory");
  // End caching

  DIR* dp;
  c_string temp (name);
  dp= opendir (temp);
  error_flag= (dp==NULL);
  if (error_flag) return array<string> ();

  array<string> dir;
  #ifdef OS_MINGW
  while (true) {
    const char* nextname =  nowide::readir_entry (dp);
    if (nextname==NULL) break;
    dir << string (nextname);
  #else
  struct dirent* ep;
  while (true) {
    ep= readdir (dp);
    if (ep==NULL) break;
    dir << string (ep->d_name);
  #endif
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
* Commands on files
******************************************************************************/

void
move (url u1, url u2) {
  c_string _u1 (concretize (u1));
  c_string _u2 (concretize (u2));
  (void) rename (_u1, _u2);
}

void
copy (url u1, url u2) {
  string s;
  if (!load_string (u1, s, false))
    (void) save_string (u2, s, false);
}

void
remove_sub (url u) {
  if (is_none (u));
  else if (is_or (u)) {
    remove_sub (u[1]);
    remove_sub (u[2]);
  }
  else {
    c_string _u (concretize (u));
#ifdef OS_MINGW
    if (nowide::remove (_u) && DEBUG_AUTO) {
#else
    if (::remove (_u) && DEBUG_AUTO) {
#endif
      std_warning << "Remove failed: " << strerror (errno) << LF;
      std_warning << "File was: " << u << LF;
    }
  }
}

void
remove (url u) {
  remove_sub (expand (complete (u)));
}

void
append_to (url what, url to) {
  string what_s;
  if (load_string (what, what_s, false) ||
      append_string (to, what_s, false))
    std_warning << "Append failed for " << to << LF;
}

void
rmdir (url u) {
  remove_sub (expand (complete (u, "dr")));
}

void
mkdir (url u) {
#if defined (HAVE_SYS_TYPES_H) && defined (HAVE_SYS_STAT_H)
  if (!exists (u)) {
    if (!is_atomic (u) && !is_root (u)) mkdir (head (u));
    c_string _u (concretize (u));
    (void) ::mkdir (_u, S_IRWXU + S_IRGRP + S_IROTH);
  }
#else
#ifdef OS_MINGW
  system ("mkdir", u);
#else
  system ("mkdir -p", u);
#endif
#endif
}

void
change_mode (url u, int mode) {
#if defined (HAVE_SYS_TYPES_H) && defined (HAVE_SYS_STAT_H)
  c_string _u (concretize (u));
  (void) ::chmod (_u, mode);
#else
  string m0= as_string ((mode >> 9) & 7);
  string m1= as_string ((mode >> 6) & 7);
  string m2= as_string ((mode >> 3) & 7);
  string m3= as_string (mode & 7);
  system ("chmod -f " * m0 * m1 * m2 * m3, u);
#endif
}

/******************************************************************************
* Tab-completion for file names
******************************************************************************/

#ifdef OS_WIN32
#define URL_CONCATER  '\\'
#else
#define URL_CONCATER  '/'
#endif

static void
file_completions_file (array<string>& a, url search, url u) {
  if (is_or (u)) {
    file_completions_file (a, search, u[1]);
    file_completions_file (a, search, u[2]);
  }
  else {
    url v= delta (search * url ("dummy"), u);
    if (is_none (v)) return;
    string s= as_string (v);
    if (is_directory (u)) s= s * string (URL_CONCATER);
    a << s;
  }
}

static void
file_completions_dir (array<string>& a, url search, url dir) {
  if (is_or (search)) {
    file_completions_dir (a, search[1], dir);
    file_completions_dir (a, search[2], dir);
  }
  else if (is_or (dir)) {
    file_completions_dir (a, search, dir[1]);
    file_completions_dir (a, search, dir[2]);
  }
  else {
    url u= search * dir * url_wildcard ("*");
    u= complete (u, "r");
    u= expand (u);
    file_completions_file (a, search, u);
  }
}

array<string>
file_completions (url search, url dir) {
  array<string> a;
  file_completions_dir (a, search, dir);
  return a;
}

/******************************************************************************
* Grepping of strings with heavy caching
******************************************************************************/

hashmap<tree,tree>   grep_cache (url_none () -> t);
hashmap<tree,string> grep_load_cache ("");
hashmap<tree,tree>   grep_complete_cache (url_none () -> t);

static bool
bad_url (url u) {
  if (is_atomic (u))
    return u == url ("aapi") || u == url (".svn");
  else if (is_concat (u))
    return bad_url (u[1]) || bad_url (u[2]);
  else return false;
}

string
grep_load (url u) {
  if (!grep_load_cache->contains (u->t)) {
    //cout << "Loading " << u << "\n";
    string s;
    if (load_string (u, s, false)) s= "";
    grep_load_cache (u->t)= s;
  }
  return grep_load_cache [u->t];
}

url
grep_sub (string what, url u) {
  if (is_or (u))
    return grep_sub (what, u[1]) | grep_sub (what, u[2]);
  else if (bad_url (u))
    return url_none ();
  else {
    string contents= grep_load (u);
    if (occurs (what, contents)) return u;
    else return url_none ();
  }
}

url
grep (string what, url u) {
  tree key= tuple (what, u->t);
  if (!grep_cache->contains (key)) {
    if (!grep_complete_cache->contains (u->t))
      grep_complete_cache (u->t)= expand (complete (u)) -> t;
    url found= grep_sub (what, as_url (grep_complete_cache [u->t]));
    grep_cache (key)= found->t;
  }
  return as_url (grep_cache [key]);
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

string
escape_cork_words (string s) {
  register int i;
  string r;
  for (i=0; i<N(s); i++) {
    if (s[i]=='<') {
      register int j;
      for (j=i+1; j<N(s); j++)
        if (s[j]=='>') break;
      if (j<N(s)) j++;
      if (i+7==j && s[i+1]=='#' && s[j-1]=='>') {
        r << "\\<";
        r << s(i+1, j-1);
        r << "\\>";
        i=j-1;
      }
    } else {
      r << s[i];
    }
  }
  return r;
}

int
search_score (url u, array<string> a) {
  int n= N(a);
  string in= grep_load (u);
  if (N(in) == 0) return 0;

  string suf= suffix (u);
  if (suf == "tmml") {
    for (int i=0; i<n; i++)
      a[i]= cork_to_utf8 (a[i]);
  } else if (suf == "tm") {
    in= locase_all (in);
    for (int i=0; i<n; i++)
      a[i]= locase_all (escape_cork_words (a[i]));
  } else {
    in= locase_all (in);
    for (int i=0; i<n; i++)
      a[i]= locase_all (a[i]);
  }

  int score= 1;
  for (int i=0; i<n; i++) {
    string what= a[i];
    array<int> pos= search (what, in);
    score *= compute_score (what, in, pos, suf);
    if (score == 0) return 0;
    if (score > 1000000) score= 1000000;
  }
  return score;
}

/******************************************************************************
* Finding recursive non hidden subdirectories of a given directory
******************************************************************************/

static void
search_sub_dirs (url& all, url root) {
  if (is_none (root));
  else if (is_or (root)) {
    search_sub_dirs (all, root[2]);
    search_sub_dirs (all, root[1]);
  }
  else if (is_directory (root)) {
    bool err= false;
    array<string> a= read_directory (root, err);
    if (!err) {
      for (int i=N(a)-1; i>=0; i--)
        if (N(a[i])>0 && a[i][0] != '.')
          search_sub_dirs (all, root * a[i]);
    }
    all= root | all;
  }
}

url
search_sub_dirs (url root) {
  url all= url_none ();
  //cout << "Search in " << root << " -> " << expand (complete (root, "dr")) << LF;
  search_sub_dirs (all, expand (complete (root, "dr")));
  return all;
}

/******************************************************************************
* Searching files in a directory tree with caching
******************************************************************************/

array<string> no_strings;
hashmap<tree,int> dir_stamp (0);
hashmap<tree,bool> dir_is_dir (false);
hashmap<tree,array<string> > dir_contents (no_strings);

array<string>
var_read_directory (url u) {
  array<string> d;
  if (is_rooted (u, "default") || is_rooted (u, "file")) {
    bool error_flag= false;
    array<string> a= read_directory (u, error_flag);
    for (int i=0; i<N(a); i++)
      if (!starts (a[i], "."))
        d << a[i];
  }
  return d;
}

url
search_file_in (url u, string name) {
  // cout << "Search in " << u << ", " << name << LF;
  if (!dir_stamp->contains (u->t) ||
      texmacs_time () - dir_stamp [u->t] > 10000) {
    dir_is_dir->reset (u->t);
    dir_contents->reset (u->t);
  }
  dir_stamp (u->t)= texmacs_time ();

  if (!dir_is_dir->contains (u->t))
    dir_is_dir (u->t)= is_directory (u);
  if (!dir_is_dir [u->t]) {
    if (as_string (tail (u)) == name) return u;
    return url_none ();
  }

  if (!dir_contents->contains (u->t)) {
    array<string> d= var_read_directory (u);
    dir_contents (u->t)= d;
  }

  array<string> d= dir_contents [u->t];
  for (int i=0; i<N(d); i++) {
    url f= search_file_in (u * d[i], name);
    if (!is_none (f)) return f;
  }
  return url_none ();
}

bool
find_stop (url u, array<string> stops) {
  if (head (u) == u) return false;
  for (int i=0; i<N(stops); i++)
    if (as_string (tail (u)) == stops[i]) return true;
  return find_stop (head (u), stops);
}

url
search_file_upwards (url u, string name, array<string> stops) {
  // cout << "Search upwards " << u << ", " << name << LF;
  url f= search_file_in (u, name);
  if (!is_none (f)) return f;
  if (head (u) == u) return url_none ();
  if (!find_stop (head (u), stops)) return url_none ();
  for (int i=0; i<N(stops); i++)
    if (as_string (tail (u)) == stops[i]) return url_none ();
  return search_file_upwards (head (u), name, stops);
}
