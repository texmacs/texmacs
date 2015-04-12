
/******************************************************************************
* MODULE     : db_disk.cpp
* DESCRIPTION: Storing TeXmacs databases on disk
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Database/database.hpp"
#include "file.hpp"

#define DB_CREATE_ATOM   1
#define DB_CREATE_FIELD  2
#define DB_REMOVE_FIELD  3

/******************************************************************************
* Marshalling and unmarshalling
******************************************************************************/

inline int
get_byte_length (unsigned long int i) {
  int l= 0;
  while (i != 0) { l++; i >>= 8; }
  return l;
}

static void
marshall_number (string& s, unsigned long int i) {
  if (i < 248) s << ((char) ((unsigned char) (i + 8)));
  else {
    s << ((char) get_byte_length (i));
    while (i != 0) {
      s << ((char) ((unsigned char) (i & 0xff)));
      i >>= 8;
    }
  }
}

static unsigned long int
unmarshall_number (string s, int& pos) {
  int n= (int) ((unsigned char) s[pos++]);
  if (n >= 8) return n - 8;
  unsigned long int r= 0;
  unsigned long int p= 1;
  for (int k=0; k<n; k++) {
    unsigned int next= (unsigned int) ((unsigned char) s[pos++]);
    r += next * p;
    p <<= 8;
  }
  return r;
}

static void
marshall_string (string& s, string x) {
  marshall_number (s, N(x));
  s << x;
}

static string
unmarshall_string (string s, int& pos) {
  int n= unmarshall_number (s, pos);
  string r= s (pos, pos+n);
  pos += n;
  return r;
}

/******************************************************************************
* Writing to disk cache
******************************************************************************/

void
database_rep::notify_created_atom (string s) {
  pending << (char) ((unsigned char) DB_CREATE_ATOM);
  marshall_string (pending, s);
  //cout << "Notify created " << s << LF;
}

void
database_rep::notify_extended_field (db_line_nr nr) {
  db_line& l= db[nr];
  pending << (char) ((unsigned char) DB_CREATE_FIELD);
  marshall_number (pending, l->id);
  marshall_number (pending, l->attr);
  marshall_number (pending, l->val);
  marshall_number (pending, (unsigned long int) l->created);
  //cout << "Notify extended " << as_atom (l->id)
  //<< ", " << as_atom (l->attr)
  //<< ", " << as_atom (l->val) << LF;
}

void
database_rep::notify_removed_field (db_line_nr nr) {
  db_line& l= db[nr];
  pending << (char) ((unsigned char) DB_REMOVE_FIELD);
  marshall_number (pending, nr);
  marshall_number (pending, (unsigned long int) l->expires);
  //cout << "Notify removed " << as_atom (l->id)
  //<< ", " << as_atom (l->attr) << LF;
}

/******************************************************************************
* Reading from disk cache
******************************************************************************/

void
database_rep::replay (string s) {
  int pos= 0;
  while (pos < N(s)) {
    unsigned int cmd= (unsigned int) ((unsigned char) s[pos++]);
    switch (cmd) {
    case DB_CREATE_ATOM:
      {
        string x= unmarshall_string (s, pos);
        (void) create_atom (x);
        break;
      }
    case DB_CREATE_FIELD:
      {
        db_atom id  = (db_atom) unmarshall_number (s, pos);
        db_atom attr= (db_atom) unmarshall_number (s, pos);
        db_atom val = (db_atom) unmarshall_number (s, pos);
        db_time t   = (db_time) unmarshall_number (s, pos);
        extend_field (id, attr, val, t);
        break;
      }
    case DB_REMOVE_FIELD:
      {
        db_line_nr nr= (db_line_nr) unmarshall_number (s, pos);
        db_time    t = (db_time)    unmarshall_number (s, pos);
        db[nr]->expires= t;
        break;
      }
    default:
      FAILED ("corrupted TeXmacs database");
      break;
    }
  }
}

/******************************************************************************
* Actual disk operations
******************************************************************************/

void
database_rep::initialize () {
  error_flag= false;
  if (exists (db_name)) {
    if (load_string (db_name, loaded, false)) {
      std_error << "Could not load database file "
                << as_string (db_name) << LF;
      error_flag= true;
    }
    else replay (loaded);
  }
  else {
    if (save_string (db_name, "", false)) {
      std_error << "Could not open database file "
                << as_string (db_name) << LF;
      error_flag= true;
    }
  }
}

void
database_rep::purge () {
  if (error_flag || pending == "") return;
  int rnd= (int) (((unsigned int) random ()) & 0xffffff);
  url db_append= glue (db_name, ".append-" * as_string (rnd));
  if (!save_string (db_append, pending, false)) {
    append_to (db_append, db_name);
    remove (db_append);
    loaded << pending;
    pending= "";
  }
  else {
    std_error << "Could not save to database file "
              << as_string (db_name) << LF;
    error_flag= true;
  }
}

extern array<database> dbs;

void
sync_databases () {
  for (int i=0; i<N(dbs); i++)
    dbs[i]->purge ();
}
