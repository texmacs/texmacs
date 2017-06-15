
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

#ifdef OS_MINGW
#define random rand
#endif

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
        if (db[nr]->expires == DB_MAX_TIME) outdated++;
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
* Creating a new database with the active entries only
******************************************************************************/

void
database_rep::replay (database clone, int start, bool all) {
  for (int nr=start; nr<N(db); nr++) {
    db_line& l= db[nr];
    if (all || l->expires == DB_MAX_TIME) {
      db_atom id  = clone->as_atom (from_atom (l->id  ));
      db_atom attr= clone->as_atom (from_atom (l->attr));
      db_atom val = clone->as_atom (from_atom (l->val ));
      db_time t   = l->created;
      db_line_nr cnr= clone->extend_field (id, attr, val, t);
      clone->notify_extended_field (cnr);
      //cout << "  Add " << from_atom (l->id) << ", " << from_atom (l->attr) << ", " << from_atom (l->val) << LF;
      if (l->expires != DB_MAX_TIME) {
        clone->db[cnr]->expires= t;
        clone->notify_removed_field (cnr);
        clone->outdated++;
        //cout << "  Removed " << from_atom (l->id) << ", " << from_atom (l->attr) << ", " << from_atom (l->val) << LF;
      }
    }
  }
}

database
database_rep::compress () {
  //cout << "Compressing " << outdated << " items out of " << N(db) << LF;
  database clone (db_name, true);
  replay (clone, 0, false);
  return clone;
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
    else {
      replay (loaded);
      start_pending= N(db);
      time_stamp= last_modified (db_name);
    }
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
  
  if (N(pending) <= 4096) {
    // Appending is an atomic operation on most systems
    // for block sizes of less than 4096 bytes
    int rnd= (int) (((unsigned int) random ()) & 0xffffff);
    url db_append= glue (db_name, ".append-" * as_string (rnd));
    if (!save_string (db_append, pending, false)) {
      if (last_modified (db_name) > time_stamp) {
        // FIXME: this test should really be part of the atomic operation
        remove (db_append);
        return;
      }
      append_to (db_append, db_name);  // NOTE: critical atomic operation
      remove (db_append);
      //cout << "Appended latest changes in " << db_append
      //<< " to " << db_name << LF;
      loaded << pending;
      pending= "";
      start_pending= N(db);
      time_stamp= last_modified (db_name);
      return;
    }
    else remove (db_append);
  }
  else {
    // For larger appends, save the concatenation in a temporary file
    // and use an atomic move in order to replace the old file
    int rnd= (int) (((unsigned int) random ()) & 0xffffff);
    url replace= glue (db_name, ".replace-" * as_string (rnd));
    if (!save_string (replace, loaded * pending, false)) {
      if (last_modified (db_name) > time_stamp) {
        // FIXME: this test should really be part of the atomic operation
        remove (replace);
        return;
      }
      move (replace, db_name);  // NOTE: critical atomic operation
      //cout << "Replaced " << db_name
      //<< " by latest changes in " << replace << LF;
      loaded << pending;
      pending= "";
      start_pending= N(db);
      time_stamp= last_modified (db_name);
      return;
    }
    else remove (replace);
  }

  std_error << "Could not save to database file "
            << as_string (db_name) << LF;
  error_flag= true;
}

extern array<database> dbs;
bool require_check= false;

void
sync_databases () {
  require_check= true;
  for (int i=0; i<N(dbs); i++)
    if (dbs[i]->pending != "") {
      check_for_updates ();
      break;
    }
  require_check= true;
  for (int i=0; i<N(dbs); i++)
    if (dbs[i]->with_history || (2 * dbs[i]->outdated) <= N(dbs[i]->db))
      dbs[i]->purge ();
    else {
      database db= dbs[i]->compress ();
      url current= dbs[i]->db_name;
      int rnd= (int) (((unsigned int) random ()) & 0xffffff);
      url replace= glue (current, ".replace-" * as_string (rnd));
      db->db_name= replace;
      db->purge ();
      db->db_name= current;
      if (db->error_flag)
        dbs[i]->with_history= true;
      else {
        db->start_pending= N(db->db);
        db->time_stamp= last_modified (replace);
        move (replace, current);  // NOTE: critical atomic operation
        dbs[i]= db;
      }
    }
}

void
check_for_updates () {
  if (!require_check) return;
  for (int i=0; i<N(dbs); i++)
    if (last_modified (dbs[i]->db_name) > dbs[i]->time_stamp) {
      //cout << "Updating from disk\n";
      database db (dbs[i]->db_name);
      // FIXME: a more incremental form of updating would be better
      //if (dbs[i]->pending != "") cout << "Replay pending";
      dbs[i]->replay (db, dbs[i]->start_pending, true);
      db->purge ();
      dbs[i]= db;
    }
  require_check= false;
}
