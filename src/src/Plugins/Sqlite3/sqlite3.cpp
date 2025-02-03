
/******************************************************************************
* MODULE     : sqlite3.cpp
* DESCRIPTION: interface with Sqlite3
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Sqlite3/sqlite3.hpp"
#include "dyn_link.hpp"
#include "hashmap.hpp"
#include "analyze.hpp"
#include "config.h"

#ifdef USE_SQLITE3

#include <sqlite3.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/******************************************************************************
* Routines used from Sqlite3
******************************************************************************/

int (*SQLITE3_open) (
  const char *filename,   /* Database filename (UTF-8) */
  sqlite3 **ppDb          /* OUT: SQLite db handle */
);

int (*SQLITE3_close) (sqlite3 *db);

int (*SQLITE3_get_table) (
  sqlite3 *db,          /* An open database */
  const char *zSql,     /* SQL to be evaluated */
  char ***pazResult,    /* Results of the query */
  int *pnRow,           /* Number of result rows written here */
  int *pnColumn,        /* Number of result columns written here */
  char **pzErrmsg       /* Error msg written here */
);

void (*SQLITE3_free_table) (char **result);

/******************************************************************************
* Initialization
******************************************************************************/

static bool sqlite3_initialized= false;
static bool sqlite3_error      = false;

#ifdef LINKED_SQLITE3
#define sqlite3_bind(orig,tm) \
  tm= orig;
#else
#ifdef OS_MACOS
#define sqlite3_bind(orig,tm) \
  (void) symbol_install ("/usr/lib/libsqlite3.dylib", #orig, (pointer&) tm); \
  if (tm == NULL) return;
#else
#define sqlite3_bind(orig,tm) \
  (void) symbol_install ("libsqlite3.so", #orig, (pointer&) tm); \
  if (tm == NULL) return;
#endif
#endif

void
tm_sqlite3_initialize () {
  sqlite3_initialized= true;
  sqlite3_error      = true;

  int status= debug_off ();
  sqlite3_bind (sqlite3_open, SQLITE3_open);
  sqlite3_bind (sqlite3_close, SQLITE3_close);
  sqlite3_bind (sqlite3_get_table, SQLITE3_get_table);
  sqlite3_bind (sqlite3_free_table, SQLITE3_free_table);
  debug_on (status);

#ifdef LINKED_SQLITE3
  if (DEBUG_AUTO) debug_automatic << "With linked Sqlite3 support\n";
#else
  if (DEBUG_AUTO) debug_automatic << "Installed Sqlite3 support\n";
#endif

  sqlite3_error= false;
}

/******************************************************************************
* Functionality provided by the plug-in
******************************************************************************/

bool
sqlite3_present () {
  if (!sqlite3_initialized)
    tm_sqlite3_initialize ();
  return !sqlite3_error;
}

hashmap<tree,pointer> sqlite3_connections (NULL);

string
sql_escape (string s) {
  //return cork_to_utf8 (s);
  return s;
}

string
sql_unescape (string s) {
  //return utf_8_to_cork (s);
  return s;
}

tree
sql_exec (url db_name, string cmd) {
  if (!sqlite3_initialized)
    tm_sqlite3_initialize ();
  if (sqlite3_error) {
    cout << "TeXmacs] ERROR: SQLite support not properly configured.\n";
    return tree (TUPLE);
  }
  string name= concretize (db_name);
  if (!sqlite3_connections->contains (name)) {
    c_string _name (name);
    sqlite3* db= NULL;
    //cout << "Opening " << _name << "\n";
    int status= SQLITE3_open (_name, &db);
    if (status == SQLITE_OK)
      sqlite3_connections (name) = (void*) db;
  }
  if (!sqlite3_connections->contains (name)) {
    cout << "TeXmacs] SQL error: database " << name << " could not be opened\n";
    return tree (TUPLE);
  }
  tree ret (TUPLE);
  sqlite3* db= (sqlite3*) sqlite3_connections [name];
  c_string _cmd (sql_escape (cmd));
  char** tab;
  int rows, cols;
  char* err;
  //cout << "Executing " << _cmd << "\n";
  int status= SQLITE3_get_table (db, _cmd, &tab, &rows, &cols, &err);

  int attempt= 0;
  while (status != SQLITE_OK &&
         string (err) == string ("database is locked") &&
         attempt < 100) {
    usleep (100000);
    attempt++;
    status= SQLITE3_get_table (db, _cmd, &tab, &rows, &cols, &err);
  }

  if (status != SQLITE_OK) {
    // TODO: improve error handling
    cout << "TeXmacs] SQL error\n";
    if (err != NULL) cout << "TeXmacs] " << err << "\n";
  }

  for (int r=0; r<=rows; r++) {
    tree row (TUPLE);
    //cout << "  Row " << r << LF;
    for (int c=0; c<cols; c++) {
      int i= r*cols + c;
      if (tab[i] == NULL) row << tree (TUPLE);
      else {
        row << tree (scm_quote (sql_unescape (tab[i])));
        //cout << "    Column " << c << ": " << tab[i] << LF;
      }
    }
    ret << row;
  }

  SQLITE3_free_table (tab);
  //cout << "Return " << ret << "\n";
  return ret;
}

#else // USE_SQLITE3

/******************************************************************************
* If Sqlite3 is not present...
******************************************************************************/

bool sqlite3_present () {
  return false; }
tree sql_exec (url db_name, string cmd) {
  (void) db_name; (void) cmd; return tree (TUPLE); }

#endif // USE_SQLITE3

/******************************************************************************
* Other routines
******************************************************************************/

string
sql_quote (string s) {
  int i, n= N(s);
  string r;
  r << "'";
  for (i=0; i<n; i++)
    if (s[i] != '\'') r << s[i];
    else r << "''";
  r << "'";
  return r;
}
