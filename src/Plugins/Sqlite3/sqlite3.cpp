
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

#ifdef USE_SQLITE3

#include <sqlite3.h>
#include <stdio.h>
#include <string.h>

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
#define sqlite3_bind(orig,tm) \
  (void) symbol_install ("libsqlite3.so", #orig, (pointer&) tm); \
  if (tm == NULL) return;
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
  if (DEBUG_AUTO) cout << "TeXmacs] With linked Sqlite3 support\n";
#else
  if (DEBUG_AUTO) cout << "TeXmacs] Installed Sqlite3 support\n";
#endif

  sqlite3_error= false;
}

/******************************************************************************
* Functionality provided by the plug-in
******************************************************************************/

hashmap<tree,tree> sqlite3_size_cache ("");

bool
sqlite3_present () {
  if (!sqlite3_initialized)
    tm_sqlite3_initialize ();
  return !sqlite3_error;
}

#else // USE_SQLITE3

/******************************************************************************
* If Sqlite3 is not present...
******************************************************************************/

bool sqlite3_present () { return false; }

#endif // USE_SQLITE3
