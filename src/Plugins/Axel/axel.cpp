
/******************************************************************************
* MODULE     : axel.cpp
* DESCRIPTION: interface with Axel
* COPYRIGHT  : (C) 2008  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Axel/axel.hpp"
#include "dyn_link.hpp"

#ifdef USE_AXEL

/******************************************************************************
* Routines used from Axel
******************************************************************************/

void (*AXEL_test) (void);

/******************************************************************************
* Initialization
******************************************************************************/

static bool axel_initialized= false;
static bool axel_error      = false;

#ifdef LINKED_AXEL
#define axel_bind(orig,tm) \
  tm= orig;
#else
#define axel_bind(orig,tm) \
  (void) symbol_install ("libAxel.so", #orig, (pointer&) tm); \
  if (tm == NULL) return;
#endif

void
axel_initialize () {
  axel_initialized= true;
  axel_error      = true;

  int status= debug_off ();
  axel_bind (axel_test, AXEL_test);
  debug_on (status);

#ifdef LINKED_AXEL
  if (DEBUG_AUTO) cout << "TeXmacs] With linked Axel support\n";
#else
  if (DEBUG_AUTO) cout << "TeXmacs] Installed Axel support\n";
#endif

  axel_error= false;
}

/******************************************************************************
* Functionality provided by the plug-in
******************************************************************************/

bool
axel_present () {
  if (!axel_initialized)
    axel_initialize ();
  return !axel_error;
}

void
axel_test () {
  AXEL_test ();
}

#else // USE_AXEL

/******************************************************************************
* If Axel is not present...
******************************************************************************/

bool axel_present () { return false; }
void axel_test () {}

#endif // USE_AXEL
