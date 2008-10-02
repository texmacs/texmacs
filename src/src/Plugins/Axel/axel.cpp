
/******************************************************************************
* MODULE     : axel.cpp
* DESCRIPTION: interface with Axel
* COPYRIGHT  : (C) 2008  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
