/******************************************************************************
* MODULE     : android_stacktrace.cpp
* DESCRIPTION: Debugging facilities
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "string.hpp"

string
get_stacktrace (unsigned int max_frames) {
  (void) max_frames;
  return "Backtrace of C++ stack not supported\n";
}
