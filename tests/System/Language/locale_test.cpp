/******************************************************************************
* MODULE     : locale_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "locale.hpp"

TEST (get_locale_language, work) {
  cout << get_locale_language () << LF;
}

TEST (get_locale_charset, work) {
  cout << get_locale_charset() << LF;
}

