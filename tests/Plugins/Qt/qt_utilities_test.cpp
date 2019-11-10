
/******************************************************************************
* MODULE     : qt_utilities_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "Qt/qt_utilities.hpp"

TEST (qt_supports, work) {
  ASSERT_TRUE (qt_supports (url ("x.svg")));
  ASSERT_TRUE (qt_supports (url ("x.png")));
}
