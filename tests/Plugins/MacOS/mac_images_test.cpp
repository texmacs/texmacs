
/******************************************************************************
* MODULE     : mac_images_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "MacOS/mac_images.h"

TEST (mac_supports, work) {
  ASSERT_FALSE (mac_supports (url ("$TEXMACS_PATH/misc/images/fancy-c.svg")));
  ASSERT_TRUE (mac_supports (url ("$TEXMACS_PATH/misc/images/fancy-c.png")));
}
