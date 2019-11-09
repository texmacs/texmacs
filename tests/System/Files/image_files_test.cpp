
/******************************************************************************
* MODULE     : image_files_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "image_files.hpp"
#include "url.hpp"
#include "sys_utils.hpp"

TEST (image_files, svg_image_size) {
  int w=0, h=0;
  svg_image_size (url ("$TEXMACS_PATH/misc/images/fancy-c.svg"), w, h);
  ASSERT_EQ (w, 24);
  ASSERT_EQ (h, 24);
}
