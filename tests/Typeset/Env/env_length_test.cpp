
/******************************************************************************
* MODULE     : python_language_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "env.hpp"
#include "gtest/gtest.h"

auto env= edit_env ();

TEST (edit_env_rep, is_length) {
  auto valid_lengths= list<string>()
    * string("1cm") * string("1mm") * string("1in") * string("1pt")
    * string("1bp") * string("1dd") * string("1pc") * string("1cc")
    * string("1fs") * string("1fbs") * string("1ln") * string("1sep")
    * string("1yfrac") * string("1ex") * string("1emunit")
    * string("1fn") * string("1fns") * string("1bls")
    * string("1spc") * string("1xpsc")
    * string("1w") * string("1h") * string("1l") * string("1r")
    * string("1b") * string("1t")
    * string("1par") * string("1pag") * string("1px") * string("1tmpt");

  for (auto i=0; i<N(valid_lengths); i++) {
    ASSERT_TRUE (env->is_length (valid_lengths[i]));
  }
}
