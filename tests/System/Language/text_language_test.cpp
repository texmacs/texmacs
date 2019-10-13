#include "gtest/gtest.h"

#include "language.hpp"

TEST (get_locale_charset, work) {
  string charset = get_locale_charset ();
  cout << charset << LF;
}
