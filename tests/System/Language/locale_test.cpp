#include "gtest/gtest.h"

#include "language.hpp"

TEST (get_locale_language, work) {
  cout << get_locale_language () << LF;
}

TEST (get_locale_charset, work) {
  cout << get_locale_charset() << LF;
}

