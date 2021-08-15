
/******************************************************************************
* MODULE     : list_test.cpp
* DESCRIPTION: test on linked lists with reference counting
* COPYRIGHT  : (C) 2018 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "list.hpp"

static list<long> gen(int64_t n) {
  auto normal = list<long>();
  for (long i=0; i<n; i++)
    normal << i;
  return normal;
}

auto the_nil_list = list<string>();
auto the_atom_list = list<long>(1);
auto normal = list<long>(1, 2, 3, list<long>());

class TestList: public QObject {
  Q_OBJECT

private slots:
  void test_is_nil ();
  void test_is_atom ();

  void test_access ();
  void operate_on_the_last ();
  void test_size ();
  void test_copy ();
  void test_append ();
  void head_and_tail ();
  void test_reverse ();
  void test_remove ();
  void test_contains ();
};

void
TestList::test_is_nil () {
  QCOMPARE (is_nil (the_nil_list), true);
  QCOMPARE (is_nil (the_atom_list), false);
}

void
TestList::test_is_atom () {
  QCOMPARE (is_atom (the_nil_list), false);
  QCOMPARE (is_atom (the_atom_list), true);
}

/******************************************************************************
* tests on output and convertion
******************************************************************************/

void
TestList::test_access () {
  QVERIFY_EXCEPTION_THROWN (the_nil_list[0], string);

  QCOMPARE (the_atom_list[0], 1L);

  QCOMPARE (normal[0], 1);
  QCOMPARE (normal[1], 2);
  QCOMPARE (normal[2], 3);
}

/******************************************************************************
* tests on insertion and suppression
******************************************************************************/

void
TestList::operate_on_the_last () {
  QVERIFY_EXCEPTION_THROWN (access_last(the_nil_list), string);
  QVERIFY_EXCEPTION_THROWN (suppress_last(the_nil_list), string);
  QVERIFY_EXCEPTION_THROWN (last_item(the_nil_list), string);

  auto the_atom_list_copy = copy(the_atom_list);
  access_last(the_atom_list_copy) = 2L;
  QCOMPARE (the_atom_list_copy, list<long>(2L));
  suppress_last (the_atom_list_copy);
  QCOMPARE (the_atom_list_copy, list<long>());
  QCOMPARE (last_item(the_atom_list), 1);

  auto normal_copy = copy(normal);
  access_last (normal_copy) = 4;
  QCOMPARE (normal_copy, list<long>(1, 2, 4, list<long>()));
  suppress_last (normal_copy);
  QCOMPARE (normal_copy, list<long>(1, 2, list<long>()));
  QCOMPARE (last_item(normal), 3);
}

/******************************************************************************
* tests on computations with list<T> structures
******************************************************************************/

void
TestList::test_size () {
  QCOMPARE (N (the_nil_list), 0);
  QCOMPARE (N (the_atom_list), 1);
  QCOMPARE (N (normal), 3);
  for (auto i=4; i<=100; i++) {
    auto list = gen (i);
    QCOMPARE (N (list), i);
  }
}

void
TestList::test_copy () {
  QCOMPARE (copy(the_nil_list), the_nil_list);
  QCOMPARE (copy(the_atom_list), the_atom_list);
  QCOMPARE (copy(normal), normal);
}

void
TestList::test_append() {
  auto appended = the_nil_list * string("a");
  QCOMPARE (appended, list<string>(string("a")));

  auto to_append = list<string>("a");
  QCOMPARE (the_nil_list * to_append, to_append);

  auto nil_to_append = list<string>();
  QCOMPARE (the_nil_list * nil_to_append, nil_to_append);

  QCOMPARE (the_atom_list * list<long>(), the_atom_list);

  QCOMPARE (list<long>(1L, 2L, list<long>()) * list<long>(3L), normal);
  QCOMPARE (list<long>(1L) * list<long>(2L, 3L, list<long>()), normal);
}

void
TestList::head_and_tail() {
  QVERIFY_EXCEPTION_THROWN (head(the_nil_list), string);
  QVERIFY_EXCEPTION_THROWN (tail(the_nil_list), string);

  QCOMPARE (head(the_atom_list), the_atom_list);
  QCOMPARE (tail(the_atom_list), list<long>());

  QCOMPARE (head(normal), list<long>(1));
  QCOMPARE (tail(normal), list<long>(2, 3, list<long>()));
}

void
TestList::test_reverse () {
  QCOMPARE (reverse (the_nil_list), the_nil_list);
  QCOMPARE (reverse (the_atom_list), the_atom_list);
  QCOMPARE (reverse (normal), list<long>(3, 2, 1, list<long>()));
}

void
TestList::test_remove () {
  QCOMPARE (remove (the_nil_list, string("a")), the_nil_list);
  QCOMPARE (remove (the_atom_list, 1L), list<long>());
  QCOMPARE (remove (normal, 2L), list<long>(1, 3, list<long>()));
}

void
TestList::test_contains () {
  QCOMPARE (contains (the_nil_list, string("a")), false);
  QCOMPARE (contains (the_atom_list, 1L), true);
  QCOMPARE (contains (normal, 1L), true);

  QCOMPARE (contains (normal, 2L), true);
  QCOMPARE (contains (normal, 3L), true);
}

QTEST_MAIN(TestList)
#include "list_test.moc"
