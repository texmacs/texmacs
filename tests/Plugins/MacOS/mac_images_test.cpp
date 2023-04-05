
/******************************************************************************
* MODULE     : mac_images_test.cpp
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>

#include "MacOS/mac_images.h"

class TestMacImages: public QObject {
  Q_OBJECT

private:
  url jpg_file= url ("$TEXMACS_PATH/misc/patterns/wall/solid.jpg");
  int jpg_file_width= 500;
  int jpg_file_height= 500;

private slots:
  void test_mac_supports ();
  void test_mac_image_size ();
};

void
TestMacImages::test_mac_supports () {
#ifdef MACOSX_EXTENSIONS 
  QVERIFY (!mac_supports (url ("$TEXMACS_PATH/misc/images/fancy-c.svg")));
  QVERIFY (mac_supports (url ("$TEXMACS_PATH/misc/images/fancy-c.png")));
  QVERIFY (mac_supports (jpg_file));
#endif
}

void
TestMacImages::test_mac_image_size () {
#ifdef MACOSX_EXTENSIONS 
  int w, h;
  mac_image_size (jpg_file, w, h);
  QCOMPARE (jpg_file_width, w);
  QCOMPARE (jpg_file_height, h);
#endif
}

QTEST_MAIN(TestMacImages)
#include "mac_images_test.moc"
