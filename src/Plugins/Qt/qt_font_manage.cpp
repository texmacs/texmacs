
/******************************************************************************
* MODULE     : qt_font_manage.cpp
* DESCRIPTION: Manage the collection of installed fonts
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QFontDatabase>

#include "Qt/qt_utilities.hpp"
#include "Qt/qt_font.hpp"
#include "file.hpp"
#include "iterator.hpp"
#include "analyze.hpp"

/******************************************************************************
* Heuristic correspondence between file name and family/style
******************************************************************************/

string
strip_suffix (string name) {
  while (occurs (".", name)) {
    int pos= search_backwards (".", name);
    name= name (0, pos);
  }
  return name;
}

int
good_match_quality (string name, string fam, string sty) {
  if (name == fam * " " * sty || name == fam * sty) return 4;
  if (sty == "Normal" && name == fam) return 3;
  string lname= locase_all (replace (name, " ", ""));
  string lfam = locase_all (replace (fam , " ", ""));
  string lsty = locase_all (replace (sty , " ", ""));
  if (lname == lfam * lsty) return 2;
  if (lsty == "normal" && lname == lfam) return 1;
  return 0;
}

int
match_quality (string name, string fam, string sty) {
  name= strip_suffix (name);
  int q= good_match_quality (name, fam, sty);
  if (q > 0) return 20+q;
  name= replace (name, "Regular", "Normal");
  name= replace (name, "Inclined", "Italic");
  name= replace (name, "Oblique", "Italic");
  q= good_match_quality (name, fam, sty);
  if (q > 0) return 10+q;
  name= replace (name, "ArialHB", "Arial Hebrew");
  name= replace (name, "EuphemiaCAS", "EuphemiaUCAS");
  return good_match_quality (name, fam, sty);
}

/******************************************************************************
* Building the font database
******************************************************************************/

void
qt_font_insert (const QFontDatabase& db, url u) {
  if (is_or (u)) {
    qt_font_insert (db, u[1]);
    qt_font_insert (db, u[2]);
  }
  else if (is_regular (u)) {
    //cout << "Process " << u << "\n";
    string  s = materialize (u);
    QString qs= to_qstring (s);
    int     h = db.addApplicationFont (qs);
    if (h >= 0) {
      QStringList l= db.applicationFontFamilies (h);
      foreach (const QString &qfamily, l) {
        foreach (const QString &qstyle, db.styles (qfamily)) {
          int i;
          string fam= from_qstring (qfamily);
          string sty= from_qstring (qstyle);
          tree   key= tuple (fam, sty);
          string im = as_string (tail (u));
          tree   all= tree (TUPLE);
          if (font_table->contains (key))
            all= font_table [key];
          for (i=0; i<N(all); i++) {
            int qnew= match_quality (im, fam, sty);
            int qold= match_quality (all[i]->label, fam, sty);
            if (qold > qnew || all[i]->label == im) break;
            if (qnew > qold) { all= tree (TUPLE); break; }
          }
          if (i >= N(all)) all << tree (im);
          font_table (key)= all;
        }
      }
      db.removeAllApplicationFonts ();
    }
  }
}

void
font_database_build (url u) {
  QFontDatabase db;
  qt_font_insert (db, u);
}
