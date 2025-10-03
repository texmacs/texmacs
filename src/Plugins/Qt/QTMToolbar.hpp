/******************************************************************************
* MODULE     : QTMTToolbar.hpp
* DESCRIPTION: Custom toolbar for TeXmacs, that can scroll on Android.
* COPYRIGHT  : (C) 2025 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMTOOLBAR_HPP
#define QTMTOOLBAR_HPP

#ifdef OS_ANDROID
#define ENABLE_EXPERIMENTAL_TOOLBAR
#endif

#include "basic.hpp"

#include <QToolBar>
#include <QAction>
#include <QScrollArea>
#include <QHBoxLayout>

class QTMToolbar : public QToolBar {
  Q_OBJECT

public:
  QTMToolbar (const QString& title, QSize iconSize = QSize(), QWidget* parent = 0);
  virtual ~QTMToolbar ();

  void replaceActions (QList<QAction*>* src);
  void replaceButtons (QList<QAction*>* src);

  void addAction (QAction* action);
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  void removeAction (QAction* action);
  void clear ();
  void addSeparator ();
  void addRightSpacer ();
#endif

private:
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  QScrollArea* mScrollArea;
  QHBoxLayout* mLayout;
#endif

};

#endif // QTMTOOLBAR_HPP