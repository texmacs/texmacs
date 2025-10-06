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
#include <QToolButton>

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
  void addSmallSeparator ();
  void addRightSpacer ();
#endif

protected:
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  bool eventFilter (QObject* watched, QEvent* event) override;
#endif

private:
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  QScrollArea* mScrollArea;
  QHBoxLayout* mLayout;
  QToolButton* mLeftBtn;
  QToolButton* mRightBtn;
  QAction*     mLeftAct;
  QAction*     mRightAct;

  void scrollBy (int dx);
  void updateNavButtons ();
  int scrollStep () const;
#endif
};

#endif // QTMTOOLBAR_HPP
