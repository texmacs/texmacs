/******************************************************************************
* MODULE     : QTMMainTab.hpp
* DESCRIPTION: Base class for widgets that can be displayed as tabs or windows.
* COPYRIGHT  : (C) 2026 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMMAINTAB_HPP
#define QTMMAINTAB_HPP

#include <QtGlobal>

#if QT_VERSION >= 0x060000

#include <QMainWindow>
#include <QPointer>

class QTMMainTabWindow;

class QTMMainTab : public QMainWindow {
  Q_OBJECT

public:
  QTMMainTab (QWidget *parent = nullptr);
  ~QTMMainTab () override;

  QPointer<QTMMainTabWindow> parentTabWindow () const;

  void setWindowOrTabTitle (const QString &title);
  void setWindowOrTabIcon (const QIcon &icon);
  void resizeWindowOrTab (const QSize &size);
  void closeWindowOrTab ();

signals:
  void requestClose ();
  void windowOrTabClosed ();
  void tabTitleChanged (const QString &title);
  void tabIconChanged (const QIcon &icon);

private:
  QString mTitle;
  QIcon   mIcon;
};

#endif // QT_VERSION >= 0x060000
#endif // QTMMAINTAB_HPP
