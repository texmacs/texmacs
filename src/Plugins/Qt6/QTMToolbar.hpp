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

#include "basic.hpp"
#include "QTMApplication.hpp"

#include <QToolBar>
#include <QAction>
#include <QScrollArea>
#include <QHBoxLayout>
#include <QToolButton>
#include <QList>
#include <QPointer>

class QTMToolbar : public QWidget {
  Q_OBJECT
  Q_PROPERTY(int toolbarMargin READ toolbarMargin WRITE setToolbarMargin)
  Q_PROPERTY(int iconWidth READ iconWidth WRITE setIconWidth)
  Q_PROPERTY(int iconHeight READ iconHeight WRITE setIconHeight)

public:
  QTMToolbar (const QString& title, QSize iconSize = QSize(), QWidget* parent = 0);
  virtual ~QTMToolbar ();

  void replaceActions (QList<QAction*>* src);
  void replaceButtons (QList<QAction*>* src);

  void addAction (QAction* action);
  void removeAction (QAction* action);
  void clear ();
  void addSeparator ();
  void addSmallSeparator ();
  void addRightSpacer ();

  QList<QTMToolbar*> getAllToolbarsFromMainWindow () const;
  QList<QToolButton*> getAllButtonsFromAllToolbars () const;
  void resetAllButtons(QToolButton* except = nullptr);
  void resetButton(QToolButton* button);

  void setIconSize(const QSize& size); // Sets the icon size for the toolbar
  int toolbarMargin() const { return mToolbarMargin; }
  void setToolbarMargin(int margin);
  int iconWidth() const { return mIconWidth; }
  void setIconWidth(int width);
  int iconHeight() const { return mIconHeight; }
  void setIconHeight(int height);

protected:
  bool eventFilter (QObject* watched, QEvent* event) override;
  void setRightActVisible (bool v);
  void setLeftActVisible (bool v);

private slots:
  void onLeftButtonClicked();
  void onRightButtonClicked();
  void onAnyButtonClicked();
  void onActionMenuAboutToShow();
  void onActionMenuAboutToHide();
  void deferredResetButton(qulonglong buttonPtr);

private:
  QPointer<QScrollArea> mScrollArea;
  QPointer<QHBoxLayout> mLayout;
  QPointer<QPushButton> mLeftBtn;
  QPointer<QPushButton> mRightBtn;
  QPointer<QMenu>   mCurrentMenu;
  int mIconWidth;
  int mIconHeight;
  int mToolbarMargin;

  void scrollBy (int dx);
  void updateNavButtons ();
  void updateToolbarMetrics ();
  int scrollStep () const;
};

#endif // QTMTOOLBAR_HPP
