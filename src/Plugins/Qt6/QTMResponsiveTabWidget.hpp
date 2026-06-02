/******************************************************************************
* MODULE     : QTMResponsiveTabWidget.hpp
* DESCRIPTION: A responsive tab widget that can adapts to different 
               screen sizes and orientations.
* COPYRIGHT  : (C) 2026 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMRESPONSIVETABWIDGET_HPP
#define QTMRESPONSIVETABWIDGET_HPP

#include <QWidget>
#include <QTabBar>
#include <QFrame>
#include <QList>
#include <QIcon>
#include <QPointer>

class QStackedWidget;
class QListWidget;
class QPushButton;
class QHBoxLayout;
class QBoxLayout;
class QSizeGrip;
class QGridLayout;

// When putting a QTabBar vertically, the text also becomes vertical. 
// This custom QTabBar allows us to keep the text horizontal.
class QTMHorizontalTextTabBar : public QTabBar {
  Q_OBJECT
public:
  explicit QTMHorizontalTextTabBar(QWidget* parent = nullptr);
  int addCustomTab(const QIcon &icon, const QString &text);
  QSize tabSizeHint(int index) const override;

public slots:
  void updateLabelColors(int index);
};

// This widget allows dragging the window by clicking and dragging 
// on this widget
class QTMDraggableTopBar : public QFrame {
  Q_OBJECT
public:
  explicit QTMDraggableTopBar(QWidget* parent = nullptr);
  void setDraggable(bool draggable) { mDraggable = draggable; }

protected:
  void mousePressEvent(QMouseEvent *event) override;
  void mouseMoveEvent(QMouseEvent *event) override;

private:
  QPoint mClickPos;
  bool mDraggable;
};

class QTMResponsiveTabWidget : public QWidget {
  Q_OBJECT

public:
  explicit QTMResponsiveTabWidget(QWidget *parent = nullptr);
  ~QTMResponsiveTabWidget() override;

  void addTab(QWidget* widget, const QString& title, 
              const QIcon& icon = QIcon());
  int count() const;
  void setCurrentIndex(int index);
  int currentIndex() const;

  void setAddButtonVisible(bool visible);
  void setDraggable(bool draggable);
  void setWindowFusion(bool fusion);

signals:
  void newTabRequested();

protected:
  void showEvent(QShowEvent* event) override;
  void hideEvent(QHideEvent* event) override;
  void changeEvent(QEvent *event) override;

private slots:
  void onTabSelected(int index);
  void onListSelected(int index);
  void onBackClicked();
  void onTabMoved(int from, int to);

private:
  void applyMode(int mode);
  void updateNestingVisuals();

  QTMResponsiveTabWidget* getParentTabWidget() const;
  void reevaluateBackButton();
  bool needsBackButton() const;
  bool goBack();

  QPointer<QPushButton> mAddTabBtn;
  QPointer<QPushButton> mMinBtn;
  QPointer<QPushButton> mMaxBtn;
  QPointer<QPushButton> mCloseBtn;

  QPointer<QTMDraggableTopBar> mTopBar;
  QPointer<QHBoxLayout> mTopLayout;
  QPointer<QTMHorizontalTextTabBar> mTabBar;
  QPointer<QListWidget> mListWidget;
  QPointer<QStackedWidget> mContentStack;
  QPointer<QBoxLayout> mDynamicLayout;
  QPointer<QSizeGrip> mSizeGrip;

  QPointer<QWidget> mGridContainer;
  QPointer<QGridLayout> mGridLayout;
  QList<QPointer<QWidget>> mPages;
  int mCurrentGridCols;

  bool mMobileViewingContent;
  bool mIsUpdating;
  bool mWindowFusion;
  bool mIsResizing;
  int mCurrentMode;
  int mCurrentDepth;
};

#endif // QTMRESPONSIVETABWIDGET_HPP