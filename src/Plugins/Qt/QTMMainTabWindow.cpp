/******************************************************************************
* MODULE     : QTMMainTabWindow.cpp
* DESCRIPTION: A tab window that handle multiple moving tabs into windows.
* COPYRIGHT  : (C) 2025 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMMainTabWindow.hpp"
#include "QTMOnscreenKeyboard.hpp"

#include "scheme.hpp"

#include <QMouseEvent>
#include <QTabBar>
#include <QApplication>
#include <QPushButton>
#include <QHBoxLayout>
#include <QDockWidget>
#include <QStyle>

QPointer<QTMMainTabWindow> QTMMainTabWindow::gTopTabWindow = nullptr;

QTMMainTabWindow::QTMMainTabWindow() {
  mTabWidget = new QTabWidget(this);
  mTabWidget->setObjectName("mainTabWindowTabs");
#ifdef OS_ANDROID
  mTabWidget->setProperty("tmIsAndroid", true);
#else
  mTabWidget->setProperty("tmIsAndroid", false);
#endif
  setCentralWidget(mTabWidget);

  mTabWidget->setTabsClosable(true);
  mTabWidget->setMovable(true);

  // todo : keep the tab window size and position in the user preferences
#ifndef OS_ANDROID
  setMinimumSize(800, 600);
#endif

  /*
    We do not delete the tab window ourselves.
    We let Qt delete the tab window when the user close it.
  */
  setAttribute(Qt::WA_DeleteOnClose);

  // remove the border and padding
  setDefaultStyle();

  connect(mTabWidget, &QTabWidget::tabCloseRequested, this, &QTMMainTabWindow::closeTab);

  // move the tab window to the center of the screen
#if !defined(OS_ANDROID) && QT_VERSION >= 0x060000
  QRect screenGeometry = QApplication::screens().at(0)->geometry();
  move(screenGeometry.center() - rect().center());
#endif

#if !defined(OS_ANDROID) && QT_VERSION >= 0x060000
  installEventFilter(this);
  mTabWidget->tabBar()->installEventFilter(this);
#endif

#if !defined(OS_ANDROID) && QT_VERSION >= 0x050000
  setupWindowControls();
#endif

  gTopTabWindow = this;

  show();
}

void QTMMainTabWindow::attachOnscreenKeyboard(QTMOnscreenKeyboard* keyboard) {
  if (keyboard == nullptr) return;

  if (mKeyboardDock == nullptr) {
    mKeyboardDock = new QDockWidget(this);
    mKeyboardDock->setObjectName("OnscreenKeyboardDock");
    mKeyboardDock->setAllowedAreas(Qt::BottomDockWidgetArea);
    mKeyboardDock->setFeatures(QDockWidget::NoDockWidgetFeatures);
    mKeyboardDock->setTitleBarWidget(new QWidget(mKeyboardDock));
    addDockWidget(Qt::BottomDockWidgetArea, mKeyboardDock);
  }

  mKeyboardDock->setWidget(keyboard);
  mKeyboardDock->show();
  keyboard->show();
}

void QTMMainTabWindow::setupWindowControls() {
#if QT_VERSION >= 0x050000
  setWindowFlags(windowFlags() | Qt::FramelessWindowHint);

  QWidget* controlContainer = new QWidget(this);
  controlContainer->setObjectName("mainTabWindowControls");
  QHBoxLayout* layout = new QHBoxLayout(controlContainer);

  QPushButton* closeBtn = new QPushButton(controlContainer);
  QPushButton* minBtn = new QPushButton(controlContainer);
  QPushButton* maxBtn = new QPushButton(controlContainer);
  closeBtn->setObjectName("mainTabWindowCloseButton");
  minBtn->setObjectName("mainTabWindowMinButton");
  maxBtn->setObjectName("mainTabWindowMaxButton");

#ifdef OS_MACOS
  layout->setContentsMargins(8, 10, 8, 10); 
  layout->setSpacing(8);

  int btnSize = 12;
  closeBtn->setFixedSize(btnSize, btnSize);
  minBtn->setFixedSize(btnSize, btnSize);
  maxBtn->setFixedSize(btnSize, btnSize);
  closeBtn->setProperty("tmWindowControlStyle", "mac");
  minBtn->setProperty("tmWindowControlStyle", "mac");
  maxBtn->setProperty("tmWindowControlStyle", "mac");

  layout->addWidget(closeBtn);
  layout->addWidget(minBtn);
  layout->addWidget(maxBtn);

  mTabWidget->setCornerWidget(controlContainer, Qt::TopLeftCorner);

#else
  layout->setContentsMargins(0, 0, 0, 0); 
  layout->setSpacing(0); 

  minBtn->setText(QString(QChar(0xE921)));
  maxBtn->setText(QString(QChar(0xE922)));
  closeBtn->setText(QString(QChar(0xE8BB)));

  int winBtnWidth = 40;
  int winBtnHeight = 30;
  closeBtn->setFixedSize(winBtnWidth, winBtnHeight);
  minBtn->setFixedSize(winBtnWidth, winBtnHeight);
  maxBtn->setFixedSize(winBtnWidth, winBtnHeight);
  closeBtn->setProperty("tmWindowControlStyle", "win");
  minBtn->setProperty("tmWindowControlStyle", "win");
  maxBtn->setProperty("tmWindowControlStyle", "win");

  layout->addWidget(minBtn);
  layout->addWidget(maxBtn);
  layout->addWidget(closeBtn);

  mTabWidget->setCornerWidget(controlContainer, Qt::TopRightCorner);
#endif

  connect(closeBtn, &QPushButton::clicked, this, &QWidget::close);
  connect(minBtn, &QPushButton::clicked, this, &QWidget::showMinimized);  
  connect(maxBtn, &QPushButton::clicked, [this, maxBtn]() {
    if (isMaximized()) {
        showNormal();
#ifndef OS_MACOS
        maxBtn->setText(QString(QChar(0xE922)));
#endif
    } else {
        showMaximized();
#ifndef OS_MACOS
        maxBtn->setText(QString(QChar(0xE923)));
#endif
    }
  });

  controlContainer->setVisible(true);
#endif
}

void QTMMainTabWindow::onWindowActivated() {
  gTopTabWindow = this;
}

void QTMMainTabWindow::onDoubleClickOnEmptyTabBarSpace() {
  eval ("new-document*");
}

bool QTMMainTabWindow::eventFilterWindow(QObject *obj, QEvent *event) {
#if QT_VERSION >= 0x060000
  if (event->type() == QEvent::WindowActivate) {
    onWindowActivated();
  }

  // 1. Detect Mouse Press in empty tab space
  if (event->type() == QEvent::MouseButtonPress) {
    QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
    int x = mouseEvent->pos().x();
    int y = mouseEvent->pos().y();
    int tabBarWidth = mTabWidget->tabBar()->width();
    int tabBarHeight = mTabWidget->tabBar()->height();
    
    // Check if clicked in empty space to the right of tabs
    if(x > tabBarWidth && y < tabBarHeight) {
      if (mouseEvent->button() == Qt::LeftButton) {
        isDraggingFramelessWindow = true;
        dragPosition = mouseEvent->globalPos() - frameGeometry().topLeft();
        event->accept();
        return true;
      }
    }
  }

  // 2. Handle Mouse Move to drag the window
  if (event->type() == QEvent::MouseMove && isDraggingFramelessWindow) {
    QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
    if (mouseEvent->buttons() & Qt::LeftButton) {
      move(mouseEvent->globalPos() - dragPosition);
      event->accept();
      return true;
    }
  }

  // 3. Handle Mouse Release
  if (event->type() == QEvent::MouseButtonRelease && isDraggingFramelessWindow) {
    isDraggingFramelessWindow = false;
    event->accept();
    return true;
  }

  // 4. Double click logic for "new-document*"
  if (event->type() == QEvent::MouseButtonDblClick) {
     QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
     if (mouseEvent->pos().x() > mTabWidget->tabBar()->width() && 
       mouseEvent->pos().y() < mTabWidget->tabBar()->height()) {
         onDoubleClickOnEmptyTabBarSpace();
         return true;
     }
  }

  return QMainWindow::eventFilter(obj, event);
#else
  (void) obj; (void) event;
  return false;
#endif
}

bool QTMMainTabWindow::eventFilterTabBar(QObject *obj, QEvent *event) {
#if QT_VERSION >= 0x060000
  if (event->type() == QEvent::MouseButtonPress) {
    handleTabBarMousePress(static_cast<QMouseEvent *>(event));
  }

  if (event->type() == QEvent::MouseMove && 
      (mDragState.isMovingTab || mDragState.isMovingWindow)) {
    handleTabBarMouseMove(static_cast<QMouseEvent *>(event));
  }

  if (event->type() == QEvent::MouseButtonRelease && 
      (mDragState.isMovingTab || mDragState.isMovingWindow)) {
    handleTabBarMouseRelease();
  }
  
  return QMainWindow::eventFilter(obj, event);
#else
  (void) obj; (void) event;
  return false;
#endif
}

bool QTMMainTabWindow::eventFilter(QObject *obj, QEvent *event) {
  if (obj == this) {
    return eventFilterWindow(obj, event);
  }

  return eventFilterTabBar(obj, event);
}

void QTMMainTabWindow::showWidget(QWidget *widget) {
  mTabWidget->addTab(widget, widget->windowTitle());
  mTabWidget->setCurrentWidget(widget);
}

void QTMMainTabWindow::removeWidget(QWidget *widget) {
  mTabWidget->removeTab(mTabWidget->indexOf(widget));
  if (mTabWidget->count() == 0) closeAndSetTopTabWindow();
}

void QTMMainTabWindow::closeTab(int index) {
  // send the close window signal to the widget
#ifdef OS_MACOS
  if (mTabWidget->count() > 1) {
    QWidget *w = mTabWidget->widget(index);
    emit w->close();
  }
#else
    QWidget *w = mTabWidget->widget(index);
    emit w->close();
    if (mTabWidget->count() == 0) closeAndSetTopTabWindow();
#endif
}

void QTMMainTabWindow::tabTitleChanged(QWidget *widget, QString title) {
  int index = mTabWidget->indexOf(widget);
  if (index != -1) mTabWidget->setTabText(index, title);
}

void QTMMainTabWindow::closeAndSetTopTabWindow() {
  gTopTabWindow = nullptr;
  for (QWidget *widget : QApplication::topLevelWidgets()) {
    QTMMainTabWindow *tabWindow = qobject_cast<QTMMainTabWindow *>(widget);
    if (tabWindow && tabWindow != this) {
      gTopTabWindow = tabWindow;
      break;
    }
  }
  close();
}

void QTMMainTabWindow::setDefaultStyle() {
  if (mTabWidget == nullptr) return;
  mTabWidget->setProperty("tmTabDragHover", false);
  mTabWidget->style()->unpolish(mTabWidget);
  mTabWidget->style()->polish(mTabWidget);
  mTabWidget->update();
}

void QTMMainTabWindow::setHoverStyle() {
  if (mTabWidget == nullptr) return;
  mTabWidget->setProperty("tmTabDragHover", true);
  mTabWidget->style()->unpolish(mTabWidget);
  mTabWidget->style()->polish(mTabWidget);
  mTabWidget->update();
}

void QTMMainTabWindow::handleTabBarMousePress(QMouseEvent *event) {
  if (mTabWidget->count() == 1) {
    mDragState.isMovingWindow = true;
    mDragState.newTabWindow = this;
    mDragState.movingTabIndex = 0;
    mDragState.movingTabStartPos = event->pos();
  } else {
    int x = event->pos().x();
    int y = event->pos().y();
    int tabBarWidth = mTabWidget->tabBar()->width();
    int tabBarHeight = mTabWidget->tabBar()->height();
    if (event->button() == Qt::LeftButton && 
        x >= 0 && y >= 0 && x < tabBarWidth && y < tabBarHeight) {
      mDragState.isMovingTab = true;
      mDragState.movingTabIndex = mTabWidget->tabBar()->tabAt(QPoint(x, y));
      mDragState.movingTabStartPos = event->pos();
    }
  }
}

void QTMMainTabWindow::handleTabBarMouseMove(QMouseEvent *event) {
  if (mDragState.isMovingTab) {
    int x = event->pos().x();
    int y = event->pos().y();
    int tabBarWidth = mTabWidget->tabBar()->width();
    int tabBarHeight = mTabWidget->tabBar()->height();
    
    // Check if mouse moved beyond drag threshold
    if (x >= tabBarWidth + TabDragThresholdPx || y >= tabBarHeight + TabDragThresholdPx ||
        x < -TabDragThresholdPx || y < -TabDragThresholdPx) {
      // Detach tab into new window
      mDragState.newTabWindow = new QTMMainTabWindow();
      QWidget *widgetToMove = mTabWidget->widget(mDragState.movingTabIndex);
      mTabWidget->removeTab(mDragState.movingTabIndex);
      mDragState.newTabWindow->showWidget(widgetToMove);
      mDragState.isMovingTab = false;
      mDragState.isMovingWindow = true;
      mDragState.movingTabIndex = 0;
    }
  }
  
  if (mDragState.isMovingWindow) {
    int globalX = event->globalPos().x();
    int globalY = event->globalPos().y();
    globalX -= mDragState.newTabWindow->width() / 2;
    globalY -= 10;

    if (mDragState.newTabWindow) {
      mDragState.newTabWindow->move(globalX, globalY);
    }
    
    updateDropTargetHover(event);
  }
}

void QTMMainTabWindow::updateDropTargetHover(QMouseEvent *event) {
  QTMMainTabWindow *tabWindow = nullptr;
  mDragState.targetTabWindow = nullptr;
  
  for (QWidget *tabWidget : QApplication::topLevelWidgets()) {
    tabWindow = qobject_cast<QTMMainTabWindow *>(tabWidget);
    if (tabWindow == nullptr) continue;

    QPoint globalPos = event->globalPos();
    QPoint localPos = tabWindow->mapFromGlobal(globalPos);
    QRect tabBarRect = tabWindow->mTabWidget->tabBar()->rect();
    tabBarRect.setWidth(tabWindow->width());

    if (tabWindow && tabWindow != mDragState.newTabWindow && 
        tabBarRect.contains(localPos)) {
      if (DEBUG_QT_WIDGETS) cout << "mouse is over another tab bar" << LF;
      mDragState.targetTabWindow = tabWindow;
      tabWindow->setHoverStyle();
      break;
    }
    tabWindow->setDefaultStyle();
  }
}

void QTMMainTabWindow::handleTabBarMouseRelease() {
  mDragState.isMovingWindow = false;
  mDragState.isMovingTab = false;
  
  if (mDragState.targetTabWindow != nullptr) {
    if (DEBUG_QT_WIDGETS) cout << "move the tab to the target tab window" << LF;
    QWidget *widgetToMove = mTabWidget->widget(mDragState.movingTabIndex);
    mTabWidget->removeTab(mDragState.movingTabIndex);
    mDragState.targetTabWindow->showWidget(widgetToMove);
    mDragState.targetTabWindow->setDefaultStyle();
    mDragState.targetTabWindow->activateWindow();
    mDragState.targetTabWindow = nullptr;
    
    if (mTabWidget->count() == 0) {
      if (DEBUG_QT_WIDGETS) cout << "close the tab window" << LF;
      closeAndSetTopTabWindow();
    }
  }
}

