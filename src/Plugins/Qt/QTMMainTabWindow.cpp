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

#ifdef TEXMACS_EXPERIMENTAL_TABWINDOW

#include "scheme.hpp"

#include <QMouseEvent>
#include <QTabBar>
#include <QApplication>

QTMMainTabWindow *QTMMainTabWindow::gTopTabWindow = nullptr;

bool isMovingTab = false;
bool isMovingWindow = false;
int movingTabIndex = -1;
QPoint movingTabStartPos;
QTMMainTabWindow *newTabWindow = nullptr;
QTMMainTabWindow *targetTabWindow = nullptr;

QTMMainTabWindow::QTMMainTabWindow() {
  setTabsClosable(true);
  setMovable(true);

  // todo : keep the tab window size and position in the user preferences
  setMinimumSize(800, 600);

  /*
    We do not delete the tab window ourselves.
    We let Qt delete the tab window when the user close it.
  */
  setAttribute(Qt::WA_DeleteOnClose);

  // remove the border and padding
  setDefaultStyle();

  connect(this, SIGNAL(tabCloseRequested(int)), this, SLOT(closeTab(int)));
  show();

  // move the tab window to the center of the screen
  QRect screenGeometry = QApplication::screens().at(0)->geometry();
  move(screenGeometry.center() - rect().center());

  installEventFilter(this);
  tabBar()->installEventFilter(this);

  gTopTabWindow = this;
}

void QTMMainTabWindow::onWindowActivated() {
  gTopTabWindow = this;
}

void QTMMainTabWindow::onDoubleClickOnEmptyTabBarSpace() {
  eval ("new-document*");
}

bool QTMMainTabWindow::eventFilterWindow(QObject *obj, QEvent *event) {

  // if the window is a top level window
  if (event->type() == QEvent::WindowActivate) {
    if (DEBUG_QT_WIDGETS) cout << "TabWindow: WindowActivated" << LF;
    onWindowActivated();
  }

  if (event->type() == QEvent::MouseButtonPress) {
    if (DEBUG_QT_WIDGETS) cout << "TabWindow: MouseButtonPress" << LF;
    QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
    int x = mouseEvent->position().toPoint().x();
    int y = mouseEvent->position().toPoint().y();
    int tabBarWidth = tabBar()->width();
    int tabBarHeight = tabBar()->height();
    if(mouseEvent->position().toPoint().x() > tabBar()->width() && 
       mouseEvent->position().toPoint().y() < tabBar()->height())
    {
      if (DEBUG_QT_WIDGETS) cout << "Mouse on an empty tab bar space" << LF;
      onDoubleClickOnEmptyTabBarSpace();
    }
  }

  return QTabWidget::eventFilter(obj, event);
}

bool QTMMainTabWindow::eventFilterTabBar(QObject *obj, QEvent *event) {
  if (event->type() == QEvent::MouseButtonPress) {
    /* 
      The user pressed the mouse button on the single tab button.
      In that case, the user wants to move the tab window,
      or put the tab into another tab window.
    */
    if (count() == 1) {
      isMovingWindow = true;
      newTabWindow = this;
      movingTabIndex = 0;
      QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
      movingTabStartPos = mouseEvent->position().toPoint();
    } 
    else 
    /* 
      The user pressed the mouse button on one of the tab buttons.
      Maybe the user wants to move the tab to another tab window ?
      Put isMovingTab to true. We will check later if the mouse move too far.
      (in that case, that confirm that the user wants 
      to move the tab to another tab window)
    */
    {
      QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
      int x = mouseEvent->position().toPoint().x();
      int y = mouseEvent->position().toPoint().y();
      int tabBarWidth = tabBar()->width();
      int tabBarHeight = tabBar()->height();
      if (mouseEvent->button() == Qt::LeftButton && 
          x >= 0 && y >= 0 && x < tabBarWidth && y < tabBarHeight) {
        isMovingTab = true;
        movingTabIndex = tabBar()->tabAt(QPoint(x, y));
        movingTabStartPos = mouseEvent->position().toPoint();
      }
    }
  }

  /*
    Here, we know that the user may want to move the tab to another tab window.
    We will check if the mouse move too far. If it is the case, 
    we will create a new tab window.
  */
  if (event->type() == QEvent::MouseMove && isMovingTab) {
    QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
    int x = mouseEvent->position().toPoint().x();
    int y = mouseEvent->position().toPoint().y();
    int tabBarWidth = tabBar()->width();
    int tabBarHeight = tabBar()->height();
    const int dist = 10;
    if (x >= tabBarWidth + dist || y >= tabBarHeight + dist ||
        x < -dist || y < -dist) {
      /* 
        When creating a new tab window, the mouse continue to be pressed
        and to move. While the mouse continue to be pressed, the new tab 
        window will move with the mouse. 
        For that, we put isMovingWindow to true.
      */
      newTabWindow = new QTMMainTabWindow();
      int globalX = mapToGlobal(movingTabStartPos).x();
      int globalY = mapToGlobal(movingTabStartPos).y();
      QWidget *widgetToMove = widget(movingTabIndex);
      removeTab(movingTabIndex);
      newTabWindow->showWidget(widgetToMove);
      isMovingTab = false;
      isMovingWindow = true;
      movingTabIndex = 0;
    }
  }

  /*
    If the mouse move and that isMovingWindow is true, 
    we move the new tab window with the mouse.
    isMovingWindow will be set to false when the mouse button is released.
  */
  if (event->type() == QEvent::MouseMove && isMovingWindow) {
    QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
    int globalX = mouseEvent->globalPosition().toPoint().x();
    int globalY = mouseEvent->globalPosition().toPoint().y();
    globalX -= newTabWindow->width() / 2;
    globalY -= 10;
    newTabWindow->move(globalX, globalY);
    
    /*
      Check if the mouse is over another tab bar.
      If the user release the mouse button over another tab bar, 
      we will move the tab to this tab bar.
    */
    QTMMainTabWindow *tabWindow = nullptr;
    targetTabWindow = nullptr;
    for (QWidget *tabWidget : QApplication::topLevelWidgets()) {
      tabWindow = qobject_cast<QTMMainTabWindow *>(tabWidget);
      if (tabWindow == nullptr) continue;

      QPoint globalPos = mouseEvent->globalPosition().toPoint();
      QPoint localPos = tabWindow->mapFromGlobal(globalPos);
      QRect tabBarRect = tabWindow->tabBar()->rect();
      tabBarRect.setWidth(tabWindow->width());

      if (tabWindow && tabWindow != newTabWindow && 
          tabBarRect.contains(localPos)) {
        if (DEBUG_QT_WIDGETS) cout << "mouse is over another tab bar" << LF;
        targetTabWindow = tabWindow;
        tabWindow->setHoverStyle();
        break;
      }
      tabWindow->setDefaultStyle();
    }
  }

  /*
    If the mouse button is released and that we have a target tab window,
    we move the tab to the target tab window.
  */
  if (event->type() == QEvent::MouseButtonRelease) {
    isMovingWindow = false;
    isMovingTab = false;
    if (targetTabWindow != nullptr) {
      if (DEBUG_QT_WIDGETS) cout << "move the tab to the target tab window" << LF;
      QWidget *widgetToMove = widget(movingTabIndex);
      removeTab(movingTabIndex);
      targetTabWindow->showWidget(widgetToMove);
      targetTabWindow->setDefaultStyle();
      targetTabWindow->activateWindow();
      targetTabWindow = nullptr;
      if (count() == 0) {
        if (DEBUG_QT_WIDGETS) cout << "close the tab window" << LF;
        closeAndSetTopTabWindow();
      }
    }
  }
  return QTabWidget::eventFilter(obj, event);
}

bool QTMMainTabWindow::eventFilter(QObject *obj, QEvent *event) {
  if (obj == this) {
    return eventFilterWindow(obj, event);
  }

  return eventFilterTabBar(obj, event);
}

void QTMMainTabWindow::showWidget(QWidget *widget) {
  addTab(widget, widget->windowTitle());
  setCurrentWidget(widget);
}

void QTMMainTabWindow::removeWidget(QWidget *widget) {
  removeTab(indexOf(widget));
  if (count() == 0) closeAndSetTopTabWindow();
}

void QTMMainTabWindow::closeTab(int index) {
  // send the close window signal to the widget
  QWidget *w = this->widget(index);
  emit w->close();
  if (count() == 0) closeAndSetTopTabWindow();
}

void QTMMainTabWindow::tabTitleChanged(QWidget *widget, QString title) {
  int index = indexOf(widget);
  if (index != -1) setTabText(index, title);
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
  // todo : put this into a css file, and make this more beautiful
  setStyleSheet(
    "QTabBar::tab { "
    "   height: 30px; "
    "   width: 150px; "
    "   border-radius: 0px; "
    "   padding: 0px; "
    "} "
    "QTabWidget::pane { "
    "   border: 0px; "
    "   padding: 0px; "
    "}"
  );
}

void QTMMainTabWindow::setHoverStyle() {
  // todo : put this into a css file, and make this more beautiful
  setStyleSheet(
    "QTabBar::tab { "
    "   height: 30px; "
    "   width: 150px; "
    "   border-radius: 0px; "
    "   padding: 0px; "
    "   background-color: rgba(255, 0, 0, 0.5); "
    "} "
    "QTabWidget::pane { "
    "   border: 0px; "
    "   padding: 0px; "
    "}"
  );
}

#endif // TEXMACS_EXPERIMENTAL_TABWINDOW