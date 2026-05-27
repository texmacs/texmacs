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
#include "QTMApplication.hpp"

#include "scheme.hpp"

#include <QMouseEvent>
#include <QTabBar>
#include <QApplication>
#include <QPushButton>
#include <QPainter>
#include <QPaintEvent>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QWidget>
#include <QStackedLayout>
#include <QDockWidget>
#include <QStyle>
#include <QStylePainter>
#include <QStyleOptionTab>

#if defined(OS_MINGW)
#include <dwmapi.h> 
#include <windowsx.h>
void applyWindowsAcrylicAndUnifiedToolbar(QWidget* widget);
#endif

#ifdef OS_MACOS
void applyMacOSUnifiedBar(QWidget* widget);
#endif

QPointer<QTMMainTabWindow> QTMMainTabWindow::gTopTabWindow = nullptr;


class QTMLeftAlignedTabBar : public QTabBar {
public:
  using QTabBar::QTabBar;

protected:
  void paintEvent(QPaintEvent *event) override {
    Q_UNUSED(event);

    QStylePainter painter(this);
    for (int i = 0; i < count(); ++i) {
      QStyleOptionTab option;
      initStyleOption(&option, i);

      painter.drawControl(QStyle::CE_TabBarTabShape, option);

      QRect textRect = style()->subElementRect(QStyle::SE_TabBarTabText, &option, this);
      textRect.adjust(6, 0, -4, 0);

      painter.setPen(option.palette.color(isEnabled() ? QPalette::Normal : QPalette::Disabled,
                                          QPalette::WindowText));
      painter.drawText(textRect, Qt::AlignLeft | Qt::AlignVCenter, option.text);
    }
  }
};


static int findCloseButtonTabIndex(QTabBar* tabBar, QPushButton* closeBtn) {
  if (tabBar == nullptr || closeBtn == nullptr) return -1;
  for (int i = 0; i < tabBar->count(); ++i) {
    if (tabBar->tabButton(i, QTabBar::RightSide) == closeBtn) return i;
  }
  return -1;
}


QTMMainTabWindow::QTMMainTabWindow() {
#ifdef OS_MACOS
  this->setUnifiedTitleAndToolBarOnMac(true);
  applyMacOSUnifiedBar(this);
  QWidget* central = this;
#else // OS_MACOS
  QWidget* central = new QWidget(this);
  QVBoxLayout* centralLayout = new QVBoxLayout(central);
  centralLayout->setContentsMargins(0, 0, 0, 0);
  centralLayout->setSpacing(0);

  QWidget* header = new QWidget(central);
  QHBoxLayout* headerLayout = new QHBoxLayout(header);
  headerLayout->setContentsMargins(0, 0, 0, 0);
  headerLayout->setSpacing(0);
#endif // OS_MACOS

  mTabBar = new QTMLeftAlignedTabBar(central);
  mTabBar->setTabsClosable(true);
  mTabBar->setMovable(true);
  mTabBar->setObjectName("mainTabWindowTabs");

  QWidget* stackHost = new QWidget(central);
  mStackedLayout = new QStackedLayout(stackHost);
  mStackedLayout->setContentsMargins(0, 0, 0, 0);

#ifdef OS_ANDROID
  mTabBar->setProperty("tmIsAndroid", true);
#else
  mTabBar->setProperty("tmIsAndroid", false);
#endif

#ifndef OS_MACOS
  headerLayout->addWidget(mTabBar, 1);
#endif

#if defined(OS_MINGW)
  mWindowsCaptionSpacer = new QWidget(header);
  mWindowsCaptionSpacer->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Expanding);

  const int reservedCaptionWidth = 200;
  mWindowsCaptionSpacer->setFixedWidth(reservedCaptionWidth);
  headerLayout->addWidget(mWindowsCaptionSpacer);
#endif

  mTabContainer = central;
#ifdef OS_MACOS
  setCentralWidget(stackHost);
#else
  centralLayout->addWidget(header);
  centralLayout->addWidget(stackHost);
  setCentralWidget(central);
#endif

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

  connect(mTabBar, &QTabBar::currentChanged, this, [this](int index) {
    if (mStackedLayout != nullptr) mStackedLayout->setCurrentIndex(index);
  });
  connect(mTabBar, &QTabBar::tabCloseRequested, this, &QTMMainTabWindow::closeTab);

  // move the tab window to the center of the screen
#if !defined(OS_ANDROID) && QT_VERSION >= 0x060000
  QRect screenGeometry = QApplication::screens().at(0)->geometry();
  move(screenGeometry.center() - rect().center());
#endif

#if !defined(OS_ANDROID) && QT_VERSION >= 0x060000
  installEventFilter(this);
  mTabBar->installEventFilter(this);
#endif

#if !defined(OS_ANDROID) && QT_VERSION >= 0x050000
  setupWindowControls();
#endif

  gTopTabWindow = this;

#if defined(OS_MINGW)
  applyWindowsAcrylicAndUnifiedToolbar(this);
#elif defined(OS_MACOS)
    show();
   applyMacOSUnifiedBar(this);
#else
  show();
#endif
}

void QTMMainTabWindow::resizeEvent(QResizeEvent *event) {
  QMainWindow::resizeEvent(event);

#ifdef OS_MACOS
  // set the position and size of mTabBar
  if (mTabBar != nullptr) {
    // mTabBar->setGeometry(0, 0, width(), mTabBar->height());
    // 100 px for the window controls
    const int controlsWidth = 100;
    mTabBar->setGeometry(controlsWidth, 0, width() - controlsWidth, mTabBar->height());
  }
#endif
  
}

void QTMMainTabWindow::showEvent(QShowEvent *event) {
#ifdef OS_MACOS
  applyMacOSUnifiedBar(this);
#endif
}

void QTMMainTabWindow::setupWindowControls() {
  
}

void QTMMainTabWindow::onWindowActivated() {
  gTopTabWindow = this;
}

void QTMMainTabWindow::onDoubleClickOnEmptyTabBarSpace() {
  eval ("new-document*");
}

bool QTMMainTabWindow::eventFilterWindow(QObject *obj, QEvent *event) {

  // on activated, attach the keyboard
  if (event->type() == QEvent::WindowActivate) {

    QTMOnscreenKeyboard* keyboard = tmapp()->onscreenKeyboard();

    if (keyboard == nullptr) return false;

    // if the dock widget is created and the keyboard is not attached, attach it
    if (mKeyboardDock == nullptr) {
      mKeyboardDock = new QDockWidget(this);
      mKeyboardDock->setObjectName("OnscreenKeyboardDock");
      mKeyboardDock->setAllowedAreas(Qt::BottomDockWidgetArea);
      mKeyboardDock->setFeatures(QDockWidget::NoDockWidgetFeatures);
      mKeyboardDock->setTitleBarWidget(new QWidget(mKeyboardDock));
      addDockWidget(Qt::BottomDockWidgetArea, mKeyboardDock);
    }

    QWidget *parentWidget = keyboard->parentWidget();

    if (parentWidget != mKeyboardDock) {
      QTMMainTabWindow *otherWindow = qobject_cast<QTMMainTabWindow *>(parentWidget);
      if (otherWindow != nullptr) {
        otherWindow->mKeyboardDock->hide();
      }
      mKeyboardDock->setWidget(keyboard);
      mKeyboardDock->show();
      keyboard->show();
    }
  }

#if QT_VERSION >= 0x060000

#ifdef OS_WINDOWS
  const bool isFrameless = true;
#else
  const bool isFrameless = windowFlags().testFlag(Qt::FramelessWindowHint);
#endif

  if (event->type() == QEvent::WindowActivate) {
    onWindowActivated();
  }

  // 1. Detect Mouse Press in empty tab space (frameless fallback only)
  if (isFrameless && event->type() == QEvent::MouseButtonPress) {
    QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
    int x = mouseEvent->pos().x();
    int y = mouseEvent->pos().y();
    int tabBarWidth = mTabBar->width();
    int tabBarHeight = mTabBar->height();
    
    // Check if clicked in empty space to the right of tabs
    if(x > tabBarWidth && y < tabBarHeight) {
      if (mouseEvent->button() == Qt::LeftButton) {
        isDraggingFramelessWindow = true;
        dragPosition = mouseEvent->globalPosition().toPoint() - frameGeometry().topLeft();
        event->accept();
        return true;
      }
    }
  }

  // 2. Handle Mouse Move to drag the window (frameless fallback only)
  if (isFrameless && event->type() == QEvent::MouseMove && isDraggingFramelessWindow) {
    QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
    if (mouseEvent->buttons() & Qt::LeftButton) {
      move(mouseEvent->globalPosition().toPoint() - dragPosition);
      event->accept();
      return true;
    }
  }

  // 3. Handle Mouse Release (frameless fallback only)
  if (isFrameless && event->type() == QEvent::MouseButtonRelease && isDraggingFramelessWindow) {
    isDraggingFramelessWindow = false;
    event->accept();
    return true;
  }

  // 4. Double click logic for "new-document*"
  if (event->type() == QEvent::MouseButtonDblClick) {
     QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
     if (mouseEvent->pos().x() > mTabBar->width() && 
       mouseEvent->pos().y() < mTabBar->height()) {
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

  if (obj == mTabBar) {
    return eventFilterTabBar(obj, event);
  }

  return QMainWindow::eventFilter(obj, event);
}

void QTMMainTabWindow::showWidget(QWidget *widget) {
  if (widget == nullptr || mTabBar == nullptr || mStackedLayout == nullptr) return;

  const int tabIndex = mTabBar->addTab(widget->windowTitle());
  mStackedLayout->addWidget(widget);
  mTabBar->setCurrentIndex(tabIndex);
  mStackedLayout->setCurrentWidget(widget);

  // Create a custom close button for this tab
  QPushButton* closeBtn = new QPushButton(mTabBar);
  closeBtn->setObjectName("closebutton");
  closeBtn->setFlat(true);
  closeBtn->setFocusPolicy(Qt::NoFocus);
  closeBtn->setToolTip("Close tab");
  // Use a Unicode cross character for the close button
  closeBtn->setText(QString::fromUtf8("\u00D7")); // ×
  QFont f = closeBtn->font();
  f.setBold(true);
  closeBtn->setFont(f);

  // Connect the button to close the tab
  connect(closeBtn, &QPushButton::clicked, this, [this, closeBtn]() {
    const int idx = findCloseButtonTabIndex(mTabBar, closeBtn);
    if (idx != -1) closeTab(idx);
  });

  // Set the custom close button on the right side of the tab
  mTabBar->setTabButton(tabIndex, QTabBar::RightSide, closeBtn);
  mTabBar->setTabButton(tabIndex, QTabBar::LeftSide, nullptr);
}

void QTMMainTabWindow::removeWidget(QWidget *widget) {
  if (widget == nullptr || mTabBar == nullptr || mStackedLayout == nullptr) return;
  const int index = mStackedLayout->indexOf(widget);
  if (index == -1) return;

  QWidget* closeButton = mTabBar->tabButton(index, QTabBar::RightSide);
  mTabBar->removeTab(index);
  mStackedLayout->removeWidget(widget);
  widget->setParent(nullptr);
  if (closeButton != nullptr) closeButton->deleteLater();

  if (mTabBar->count() > 0 && mTabBar->currentIndex() == -1) {
    mTabBar->setCurrentIndex(0);
  }

  if (mTabBar->count() == 0) closeAndSetTopTabWindow();
}

void QTMMainTabWindow::closeTab(int index) {
  if (mStackedLayout == nullptr || mTabBar == nullptr) return;
  if (index < 0 || index >= mStackedLayout->count()) return;

  // send the close window signal to the widget
#ifdef OS_MACOS
  if (mTabBar->count() > 1) {
    QWidget *w = mStackedLayout->widget(index);
    emit w->close();
  }
#else
    QWidget *w = mStackedLayout->widget(index);
    emit w->close();
    if (mTabBar->count() == 0) closeAndSetTopTabWindow();
#endif
}

void QTMMainTabWindow::tabTitleChanged(QWidget *widget, QString title) {
  if (mStackedLayout == nullptr || mTabBar == nullptr) return;
  int index = mStackedLayout->indexOf(widget);
  if (index != -1) mTabBar->setTabText(index, title);
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
  if (mTabBar == nullptr) return;
  mTabBar->setProperty("tmTabDragHover", false);
  mTabBar->style()->unpolish(mTabBar);
  mTabBar->style()->polish(mTabBar);
  mTabBar->update();
}

void QTMMainTabWindow::setHoverStyle() {
  if (mTabBar == nullptr) return;
  mTabBar->setProperty("tmTabDragHover", true);
  mTabBar->style()->unpolish(mTabBar);
  mTabBar->style()->polish(mTabBar);
  mTabBar->update();
}

void QTMMainTabWindow::handleTabBarMousePress(QMouseEvent *event) {
  if (mTabBar == nullptr) return;

  if (mTabBar->count() == 1) {
    mDragState.isMovingWindow = true;
#if QT_VERSION >= 0x060000
    mDragState.movingWindowStartPos = event->globalPosition().toPoint();
#else
    mDragState.movingWindowStartPos = event->globalPos();
#endif
    mDragState.newTabWindow = this;
    mDragState.movingTabIndex = 0;
    mDragState.movingTabStartPos = event->pos();
  } else {
    int x = event->pos().x();
    int y = event->pos().y();
    int tabBarWidth = mTabBar->width();
    int tabBarHeight = mTabBar->height();
    if (event->button() == Qt::LeftButton && 
        x >= 0 && y >= 0 && x < tabBarWidth && y < tabBarHeight) {
      mDragState.isMovingTab = true;
      mDragState.movingTabIndex = mTabBar->tabAt(QPoint(x, y));
      mDragState.movingTabStartPos = event->pos();
    }
  }
}

void QTMMainTabWindow::handleTabBarMouseMove(QMouseEvent *event) {
  if (mTabBar == nullptr || mStackedLayout == nullptr) return;

  if (mDragState.isMovingTab) {
    int x = event->pos().x();
    int y = event->pos().y();
    int tabBarWidth = mTabBar->width();
    int tabBarHeight = mTabBar->height();
    
    // Check if mouse moved beyond drag threshold
    if (x >= tabBarWidth + TabDragThresholdPx || y >= tabBarHeight + TabDragThresholdPx ||
        x < -TabDragThresholdPx || y < -TabDragThresholdPx) {
      // Detach tab into new window
      mDragState.newTabWindow = new QTMMainTabWindow();
      QWidget *widgetToMove = mStackedLayout->widget(mDragState.movingTabIndex);
      QWidget* closeButton = mTabBar->tabButton(mDragState.movingTabIndex, QTabBar::RightSide);
      mTabBar->removeTab(mDragState.movingTabIndex);
      mStackedLayout->removeWidget(widgetToMove);
      widgetToMove->setParent(nullptr);
      if (closeButton != nullptr) closeButton->deleteLater();
      mDragState.newTabWindow->showWidget(widgetToMove);
      mDragState.isMovingTab = false;
      mDragState.isMovingWindow = true;
    #if QT_VERSION >= 0x060000
      mDragState.movingWindowStartPos = event->globalPosition().toPoint();
    #else
      mDragState.movingWindowStartPos = event->globalPos();
    #endif
      mDragState.movingTabIndex = 0;
    }
  }
  
  if (mDragState.isMovingWindow) {
#if QT_VERSION >= 0x060000
    const QPoint globalPos = event->globalPosition().toPoint();
#else
    const QPoint globalPos = event->globalPos();
#endif
    int globalX = globalPos.x();
    int globalY = globalPos.y();
    globalX -= mDragState.movingTabStartPos.x();
    globalY -= mDragState.movingTabStartPos.y();

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

#if QT_VERSION >= 0x060000
    QPoint globalPos = event->globalPosition().toPoint();
#else
    QPoint globalPos = event->globalPos();
#endif
    QPoint localPos = tabWindow->mapFromGlobal(globalPos);
    QRect tabBarRect = tabWindow->mTabBar->rect();
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
  if (mTabBar == nullptr || mStackedLayout == nullptr) return;

  mDragState.isMovingWindow = false;
  mDragState.isMovingTab = false;
  
  if (mDragState.targetTabWindow != nullptr) {
    if (DEBUG_QT_WIDGETS) cout << "move the tab to the target tab window" << LF;
    QWidget *widgetToMove = mStackedLayout->widget(mDragState.movingTabIndex);
    QWidget* closeButton = mTabBar->tabButton(mDragState.movingTabIndex, QTabBar::RightSide);
    mTabBar->removeTab(mDragState.movingTabIndex);
    mStackedLayout->removeWidget(widgetToMove);
    widgetToMove->setParent(nullptr);
    if (closeButton != nullptr) closeButton->deleteLater();
    mDragState.targetTabWindow->showWidget(widgetToMove);
    mDragState.targetTabWindow->setDefaultStyle();
    mDragState.targetTabWindow->activateWindow();
    mDragState.targetTabWindow = nullptr;
    
    if (mTabBar->count() == 0) {
      if (DEBUG_QT_WIDGETS) cout << "close the tab window" << LF;
      closeAndSetTopTabWindow();
    }
  }
}

#if defined(OS_MINGW) || defined(OS_MACOS)
bool QTMMainTabWindow::nativeEvent(const QByteArray &eventType, void *message, qintptr *result) {
#if defined(OS_MINGW)
    MSG* msg = static_cast<MSG*>(message);
    HWND hwnd = msg->hwnd;

    if (msg->message == WM_NCHITTEST) {
        LRESULT hit = 0;
        bool handled = DwmDefWindowProc(hwnd, msg->message, msg->wParam, msg->lParam, &hit);
        
        // 1. Laisser DWM gérer ses propres boutons système
        if (handled && hit != HTNOWHERE && hit != HTCLIENT) {
            *result = hit;
            return true;
        }

        // 2. Extraire les coordonnées globales
        int x = GET_X_LPARAM(msg->lParam);
        int y = GET_Y_LPARAM(msg->lParam);
        
        // CORRECTION : Calcul 100% fiable via l'OS pour éviter le décalage de Qt
        RECT winRect;
        GetWindowRect(hwnd, &winRect);
        int localX = x - winRect.left;
        int localY = y - winRect.top;

        // 3. Recréer les bordures de redimensionnement (Essentiel car WM_NCCALCSIZE les a supprimées)
        bool isMaximized = IsZoomed(hwnd);
        if (!isMaximized) {
            const int BORDER_WIDTH = 8; // Épaisseur standard d'une bordure Windows
            int winWidth = winRect.right - winRect.left;
            int winHeight = winRect.bottom - winRect.top;

            bool left = localX < BORDER_WIDTH;
            bool right = localX >= winWidth - BORDER_WIDTH;
            bool top = localY < BORDER_WIDTH;
            bool bottom = localY >= winHeight - BORDER_WIDTH;

            if (top && left) { *result = HTTOPLEFT; return true; }
            if (top && right) { *result = HTTOPRIGHT; return true; }
            if (bottom && left) { *result = HTBOTTOMLEFT; return true; }
            if (bottom && right) { *result = HTBOTTOMRIGHT; return true; }
            if (left) { *result = HTLEFT; return true; }
            if (right) { *result = HTRIGHT; return true; }
            if (top) { *result = HTTOP; return true; }
            if (bottom) { *result = HTBOTTOM; return true; }
        }

        // 4. Gérer le déplacement de la fenêtre (HTCAPTION)
        int tabBarWidth = mTabBar->width();
        int tabBarHeight = mTabBar->height();
        
        // Si on est en haut (dans la barre) ET à droite des onglets
        if (localY < tabBarHeight && localX > tabBarWidth) {
            *result = HTCAPTION;
            return true;
        }

        // Sinon, c'est un clic normal dans l'application
        *result = HTCLIENT;
        return true;
    }

    if (msg->message == WM_NCCALCSIZE && msg->wParam == TRUE) {
        *result = 0;
        return true;
    }
#endif
    
    return QMainWindow::nativeEvent(eventType, message, result);
}
#endif