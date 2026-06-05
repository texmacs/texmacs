// todo : faire une class Window pour le close
// todo : faire une class Window pour avoir la bare de move sur android
// todo : bouton retour sur android

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
#include <QLabel>

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

#ifdef OS_MINGW
      // on windows, the text has a smaller font
      painter.setPen(option.palette.color(isEnabled() ? QPalette::Normal : QPalette::Disabled,
                                          QPalette::WindowText));
      // reduce the font size
      QFont f = painter.font();
      f.setPointSize(9);
      painter.setFont(f);
#else
      painter.setPen(option.palette.color(isEnabled() ? QPalette::Normal : QPalette::Disabled,
                                          QPalette::WindowText));
#endif
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
  mCentralContainer = this;
#else // OS_MACOS
  mCentralContainer = new QWidget(this);
  mCentralLayout = new QVBoxLayout(mCentralContainer);
  mCentralLayout->setContentsMargins(0, 0, 0, 0);
  mCentralLayout->setSpacing(0);

  mHeader = new QWidget(mCentralContainer);
  mHeaderLayout = new QHBoxLayout(mHeader);
  mHeaderLayout->setContentsMargins(0, 0, 0, 0);
  mHeaderLayout->setSpacing(0);
#endif // OS_MACOS

#ifdef OS_MINGW
  // add a spacer of a few pixel
  mIconSpacerLeft = new QWidget(mHeader);
  mIconSpacerLeft->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Expanding);
  mIconSpacerLeft->setFixedWidth(10);
  mHeaderLayout->addWidget(mIconSpacerLeft);
  // add the texmacs icon as a first widget of the headerLayout
  mIconLabel = new QLabel(mHeader);
  QIcon tmicon = tmapp()->icon_manager().getIcon("TeXmacs");
  mIconLabel->setPixmap(tmicon.pixmap(16, 16));
  mHeaderLayout->addWidget(mIconLabel, 0, Qt::AlignVCenter);
  // add another spacer
  mIconSpacerRight = new QWidget(mHeader);
  mIconSpacerRight->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Expanding);
  mIconSpacerRight->setFixedWidth(10);
  mHeaderLayout->addWidget(mIconSpacerRight);
#endif

  mTabBar = new QTMLeftAlignedTabBar(mCentralContainer);
  mTabBar->setTabsClosable(true);
  mTabBar->setMovable(false);
  mTabBar->setUsesScrollButtons(false);
  mTabBar->setElideMode(Qt::ElideRight);
  mTabBar->setObjectName("mainTabWindowTabs");

  mBackButton = new QPushButton(QString::fromUtf8("\u2190"), mCentralContainer);
  mBackButton->setObjectName("BackButton");
  mBackButton->setVisible(false);
  connect(mBackButton, &QPushButton::clicked, this, [this]() {
    refreshBackButtonState();
    for (int i = mBackButtonProviders.count() - 1; i >= 0; --i) {
      const BackButtonProvider &entry = mBackButtonProviders.at(i);
      if (entry.provider == nullptr) continue;
      if (!entry.visible) continue;
      if (!entry.provider->isVisibleTo(this)) continue;
      if (entry.onBack) entry.onBack();
      break;
    }
    refreshBackButtonState();
  });

  mStackHost = new QWidget(mCentralContainer);
  mStackedLayout = new QStackedLayout(mStackHost);
  mStackedLayout->setContentsMargins(0, 0, 0, 0);

  mTabBar->setProperty("tmSingleTab", true);

#ifndef OS_MACOS
  mHeaderLayout->addWidget(mBackButton, 0, Qt::AlignVCenter);
  mHeaderLayout->addWidget(mTabBar, 1);
#endif

#if defined(OS_MINGW)
  mWindowsCaptionSpacer = new QWidget(mHeader);
  mWindowsCaptionSpacer->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Expanding);

  const int reservedCaptionWidth = 200;
  mWindowsCaptionSpacer->setFixedWidth(reservedCaptionWidth);
  mHeaderLayout->addWidget(mWindowsCaptionSpacer);
#endif

#ifdef OS_MACOS
  setCentralWidget(mStackHost);
#else
  mCentralLayout->addWidget(mHeader);
  mCentralLayout->addWidget(mStackHost);
  setCentralWidget(mCentralContainer);
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
    refreshBackButtonState();
  });
  connect(mTabBar, &QTabBar::tabMoved,
          this, &QTMMainTabWindow::onTabMoved);
  connect(mTabBar, &QTabBar::tabCloseRequested, this, &QTMMainTabWindow::closeTab);

  // move the tab window to the center of the screen
  QRect screenGeometry = QApplication::screens().at(0)->geometry();
#if !defined(OS_ANDROID)
  move(screenGeometry.center() - rect().center());

  installEventFilter(this);
  mTabBar->installEventFilter(this);
#else
  showMaximized();
  setMaximumSize(screenGeometry.width(), screenGeometry.height());
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

#ifdef OS_ANDROID
  attachKeyboard();
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

  // update the dock max height if exist
  if (mKeyboardDock != nullptr) {
    mKeyboardDock->setMaximumHeight(height() / 3);
  }
  
}

void QTMMainTabWindow::showEvent(QShowEvent *event) {
#ifdef OS_MACOS
  applyMacOSUnifiedBar(this);
#endif
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
    attachKeyboard();
  }


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
}

bool QTMMainTabWindow::eventFilterTabBar(QObject *obj, QEvent *event) {
#if defined(OS_MINGW)
  if (event->type() == QEvent::MouseButtonDblClick) {
    QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
    if (mouseEvent->button() == Qt::LeftButton) {
      if (isMaximized()) showNormal();
      else showMaximized();
      return true;
    }
  }
#endif

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

void QTMMainTabWindow::showWidget(QTMMainTab *widget) {
  if (widget == nullptr || mTabBar == nullptr || mStackedLayout == nullptr) return;

  const int tabIndex = mTabBar->addTab(widget->windowIcon(), widget->windowTitle());
  mStackedLayout->addWidget(widget);
  mTabBar->setCurrentIndex(tabIndex);
  mStackedLayout->setCurrentWidget(widget);

  
  connect(widget, &QTMMainTab::windowOrTabClosed, this, [this, widget]() {
    if (mStackedLayout == nullptr) return;
    const int index = mStackedLayout->indexOf(widget);
    if (index != -1) closeTab(index);
  });

  connect(widget, &QTMMainTab::tabTitleChanged, this,
          [this, widget](const QString &title) {
            tabTitleChanged(widget, title);
          });

  connect(widget, &QTMMainTab::tabIconChanged, this,
          [this, widget](const QIcon &icon) {
            if (mStackedLayout == nullptr || mTabBar == nullptr) return;
            const int index = mStackedLayout->indexOf(widget);
            if (index != -1) mTabBar->setTabIcon(index, icon);
          });

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

  onTabBarCountChange();
}

void QTMMainTabWindow::removeWidget(QTMMainTab *widget) {
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

  onTabBarCountChange();

  if (mTabBar->count() == 0) closeAndSetTopTabWindow();
}

void QTMMainTabWindow::closeTab(int index) {
  if (mStackedLayout == nullptr || mTabBar == nullptr) return;
  if (index < 0 || index >= mStackedLayout->count()) return;

  QWidget *closeButton = mTabBar->tabButton(index, QTabBar::RightSide);
  QWidget *w = mStackedLayout->widget(index);
  mTabBar->removeTab(index);
  mStackedLayout->removeWidget(w);
  //w->setParent(nullptr);
  w->deleteLater(); // todo : is this ok ?
  if (closeButton != nullptr) closeButton->deleteLater();

  if (mTabBar->count() > 0 && mTabBar->currentIndex() == -1) {
    mTabBar->setCurrentIndex(0);
  }

  onTabBarCountChange();

  if (mTabBar->count() == 0) closeAndSetTopTabWindow();
}

void QTMMainTabWindow::tabTitleChanged(QTMMainTab *widget, QString title) {
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

void QTMMainTabWindow::registerBackButtonProvider(
  QWidget *provider,
    const std::function<void()> &onBack) {
  if (provider == nullptr) return;

  for (BackButtonProvider &entry : mBackButtonProviders) {
    if (entry.provider == provider) {
      entry.onBack = onBack;
      refreshBackButtonState();
      return;
    }
  }

  BackButtonProvider entry;
  entry.provider = provider;
  entry.onBack = onBack;
  entry.visible = false;
  mBackButtonProviders.append(entry);

  connect(provider, &QObject::destroyed, this, [this, provider]() {
    unregisterBackButtonProvider(provider);
  });

  refreshBackButtonState();
}

void QTMMainTabWindow::unregisterBackButtonProvider(QWidget *provider) {
  if (provider == nullptr) return;

  for (int i = mBackButtonProviders.count() - 1; i >= 0; --i) {
    if (mBackButtonProviders[i].provider == provider) {
      mBackButtonProviders.removeAt(i);
    }
  }

  refreshBackButtonState();
}

void QTMMainTabWindow::setBackButtonProviderVisible(QWidget *provider, bool visible) {
  if (provider == nullptr) return;

  for (BackButtonProvider &entry : mBackButtonProviders) {
    if (entry.provider == provider) {
      entry.visible = visible;
      refreshBackButtonState();
      return;
    }
  }
}

void QTMMainTabWindow::refreshBackButtonState() {
  if (mTabBar == nullptr || mBackButton == nullptr) return;

  BackButtonProvider *activeProvider = nullptr;
  for (int i = mBackButtonProviders.count() - 1; i >= 0; --i) {
    BackButtonProvider &entry = mBackButtonProviders[i];
    if (entry.provider == nullptr) continue;
    if (!entry.visible) continue;
    if (!entry.provider->isVisibleTo(this)) continue;
    activeProvider = &entry;
    break;
  }

  if (activeProvider == nullptr || mTabBar->count() <= 0) {
    mBackButton->setVisible(false);
    if (mIconLabel != nullptr) mIconLabel->setVisible(true);
    if (mIconSpacerLeft != nullptr) mIconSpacerLeft->setVisible(true);
    return;
  }

  mBackButton->setVisible(true);
  if (mIconLabel != nullptr) mIconLabel->setVisible(false);
  if (mIconSpacerLeft != nullptr) mIconSpacerLeft->setVisible(false);
}

void QTMMainTabWindow::handleTabBarMousePress(QMouseEvent *event) {
  if (mTabBar == nullptr) return;

  if (mTabBar->count() == 1) {
    mDragState.isMovingWindow = true;
    mDragState.movingWindowStartPos = event->globalPosition().toPoint();
    mDragState.newTabWindow = this;
    mDragState.movingTabIndex = 0;
    QRect firstTabRect = mTabBar->tabRect(0);
    mDragState.movingTabStartPos = event->pos() - firstTabRect.topLeft();
  } else {
    int x = event->pos().x();
    int y = event->pos().y();
    int tabBarWidth = mTabBar->width();
    int tabBarHeight = mTabBar->height();
    if (event->button() == Qt::LeftButton && 
        x >= 0 && y >= 0 && x < tabBarWidth && y < tabBarHeight) {
      int pressedTabIndex = mTabBar->tabAt(QPoint(x, y));
      if (pressedTabIndex != -1) {
        mDragState.isMovingTab = true;
        mDragState.movingTabIndex = pressedTabIndex;
        QRect pressedTabRect = mTabBar->tabRect(pressedTabIndex);
        mDragState.movingTabStartPos = event->pos() - pressedTabRect.topLeft();
      }
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
        QTMMainTab *widgetToMove =
          qobject_cast<QTMMainTab *>(mStackedLayout->widget(mDragState.movingTabIndex));
        if (widgetToMove == nullptr) return;
      QWidget* closeButton = mTabBar->tabButton(mDragState.movingTabIndex, QTabBar::RightSide);
      mTabBar->removeTab(mDragState.movingTabIndex);
      mStackedLayout->removeWidget(widgetToMove);
      widgetToMove->setParent(nullptr);
      if (closeButton != nullptr) closeButton->deleteLater();
      mDragState.newTabWindow->showWidget(widgetToMove);
      mDragState.isMovingTab = false;
      mDragState.isMovingWindow = true;
      mDragState.movingWindowStartPos = event->globalPosition().toPoint();
      mDragState.movingTabIndex = 0;
    } else if (x >= 0 && y >= 0 && x < tabBarWidth && y < tabBarHeight) {
      // Reorder tabs inside the same bar while dragging.
      const int hoverTabIndex = mTabBar->tabAt(QPoint(x, y));
      if (hoverTabIndex != -1 && hoverTabIndex != mDragState.movingTabIndex) {
        mTabBar->moveTab(mDragState.movingTabIndex, hoverTabIndex);
        mDragState.movingTabIndex = hoverTabIndex;
      }
    }
  }
  
  if (mDragState.isMovingWindow) {
    const QPoint globalPos = event->globalPosition().toPoint();
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

    QPoint globalPos = event->globalPosition().toPoint();
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
    QTMMainTab *widgetToMove =
      qobject_cast<QTMMainTab *>(mStackedLayout->widget(mDragState.movingTabIndex));
    if (widgetToMove == nullptr) return;
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


void QTMMainTabWindow::onTabBarCountChange() {
  if (mTabBar->count() <= 1) {
    mTabBar->setProperty("tmSingleTab", true);
    mTabBar->style()->unpolish(mTabBar);
    mTabBar->style()->polish(mTabBar);

    // hide the close button of all tabs
    for (int i = 0; i < mTabBar->count(); i++) {
      QWidget* closeButton = mTabBar->tabButton(i, QTabBar::RightSide);
      if (closeButton != nullptr) closeButton->setVisible(false);
    }
  } else {
    mTabBar->setProperty("tmSingleTab", false);
    mTabBar->style()->unpolish(mTabBar);
    mTabBar->style()->polish(mTabBar);

    // show the close button of all tabs
    for (int i = 0; i < mTabBar->count(); i++) {
      QWidget* closeButton = mTabBar->tabButton(i, QTabBar::RightSide);
      if (closeButton != nullptr) closeButton->setVisible(true);
    }
  }

  refreshBackButtonState();
}

void QTMMainTabWindow::onTabMoved(int from, int to) {
  if (mStackedLayout == nullptr) return;
  if (from == to) return;
  if (from < 0 || to < 0) return;
  if (from >= mStackedLayout->count() || to >= mStackedLayout->count()) return;

  QWidget *widget = mStackedLayout->widget(from);
  if (widget == nullptr) return;

  mStackedLayout->removeWidget(widget);
  mStackedLayout->insertWidget(to, widget);
  mStackedLayout->setCurrentIndex(mTabBar->currentIndex());
}

void QTMMainTabWindow::attachKeyboard() {
  QTMOnscreenKeyboard* keyboard = tmapp()->onscreenKeyboard();

  if (keyboard == nullptr) return;

  // if the dock widget is created and the keyboard is not attached, attach it
  if (mKeyboardDock == nullptr) {
    mKeyboardDock = new QDockWidget(this);
    mKeyboardDock->setObjectName("OnscreenKeyboardDock");
    mKeyboardDock->setAllowedAreas(Qt::BottomDockWidgetArea);
    mKeyboardDock->setFeatures(QDockWidget::NoDockWidgetFeatures);
    mKeyboardDock->setTitleBarWidget(new QWidget(mKeyboardDock));
    addDockWidget(Qt::BottomDockWidgetArea, mKeyboardDock);

    // limit the size of the dock widget to 1/3 of the window height
    mKeyboardDock->setMaximumHeight(height() / 3);

    connect(keyboard, &QTMOnscreenKeyboard::visibilityChanged, this, [this](bool visible) {
      mKeyboardDock->setVisible(visible);
    });
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