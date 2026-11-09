/******************************************************************************
* MODULE     : QTMMainTabWindow.hpp
* DESCRIPTION: A tab window that handles multiple moving tabs into windows.
* COPYRIGHT  : (C) 2026 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMMAINTABWINDOW_HPP
#define QTMMAINTABWINDOW_HPP

#include <QtGlobal>

#if QT_VERSION >= 0x060000

#include "config.h"

#include "QTMMainTab.hpp"

#include <QMainWindow>
#include <QWidget>
#include <QLabel>
#include <QPushButton>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QTabBar>
#include <QStackedLayout>
#include <QDockWidget>
#include <QPointer>
#include <QList>

#include <functional>

class QTMOnscreenKeyboard;
class QTMMainTabWindow;

/**
 * @brief Drag operation states.
 */
enum class DragMode {
  Idle,              // No drag in progress
  PotentialTabDrag,  // Mouse pressed on tab, may drag
  MovingDetached,    // Tab detached, moving with mouse
  Dropping           // About to drop on target
};

/**
 * @brief State for tab drag and drop operations.
 */
struct DragState {
  DragMode mode = DragMode::Idle;
  bool isMovingTab = false;
  bool isMovingWindow = false;
  int movingTabIndex = -1;
  QPoint movingTabStartPos;
  QPoint movingWindowStartPos;
  QPointer<QTMMainTabWindow> newTabWindow = nullptr;
  QPointer<QTMMainTabWindow> targetTabWindow = nullptr;
  
  void reset() {
    mode = DragMode::Idle;
    isMovingTab = false;
    isMovingWindow = false;
    movingTabIndex = -1;
    movingTabStartPos = QPoint();
    newTabWindow = nullptr;
    targetTabWindow = nullptr;
  }
};

/**
 * @brief A tab window that allows moving tabs between windows.
 */
class QTMMainTabWindow : public QMainWindow {
  Q_OBJECT

public:
  QTMMainTabWindow();
  ~QTMMainTabWindow() override;

  void showWidget(QTMMainTab *widget);
  void removeWidget(QTMMainTab *widget);
  void tabTitleChanged(QTMMainTab *widget, QString title);
  void closeAndSetTopTabWindow();

  static QTMMainTabWindow *topTabWindow() { 
    return gTopTabWindow.data();
  }

  void attachKeyboard();

  void registerBackButtonProvider(QWidget *provider,
                                  const std::function<void()> &onBack);
  void unregisterBackButtonProvider(QWidget *provider);
  void setBackButtonProviderVisible(QWidget *provider, bool visible);

protected:
  void resizeEvent(QResizeEvent *event) override;
  void showEvent(QShowEvent *event) override;
  void closeEvent(QCloseEvent *event) override;

  bool eventFilter(QObject * obj, QEvent * event) override;
  bool eventFilterWindow(QObject * obj, QEvent * event);
  bool eventFilterTabBar(QObject * obj, QEvent * event);

  void setDefaultStyle();
  void setHoverStyle();
  void onWindowActivated();
  void onDoubleClickOnEmptyTabBarSpace();

#if defined(OS_MINGW) || defined(OS_MACOS)
#if QT_VERSION >= 0x060000
  bool nativeEvent(const QByteArray &eventType, void *message, qintptr *result) override;
#else
  bool nativeEvent(const QByteArray &eventType, void *message, long *result) override;
#endif
#endif

private:
  struct BackButtonProvider {
    QPointer<QWidget> provider;
    std::function<void()> onBack;
    bool visible = false;
  };

private slots:
  void onBackButtonClicked();
  void onCurrentTabChanged(int index);
  void onWindowOrTabClosed();
  void onTabTitleChangedFromSender(const QString &title);
  void onTabIconChangedFromSender(const QIcon &icon);
  void onCloseButtonClicked();
  void onCloseResetTimeout();
  void onBackButtonProviderDestroyed(QObject *provider);
  void onKeyboardVisibilityChanged(bool visible);

  void handleTabBarMousePress(QMouseEvent *event);
  void handleTabBarMouseMove(QMouseEvent *event);
  void handleTabBarMouseRelease();
  void updateDropTargetHover(QMouseEvent *event);
  void closeTab(int index);
  void onTabBarCountChange();
  void onTabMoved(int from, int to);
  void refreshBackButtonState();

private:
  static QPointer<QTMMainTabWindow> gTopTabWindow;
  
  static constexpr int TabDragThresholdPx = 10;
  static constexpr int DefaultTabHeightDesktop = 30;
  static constexpr int DefaultTabHeightAndroid = 45;
  static constexpr int DefaultMaxTabWidth = 200;
  
  QPointer<QWidget> mCentralContainer;
  QPointer<QVBoxLayout> mCentralLayout;
  QPointer<QWidget> mHeader;
  QPointer<QHBoxLayout> mHeaderLayout;
  QPointer<QWidget> mIconSpacerLeft;
  QPointer<QLabel> mIconLabel;
  QPointer<QWidget> mIconSpacerRight;
  QPointer<QWidget> mStackHost;
  QPointer<QPushButton> mBackButton;
  QPointer<QTabBar> mTabBar;
  QPointer<QStackedLayout> mStackedLayout;
  QPointer<QWidget> mWindowsCaptionSpacer;
  QPointer<QDockWidget> mKeyboardDock;
  QPoint dragPosition;
  bool isDraggingFramelessWindow = false;
  DragState mDragState;
  QList<BackButtonProvider> mBackButtonProviders;
};

#endif // QT_VERSION >= 0x060000
#endif // QTMMAINTABWINDOW_HPP
