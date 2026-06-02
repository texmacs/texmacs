/******************************************************************************
* MODULE     : QTMMainTabWindow.hpp
* DESCRIPTION: A tab window that handle multiple moving tabs into windows.
* COPYRIGHT  : (C) 2026 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMMAINTABWINDOW_HPP
#define QTMMAINTABWINDOW_HPP

#include "config.h"

#include <QMainWindow>
#include <QTabBar>
#include <QStackedLayout>
#include <QDockWidget>
#include <QPointer>
#include <QList>

#include <functional>

class QTMOnscreenKeyboard;
class QTMMainTabWindow;
class QPushButton;

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
 * 
 * Encapsulates the state of an ongoing or potential tab drag operation.
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
 * 
 * This class extends QMainWindow and embeds a QTabBar + QStackedLayout to enable dynamic tab management,
 * including the ability to move tabs between separate windows.
 */
class QTMMainTabWindow : public QMainWindow {
  Q_OBJECT

public:
  /**
   * @brief Constructs a new QTMMainTabWindow.
   * 
   * Initializes the tab window, sets up event filters, and applies default styles.
   */
  QTMMainTabWindow();

  /**
   * @brief Displays the given widget in the tab window.
   * 
   * If the widget is not already added, it is inserted as a new tab.
   * 
   * @param widget The widget to display.
   */
  void showWidget(QWidget *widget);

  /**
   * @brief Removes the given widget from the tab window.
   * 
   * If the widget is the last one, the tab window may close.
   * 
   * @param widget The widget to remove.
   */
  void removeWidget(QWidget *widget);

  /**
   * @brief Updates the title of a tab.
   * 
   * If the widget is part of the tab window, its tab text is updated.
   * 
   * @param widget The widget whose tab title needs updating.
   * @param title The new title for the tab.
   */
  void tabTitleChanged(QWidget *widget, QString title);

  /**
   * @brief Closes the tab window and updates the global top tab window.
   * 
   * If other tab windows are open, one of them will be set as the active top window.
   */
  void closeAndSetTopTabWindow();

  /**
   * @brief Return the current top-level tab window.
   * 
   * @return A pointer to the top tab window, or nullptr if no tab windows are open.
   */
  static QTMMainTabWindow *topTabWindow() { 
    return gTopTabWindow.data(); // todo : should we return a QPointer ? 
  }

  void registerBackButtonProvider(QWidget *provider,
                                  const std::function<void()> &onBack);
  void unregisterBackButtonProvider(QWidget *provider);
  void setBackButtonProviderVisible(QWidget *provider, bool visible);

protected:
  void resizeEvent(QResizeEvent *event) override;

  void showEvent(QShowEvent *event) override;

  /**
   * @brief Event filter to handle custom events on this object.
   * 
   * Used to detect specific interactions, such as moving tabs between windows.
   * 
   * @param obj The object receiving the event.
   * @param event The event being processed.
   * 
   * @return true if the event is handled, false otherwise.
   */
  bool eventFilter(QObject * obj, QEvent * event) override;

  /**
   * @brief Event filter for the tab window itself.
   * 
   * Handles window activation and mouse interactions within the window.
   * 
   * @param obj The object receiving the event.
   * @param event The event being processed.
   * 
   * @return true if the event is handled, false otherwise.
   */
  bool eventFilterWindow(QObject * obj, QEvent * event);

  /**
   * @brief Event filter for the tab bar.
   * 
   * Detects and handles mouse interactions for moving tabs between windows.
   * 
   * @param obj The object receiving the event.
   * @param event The event being processed.
   * 
   * @return true if the event is handled, false otherwise.
   */
  bool eventFilterTabBar(QObject * obj, QEvent * event);

  /**
   * @brief Applies the default style to the tab window.
   * 
   * Removes borders and padding to achieve a clean UI.
   */
  void setDefaultStyle();

  /**
   * @brief Applies a hover style to indicate tab movement.
   * 
   * Changes the tab appearance when a tab is being dragged.
   */
  void setHoverStyle();

  /**
   * @brief Handles window activation events.
   * 
   * Updates the global top tab window when this window is activated.
   */
  void onWindowActivated();

  /**
   * @brief Handles double-click events on empty tab bar space.
   * 
   * Triggers the creation of a new document.
   */
  void onDoubleClickOnEmptyTabBarSpace();

protected:
#if defined(OS_MINGW) || defined(OS_MACOS)
  bool nativeEvent(const QByteArray &eventType, void *message, qintptr *result) override;
#endif

private:
  struct BackButtonProvider {
    QPointer<QWidget> provider;
    std::function<void()> onBack;
    bool visible = false;
  };

  /**
   * @brief Handles mouse press on tab bar.
   * 
   * Initiates tab drag or window move depending on tab count.
   */
  void handleTabBarMousePress(QMouseEvent *event);

  /**
   * @brief Handles mouse move on tab bar.
   * 
   * Updates tab position during drag and manages drop target detection.
   */
  void handleTabBarMouseMove(QMouseEvent *event);

  /**
   * @brief Handles mouse release on tab bar.
   * 
   * Completes tab drop operation or resets drag state.
   */
  void handleTabBarMouseRelease();

  /**
   * @brief Updates hover style for potential drop targets.
   * 
   * Highlights tab windows under the dragged tab.
   */
  void updateDropTargetHover(QMouseEvent *event);
  /**
   * @brief Closes a tab at the given index.
   * 
   * Emitted when the user requests to close a tab with the close button.
   * 
   * @param index The index of the tab to close.
   */
  void closeTab(int index);

  void onTabBarCountChange();

  void onTabMoved(int from, int to);

  void refreshBackButtonState();

  void attachKeyboard();

private:
  /**
   * @brief Pointer to the current top tab window.
   * 
   * Tracks the most recently activated tab window.
   */
  static QPointer<QTMMainTabWindow> gTopTabWindow;
  
  // Constants for drag and window sizing
  static constexpr int TabDragThresholdPx = 10;
  static constexpr int DefaultTabHeightDesktop = 30;
  static constexpr int DefaultTabHeightAndroid = 45;
  static constexpr int DefaultMaxTabWidth = 200;
  
  QPointer<QWidget> mTabContainer;
  QPointer<QPushButton> mBackBtn;
  QPointer<QTabBar> mTabBar;
  QPointer<QStackedLayout> mStackedLayout;
  QPointer<QWidget> mWindowsCaptionSpacer;
  QPointer<QDockWidget> mKeyboardDock;
  QPoint dragPosition;
  bool isDraggingFramelessWindow = false;
  DragState mDragState;
  QList<BackButtonProvider> mBackButtonProviders;
   
};

#endif // QTMMAINTABWINDOW_HPP
