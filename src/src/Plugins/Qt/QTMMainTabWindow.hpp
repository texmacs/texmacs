/******************************************************************************
* MODULE     : QTMMainTabWindow.hpp
* DESCRIPTION: A tab window that handle multiple moving tabs into windows.
* COPYRIGHT  : (C) 2025 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMMAINTABWINDOW_HPP
#define QTMMAINTABWINDOW_HPP

#include "config.h"

#ifdef OS_ANDROID
#define TEXMACS_EXPERIMENTAL_TABWINDOW
#endif

#include <QTabWidget>

/**
 * @brief A tab window that allows moving tabs between windows.
 * 
 * This class extends QTabWidget to enable dynamic tab management,
 * including the ability to move tabs between separate windows.
 */
class QTMMainTabWindow : public QTabWidget {
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
    return gTopTabWindow; 
  }

protected:
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

public slots:
  /**
   * @brief Closes a tab at the given index.
   * 
   * Emitted when the user requests to close a tab with the close button.
   * 
   * @param index The index of the tab to close.
   */
  void closeTab(int index);

private:
  /**
   * @brief Pointer to the current top tab window.
   * 
   * Tracks the most recently activated tab window.
   */
  static QTMMainTabWindow *gTopTabWindow;
   
};

#endif // QTMMAINTABWINDOW_HPP
