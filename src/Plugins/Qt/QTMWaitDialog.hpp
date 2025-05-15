/******************************************************************************
* MODULE     : QTMWaitDialog.hpp
* DESCRIPTION: A wait dialog that displays a message while TeXmacs is
               processing, and prevents user interaction with other widgets.
* COPYRIGHT  : (C) 2025 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTM_SPLASHSCREEN_HPP
#define QTM_SPLASHSCREEN_HPP

#include <QDialog>
#include <QLabel>
#include <QVBoxLayout>
#include <QPixmap>
#include "string.hpp"

/**
 * @brief A modal wait dialog that prevents user interaction while TeXmacs is processing.
 * 
 * This dialog displays a message and prevents interaction with other widgets.
 */
class QTMWaitDialog : public QDialog {

    Q_OBJECT

public:
  /**
   * @brief Constructs a new QTMWaitDialog.
   * 
   * Initializes the dialog layout, sets it to modal, and starts a timer to update 
   * the default message periodically.
   *
   *  From the Qt documentation : https://doc.qt.io/qt-5/qdialog.html#modal-prop
   *  "A modal widget prevents other widgets from getting input."
   */
  QTMWaitDialog();

  /**
   * @brief Sets the wait message.
   * 
   * Updates the message displayed in the dialog.
   * 
   * @param message The message to display.
   */
  void setMessage(string message);

  /**
   * @brief Pushes a new message onto the message stack.
   * 
   * The latest pushed message will be displayed until it is popped.
   * 
   * @param message The message to push.
   */
  void pushMessage(string message);

  /**
   * @brief Pops the last pushed message from the stack.
   * 
   * If the stack is empty, the message display is cleared.
   */
  void popMessage();

  /**
   * @brief Checks if the wait dialog is active.
   * An active dialog doesn't mean it is visible yet.
   * 
   * @return True if active, false otherwise.
   */
  inline bool isActive() {
    return active;
  }

  /**
   * @brief Sets the active state of the wait dialog.
   * 
   * If set to active, the dialog is shown; otherwise, it is hidden.
   * 
   * @param active The active state to set.
   */
  inline void setActive(bool active) {
    this->active = active;
    if (active) {
      show();
    } else {
      hide();
    }
  }

private:
  QList<QString> messages;
  QHBoxLayout *layout;
  QLabel *waitMessage;
  QLabel *defaultMessage;
  bool active;
  QPixmap originalPixmap;
};

#endif
