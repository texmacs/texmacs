/******************************************************************************
* MODULE     : QTMOnscreenKeyboard.hpp
* DESCRIPTION: On-screen keyboard widget.
* COPYRIGHT  : (C) 2026 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMONSCREENKEYBOARD_HPP
#define QTMONSCREENKEYBOARD_HPP

#include "QTMDelayedMethodCall.hpp"
#include "scheme.hpp"

#include <QWidget>
#include <QList>
#include <QPointer>
#include <QVBoxLayout>
#include <QPushButton>
#include <QString>


/**
 * @brief On-screen keyboard widget from the custom-keyboard-layout scheme hook.
 *
 * The widget builds rows of QPushButton objects from the layout returned by
 * custom-keyboard-layout, and updates the existing widgets in place.
 */
class QTMOnscreenKeyboard : public QWidget {
  Q_OBJECT

  /**
   * @brief Parsed data for a single key.
   */
  struct KeyData {
    double widthUnits;
    QString label;
    QString cmd;
  };

  using RowData = array<KeyData>;
  using KeyboardRows = array<RowData>;

public:
  /**
   * @brief Construct a new on-screen keyboard widget.
   */
  QTMOnscreenKeyboard();

  inline void show() {
    emit visibilityChanged(true);
    QWidget::show();
  }

  inline void hide() {
    emit visibilityChanged(false);
    QWidget::hide();
  }

  inline void setVisible(bool visible) {
    emit visibilityChanged(visible);
    QWidget::setVisible(visible);
  }

signals:
  void visibilityChanged(bool visible);

private slots:
  /**
   * @brief Fetch and apply the current keyboard layout.
   */
  void initializeKeyboard();

  /**
   * @brief Execute the command bound to the clicked key.
   * @param key Clicked key button.
   */
  void onKeyboardButtonClicked(QPushButton* key);

protected:
  /**
   * @brief Resize handler used to update button geometry.
   */
  void resizeEvent(QResizeEvent* event) override;

  /**
   * @brief Delayed resize callback.
   */
  void onResizeEvent();

  inline void showEvent(QShowEvent* event) override {
    QWidget::showEvent(event);
    emit visibilityChanged(true);
  }

  inline void hideEvent(QHideEvent* event) override {
    QWidget::hideEvent(event);
    emit visibilityChanged(false);
  }

private:
  /**
   * @brief Remove and delete all keyboard row widgets from the layout.
   */
  void clearKeyboardWidgets();

  /**
   * @brief Update key minimum height and font size from widget scale.
   */
  void updateButtonGeometry();

  /**
   * @brief Recompute keyboard zoom-dependent geometry.
   */
  void updateKeyboardZoom();

  /**
   * @brief Return whether any emulated modifier is currently active.
   */
  bool hasActiveEmuModifier() const;

  /**
   * @brief Decide whether a command requires a keyboard layout refresh.
   * @param cmd Command associated with a key.
   * @return true when the layout should be rebuilt or updated.
   */
  bool shouldRefreshLayoutAfterCommand(const QString& cmd) const;

  /**
   * @brief Parse one raw key object into KeyData.
   * @param key Raw key object array.
   * @return Parsed key data.
   */
  KeyData parseKeyData(array<object> key) const;

  /**
   * @brief Parse the full layout object into typed keyboard rows.
   * @param layoutData Raw layout object from Scheme.
   * @return Parsed keyboard rows.
   */
  KeyboardRows parseKeyboardRows(object layoutData) const;

  /**
   * @brief Check whether the current widget tree can be updated in place.
   * @return true if in-place update is possible.
   */
  bool checkCanUpdateInPlace();

  /**
   * @brief Update existing key widgets from parsed rows.
   */
  void updateInPlace();

  /**
   * @brief Rebuild the keyboard widget tree from parsed rows.
   */
  void rebuildKeyboard();

private:
  QPointer<QVBoxLayout> mLayout;
  QList<QPointer<QPushButton>> mButtons;
  QSize mKeyboardSizeHint;
  KeyboardRows mRows;

  QTM_DECL_DELAYED(onResizeEvent);
};

#endif // QTMONSCREENKEYBOARD_HPP