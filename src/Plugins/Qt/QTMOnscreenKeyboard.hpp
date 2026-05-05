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

#include "widget.hpp"
#include <QWidget>
#include "QTMDelayedMethodCall.hpp"

class QTMOnscreenKeyboard : public QWidget {
  Q_OBJECT

public:
  QTMOnscreenKeyboard();

private slots:
  void initializeKeyboard();

protected:
  void resizeEvent (QResizeEvent* event) override;
  void onResizeEvent ();

private:
  void updateKeyboardZoom ();
  widget mKeyboardWidget;
  QSize mKeyboardSizeHint;

QTM_DECL_DELAYED(onResizeEvent);


};

#endif // QTMONSCREENKEYBOARD_HPP