/******************************************************************************
* MODULE     : QTMOnscreenKeyboard.cpp
* DESCRIPTION: On-screen keyboard widget.
* COPYRIGHT  : (C) 2026 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMOnscreenKeyboard.hpp"

#include "qt_widget.hpp"
#include "scheme.hpp"

#include <QVBoxLayout>
#include <QResizeEvent>
#include <QTimer>

#include <cmath>
#include <cstdio>

QTMOnscreenKeyboard::QTMOnscreenKeyboard() {

	QVBoxLayout* layout = new QVBoxLayout(this);
	layout->setContentsMargins(0, 0, 0, 0);
	layout->setAlignment(Qt::AlignCenter);

	QTimer::singleShot(10, this, SLOT(initializeKeyboard()));

    setMinimumHeight(200);
    setMinimumWidth(450);

}

void
QTMOnscreenKeyboard::resizeEvent (QResizeEvent* event) {
	(void) event;
    QTM_CALL_DELAYED(onResizeEvent);
}

void
QTMOnscreenKeyboard::onResizeEvent () {
    if (is_nil(mKeyboardWidget)) return;
	updateKeyboardZoom();
    initializeKeyboard();   
}

void
QTMOnscreenKeyboard::updateKeyboardZoom () {
    double w = width();
    double h = height();
    double w0 = mKeyboardSizeHint.width();
    double h0 = mKeyboardSizeHint.height();
    if (w0 <= 0 || h0 <= 0) {
        w0 = 450;
        h0 = 200;
    }
	double z = min(w / w0, h / h0);

	char buf[32];
	snprintf (buf, sizeof (buf), "%.3f", z);
	string zs (buf);

	try {
		eval ("(set-custom-keyboard-magnification \"" * zs * "\")");
	} catch (string &err) {
        cout << "Failed to set custom keyboard magnification: " << err << LF;
	}
}

void QTMOnscreenKeyboard::initializeKeyboard() {
	try {

		object cmd = eval("(lambda args #f)");
		object menu = call("custom-keyboard-widget", cmd);
		object xwid = call("make-menu-widget", menu, 0);
		if (!is_widget(xwid)) {
			cout << "Failed to create custom keyboard widget (not a widget object)." << LF;
			return;
		}

		mKeyboardWidget = as_widget(xwid);
		if (is_nil(mKeyboardWidget)) {
			cout << "Failed to create custom keyboard widget (nil widget)." << LF;
			return;
		}

		QWidget* keyboard = concrete(mKeyboardWidget)->as_qwidget(this);
		if (keyboard == nullptr) {
			cout << "Failed to convert custom keyboard widget to QWidget." << LF;
			return;
		}

        if (!mKeyboardSizeHint.isValid()) {
            mKeyboardSizeHint = keyboard->sizeHint();
            setMinimumSize(mKeyboardSizeHint);
        }

		QVBoxLayout* layout = qobject_cast<QVBoxLayout*> (this->layout());

        // remove any existing widgets from the layout
        while (QLayoutItem* item = layout->takeAt(0)) {
            if (QWidget* widget = item->widget()) {
                widget->hide();
                widget->setParent(nullptr);
                widget->deleteLater();
            }
            delete item;
        }

		if (layout == nullptr) return;
		layout->addWidget(keyboard, 0, Qt::AlignCenter);

	} catch (string& err) {
		cout << "custom-keyboard-widget failed: " << err << LF;
	}
}