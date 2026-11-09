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

#include "scheme.hpp"
#include "boot.hpp"
#include "qt_utilities.hpp"
#include "QTMWidget.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QResizeEvent>
#include <QMoveEvent>
#include <QCloseEvent>
#include <QTimer>

#include <cmath>
#include <cstdio>

QTMOnscreenKeyboard::QTMOnscreenKeyboard()
  : mWasShown(false), mRestoringSize(false),
    mLastReportedVisibility(false), mLastPos(-1, -1) {
	setWindowTitle("On-screen Keyboard");
	QSizePolicy sp_retain = sizePolicy();
	sp_retain.setRetainSizeWhenHidden(true);
	setSizePolicy(sp_retain);

	mLayout = new QVBoxLayout(this);
	mLayout->setContentsMargins(0, 0, 0, 0);
	mLayout->setSpacing(0);

	QTimer::singleShot(10, this, SLOT(initializeKeyboard()));
}

void QTMOnscreenKeyboard::loadPreferences() {
	if (hasValidSize()) return;
	string sh = get_user_preference ("height onscreen-keyboard", "");
	string sw = get_user_preference ("width onscreen-keyboard", "");
	string sx = get_user_preference ("abscissa onscreen-keyboard", "");
	string sy = get_user_preference ("ordinate onscreen-keyboard", "");
	if (sh != "") {
		int h = as_int (sh);
		int w = sw != "" ? as_int (sw) : 0;
		if (isValidKeyboardHeight(h)) {
			mLastSize = QSize (w > 50 ? w : 450, h);
		}
	}
	if (sx != "" && sy != "") {
		int x = as_int (sx);
		int y = as_int (sy);
		if (x >= 0 && y >= 0) {
			mLastPos = QPoint (x, y);
		}
	}
}

void QTMOnscreenKeyboard::savePreferences() {
	if (!mWasShown || mRestoringSize) return;
	if (height() >= 100) {
		mLastSize = size();
		set_user_preference ("height onscreen-keyboard", as_string (height()));
		if (isWindow()) {
			if (width() > 50) {
				set_user_preference ("width onscreen-keyboard", as_string (width()));
			}
			QPoint p = pos();
			if (p.x() >= 0 && p.y() >= 0) {
				mLastPos = p;
				set_user_preference ("abscissa onscreen-keyboard", as_string (p.x()));
				set_user_preference ("ordinate onscreen-keyboard", as_string (p.y()));
			}
		}
		save_user_preferences ();
	}
}

int QTMOnscreenKeyboard::lastHeight() {
	loadPreferences();
	if (hasValidSize()) {
		return mLastSize.height();
	}
	if (mKeyboardSizeHint.isValid() && mKeyboardSizeHint.height() >= 100) {
		return mKeyboardSizeHint.height();
	}
	return 200;
}

QSize QTMOnscreenKeyboard::lastSize() {
	loadPreferences();
	if (hasValidSize()) {
		return mLastSize;
	}
	if (mKeyboardSizeHint.isValid()) {
		return mKeyboardSizeHint;
	}
	return QSize(450, 200);
}

void QTMOnscreenKeyboard::reportVisibility(bool visible) {
	if (mLastReportedVisibility == visible) return;
	mLastReportedVisibility = visible;
	emit visibilityChanged(visible);
}

void QTMOnscreenKeyboard::show() {
	setVisible(true);
}

void QTMOnscreenKeyboard::hide() {
	setVisible(false);
}

void QTMOnscreenKeyboard::setVisible(bool visible) {
	if (visible == isVisible()) return;

	if (visible) {
		loadPreferences();
		mRestoringSize = true;
		if (isWindow()) {
			if (hasValidSize()) {
				resize(mLastSize);
			}
			if (mLastPos.x() >= 0 && mLastPos.y() >= 0) {
				move(mLastPos);
			}
		}
	} else {
		if (mWasShown && !mRestoringSize && height() >= 100) {
			savePreferences();
		}
	}

	reportVisibility(visible);
	QWidget::setVisible(visible);

	if (visible) {
		mWasShown = true;
		QTimer::singleShot(300, this, [this]() {
			mRestoringSize = false;
		});
	}
}

void QTMOnscreenKeyboard::resizeEvent(QResizeEvent* event) {
	(void)event;
    QTM_CALL_DELAYED(onResizeEvent);
}

void QTMOnscreenKeyboard::moveEvent(QMoveEvent* event) {
	QWidget::moveEvent(event);
	if (mWasShown && !mRestoringSize && isWindow() && isVisible()) {
		QTM_CALL_DELAYED(onResizeEvent);
	}
}

void QTMOnscreenKeyboard::closeEvent(QCloseEvent* event) {
	hide();
	event->accept();
}

void QTMOnscreenKeyboard::onResizeEvent() {
	updateKeyboardZoom();
	if (mWasShown && !mRestoringSize && isVisible() && height() >= 100) {
		savePreferences();
	}
}

void QTMOnscreenKeyboard::showEvent(QShowEvent* event) {
	loadPreferences();
	if (isWindow()) {
		if (hasValidSize()) {
			resize(mLastSize);
		}
		if (mLastPos.x() >= 0 && mLastPos.y() >= 0) {
			move(mLastPos);
		}
	}
	QWidget::showEvent(event);
	reportVisibility(true);
	mWasShown = true;
	QTimer::singleShot(300, this, [this]() {
		mRestoringSize = false;
	});
}

void QTMOnscreenKeyboard::hideEvent(QHideEvent* event) {
	if (mWasShown && !mRestoringSize && height() >= 100) {
		savePreferences();
	}
	QWidget::hideEvent(event);
	reportVisibility(false);
}

QSize QTMOnscreenKeyboard::sizeHint() const {
	if (hasValidSize()) {
		int w = (mLastSize.width() > 50) ? mLastSize.width() :
				(mKeyboardSizeHint.isValid() ? mKeyboardSizeHint.width() : 450);
		return QSize(w, mLastSize.height());
	}
	string sh = get_user_preference ("height onscreen-keyboard", "");
	string sw = get_user_preference ("width onscreen-keyboard", "");
	if (sh != "") {
		int h = as_int (sh);
		int w = sw != "" ? as_int (sw) : 0;
		if (isValidKeyboardHeight(h)) {
			return QSize(w > 50 ? w : 450, h);
		}
	}
	if (mKeyboardSizeHint.isValid()) return mKeyboardSizeHint;
	return QSize(450, 200);
}

QSize QTMOnscreenKeyboard::minimumSizeHint() const {
	return QSize(200, 60);
}

void QTMOnscreenKeyboard::updateKeyboardZoom() {
    updateButtonGeometry();
}

void QTMOnscreenKeyboard::clearKeyboardWidgets() {
	if (mLayout == nullptr) return;
	mButtons.clear();

	while (QLayoutItem* item = mLayout->takeAt(0)) {
		if (QWidget* widget = item->widget()) {
			widget->hide();
			widget->setParent(nullptr);
			widget->deleteLater();
		}
		delete item;
	}
}

void QTMOnscreenKeyboard::updateButtonGeometry() {
	const double w0= (mKeyboardSizeHint.width() > 0)?
					mKeyboardSizeHint.width(): 450.0;
	const double h0= (mKeyboardSizeHint.height() > 0)?
					mKeyboardSizeHint.height(): 200.0;
    const double z = max(0.6, min(width() / w0, height() / h0));

    // const int minHeight = (int) round(26.0 * z);
    const int fontSize = (int) round(10.0 * z);
	for (const QPointer<QPushButton>& key : mButtons) {
        if (key == nullptr) continue;
        //key->setMinimumHeight(minHeight);
        QFont f = key->font();
        f.setPointSize(max(8, fontSize));
        key->setFont(f);
    }
}

void QTMOnscreenKeyboard::onKeyboardButtonClicked(QPushButton* key) {
    if (key == nullptr) return;
    QString cmd = key->property("tm-cmd").toString();
    if (cmd.isEmpty()) return;
	bool refreshLayout = shouldRefreshLayoutAfterCommand(cmd);

    try {
		eval(string(cmd.toUtf8().constData()));
    } catch (string& err) {
        cout << "custom-keyboard key command failed: " << err << LF;
    }

	if (refreshLayout)
	  QTimer::singleShot(0, this, SLOT(initializeKeyboard()));

	QTMWidget::setFocusToLast();
}

bool QTMOnscreenKeyboard::hasActiveEmuModifier() const {
	const char* mods[] = { "fn", "shift", "ctrl", "alt", "cmd", "lock" };
	for (const char* mod : mods) {
		try {
			object active = call("emu-active-modifier?", object(mod));
			if (is_bool(active) && as_bool(active)) return true;
		} catch (string&) {
			// Ignore and keep trying other modifiers.
		}
	}
	return false;
}

bool QTMOnscreenKeyboard::shouldRefreshLayoutAfterCommand(const QString& cmd) const {
	// todo : we need a more intelligent way to determine whether the layout should be refreshed
	QString s = cmd.trimmed();
	if (s.startsWith("(emu-toggle-modifier ")) return true;
	if (s.startsWith("(emu-key ")) return hasActiveEmuModifier();
	return false;
}

QTMOnscreenKeyboard::KeyData
QTMOnscreenKeyboard::parseKeyData(array<object> key) const {
	KeyData data;
	data.widthUnits = 1.0;
	if (is_double(key[0])) data.widthUnits = as_double(key[0]);
	else if (is_int(key[0])) data.widthUnits = (double) as_int(key[0]);

	string label = is_string(key[1])? as_string(key[1]): string("");
	string cmd = is_string(key[2])? as_string(key[2]): string("");
	data.label = to_qstring(label);
	data.cmd = to_qstring(cmd);
	return data;
}

QTMOnscreenKeyboard::KeyboardRows
QTMOnscreenKeyboard::parseKeyboardRows(object layoutData) const {
	KeyboardRows parsedRows;
	if (!is_list(layoutData)) return parsedRows;

	array<object> rows = as_array_object(layoutData);
	for (int i = 0; i < N(rows); i++) {
		if (!is_list(rows[i])) continue;
		array<object> cols = as_array_object(rows[i]);

		RowData row;
		for (int j = 0; j < N(cols); j++) {
			if (!is_list(cols[j])) continue;
			array<object> key = as_array_object(cols[j]);
			if (N(key) < 3) continue;
			row << parseKeyData(key);
		}

		parsedRows << row;
	}

	return parsedRows;
}

bool QTMOnscreenKeyboard::checkCanUpdateInPlace() {
	if (mLayout == nullptr || mLayout->count() != N(mRows))
	  return false;
	for (int i = 0; i < N(mRows); i++) {
		QLayoutItem* rowItem = mLayout->itemAt(i);
		QWidget* rowWidget = nullptr;
		if (rowItem) {
			rowWidget = rowItem->widget();
		}
		QHBoxLayout* rowLayout = nullptr;
		if (rowWidget) {
			rowLayout = qobject_cast<QHBoxLayout*> (rowWidget->layout());
		}
		if (rowLayout == nullptr) {
			return false;
		}

		int expected = N(mRows[i]);
		if (rowLayout->count() != expected) {
			return false;
		}

		for (int k = 0; k < rowLayout->count(); k++) {
			QWidget* w = nullptr;
			if (rowLayout->itemAt(k)) {
			  w = rowLayout->itemAt(k)->widget();
			}
			if (qobject_cast<QPushButton*> (w) == nullptr) {
				return false;
			}
		}
	}
	return true;
}

void QTMOnscreenKeyboard::updateInPlace() {
	for (int i=0; i<N(mRows); ++i) {
		mLayout->setStretch(i, 1);
		RowData row = mRows[i];
		QHBoxLayout* rowLayout = qobject_cast<QHBoxLayout*> (mLayout->itemAt(i)->widget()->layout());
		for (int k=0; k<N(row); ++k) {
			KeyData keyData = row[k];

			QPushButton* button = qobject_cast<QPushButton*> (rowLayout->itemAt(k)->widget());
			if (button->text() != keyData.label) {
				button->setText(keyData.label);
			}
			if (button->property("tm-cmd").toString() != keyData.cmd) {
				button->setProperty("tm-cmd", keyData.cmd);
			}
			int stretch = max(1, (int) round(keyData.widthUnits * 100.0));
			if (rowLayout->stretch(k) != stretch) {
				rowLayout->setStretch(k, stretch);
			}
		}
	}
}

void QTMOnscreenKeyboard::rebuildKeyboard() {
	clearKeyboardWidgets();

	double maxUnits = 0.0;
	for (int i = 0; i < N(mRows); i++) {
		RowData row = mRows[i];

		QWidget* rowWidget = new QWidget(this);
		rowWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
		QHBoxLayout* rowLayout = new QHBoxLayout(rowWidget);
		rowLayout->setContentsMargins(0, 0, 0, 0);
		rowLayout->setSpacing(0);

		double rowUnits = 0.0;
		for (int j = 0; j < N(row); j++) {
			KeyData keyData = row[j];

			QPushButton* button = new QPushButton(keyData.label, rowWidget);
			button->setObjectName("onscreenKeyboardButton");
			mButtons << button;
			button->setProperty("tm-cmd", keyData.cmd);
			button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
			connect(button, &QPushButton::pressed, this,
				&QTMOnscreenKeyboard::onKeyboardButtonClickedFromSender);

			int stretch = max(1, (int) round(keyData.widthUnits * 100.0));
			rowLayout->addWidget(button, stretch);
			rowUnits += keyData.widthUnits;
		}

		maxUnits = max(maxUnits, rowUnits);
		mLayout->addWidget(rowWidget, 1);
	}

	if (!mKeyboardSizeHint.isValid()) {
		const int hintWidth = (int) round(max(10.0, maxUnits) * 42.0);
		const int hintHeight = max(200, N(mRows) * 44 + 10);
		mKeyboardSizeHint = QSize(hintWidth, hintHeight);
		//setMinimumSize(mKeyboardSizeHint);
	}
	if (!hasValidSize()) {
		loadPreferences();
		if (!hasValidSize()) {
			mLastSize = mKeyboardSizeHint;
		}
	}

	updateButtonGeometry();
}

void QTMOnscreenKeyboard::initializeKeyboard() {
	try {
		object layoutData = call("custom-keyboard-layout");
		if (!is_list(layoutData)) {
			cout << "custom-keyboard-layout returned a non-list result." << LF;
			return;
		}

		array<object> rows = as_array_object(layoutData);
		mRows = parseKeyboardRows(layoutData);
		if (N(rows) == 0) return;
		if (mLayout == nullptr) return;

		if (checkCanUpdateInPlace()) {
			updateInPlace();
			updateButtonGeometry();
			return;
		}

		rebuildKeyboard();

	} catch (string& err) {
		cout << "custom-keyboard-layout failed: " << err << LF;
	}
}

void QTMOnscreenKeyboard::onKeyboardButtonClickedFromSender() {
	QPushButton* button = qobject_cast<QPushButton*>(sender());
	if (button == nullptr) return;
	onKeyboardButtonClicked(button);
}