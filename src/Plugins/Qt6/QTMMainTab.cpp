#include "string.hpp"

#include "QTMMainTab.hpp"
#include "QTMMainTabWindow.hpp"

#include <QWidget>

static QTMMainTabWindow *findParentTabWindow(const QWidget *widget) {
	const QWidget *current = widget;
	while (current != nullptr) {
		QTMMainTabWindow *tabWindow = qobject_cast<QTMMainTabWindow *>(const_cast<QWidget *>(current));
		if (tabWindow != nullptr) return tabWindow;
		current = current->parentWidget();
	}
	return nullptr;
}

QTMMainTab::QTMMainTab(QWidget *parent) : QMainWindow(parent) {
	mTitle = windowTitle();
	mIcon = windowIcon();
}

QTMMainTab::~QTMMainTab() {
	emit windowOrTabClosed();
}

QPointer<QTMMainTabWindow> QTMMainTab::parentTabWindow() const {
	return QPointer<QTMMainTabWindow>(findParentTabWindow(this));
}

void QTMMainTab::setWindowOrTabTitle(const QString &title) {
	mTitle = title;
	setWindowTitle(title);
	emit tabTitleChanged(title);
}

void QTMMainTab::setWindowOrTabIcon(const QIcon &icon) {
	mIcon = icon;
	setWindowIcon(icon);
	emit tabIconChanged(icon);
}

void QTMMainTab::resizeWindowOrTab(const QSize &size) {
	QTMMainTabWindow *tabWindow = findParentTabWindow(this);
	if (tabWindow != nullptr) {
		resize(size);
		updateGeometry();
		return;
	}

	resize(size);
}

void QTMMainTab::closeWindowOrTab() {
    emit windowOrTabClosed();
	if (findParentTabWindow(this) == nullptr) {
		close();
	}
}