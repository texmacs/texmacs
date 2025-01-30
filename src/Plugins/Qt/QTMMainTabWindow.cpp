#include "QTMMainTabWindow.hpp"

#ifdef TEXMACS_EXPERIMENTAL_TABWINDOW

QTMMainTabWindow::QTMMainTabWindow() {
    setTabsClosable(true);
    setMovable(true);
    setMinimumSize(800, 600);
    connect(this, SIGNAL(tabCloseRequested(int)), this, SLOT(closeTab(int)));
    show();
}

void QTMMainTabWindow::showWidget(QWidget *widget) {
    addTab(widget, widget->windowTitle());
    setCurrentWidget(widget);
}

void QTMMainTabWindow::removeWidget(QWidget *widget) {
    removeTab(indexOf(widget));
}

void QTMMainTabWindow::closeTab(int index) {
    // send the close window signal to the widget
    QWidget *w = this->widget(index);
    emit w->close();
}

void QTMMainTabWindow::tabTitleChanged(QWidget *widget, QString title) {
    int index = indexOf(widget);
    if (index != -1) {
        setTabText(index, title);
    }
}

#endif // TEXMACS_EXPERIMENTAL_TABWINDOW