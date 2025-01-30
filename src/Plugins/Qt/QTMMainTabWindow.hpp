#ifndef QTMMAINTABWINDOW_HPP
#define QTMMAINTABWINDOW_HPP

#include "config.h"

#ifdef OS_ANDROID
#define TEXMACS_EXPERIMENTAL_TABWINDOW
#endif

#ifdef TEXMACS_EXPERIMENTAL_TABWINDOW

#include <QTabWidget>

class QTMMainTabWindow : public QTabWidget {
    Q_OBJECT

public:
    QTMMainTabWindow();

    void showWidget(QWidget *widget);
    void removeWidget(QWidget *widget);
    void tabTitleChanged(QWidget *widget, QString title);

public slots:
    void closeTab(int index);

};

#endif // TEXMACS_EXPERIMENTAL_TABWINDOW

#endif