#ifndef QTMMAINTAB_HPP
#define QTMMAINTAB_HPP

#include <QMainWindow>

class QTMMainTabWindow;

class QTMMainTab : public QMainWindow {
  Q_OBJECT

public:
    QTMMainTab(QWidget *parent = nullptr);

    ~QTMMainTab();

    QPointer<QTMMainTabWindow> parentTabWindow() const;

    void setWindowOrTabTitle(const QString &title);

    void setWindowOrTabIcon(const QIcon &icon);

    void resizeWindowOrTab(const QSize &size);

    void closeWindowOrTab();

signals:
    void requestClose();

    void windowOrTabClosed();

    void tabTitleChanged(const QString &title);

    void tabIconChanged(const QIcon &icon);

private:
    QString mTitle;
    QIcon mIcon;

};

#endif // QTMMAINTAB_HPP