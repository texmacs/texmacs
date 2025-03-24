/******************************************************************************
* MODULE     : QTMWaitDialog.cpp
* DESCRIPTION: A wait dialog that displays a message while TeXmacs is
               processing, and prevents user interaction with other widgets.
* COPYRIGHT  : (C) 2025 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMWaitDialog.hpp"
#include "QTMApplication.hpp"

#include "string.hpp"

#include <QApplication>
#include <QScreen>
#include <QPainter>
#include <QPen>
#include <QTimer>
#include <QIcon>

QTMWaitDialog::QTMWaitDialog() : QDialog(), active(false) {
  setModal(true);

  setWindowTitle("TeXmacs");

  layout = new QHBoxLayout(this);
  setLayout(layout);

  defaultMessage = new QLabel(this);
  layout->addWidget(defaultMessage);

  waitMessage = new QLabel(this);
  layout->addWidget(waitMessage);

#if QT_VERSION >= 0x060000
  QIcon icon= tmapp()->icon_manager().getIcon("TeXmacs");
  originalPixmap = icon.pixmap(256, 256);
  defaultMessage->setPixmap(originalPixmap);
  defaultMessage->setAlignment(Qt::AlignCenter);
  defaultMessage->setFixedSize(32, 32);
  defaultMessage->setScaledContents(true);

  startTimer(1000 / 30);
#endif
}

void QTMWaitDialog::timerEvent(QTimerEvent *event) {
#if QT_VERSION >= 0x060000
    (void) event;
    static qreal angle = 0;
    static qreal direction = 1;
    angle += direction;
    if (angle == 20 || angle == -20) direction = -direction;
    QPixmap pixmap = originalPixmap.transformed(QTransform().rotate(angle));
    defaultMessage->setPixmap(pixmap);
#endif
}

void QTMWaitDialog::setMessage(string message) {
  if (message == "") {
    waitMessage->setText("");
    return;
  }
  QString qmessage = QString::fromUtf8 (&message[0], N(message));
  waitMessage->setText(qmessage);
}

void QTMWaitDialog::pushMessage(string message) {
  if (message == "") return;
  QString qmessage = QString::fromUtf8 (&message[0], N(message));
  messages.push_back(qmessage);
  waitMessage->setText(messages.last());
}

void QTMWaitDialog::popMessage() {
  if (!messages.isEmpty()) messages.removeLast();
  if (messages.isEmpty()) {
    waitMessage->setText("");
    return;
  }
  waitMessage->setText(messages.last());
}
