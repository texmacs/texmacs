/******************************************************************************
 * MODULE     : QTMInteractivePrompt.cpp
 * DESCRIPTION: interactive prompt a la emacs
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMInteractivePrompt.hpp"

#include <QLineEdit>
#include <QMainWindow>
#include <QHBoxLayout>
#include <QLabel>
#include <QEventLoop>
#include <QDialogButtonBox>
#include <QCompleter>
#include <QDirModel>
#include <QStatusBar>
#include <QDialog>
#include <QKeyEvent>
#include <QApplication>
#include <QWidgetAction>
#include <QObject>

#include "QTMStyle.hpp"

QTMInteractivePrompt::QTMInteractivePrompt(qt_widget _int_prompt, qt_widget _int_input, 
																					 QMainWindow* mw, QWidget* parent)
  : QWidget(parent),int_prompt(_int_prompt), int_input(_int_input), _mw(mw)
{
  QLayoutItem *li = int_prompt->as_qlayoutitem ();
  QLayoutItem *li2 = int_input->as_qlayoutitem ();
  _le = qobject_cast<QLineEdit*>(li2->widget());
  _hl = new QHBoxLayout();

	setStyle (qtmstyle());
  
	if (QLabel *_la = qobject_cast<QLabel*>(li->widget()))
    _la->setBuddy (li2->widget());
  _hl->addItem(li);
  _hl->addItem(li2);
	_hl->setContentsMargins(3,0,0,0);
  setLayout(_hl);
	
  QFont f = font();
  f.setPointSize(11);
  setFont(f);
}


void QTMInteractivePrompt::start() {
  if (_le) {
  _le->show();
  _le->setFocus(Qt::OtherFocusReason);  
  }
  QObject::connect(qApp, SIGNAL(focusChanged( QWidget * , QWidget *  ) ), this, SLOT(appFocusChanged( QWidget * , QWidget * ) ));
}

void QTMInteractivePrompt::end() {
  QObject::disconnect(qApp, SIGNAL(focusChanged( QWidget * , QWidget *  ) ), this, SLOT(appFocusChanged( QWidget * , QWidget * ) ));
}

void 
QTMInteractivePrompt::appFocusChanged ( QWidget * old, QWidget * now ) {
  // We check wether the focus has gone to one of our children widgets, or it has simply "jumped" inside the QComboBox:
  // Clicking its dropdown arrow fires a QFocusEvent, but doesn't change the widget, so we must take care of that
  // TODO: Don't lose focus if the user pressed too many times the tab key (I tend to do that...)
  if (now && !isAncestorOf(now) && now->parent() != old) {
    reject();
  }
}

void 
QTMInteractivePrompt::accept() {	
 // _ev->exit(QDialog::Accepted); 
}

void 
QTMInteractivePrompt::reject() {	
 // _ev->exit(QDialog::Rejected); 
}

