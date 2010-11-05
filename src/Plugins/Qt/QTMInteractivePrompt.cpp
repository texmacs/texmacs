
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

#include <QComboBox>;
#include <QMainWindow>;
#include <QHBoxLayout>;
#include <QLabel>;
#include <QEventLoop>;
#include <QDialogButtonBox>;
#include <QCompleter>
#include <QDirModel>
#include <QStatusbar>
#include <QDialog>
#include <QKeyEvent>
#include <QApplication>


QTMInteractivePrompt::QTMInteractivePrompt(const QString& label, const QStringList& items, const QString& type, 
																					 QMainWindow* mw, QWidget* parent)
  : QWidget(parent),_ty(type), _mw(mw)
{
  setWindowModality(Qt::WindowModal);
  
	_ev = new QEventLoop(this);
	_hl = new QHBoxLayout(this);
	_la = new QLabel(label, this);
  _cb = new QComboBox(this);
	_bb = new QDialogButtonBox (QDialogButtonBox::Ok | QDialogButtonBox::Cancel, Qt::Horizontal, this);
	
	_cb->addItems(items);
  _cb->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLength);
  _cb->setEditText (items[0]);
	int minlen = 0;
	foreach(QString item, items)
		minlen = (minlen < item.size()) ? item.size() : minlen;
	_cb->setMinimumContentsLength(minlen);
  _cb->setEditable (true);
	// apparently the following flag prevents Qt from substituting a history item
  // for an input when they differ only from the point of view of case (upper/lower)
  // eg. if the history contains aAAAAa and you type AAAAAA then the combo box
  // will retain the string aAAAAa
  _cb->setDuplicatesEnabled(true); 
	
	_la->setBuddy(_cb);
  
	_hl->addWidget(_la);
  _hl->addWidget(_cb);
  _hl->addWidget(_bb);
	_hl->setContentsMargins(3,0,0,0);
  setLayout(_hl);
  
  // stylesheets seems overkill
  // if they pose problems, you can use the alternative code below
#if 1  
	setStyleSheet("* { font-size: 11px; }");
#else	
  QFont f = font();
  f.setPointSize(11);
  setFont(f);
#endif
  
	if (_ty.endsWith("file", Qt::CaseInsensitive) || _ty.endsWith("directory", Qt::CaseInsensitive)) {
		// autocompletion
    QCompleter* completer = new QCompleter(this);
    QDirModel* dirModel = new QDirModel(completer);
    completer->setModel(dirModel);
    _cb->setCompleter(completer);
  }
	
	QObject::connect(_bb, SIGNAL (accepted()), this, SLOT (accept()));
	QObject::connect(_bb, SIGNAL (rejected()), this, SLOT (reject()));
  
  setTabOrder(_cb, _bb);
}


int QTMInteractivePrompt::exec() {
	QStatusBar* _ns = new QStatusBar();
	_ns->addWidget(this);
	//_ns->setStyleSheet("* {background-color: yellow; } ");
	
	QStatusBar* _os = _mw->statusBar();
	_os->setParent(0);
	_mw->setStatusBar(_ns);
	
//	setFocus(Qt::PopupFocusReason);
  _cb->setFocus();
  QObject::connect(qApp, SIGNAL(focusChanged( QWidget * , QWidget *  ) ), this, SLOT(appFocusChanged( QWidget * , QWidget * ) ));
	int ret = _ev->exec();
  QObject::disconnect(qApp, SIGNAL(focusChanged( QWidget * , QWidget *  ) ), this, SLOT(appFocusChanged( QWidget * , QWidget * ) ));
	
  
  
	_ns->removeWidget(this);
	_mw->setStatusBar(_os);
	
	return ret;
	 
}

void 
QTMInteractivePrompt::keyPressEvent(QKeyEvent* ev) {
	switch(ev->key()) {
		case Qt::Key_Escape:
			reject();
			break;
		case Qt::Key_Enter:
		case Qt::Key_Return:
			accept();
			break;
		default:
			QWidget::keyPressEvent(ev);
	}
}


void 
QTMInteractivePrompt::focusOutEvent ( QFocusEvent * event ) {
//  _ev->exit(QDialog::Rejected); 
  QWidget::focusOutEvent(event);
}

void 
QTMInteractivePrompt::appFocusChanged ( QWidget * old, QWidget * now ) {	
  (void) old;
  //cout << "focus changed" << LF;
  if (now && !isAncestorOf(now)) {
    _ev->exit(QDialog::Rejected); 
  }
}

void 
QTMInteractivePrompt::accept() {	
  _ev->exit(QDialog::Accepted); 
}

void 
QTMInteractivePrompt::reject() {	
  _ev->exit(QDialog::Rejected); 
}

const QString 
QTMInteractivePrompt::currentText() { 
  return _cb->currentText(); 
}
