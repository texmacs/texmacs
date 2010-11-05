
/******************************************************************************
 * MODULE     : QTMInteractivePrompt.hpp
 * DESCRIPTION: interactive prompt a la emacs
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTMINTERACTIVEPROMPT_HPP
#define QTMINTERACTIVEPROMPT_HPP

#include <QWidget>

class QComboBox;
class QMainWindow;
class QHBoxLayout;
class QLabel;
class QEventLoop;
class QDialogButtonBox;

class QTMInteractivePrompt : public QWidget
{
	Q_OBJECT
	
public:
	QTMInteractivePrompt(const QString&, const QStringList&, const QString&, QMainWindow*, QWidget* p=0);
	const QString currentText();
	int exec ();
	
public slots:
	void accept ();
	void reject ();
	void appFocusChanged  ( QWidget * old, QWidget * now );
  
protected:
	void keyPressEvent(QKeyEvent*);
	void focusOutEvent ( QFocusEvent * event );
  
	QHBoxLayout*      _hl;
	QLabel*           _la;
	QComboBox*        _cb;
	QDialogButtonBox* _bb;
	
	QEventLoop*       _ev;
	QString           _ty;
	QMainWindow*      _mw;   // Hack, would be nicer to have access to a singleton through some static member somewehere...
	
};

#endif // QTMINTERACTIVEPROMPT_HPP