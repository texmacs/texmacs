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
#include "qt_widget.hpp"

class QLineEdit;
class QMainWindow;
class QHBoxLayout;
class QEventLoop;
class QDialogButtonBox;
class QLabel;
class QLayoutItem;
class QStatusBar;

class QTMInteractivePrompt : public QWidget
{
	Q_OBJECT
	
public:
	QTMInteractivePrompt(qt_widget, qt_widget, QMainWindow*, QWidget* p=0);
	void start ();
	void end ();
	
public slots:
	void accept ();
	void reject ();
	void appFocusChanged  ( QWidget * old, QWidget * now );
  
protected:
  
  qt_widget int_prompt;
  qt_widget int_input;
  
  
	QHBoxLayout*      _hl;
	QLineEdit*        _le;
	
	QMainWindow*      _mw;   // the main window the prompt belongs (not a singleton)

};

#endif // QTMINTERACTIVEPROMPT_HPP

