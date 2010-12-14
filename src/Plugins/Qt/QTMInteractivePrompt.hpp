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


class QTMInteractivePrompt : public QWidget
{
	Q_OBJECT
	
public:
	QTMInteractivePrompt (qt_widget, qt_widget, QWidget* p=0);
	void start ();
	void end ();
};

#endif // QTMINTERACTIVEPROMPT_HPP

