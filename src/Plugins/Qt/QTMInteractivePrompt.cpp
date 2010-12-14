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

#include <QHBoxLayout>
#include "QTMStyle.hpp"
#include <QLabel>
#include <QLineEdit>

QTMInteractivePrompt::QTMInteractivePrompt(qt_widget int_prompt, 
                                           qt_widget int_input, 
																					 QWidget* parent)
  : QWidget(parent)
{
  QLayoutItem *li = int_prompt->as_qlayoutitem ();
  QLayoutItem *li2 = int_input->as_qlayoutitem ();
  QHBoxLayout *_hl = new QHBoxLayout ();

	setStyle (qtmstyle ());
  
	if (QLabel *_la = qobject_cast<QLabel*> (li->widget ()))
    _la->setBuddy (li2->widget ());
	if (QWidget *w = qobject_cast<QWidget*> (li2->widget ())) 
    w->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
  _hl->addItem (li);
  _hl->addSpacing (6);
  _hl->addItem (li2);
	_hl->setContentsMargins (3,0,0,0);
  setLayout (_hl);
	
  QFont f = font ();
  f.setPointSize (11);
  setFont (f);

  
}


void QTMInteractivePrompt::start() {
  QWidget *_le = findChild<QLineEdit*>();
  if (_le) {
    _le->show ();
    _le->setFocus (Qt::OtherFocusReason);  
  }
}

void QTMInteractivePrompt::end() {
}

