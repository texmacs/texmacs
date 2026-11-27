/******************************************************************************
 * MODULE     : QTMInteractivePrompt.cpp
 * DESCRIPTION: interactive prompt a la emacs
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "scheme.hpp"
#include "gui.hpp"

#include "QTMInteractivePrompt.hpp"
#include "QTMStyle.hpp"
#include "qt_widget.hpp"
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>

QTMInteractivePrompt::QTMInteractivePrompt(qt_widget int_prompt, 
                                           qt_widget int_input, 
                                           QWidget* parent)
: QWidget (parent), active (false)
{
  QLayoutItem*  li = !is_nil (int_prompt) ? int_prompt->as_qlayoutitem (this) : nullptr;
  QLayoutItem* li2 = !is_nil (int_input)  ? int_input->as_qlayoutitem (this)  : nullptr;
  QHBoxLayout* _hl = new QHBoxLayout ();
  
  setStyle (qtmstyle ());
  setSizePolicy (QSizePolicy::Expanding, QSizePolicy::Fixed);
  
  if (li && li2) {
    if (QLabel *_la = qobject_cast<QLabel*> (li->widget ()))
      _la->setBuddy (li2->widget ());
  }
  if (li2) {
    if (QWidget *w = qobject_cast<QWidget*> (li2->widget ())) 
      w->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
  }
  _hl->addSpacing (6);
  if (li) _hl->addItem (li);
  _hl->addSpacing (6);
  if (li2) _hl->addItem (li2);
  _hl->setContentsMargins (3,0,0,0);
  setLayout (_hl);

  if (tm_style_sheet == "" && use_mini_bars) {
    QFont f = font ();
    int fs = as_int (get_preference ("gui:mini-fontsize", QTM_MINI_FONTSIZE));
    f.setPointSize (qt_zoom (fs > 0 ? fs : QTM_MINI_FONTSIZE));
    if (li && li->widget()) li->widget()->setFont (f);
    if (li2 && li2->widget()) li2->widget()->setFont (f);
  }
}


void QTMInteractivePrompt::start() {
  QWidget *_le = findChild<QLineEdit*>();
  if (_le) {
    active = true;
    _le->show ();
    _le->setFocus (Qt::OtherFocusReason);  
  }
}

void QTMInteractivePrompt::end() {
  active = false;
}

