
/******************************************************************************
* MODULE     : qt_dialogues.cpp
* DESCRIPTION: Widgets for automatically created dialogues (questions in popups)
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "widget.hpp"
#include "message.hpp"
#include "qt_dialogues.hpp"
#include "qt_utilities.hpp"
#include "qt_tm_widget.hpp"
#include "qt_chooser_widget.hpp"
#include "qt_color_picker_widget.hpp"
#include "url.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "QTMMenuHelper.hpp"
#include "QTMGuiHelper.hpp"
#include "QTMApplication.hpp"

#include <QMessageBox>
#include <QLabel>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLineEdit>
#include <QCompleter>
#include <QFileSystemModel>
#include <QDir>
#include <QVector>
#include <QPushButton>
#include <QDialogButtonBox>

#include "string.hpp"
#include "scheme.hpp"


/******************************************************************************
 * qt_field_widget_rep
 ******************************************************************************/

qt_field_widget_rep::qt_field_widget_rep (qt_inputs_list_widget_rep* _parent,
                                          string _prompt)
  : qt_widget_rep (field_widget),
    prompt (_prompt), input (""), proposals (), parent (_parent)
{ }

void
qt_field_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_field_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    check_type<string>(val, s);
    input= scm_quote (open_box<string> (val));
    break;
  case SLOT_INPUT_TYPE:
    check_type<string>(val, s);
    type= open_box<string> (val);
    break;
  case SLOT_INPUT_PROPOSAL:
    check_type<string>(val, s);
    proposals << open_box<string> (val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    parent->send (s, val);
    break;
  default:
    qt_widget_rep::send (s, val);
  }
}

blackbox
qt_field_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_field_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    check_type_id<string> (type_id, s);
    return close_box<string> (input);
  default:
    return qt_widget_rep::query (s, type_id);
  }
}

QWidget*
qt_field_widget_rep::as_qwidget () {
  qwid = new QWidget ();
  
  QHBoxLayout* hl = new QHBoxLayout (qwid);
  QLabel*     lab = new QLabel (to_qstring (prompt), qwid);
  
  qwid->setLayout (hl);
  hl->addWidget (lab, 0, Qt::AlignRight);

  if (ends (type, "file") || type == "directory") {
    widget wid    = input_text_widget (command(), type,
                                       array<string>(0), 0, "20em");
    QLineEdit* le = qobject_cast<QTMLineEdit*> (concrete(wid)->as_qwidget());
    ASSERT (le != NULL, "qt_field_widget_rep: expecting QTMLineEdit");
    le->setObjectName (to_qstring (type));
    lab->setBuddy (le);
    hl->addWidget (le);
  } 
  else if (type == "password") {
    QTMLineEdit* le= new QTMLineEdit (qwid, "password", "20em", 0);
    QTMFieldWidgetHelper* helper = new QTMFieldWidgetHelper (this, le);
    (void) helper;
    le->setCompleter (0);
    lab->setBuddy (le);
    hl->addWidget (le);
  }
  else {
    QTMComboBox* cb              = new QTMComboBox (qwid);
    QTMFieldWidgetHelper* helper = new QTMFieldWidgetHelper (this, cb);
    (void) helper;
    cb->addItems (to_qstringlist (proposals));
    cb->setEditText (to_qstring (scm_unquote (input)));
    cb->setEditable (true);
    cb->setLineEdit (new QTMLineEdit (cb, type, "1w", WIDGET_STYLE_MINI));
    cb->setSizeAdjustPolicy (QComboBox::AdjustToContents);
    cb->setSizePolicy (QSizePolicy::Expanding, QSizePolicy::Fixed);
    cb->setDuplicatesEnabled (true); 
    cb->completer()->setCaseSensitivity (Qt::CaseSensitive);
    if (N(type) == 0) cb->setObjectName ("default focus target");
    else              cb->setObjectName (to_qstring (type));
    lab->setBuddy (cb);
    hl->addWidget (cb);
  }
  return qwid;
}


/******************************************************************************
 * qt_inputs_list_widget_rep
 ******************************************************************************/

#ifdef Q_OS_MAC
#include <QKeyEvent>

/*! An event filter to circumvent a Qt Mac bug in QMessageBox.
 
 Pressing tab has no effect on QMessageBox dialogs under MacOS.
 See e.g. https://bugreports.qt-project.org/browse/QTBUG-13330
 
 The bug is present at least in versions >= 4.6.1 and <= 4.8.5
 */
class QTMFilterHack : public QWidget {
  typedef QList<QAbstractButton*> ButtonList;
  ButtonList buttons;
  int current;
  int N;
public:
  QTMFilterHack (ButtonList _buttons) : buttons (_buttons), current (1),
  N (_buttons.size()) { }
  bool eventFilter(QObject *target, QEvent *event)
  {
    if (event->type() == QEvent::KeyPress) {
      QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
      if (keyEvent->key() == Qt::Key_Tab) {
        if (keyEvent->modifiers() & Qt::ShiftModifier)
          current = current - 1 < 0 ? N-1 : current - 1;
        else
          current = current + 1 >= N? 0 : current + 1;
        buttons[current]->setFocus (Qt::TabFocusReason);
        return true;
      }
    }
    return QWidget::eventFilter (target, event);
  }
};
#endif

qt_inputs_list_widget_rep::qt_inputs_list_widget_rep (command _cmd,
                                                      array<string> _prompts)
: qt_widget_rep (input_widget), cmd (_cmd), size (coord2 (100, 100)),
  position (coord2 (0, 0)), win_title (""), style (0)
{
  for (int i = 0; i < N(_prompts); i++)
    add_child (tm_new<qt_field_widget_rep> ((qt_inputs_list_widget_rep*)this, _prompts[i]));
}

widget
qt_inputs_list_widget_rep::plain_window_widget (string s, command q)
{
  (void) q; // The widget already has a command (dialogue_command)
  win_title = s;
  return this;
}

void
qt_inputs_list_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_inputs_list_widget_rep::send " << slot_name(s) << LF;

  switch (s) {
  case SLOT_VISIBILITY:
    {   
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);
      (void) flag;
      NOT_IMPLEMENTED("qt_inputs_list_widget::SLOT_VISIBILITY")
    }   
    break;
  case SLOT_SIZE:
    check_type<coord2> (val, s);
    size = open_box<coord2> (val);
    break;
  case SLOT_POSITION:
    check_type<coord2> (val, s);
    position = open_box<coord2> (val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    check_type<bool> (val, s);
    perform_dialog ();
    break;
  default:
    qt_widget_rep::send (s, val);
  }
}

blackbox
qt_inputs_list_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_inputs_list_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (position);
    }
  case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (size);
    }
  case SLOT_STRING_INPUT:
    if (N(children) > 0) return field(0)->query (s, type_id);
  default:
    return qt_widget_rep::query (s, type_id);
  }
}

widget
qt_inputs_list_widget_rep::read (slot s, blackbox val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_inputs_list_widget_rep::read " << slot_name(s) << LF;
  switch (s) {
  case SLOT_WINDOW:
    check_type_void (val, s);
    return this;
  case SLOT_FORM_FIELD:
  {
    check_type<int> (val, s);
    int index = open_box<int> (val);
    if (N(children) > index)
      return static_cast<widget_rep*> (children[index].rep);
  }
  default:
    return qt_widget_rep::read (s, val);
  }
}

qt_field_widget_rep*
qt_inputs_list_widget_rep::field (int i) {
  return static_cast<qt_field_widget_rep*> (children[i].rep);
}

void
qt_inputs_list_widget_rep::perform_dialog() {
  if ((N(children)==1) && (field(0)->type == "question")) {
   // then use Qt messagebox for smoother, more standard UI
    QWidget* mainwindow = QApplication::activeWindow ();
    // main texmacs window. There are probably better ways...
    // Presently not checking if the windows has the focus;
    // In case it has not, it should be brought into focus
    // before calling the dialog
    QMessageBox msgBox (mainwindow);
    //sets parent widget, so that it appears at the proper location	
    msgBox.setText (to_qstring (field(0)->prompt));
    msgBox.setStandardButtons (QMessageBox::Cancel);
    
      // Allow any number of choices. The first one is the default.
    int choices = N(field(0)->proposals);
    QVector<QPushButton*> buttonlist (choices);
    if (choices > 0) {
      for(int i = 0; i < choices; ++i) {
          // Capitalize the first character?
        string blabel= "&" * upcase_first (field(0)->proposals[i]);
        buttonlist[i] = msgBox.addButton (to_qstring (blabel),
                                           QMessageBox::ActionRole);
      }
      msgBox.setDefaultButton (buttonlist[0]);
      for (int i = 0; i < choices - 1; ++i)
        QWidget::setTabOrder (buttonlist[i], buttonlist[i+1]);
      QWidget::setTabOrder (buttonlist[choices-1], msgBox.escapeButton());
    }
    msgBox.setWindowTitle (qt_translate ("Question"));
    msgBox.setIcon (QMessageBox::Question);
#ifdef Q_OS_MAC
    QTMFilterHack filter (msgBox.buttons());
    msgBox.installEventFilter (&filter);
#endif
    msgBox.exec();
    bool buttonclicked=false;
    for(int i=0; i<choices; i++) {
      if (msgBox.clickedButton() == buttonlist[i]) {
        field(0)->input = scm_quote (field(0)->proposals[i]);
        buttonclicked=true;
        break;
      }
    }
    if (!buttonclicked) {field(0)->input = "#f";} //cancelled
  } 
  
  else {  //usual dialog layout
    QDialog d (0, Qt::Sheet);
    QVBoxLayout* vl = new QVBoxLayout(&d);
    QVector<QWidget*> widgets;
    for(int i = 0; i < N(children); ++i) {
      widgets.push_back (field(i)->as_qwidget());
      vl->addWidget(widgets[i]);
    }
    for (int i = 0; i < N(children) - 1; ++i)
      QWidget::setTabOrder (widgets[i], widgets[i+1]);
    
    QDialogButtonBox* buttonBox =
          new QDialogButtonBox (QDialogButtonBox::Ok | QDialogButtonBox::Cancel,
                                Qt::Horizontal, &d);
    QObject::connect (buttonBox, SIGNAL (accepted()), &d, SLOT (accept()));
    QObject::connect (buttonBox, SIGNAL (rejected()), &d, SLOT (reject()));
    vl->addWidget (buttonBox);
    
    d.setWindowTitle (to_qstring (win_title)); 
    d.updateGeometry();
    QRect r;
    r.setSize (d.sizeHint ());
    r.moveCenter (to_qpoint (position));
    d.setGeometry (r);
    d.setSizePolicy (QSizePolicy::Preferred, QSizePolicy::Fixed);
    
    if (d.exec() != QDialog::Accepted)
      for(int i=0; i < N(children); ++i)
        field(i)->input = "#f";
  }

  if (!is_nil(cmd)) cmd ();
}


/******************************************************************************
 * qt_input_text_widget_rep
 ******************************************************************************/

qt_input_text_widget_rep::qt_input_text_widget_rep (command _cmd,
                                                    string _type,
                                                    array<string> _proposals,
                                                    int _style,
                                                    string _width)
: qt_widget_rep (input_widget), cmd (_cmd), type (_type),
  proposals (_proposals), input (""), style (_style), width (_width),
  ok (false), done (false)
{
  if (type == "password") proposals = array<string> (0);
  if (N(proposals) > 0) input = proposals[0];
}

QAction*
qt_input_text_widget_rep::as_qaction () {
  return new QTMWidgetAction (this);
}

/*!
 Returns a QTMLineEdit with the proper completer and the helper object to
 keep it in sync with us.
 */
QWidget*
qt_input_text_widget_rep::as_qwidget () {
  QTMLineEdit* le = new QTMLineEdit (NULL, type, width, style, cmd);
  qwid = le;
  QTMInputTextWidgetHelper* helper = new QTMInputTextWidgetHelper (this);
  (void) helper;
  le->setText (to_qstring (input));
  le->setObjectName (to_qstring (type));
  if (ends (type, "file") || type == "directory") {
    QCompleter*     completer = new QCompleter(le);
    QFileSystemModel* fsModel = new QFileSystemModel(le);
    fsModel->setRootPath (QDir::homePath());// This is NOT the starting location
    completer->setModel (fsModel);
    le->setCompleter (completer);
  }
  else if (type != "password" && N(proposals) > 0 && ! (N(proposals) == 1 && N(proposals[0]) == 0)){
    //else if (N(proposals) > 0 && ! (N(proposals) == 1 && N(proposals[0]) == 0)){
    QCompleter* completer = new QCompleter (to_qstringlist(proposals), le);
    completer->setCaseSensitivity (Qt::CaseSensitive);
    completer->setCompletionMode (QCompleter::InlineCompletion);
    le->setCompleter (completer);
  }
  return qwid;
}

void
qt_input_text_widget_rep::commit(bool flag) {
  QTMLineEdit* le = qobject_cast<QTMLineEdit*>(qwid);
  widget_rep* win = qt_window_widget_rep::widget_from_qwidget (le);

  if (flag) {
    done         = false;
    ok    = true;
    input = from_qstring (le->text());
    // HACK: restore focus to the main editor widget
    if (win) send_keyboard_focus(win);
  } else {
    le->setText (to_qstring (input));
  }
  if (win) // This is 0 inside a dialog => no command
  {
    if (done) return;
    done = true;
    the_gui->process_command (cmd, ok
                              ? list_object (object (input))
                              : list_object (object (false)));
  }
}
