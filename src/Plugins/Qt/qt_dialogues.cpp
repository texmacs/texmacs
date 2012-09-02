
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
#include "qt_printer_widget.hpp"
#include "qt_color_picker_widget.hpp"
#include "url.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "QTMMenuHelper.hpp"
#include "QTMGuiHelper.hpp"

#include <QtGui>
#include "string.hpp"
#include "scheme.hpp"


  ////////////////////////// qt_field_widget_rep ////////////////////////////


void
qt_field_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_field_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    check_type<string>(val, s);
    input= open_box<string> (val);
    // send_string (THIS, "input", val);
    break;
  case SLOT_INPUT_TYPE:
    check_type<string>(val, s);
    type= open_box<string> (val);
    break;
  case SLOT_INPUT_PROPOSAL:
    check_type<string>(val, s);
    proposals << open_box<string> (val);
    // send_string (THIS, "default", val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    parent->send(s,val);
    break;
  default:
    qt_widget_rep::send (s, val);
  }
}

blackbox
qt_field_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_field_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    check_type_id<string> (type_id, s);
    return close_box<string> (input);
  default:
    return qt_widget_rep::query (s, type_id);
  }
}


  ////////////////////////// qt_inputs_list_widget_rep ////////////////////////////


qt_inputs_list_widget_rep::qt_inputs_list_widget_rep (command _cmd, array<string> _prompts):
   qt_widget_rep (input_widget), cmd (_cmd), fields (N (_prompts)),
   size (coord2 (100, 100)), position (coord2 (0, 0)), win_title (""), style (0)
{
  for (int i=0; i < N(_prompts); i++) {
    fields[i] = tm_new<qt_field_widget_rep> (this);
    fields[i]->prompt = _prompts[i];
  }
}

qt_inputs_list_widget_rep::~qt_inputs_list_widget_rep()  {}

widget
qt_inputs_list_widget_rep::plain_window_widget (string s, command q)
{
  win_title = s;
  (void) q;  // FIXME? ignore ok?
  return this;
}

void
qt_inputs_list_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_inputs_list_widget_rep::send " << slot_name(s) << LF;

  switch (s) {
  case SLOT_VISIBILITY:
    {   
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);
      (void) flag;
      NOT_IMPLEMENTED
    }   
    break;
  case SLOT_SIZE:
    check_type<coord2>(val, s);
    size = open_box<coord2> (val);
    break;
  case SLOT_POSITION:
    check_type<coord2>(val, s);
    position = open_box<coord2> (val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    check_type<bool>(val, s);
    perform_dialog ();
    break;
  default:
    qt_widget_rep::send (s, val);
  }
}

blackbox
qt_inputs_list_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_inputs_list_widget_rep::query " << slot_name(s) << LF;
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
    return fields[0]->query (s, type_id);
  default:
    return qt_widget_rep::query (s, type_id);
  }
}

widget
qt_inputs_list_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT)
    cout << "qt_inputs_list_widget_rep::read " << slot_name(s) << LF;
  switch (s) {
  case SLOT_WINDOW:
    check_type_void (index, s);
    return this;
  case SLOT_FORM_FIELD:
    check_type<int> (index, s);
    return static_cast<widget_rep*> (fields[open_box<int> (index)].rep);
  default:
    return qt_widget_rep::read (s, index);
  }
}

void
qt_inputs_list_widget_rep::perform_dialog() {
  if ((N(fields)==1) && (fields[0]->type == "question")) {
   // then use Qt messagebox for smoother, more standard UI
    QWidget* mainwindow = QApplication::activeWindow ();
    // main texmacs window. There are probably better ways...
    // Presently not checking if the windows has the focus;
    // In case it has not, it should be brought into focus
    // before calling the dialog
    QMessageBox* msgBox=new QMessageBox (mainwindow);
    //sets parent widget, so that appears at proper location	
    msgBox->setText (msgBox->tr (as_charp (fields[0]->prompt)));
    msgBox->setStandardButtons (QMessageBox::Cancel);
    int choices = N(fields[0]->proposals);
    QVector<QPushButton*> buttonlist (choices);
    //allowing for any number of choices
    for(int i=0; i<choices; i++) {
      string blabel= "&" * upcase_first (fields[0]->proposals[i]);
      //capitalize the first character?
      buttonlist[i] = msgBox->addButton (QMessageBox::tr (as_charp (blabel)),
                                         QMessageBox::ActionRole);
    }
    msgBox->setDefaultButton (buttonlist[0]); //default is first choice
    msgBox->setWindowTitle (msgBox->tr("Question"));
    msgBox->setIcon (QMessageBox::Question);

    msgBox->exec();
    bool buttonclicked=false;
    for(int i=0; i<choices; i++) {
      if (msgBox->clickedButton() == buttonlist[i]) {
        fields[0] -> input = scm_quote (fields[0]->proposals[i]);
        buttonclicked=true;
        break;
      }
    }
    if (!buttonclicked) {fields[0] -> input = "#f";} //cancelled
  }

  else {  //usual dialogue layout
    QDialog d (0, Qt::Sheet);
    QVBoxLayout* vl = new QVBoxLayout(&d);

    QVector<QComboBox*> cbs (N (fields));

    for(int i=0; i<N(fields); i++) {
      QHBoxLayout *hl = new QHBoxLayout();

      QLabel *lab = new QLabel (to_qstring (tm_var_encode( (fields[i]->prompt))),&d);
      cbs[i] = new QComboBox(&d);
      cbs[i] -> setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLength);
      cbs[i] -> setEditText (to_qstring(fields[i]->input));
      int minlen = 0;
      for(int j=0; j < N(fields[i]->proposals); j++) {
        QString str = to_qstring (fields[i]->proposals[j]);
        cbs[i] -> addItem (str);
        int c = str.count();
        if (c > minlen) minlen = c;
      }
      cbs[i] -> setMinimumContentsLength (minlen>50 ? 50 : (minlen < 2 ? 10 : minlen));
      cbs[i] -> setEditable (true);
      // apparently the following flag prevents Qt from substituting
      // an history item for an input when they differ only from
      // the point of view of case (upper/lower)
      // eg. if the history contains aAAAAa and you type AAAAAA then
      // the combo box will retain the string aAAAAa    
      cbs[i]->setDuplicatesEnabled(true); 
      cbs[i]->completer()->setCaseSensitivity(Qt::CaseSensitive);
      lab -> setBuddy (cbs[i]);
      hl -> addWidget (lab);
      hl -> addWidget (cbs[i]);
      vl -> addLayout (hl);
      
      if (ends (fields[i]->type, "file") || fields[i]->type == "directory") {
        // autocompletion
        //QCompleter *completer = new QCompleter(cbs[i]);
        QCompleter *completer = cbs[i]->completer();
        QDirModel *dirModel = new QDirModel();
        completer->setModel(dirModel);
        //cbs[i]->setCompleter(completer);
      }
    }
    
    {
      QDialogButtonBox* buttonBox =
        new QDialogButtonBox (QDialogButtonBox::Ok | QDialogButtonBox::Cancel,
                              Qt::Horizontal, &d);
      QObject::connect (buttonBox, SIGNAL (accepted()), &d, SLOT (accept()));
      QObject::connect (buttonBox, SIGNAL (rejected()), &d, SLOT (reject()));
      vl -> addWidget (buttonBox);
    }
    //  d.setLayout (vl);
    d.setWindowTitle (d.tr (as_charp (win_title)));
    QPoint pos = to_qpoint (position);
    //cout << "Size :" << size.x1 << "," << size.x2 << LF;
    //cout << "Position :" << pos.x() << "," << pos.y() << LF;
    
    d.updateGeometry();
    QSize sz = d.sizeHint();
    QRect r; r.setSize(sz);
    r.moveCenter(pos);
    d.setGeometry(r);
    
    int result = d.exec ();
    if (result == QDialog::Accepted) {
      for(int i=0; i<N(fields); i++) {
        QString item = cbs[i]->currentText();
        fields[i] -> input = scm_quote (from_qstring (item));
      }
    } else {
      for(int i=0; i<N(fields); i++) {
        fields[i] -> input = "#f";
      }
    }
  }

  cmd ();
}


  ///////////////////////// qt_input_text_widget_rep //////////////////////////


qt_input_text_widget_rep::qt_input_text_widget_rep 
(command _cmd, string _type, array<string> _def, int _style, string _width)
: cmd (_cmd), type (_type), def (_def), text (""), style(_style), width(_width),
  helper(NULL), ok(false) 
{
  if (N(def) > 0) text = def[0];
}

qt_input_text_widget_rep::~qt_input_text_widget_rep() { }

QAction *
qt_input_text_widget_rep::as_qaction () {
  QTMWidgetAction *a = new QTMWidgetAction (this);
  return a;
}

QWidget *
qt_input_text_widget_rep::as_qwidget () {
  if (!helper) {
    helper = new QTMInputTextWidgetHelper(this);
    // helper retains the current widget
    // in toolbars the widget is not referenced directly in texmacs code
    // so must be retained by Qt objects
  }
  QLineEdit *le;
  // FIXME: how is this check necessary (out of memory check seems unlikely...)
  if (helper) {
    le = new QTMLineEdit (NULL, helper->wid()->width, style);
    
    helper->add (le);
    QObject::connect(le, SIGNAL(returnPressed ()), helper, SLOT(commit ()));
    QObject::connect(le, SIGNAL(editingFinished ()), helper, SLOT(leave ()));
    le->setText (QTMLineEdit::tr(as_charp(helper->wid()->text)));
 
    if (ends (type, "file") || type == "directory") {
      // autocompletion
      QCompleter *completer = new QCompleter(le);
      QDirModel *dirModel = new QDirModel(le);
      completer->setModel(dirModel);
      le->setCompleter(completer);
    } else if (N(def) > 0) {
      QStringList items;
      for (int j=0; j < N(def); j++)
        items << to_qstring(def[j]);

      QCompleter *completer = new QCompleter(items, le);
      completer->setCaseSensitivity(Qt::CaseSensitive);
      completer->setCompletionMode(QCompleter::InlineCompletion);
      le->setCompleter(completer);
    }
  } else {
    le = new QLineEdit(NULL);
  }
  return le;
}

QLayoutItem *
qt_input_text_widget_rep::as_qlayoutitem () {
  return new QWidgetItem (as_qwidget ());
}
