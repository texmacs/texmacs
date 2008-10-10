
/******************************************************************************
* MODULE     : qt_dialogues.cpp
* DESCRIPTION: QT dialogues widgets classes
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "widget.hpp" 
#include "message.hpp"
#include "qt_dialogues.hpp"
#include "qt_utilities.hpp"
#include "qt_other_widgets.hpp"
#include "qt_basic_widgets.hpp"
#include "url.hpp"
#include "analyze.hpp"
#include <QtGui>
#include <QFileDialog>
#include <QInputDialog>

extern char  *slot_name(slot s); // in qt_widget.cpp
#define NOT_IMPLEMENTED { if (DEBUG_EVENTS) cout << "STILL NOT IMPLEMENTED\n";  }

#pragma mark qt_chooser_widget_rep

class qt_chooser_widget_rep: public qt_widget_rep {
protected:	
  command cmd;
  string type;
  string mgn;
  string win_title;
  coord2 position;
  coord2 size;
  string file;
	
public:
  qt_chooser_widget_rep (command, string, string);
  ~qt_chooser_widget_rep ();

  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, blackbox new_val);
  //  virtual void connect (slot s, widget w2, slot s2);
  //  virtual void deconnect (slot s, widget w2, slot s2);
  virtual widget plain_window_widget (string s);

  void perform_dialog();
};

qt_chooser_widget_rep::qt_chooser_widget_rep
  (command _cmd, string _type, string _mgn):
    qt_widget_rep (), cmd (_cmd), type (_type),
    mgn (_mgn), position (coord2 (0, 0)), size (coord2 (100, 100)),
    file ("")
{
  if (DEBUG_EVENTS)
    cout << "qt_chooser_widget_rep::qt_chooser_widget_rep type=\""
	 << type << "\" mgn=\"" << mgn << "\"" << LF; 
}

qt_chooser_widget_rep::~qt_chooser_widget_rep() {}

void
qt_chooser_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_chooser_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_VISIBILITY:
    {	
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      (void) flag;
      NOT_IMPLEMENTED 
    }
    break;
  case SLOT_SIZE:
    if (type_box (val) != type_helper<coord2>::id)
      fatal_error ("type mismatch", "SLOT_SIZE");
    size = open_box<coord2> (val);
    break;
  case SLOT_POSITION:
    if (type_box (val) != type_helper<coord2>::id)
      fatal_error ("type mismatch", "SLOT_POSITION");
    position = open_box<coord2> (val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    if (type_box (val) != type_helper<bool>::id)
      fatal_error ("type mismatch", "SLOT_KEYBOARD_FOCUS");
    perform_dialog ();
    break;		
  case SLOT_STRING_INPUT:
    // send_string (THIS, "input", val);
      NOT_IMPLEMENTED 
    break;
  case SLOT_INPUT_TYPE:
    //	send_string (THIS, "type", val);
      NOT_IMPLEMENTED 
    break;
  case SLOT_INPUT_PROPOSAL:
    //send_string (THIS, "default", val);
      NOT_IMPLEMENTED 
    break;
  case SLOT_FILE:
    //send_string (THIS, "file", val);
      NOT_IMPLEMENTED 
    break;
  case SLOT_DIRECTORY:
    //send_string (THIS, "directory", val);
      NOT_IMPLEMENTED 
    break;

  default:
    qt_widget_rep::send (s, val);
  }
}

blackbox
qt_chooser_widget_rep::query (slot s, int type_id) {
  if (DEBUG_EVENTS)
    cout << "qt_chooser_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:  
    {
      typedef pair<SI,SI> coord2;
      if (type_id != type_helper<coord2>::id)
	fatal_error ("type mismatch (SLOT_POSITION)",
		     "qt_chooser_widget_rep::query");
      return close_box<coord2> (position);
    }
  case SLOT_SIZE:
    {
      typedef pair<SI,SI> coord2;
      if (type_id != type_helper<coord2>::id)
	fatal_error ("type mismatch (SLOT_SIZE)",
		     "qt_chooser_widget_rep::query");
      return close_box<coord2> (size);
    }
  case SLOT_STRING_INPUT:
    {
      if (type_id != type_helper<string>::id)
	fatal_error ("type mismatch (SLOT_STRING_INPUT)",
		     "qt_chooser_widget_rep::query");
      if (DEBUG_EVENTS) cout << "String: " << file << LF;
      return close_box<string> (file);
    }
  default:
    return qt_widget_rep::query (s, type_id);
  }
}

void
qt_chooser_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_EVENTS)
    cout << "[qt_chooser_widget_rep ]";
  switch (s) {
  default: ;
  }
  qt_widget_rep::notify (s, new_val);
}

widget
qt_chooser_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_EVENTS)
    cout << "qt_chooser_widget_rep::read " << slot_name(s) << LF;
  switch (s) {
  case SLOT_WINDOW:
    check_type_void (index, "SLOT_WINDOW");
    return this;
  case SLOT_FORM_FIELD:
    check_type<int> (index, "SLOT_FORM_FIELD");
    return this;
  case SLOT_FILE:
    check_type_void (index, "SLOT_FILE");
    return this;
  case SLOT_DIRECTORY:
    check_type_void (index, "SLOT_DIRECTORY");
    return this;
  default:
    return qt_widget_rep::read(s,index);
  }
}

void
qt_chooser_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_EVENTS)
    cout << "[qt_chooser_widget] ";
  switch (s) {
  default:
    qt_widget_rep::write(s,index,w);
  }
}

widget
qt_chooser_widget_rep::plain_window_widget (string s)
{
  win_title = s;
  return this;
}

widget
file_chooser_widget (command cmd, string type, string mgn)  {
  // file chooser widget for files of a given type; for files of type "image",
  // the widget includes a previsualizer and a default magnification
  // for importation can be specified
  return new qt_chooser_widget_rep (cmd, type, mgn);
}

void
qt_chooser_widget_rep::perform_dialog () {
  // int result;
  // FIXME: the chooser dialog is widely incomplete
  QFileDialog dialog (NULL);
  dialog.setFileMode (QFileDialog::AnyFile);
  //dialog.setNameFilter ("TeXmacs file (*.tm)");
  dialog.setViewMode (QFileDialog::Detail);
  dialog.setWindowTitle (to_qstring (win_title));
  QStringList fileNames;
  if (dialog.exec ()) {
    fileNames = dialog.selectedFiles();
    if (fileNames.count() > 0) {
      file = from_qstring (fileNames[0]);
      url u = url_system (scm_unquote (file));
      if (type == "image")
        file = "(list (url-system " *
	  scm_quote (as_string (u)) *
	  ") \"100\" \"100\" \"0\" \"0\" \"10\" \"10\")";
      //FIXME: fake image dimensions
      else
        file = "(url-system " * scm_quote (as_string (u)) * ")";
    }
  }
  cmd ();	
}

#pragma mark qt_input_widget_rep

class qt_field_widget;

class qt_input_widget_rep: public qt_widget_rep {
protected:	
  command cmd;
  array<qt_field_widget> fields;
  coord2 size, position;
  string win_title; 	
public:
  qt_input_widget_rep (command, array<string>);
  ~qt_input_widget_rep ();
	
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, blackbox new_val);
  // virtual void connect (slot s, widget w2, slot s2);
  // virtual void deconnect (slot s, widget w2, slot s2);
  virtual widget plain_window_widget (string s);

  void perform_dialog();
};

class qt_field_widget_rep: public widget_rep {
  string prompt;
  string input;
  string type;
  array<string> proposals;
  qt_input_widget_rep *parent;
public:
  qt_field_widget_rep(qt_input_widget_rep *_parent):
    widget_rep(), prompt(""), input(""),  proposals(), parent(_parent) {}
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);

  friend class qt_input_widget_rep;
};

void
qt_field_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_field_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    if (type_box (val) != type_helper<string>::id)
      fatal_error ("type mismatch", "SLOT_STRING_INPUT");
    input= open_box<string> (val);
    // send_string (THIS, "input", val);
    break;
  case SLOT_INPUT_TYPE:
    if (type_box (val) != type_helper<string>::id)
      fatal_error ("type mismatch", "SLOT_INPUT_TYPE");
    type= open_box<string> (val);
    break;
  case SLOT_INPUT_PROPOSAL:
    if (type_box (val) != type_helper<string>::id)
      fatal_error ("type mismatch", "SLOT_INPUT_PROPOSAL");
    proposals << open_box<string> (val);
    // send_string (THIS, "default", val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    parent->send(s,val);
    break;
  default:
    widget_rep::send (s, val);
  }
}

blackbox
qt_field_widget_rep::query (slot s, int type_id) {
  if (DEBUG_EVENTS)
    cout << "qt_field_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    if (type_id != type_helper<string>::id)
      fatal_error ("type mismatch (SLOT_STRING_INPUT)",
		   "qt_field_widget_rep::query");
    return close_box<string> (input);
  default:
    return widget_rep::query (s, type_id);
  }
}


class qt_field_widget {
public:
ABSTRACT_NULL(qt_field_widget);
};
ABSTRACT_NULL_CODE(qt_field_widget);

qt_input_widget_rep::qt_input_widget_rep
  (command _cmd, array<string> _prompts):
    qt_widget_rep (), cmd (_cmd),
    fields (N (_prompts)),
    size (coord2 (100, 100)), position (coord2 (0, 0)),
    win_title ("")
{
  for (int i=0; i < N(_prompts); i++) {
    fields[i] = new qt_field_widget_rep (this);
    fields[i]->prompt = _prompts[i];
  }
}

qt_input_widget_rep::~qt_input_widget_rep()  {}

void
qt_input_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_input_widget_rep::send " << slot_name(s) << LF;

  switch (s) {
  case SLOT_VISIBILITY:
    {	
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      (void) flag;
      NOT_IMPLEMENTED 
    }	
    break;
  case SLOT_SIZE:
    if (type_box (val) != type_helper<coord2>::id)
      fatal_error ("type mismatch", "SLOT_SIZE");
    size = open_box<coord2> (val);
    break;
  case SLOT_POSITION:
    if (type_box (val) != type_helper<coord2>::id)
      fatal_error ("type mismatch", "SLOT_POSITION");
    position = open_box<coord2> (val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    if (type_box (val) != type_helper<bool>::id)
      fatal_error ("type mismatch", "SLOT_KEYBOARD_FOCUS");
    perform_dialog ();
    break;
  default:
    qt_widget_rep::send (s, val);
  }
}


blackbox
qt_input_widget_rep::query (slot s, int type_id) {
  if (DEBUG_EVENTS)
    cout << "qt_input_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:  
    {
      typedef pair<SI,SI> coord2;
      if (type_id != type_helper<coord2>::id)
	fatal_error ("type mismatch (SLOT_POSITION)",
		     "qt_input_widget_rep::query");
      return close_box<coord2> (position);
    }
  case SLOT_SIZE:
    {
      typedef pair<SI,SI> coord2;
      if (type_id != type_helper<coord2>::id)
	fatal_error ("type mismatch (SLOT_SIZE)",
		     "qt_input_widget_rep::query");
      return close_box<coord2> (size);
    }
  case SLOT_STRING_INPUT:
    return fields[0]->query (s, type_id);
  default:
    return qt_widget_rep::query (s, type_id);
  }
}

void
qt_input_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_EVENTS)
    cout << "[qt_input_widget_rep] ";
  switch (s) {
  default: ;
  }
  qt_widget_rep::notify (s, new_val);
}

widget
qt_input_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_EVENTS)
    cout << "qt_input_widget_rep::read " << slot_name(s) << LF;
  switch (s) {
  case SLOT_WINDOW:
    check_type_void (index, "SLOT_WINDOW");
    return this;
  case SLOT_FORM_FIELD:
    check_type<int> (index, "SLOT_FORM_FIELD");
    return (widget_rep*) (fields [open_box<int> (index)].rep);
  default:
    return qt_widget_rep::read (s, index);
  }
}

void
qt_input_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_EVENTS)
    cout << "[qt_input_widget_rep] ";
  switch (s) {
  default:
    qt_widget_rep::write (s, index, w);
  }
}

widget qt_input_widget_rep::plain_window_widget (string s)
{
  win_title = s;
  return this;
}

#if 0
void qt_input_widget_rep::perform_dialog ()
{
  bool ok = false;
  for(int i=0; i<N(fields); i++) {
    QStringList items;
    QString label = to_qstring (fields[i]->prompt);
    items << to_qstring (fields[i]->input);
    for(int j=0; j < N(fields[i]->proposals); j++)
      items << to_qstring (fields[i]->proposals[j]);
	
    QString item =
      QInputDialog::getItem (NULL, "Interactive Prompt", label,
			     items, 0, true, &ok);
    if (ok && !item.isEmpty ())
      fields[i]->input = scm_quote (from_qstring (item));
    else { ok = false; break; }
  }
  if (ok) cmd ();
}
#endif

void
qt_input_widget_rep::perform_dialog() {
  QDialog d (0, Qt::Sheet);
  QVBoxLayout* vl = new QVBoxLayout;

  QVector<QComboBox*> cbs (N (fields));
  
  for(int i=0; i<N(fields); i++) {
    QHBoxLayout *hl = new QHBoxLayout;
    QLabel *lab = new QLabel (to_qstring (fields[i]->prompt));
    cbs[i] = new QComboBox;
    cbs[i]->setMinimumContentsLength (40);
    cbs[i]->setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLength);
    cbs[i]->setEditText (to_qstring (fields[i]->input));
    for(int j=0; j < N(fields[i]->proposals); j++)
      cbs[i]->addItem (to_qstring (fields[i]->proposals[j]));
    cbs[i]->setEditable (true);
    lab->setBuddy (cbs[i]);
    hl->addWidget (lab);
    hl->addWidget (cbs[i]);
    vl->addLayout (hl);
  }

  {
    QDialogButtonBox* buttonBox =
      new QDialogButtonBox (QDialogButtonBox::Ok | QDialogButtonBox::Cancel,
			    Qt::Horizontal, &d);
    QObject::connect (buttonBox, SIGNAL (accepted()), &d, SLOT (accept()));
    QObject::connect (buttonBox, SIGNAL (rejected()), &d, SLOT (reject()));
    vl->addWidget (buttonBox);
  }
  d.setLayout (vl);

  int result = d.exec ();
  if (result == QDialog::Accepted) {
    for(int i=0; i<N(fields); i++) {
      QString item = cbs[i]->currentText();
      fields[i]->input = scm_quote (from_qstring (item));
    }
    cmd ();  
  }
}

widget
inputs_list_widget (command call_back, array<string> prompts) {
  // a dialogue widget with Ok and Cancel buttons and a series of textual
  // input widgets with specified prompts
  return new qt_input_widget_rep (call_back, prompts);
}

widget
input_text_widget (command call_back, string type, array<string> def) {
  // a textual input widget for input of a given type and a list of suggested
  // default inputs (the first one should be displayed, if there is one)
  return new qt_input_text_widget_rep (call_back, type, def);
}

void
qt_tm_widget_rep::do_interactive_prompt () {
  QStringList items;
  QString label= to_qstring_utf8 (((qt_text_widget_rep*) int_prompt.rep)->str);
  qt_input_text_widget_rep* it = (qt_input_text_widget_rep*) (int_input.rep);
  for (int j=0; j < N(it->def); j++)
    items << to_qstring(it->def[j]);
  bool ok;
  QString item =
    QInputDialog::getItem (NULL, "Interactive Prompt", label,
			   items, 0, true, &ok );
  if (ok && !item.isEmpty()) {
    ((qt_input_text_widget_rep*) int_input.rep) -> text=
      scm_quote (from_qstring (item));
    ((qt_input_text_widget_rep*) int_input.rep) -> cmd ();
  }
}
