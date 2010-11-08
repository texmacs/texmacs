
/******************************************************************************
* MODULE     : qt_dialogues.cpp
* DESCRIPTION: QT dialogues widgets classes
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
#include "qt_other_widgets.hpp"
#include "qt_basic_widgets.hpp"
#include "url.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include <QtGui>
#include "QTMFileDialog.hpp"
#include "QTMMenuHelper.hpp"
#include "QTMGuiHelper.hpp"


#define TYPE_CHECK(b) ASSERT (b, "type mismatch")
#define NOT_IMPLEMENTED \
  { if (DEBUG_QT) cout << "STILL NOT IMPLEMENTED\n";  }


class qt_chooser_widget_rep: public qt_widget_rep {
protected:      
  command cmd;
  string type;
  bool   save;
  string win_title;
  string directory;
  coord2 position;
  coord2 size;
  string file;
        
public:
  qt_chooser_widget_rep (command, string, bool);
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
  (command _cmd, string _type, bool _save):
    qt_widget_rep (), cmd (_cmd), type (_type),
    save (_save), position (coord2 (0, 0)), size (coord2 (100, 100)),
    file ("")
{
  if (DEBUG_QT)
    cout << "qt_chooser_widget_rep::qt_chooser_widget_rep type=\""
         << type << "\" save=\"" << save << "\"" << LF;
}

qt_chooser_widget_rep::~qt_chooser_widget_rep() {}

void
qt_chooser_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
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
    TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
    size = open_box<coord2> (val);
    break;
  case SLOT_POSITION:
    TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
    position = open_box<coord2> (val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    TYPE_CHECK (type_box (val) == type_helper<bool>::id);
    perform_dialog ();
    break;              
  case SLOT_STRING_INPUT:
    // send_string (THIS, "input", val);
    TYPE_CHECK (type_box (val) == type_helper<string>::id);
    if (DEBUG_QT) cout << "string input: " << open_box<string> (val) << LF;
    NOT_IMPLEMENTED
    break;
  case SLOT_INPUT_TYPE:
    TYPE_CHECK (type_box (val) == type_helper<string>::id);
    type = open_box<string> (val);
    break;
#if 0
  case SLOT_INPUT_PROPOSAL:
    //send_string (THIS, "default", val);
      NOT_IMPLEMENTED
    break;
#endif
  case SLOT_FILE:
    //send_string (THIS, "file", val);
      TYPE_CHECK (type_box (val) == type_helper<string>::id);
      if (DEBUG_QT) cout << "file: " << open_box<string> (val) << LF;
      file = open_box<string> (val);
    break;
  case SLOT_DIRECTORY:
    TYPE_CHECK (type_box (val) == type_helper<string>::id);
    directory = open_box<string> (val);
    directory = as_string (url_pwd () * url_system (directory));
    break;

  default:
    qt_widget_rep::send (s, val);
  }
}

blackbox
qt_chooser_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_chooser_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      return close_box<coord2> (position);
    }
  case SLOT_SIZE:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      return close_box<coord2> (size);
    }
  case SLOT_STRING_INPUT:
    {
      TYPE_CHECK (type_id == type_helper<string>::id);
      if (DEBUG_QT) cout << "String: " << file << LF;
      return close_box<string> (file);
    }
  default:
    return qt_widget_rep::query (s, type_id);
  }
}

void
qt_chooser_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_QT)
    cout << "[qt_chooser_widget_rep ]";
  switch (s) {
  default: ;
  }
  qt_widget_rep::notify (s, new_val);
}

widget
qt_chooser_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT)
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
  if (DEBUG_QT)
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
file_chooser_widget (command cmd, string type, bool save)  {
  // file chooser widget for files of a given type; for files of type "image",
  // the widget includes a previsualizer and a default magnification
  // for importation can be specified
  return tm_new<qt_chooser_widget_rep> (cmd, type, save);
}

void
qt_chooser_widget_rep::perform_dialog () {
  // int result;
  // FIXME: the chooser dialog is widely incomplete

  QTMFileDialog *dialog;
  QTMImageDialog *imgdialog= 0; // to avoid a dynamic_cast
  
  if (type  == "image")
    dialog= imgdialog= new QTMImageDialog (NULL, to_qstring (win_title), to_qstring(directory * "/" * file));
  else
    dialog= new QTMFileDialog (NULL, to_qstring (win_title), to_qstring(directory * "/" * file));

#if (defined(Q_WS_MAC) && (QT_VERSION >= 0x040500))
  dialog->setOptions(QFileDialog::DontUseNativeDialog);
#endif
  
  QPoint pos = to_qpoint(position);
  //cout << "Size :" << size.x1 << "," << size.x2 << LF;
  if (DEBUG_QT) {
    cout << "Position :" << pos.x() << "," << pos.y() << LF;
    cout << "Dir: " << directory * "/" * file << LF;
  }
  
  dialog->updateGeometry();
  QSize sz = dialog->sizeHint();
  QRect r; r.setSize(sz);
  r.moveCenter(pos);
  dialog->setGeometry(r);
    
  dialog->setViewMode (QFileDialog::Detail);
  if (type == "directory") {
    dialog->setFileMode(QFileDialog::Directory);
  } else if (type == "image") {
    dialog->setFileMode(QFileDialog::ExistingFile);
  } else {
    dialog->setFileMode(QFileDialog::AnyFile);
  }

#if (QT_VERSION >= 0x040400)
  if (type == "directory") {  
  } else if (type == "texmacs") {
  dialog->setNameFilter ("TeXmacs file (*.tm *.ts *.tp)");
  dialog->setDefaultSuffix ("tm");
  } else if (type == "image") {
  dialog->setNameFilter ("Image file (*.gif *.jpg *.jpeg *.pdf *.png *.pnm *.ps *.eps *.ppm *.svg *.tif *.tiff *.fig *.xpm)");
  } else if (type == "bibtex") {
  dialog->setNameFilter ("BibTeX file (*.bib)");
  dialog->setDefaultSuffix ("bib");
  } else if (type == "html") {
  dialog->setNameFilter ("Html file (*.htm *.html *.xhtml)");
  dialog->setDefaultSuffix ("html");
  } else if (type == "latex") {
  dialog->setNameFilter ("LaTeX file (*.tex *.ltx *.sty *.cls)");
  dialog->setDefaultSuffix ("tex");
  } else if (type == "stm") {
  dialog->setNameFilter ("Scheme file (*.stm *.scm)");
  dialog->setDefaultSuffix ("stm");
  } else if (type == "verbatim") {
  dialog->setNameFilter ("Verbatim file (*.txt)");
  dialog->setDefaultSuffix ("txt");
  } else if (type == "tmml") {
  dialog->setNameFilter ("XML file (*.tmml)");
  dialog->setDefaultSuffix ("tmml");  
  } else if (type == "pdf") {
  dialog->setNameFilter ("Pdf file (*.pdf)");
  dialog->setDefaultSuffix ("pdf");
  } else if (type == "postscript") {
  dialog->setNameFilter ("PostScript file (*.ps *.eps)");
  dialog->setDefaultSuffix ("ps");  
  }
#endif
  
  dialog->setLabelText(QFileDialog::Accept, "Ok");

  QStringList fileNames;
  if (dialog->exec ()) {
    fileNames = dialog->selectedFiles();
    if (fileNames.count() > 0) {
      file = from_qstring_utf8 (fileNames[0]);
      url u = url_system (scm_unquote (file));
      if (type == "image")
        file = "(list (url-system " *
          scm_quote (as_string (u)) *
          ") " * imgdialog->getParamsAsString () * ")";
      else
        file = "(url-system " * scm_quote (as_string (u)) * ")";
    }
  } else {
    file = "#f";
  }

  delete dialog;

  cmd ();
}


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
  if (DEBUG_QT)
    cout << "qt_field_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    TYPE_CHECK (type_box (val) == type_helper<string>::id);
    input= open_box<string> (val);
    // send_string (THIS, "input", val);
    break;
  case SLOT_INPUT_TYPE:
    TYPE_CHECK (type_box (val) == type_helper<string>::id);
    type= open_box<string> (val);
    break;
  case SLOT_INPUT_PROPOSAL:
    TYPE_CHECK (type_box (val) == type_helper<string>::id);
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
  if (DEBUG_QT)
    cout << "qt_field_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    TYPE_CHECK (type_id == type_helper<string>::id);
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
    fields[i] = tm_new<qt_field_widget_rep> (this);
    fields[i]->prompt = _prompts[i];
  }
}

qt_input_widget_rep::~qt_input_widget_rep()  {}

void
qt_input_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
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
    TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
    size = open_box<coord2> (val);
    break;
  case SLOT_POSITION:
    TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
    position = open_box<coord2> (val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    TYPE_CHECK (type_box (val) == type_helper<bool>::id);
    perform_dialog ();
    break;
  default:
    qt_widget_rep::send (s, val);
  }
}


blackbox
qt_input_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_input_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      return close_box<coord2> (position);
    }
  case SLOT_SIZE:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
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
  if (DEBUG_QT)
    cout << "[qt_input_widget_rep] ";
  switch (s) {
  default: ;
  }
  qt_widget_rep::notify (s, new_val);
}

widget
qt_input_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT)
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
  if (DEBUG_QT)
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

void
qt_input_widget_rep::perform_dialog() {
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
    // apparently the following flag prevents Qt from substituting an history item
    // for an input when they differ only from the point of view of case (upper/lower)
    // eg. if the history contains aAAAAa and you type AAAAAA then the combo box
    // will retain the string aAAAAa    
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
  d.setWindowTitle(to_qstring(win_title));
  QPoint pos = to_qpoint(position);
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
  cmd ();
}

widget
inputs_list_widget (command call_back, array<string> prompts) {
  // a dialogue widget with Ok and Cancel buttons and a series of textual
  // input widgets with specified prompts
  if (DEBUG_QT) cout << "inputs_list_widget\n";
  return tm_new<qt_input_widget_rep> (call_back, prompts);
}

widget
input_text_widget (int style, command call_back, string type,
		   array<string> def, string width) {
  // a textual input widget for input of a given type and a list of suggested
  // default inputs (the first one should be displayed, if there is one)
  (void) style; (void) width;
  return tm_new<qt_input_text_widget_rep> (call_back, type, def);
}

#if 0
void
qt_tm_widget_rep::do_interactive_prompt () {
  QStringList items;
  QString label= to_qstring (((qt_text_widget_rep*) int_prompt.rep)->str);
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
#elif 1
void
qt_tm_widget_rep::do_interactive_prompt () {
  QStringList items;
  QString label= to_qstring (tm_var_encode (((qt_text_widget_rep*) int_prompt.rep)->str));
  qt_input_text_widget_rep* it = (qt_input_text_widget_rep*) (int_input.rep);
  if ( N(it->def) == 0) {
   items << "";
  } else {
    for (int j=0; j < N(it->def); j++) {
      items << to_qstring(it->def[j]);
    }
  }
  QDialog d (0, Qt::Sheet);
  QVBoxLayout* vl = new QVBoxLayout(&d);
  
  QHBoxLayout *hl = new QHBoxLayout();
    
  QLabel *lab = new QLabel (label,&d);
  QComboBox *cb = new QComboBox(&d);
  cb -> setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLength);
  cb -> setEditText (items[0]);
  int minlen = 0;
  for(int j=0; j < items.count(); j++) {
    cb -> addItem (items[j]);
    int c = items[j].count();
    if (c > minlen) minlen = c;
  }
  cb -> setMinimumContentsLength (minlen>50 ? 50 : (minlen < 2 ? 10 : minlen));
  cb -> setEditable (true);
  // apparently the following flag prevents Qt from substituting an history item
  // for an input when they differ only from the point of view of case (upper/lower)
  // eg. if the history contains aAAAAa and you type AAAAAA then the combo box
  // will retain the string aAAAAa
  cb->setDuplicatesEnabled(true); 
  cb->completer()->setCaseSensitivity(Qt::CaseSensitive);

  lab -> setBuddy (cb);
  hl -> addWidget (lab);
  hl -> addWidget (cb);
  vl -> addLayout (hl);
  
  if (ends (it->type, "file") || it->type == "directory") {
    // autocompletion
    QCompleter *completer = new QCompleter(&d);
    QDirModel *dirModel = new QDirModel(&d);
    completer->setModel(dirModel);
    cb->setCompleter(completer);
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
  
  QRect wframe = view->window()->frameGeometry();
  QPoint pos = QPoint(wframe.x()+wframe.width()/2,wframe.y()+wframe.height()/2);
  
  d.setWindowTitle("Interactive Prompt");
  d.updateGeometry();
  QSize sz = d.sizeHint();
  QRect r; r.setSize(sz);
  r.moveCenter(pos);
  d.setGeometry(r);
  
  
  int result = d.exec ();
  if (result == QDialog::Accepted) {
    QString item = cb->currentText();
    ((qt_input_text_widget_rep*) int_input.rep) -> text=
    scm_quote (from_qstring (item));
    ((qt_input_text_widget_rep*) int_input.rep) -> cmd ();
  } else {
//    ((qt_input_text_widget_rep*) int_input.rep) -> text="#f";
  }
}
#else

#include "QTMInteractivePrompt.hpp"

void
qt_tm_widget_rep::do_interactive_prompt () {
	QString label = to_qstring (tm_var_encode (((qt_text_widget_rep*) int_prompt.rep)->str));
	QStringList items;
  qt_input_text_widget_rep* it = (qt_input_text_widget_rep*) (int_input.rep);
  if ( N(it->def) == 0)
		items << "";
  else for (int j=0; j < N(it->def); j++)
		items << to_qstring(it->def[j]);

	QTMInteractivePrompt _prompt(label, items, to_qstring(it->type), tm_mainwindow());
	
	if (_prompt.exec() == QDialog::Accepted) {
		QString text = _prompt.currentText();
    ((qt_input_text_widget_rep*) int_input.rep) -> text = scm_quote (from_qstring (text));
    ((qt_input_text_widget_rep*) int_input.rep) -> cmd ();
  } else {
		//    ((qt_input_text_widget_rep*) int_input.rep) -> text="#f";
  }
}

#endif


QTMWidgetAction::QTMWidgetAction(QObject *parent)
: QWidgetAction (parent), helper(NULL) { 
  QObject::connect (the_gui->gui_helper, SIGNAL(refresh()), this, SLOT(doRefresh()));
}

QTMWidgetAction::~QTMWidgetAction() {
  QObject::disconnect (the_gui->gui_helper, 0, this, 0);
//  if (menu() && !(menu()->parent())) delete menu(); 
}


void 
QTMWidgetAction::doRefresh() {
  if (N(str)) {
    string t= tm_var_encode (str);
    if (t == "Help") t= "Help ";
    setText (to_qstring (t));
  }
}

QWidget * 
QTMWidgetAction::createWidget ( QWidget * parent ) {
  QLineEdit *le = new QLineEdit (parent);
//  le -> setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLength);
//  le -> setEditable (true);
//  cb -> setDuplicatesEnabled (true); 
//  cb -> completer () -> setCaseSensitivity (Qt::CaseSensitive);
  if (helper && 0) {
    helper -> add (le);
    QObject::connect(le, SIGNAL(returnPressed()), helper, SLOT(commit()));
    cout << N(helper->wid->def);
    cout << helper->wid->def<< LF;
    cout << helper->wid->def[0]<< LF;
    if (N(helper->wid->def) > 0) {
      string s = helper->wid->def[0];
      le -> setText (to_qstring (s));
    }
  }
  return le;  
}

QAction *
qt_input_text_widget_rep::as_qaction () {
  if (!helper) {
    helper = new QTMInputTextWidgetHelper(this);
  }
  QTMWidgetAction *a = new QTMWidgetAction ();
  a->helper = helper;
  return a;
}


qt_input_text_widget_rep::~qt_input_text_widget_rep() { 
  if (helper) delete helper; 
}

void
QTMInputTextWidgetHelper::commit () {
  QLineEdit *le = qobject_cast <QLineEdit*> (sender());
  wid->text = scm_quote (from_qstring (le -> text()));
  cout << "Committing: " << wid->text << LF;
//  wid->cmd ();
}

void
QTMInputTextWidgetHelper::remove (QObject *obj) {
  views.removeAll (qobject_cast<QLineEdit*>(obj));
}

void
QTMInputTextWidgetHelper::add(QLineEdit *obj) {
  if (!views.contains (obj)) {
    QObject::connect(obj, SIGNAL(destroyed(QObject*)), this, SLOT(remove(QObject*)));
    views << obj;
  }
}


QTMInputTextWidgetHelper::~QTMInputTextWidgetHelper() {
  cout << "deleting" << LF;
}

