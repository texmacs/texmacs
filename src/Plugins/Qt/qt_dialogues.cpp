
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
#include "qt_tm_widget.hpp"
#include "qt_basic_widgets.hpp"
#include "qt_chooser_widget.hpp"
#include "qt_printer_widget.hpp"
#include "qt_color_picker_widget.hpp"
#include "url.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "QTMMenuHelper.hpp"
#include "QTMGuiHelper.hpp"
#include "QTMStyle.hpp"

#include <QtGui>
#include "string.hpp"


widget
file_chooser_widget (command cmd, string type, bool save)  {
  // file chooser widget for files of a given type; for files of type "image",
  // the widget includes a previsualizer and a default magnification
  // for importation can be specified
  return tm_new<qt_chooser_widget_rep> (cmd, type, save);
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

widget
qt_input_widget_rep::plain_window_widget (string s)
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



/*******************************************************************************
* Input text widget implementation
*******************************************************************************/

class QTMLineEdit : public QLineEdit {
  
public:
  string ww; // width
  
  QTMLineEdit (QWidget *parent, string _ww) 
  : QLineEdit (parent), ww (_ww) {
    setStyle (qtmstyle());
    QPalette pal (palette());
    pal.setColor (QPalette::Base, QColor (252, 252, 248));
    setPalette (pal);
  };
  
  virtual QSize	sizeHint () const ;
protected:
  void keyPressEvent (QKeyEvent *event);
  void focusInEvent (QFocusEvent *evenement);

};

void 
QTMLineEdit::keyPressEvent(QKeyEvent *event)
{
  QCompleter *c = completer();
  // reset completion
  if (c) c->setCompletionPrefix (QString ());

  if (event->key() == Qt::Key_Up){
    if (c) {
      int row = c->currentRow();
      c->setCurrentRow(row-1);
      setText(c->currentCompletion());
    }
    event->accept();
    // move back in history
  } else if (event->key() == Qt::Key_Down){
    if (c) {
      int row = c->currentRow();
      c->setCurrentRow(row+1);
      setText(c->currentCompletion());
    }
    event->accept();
    // move forward in history
  } else if (event->key() == Qt::Key_Escape){
    emit editingFinished();
    event->accept();
    // exit editing
  }
  else {
    // default handler for event
    QLineEdit::keyPressEvent(event);
  }
}

QSize
QTMLineEdit::sizeHint () const {
  QSize sz(QLineEdit::sizeHint());
  string s = ww; // to avoid const casting
  if (ends (s, "w") && is_double (s (0, N(s) - 1)) && parentWidget()) {
    double x= as_double (s (0, N(s) - 1));
    sz.setWidth(parentWidget()->width() * x);
//    ev->w= max (ev->w, (4 * fn->wquad + 2*dw) / SHRINK);
  } else if (ends (s, "em") && is_double (s (0, N(s) - 2))) {
    double x= as_double (s (0, N(s) - 2));
    QFontMetrics fm(fontMetrics());
    sz.setWidth(x*fm.width("m")); //FIXME: put real font width
  }  else if (ends (s, "px") && is_double (s (0, N(s) - 2))) {
    double x= as_double (s (0, N(s) - 2));
    sz.setWidth(x);
  }  
  return sz;
}

void 
QTMLineEdit::focusInEvent (QFocusEvent *event)
{
  setCursorPosition (text().size());
//  selectAll ();
  QLineEdit::focusInEvent (event);
}


QTMWidgetAction::QTMWidgetAction (QObject *parent)
: QWidgetAction (parent), helper (NULL) { 
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
  QWidget *le;
  if (helper) {
    le = helper->wid()->as_qwidget();
    le->setParent(parent);
  } else {
    le = new QLineEdit(parent);
  }
  return le;  
}

QAction *
qt_input_text_widget_rep::as_qaction () {
  if (!helper) {
    helper = new QTMInputTextWidgetHelper(this);
    // helper retains the current widget
    // in toolbars the widget is not referenced directly in texmacs code
    // so must be retained by Qt objects
  }
  QTMWidgetAction *a = new QTMWidgetAction ();
  a->setText("QTMWidgetAction");
  a->helper = helper;
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
  if (helper) {
    le = new QTMLineEdit (NULL, helper->wid()->width);
    helper -> add (le);
    QObject::connect(le, SIGNAL(returnPressed ()), helper, SLOT(commit ()));
    QObject::connect(le, SIGNAL(editingFinished ()), helper, SLOT(leave ()));
    le -> setText (to_qstring (helper->wid()->text));
    QFont f= le->font();
    f.setPixelSize(10);
    le->setFont(f);
    //le->setFrame(false);
    if (ends(helper->wid()->width,"w")) {
      le->setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Fixed);
    } else {
      le->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    }
    
    
    if (ends (type, "file") || type == "directory") {
      // autocompletion
      QCompleter *completer = new QCompleter(le);
      QDirModel *dirModel = new QDirModel(le);
      completer->setModel(dirModel);
      le->setCompleter(completer);
    } else if (N(def) > 0) {
      QStringList items;
      for (int j=0; j < N(def); j++) {
        items << to_qstring(def[j]);
      }
      QCompleter *completer = new QCompleter(items, le);
      completer->setCaseSensitivity(Qt::CaseSensitive);
      completer->setCompletionMode(QCompleter::InlineCompletion);
      le->setCompleter(completer);
    }
    
    //le->selectAll();
    
  } else {
    le = new QLineEdit(NULL);
  }
  return le;
}


QLayoutItem *
qt_input_text_widget_rep::as_qlayoutitem () {
  return new QWidgetItem(as_qwidget());
}


qt_input_text_widget_rep::qt_input_text_widget_rep 
  (command _cmd, string _type, array<string> _def, string _width)
  : cmd (_cmd), type (_type), def (_def), text (""), width(_width), helper(NULL), ok(false) 
{
  if (N(def) > 0) {
    text = def[0];
  }
}


qt_input_text_widget_rep::~qt_input_text_widget_rep() { 
}

void
QTMInputTextWidgetHelper::doit () {
  if (done) return;
  done = true;
#if 0
  if (wid()->ok) 
   cout << "Committing: " << wid () -> text << LF;
  else 
    cout << "Leaving with text: " << wid () -> text << LF;
#endif
  wid () -> cmd (wid()->ok ? list_object (object (wid() -> text)) : 
                      list_object (object (false)));
}

void
QTMInputTextWidgetHelper::commit () {
  QLineEdit *le = qobject_cast <QLineEdit*> (sender());
  if (le) {
//    le -> setFrame(false);
    wid()->ok = true;
    done = false;
    wid () -> text = from_qstring (le -> text());
  }
}

void
QTMInputTextWidgetHelper::leave () {
  // this is executed after commit
  // and when losing focus
  QLineEdit *le = qobject_cast <QLineEdit*> (sender());  
  if (le) {
    // reset the text according to the texmacs widget
    le -> setText (to_qstring (wid () -> text));
    //ok = false;
    QTimer::singleShot (0, this, SLOT (doit ()));
  
  }
}

void
QTMInputTextWidgetHelper::remove (QObject *obj) {
  views.removeAll (qobject_cast<QLineEdit*> (obj));
  if (views.count () == 0) {
    // no more view, free the helper 
    deleteLater();
  }
}

void
QTMInputTextWidgetHelper::add(QLineEdit *obj) {
  if (!views.contains (obj)) {
    QObject::connect (obj, SIGNAL(destroyed (QObject*)), this, SLOT(remove (QObject*)));
    views << obj;
  }
}

QTMInputTextWidgetHelper::~QTMInputTextWidgetHelper() {
  //cout << "deleting" << LF;
  // remove refernce to helper in the texmacs widget
  wid()->helper = NULL;
  // if needed the texmacs widget is automatically deleted
}


/*******************************************************************************
* Interface to texmacs. See src/Graphics/Gui/widget.hpp.
*******************************************************************************/


widget
inputs_list_widget (command call_back, array<string> prompts) {
  // a dialogue widget with Ok and Cancel buttons and a series of textual
  // input widgets with specified prompts
  if (DEBUG_QT) cout << "inputs_list_widget\n";
  return tm_new<qt_input_widget_rep> (call_back, prompts);
}

widget
input_text_widget (command call_back, string type, array<string> def,
                   int style, string width) {
  // a textual input widget for input of a given type and a list of suggested
  // default inputs (the first one should be displayed, if there is one)
  (void) style; (void) width;
  return tm_new<qt_input_text_widget_rep> (call_back, type, def, width);
}

widget
color_picker_widget (command call_back, bool bg, array<tree> proposals) {
  // widgets for selecting a color, a pattern or a background image,
  // encoded by a tree. On input, we give a list of recently used proposals
  // on termination the command is called with the selected color as argument
  // the bg flag specifies whether we are picking a background color or fill
  
  return tm_new<qt_color_picker_widget_rep>(call_back, bg, proposals);  
}

widget 
printer_widget (command cmd, url ps_pdf_file) {
  // widget to print the document, offering a way for selecting a page range,
  // changing the paper type and orientation, previewing, etc.
  return tm_new<qt_printer_widget_rep>(cmd, ps_pdf_file);
}
