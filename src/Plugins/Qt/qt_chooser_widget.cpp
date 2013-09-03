
/******************************************************************************
 * MODULE     : qt_chooser_widget.cpp
 * DESCRIPTION: File chooser widget, native and otherwise
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_chooser_widget.hpp"
#include "qt_utilities.hpp"
#include "widget.hpp"
#include "message.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "scheme.hpp"
#include "dictionary.hpp"
#include "editor.hpp"
#include "new_view.hpp"      // get_current_editor()
#include "QTMFileDialog.hpp"

#include <QString>
#include <QStringList>
#include <QFileDialog>
#include <QByteArray>

/*!
 * @param _cmd  Scheme closure to execute after the dialog is closed.
 * @param _type What kind of dialog to show. Can be one of "image", "directory",
 *              or any of the supported file formats: "texmacs", "tmml",
                "postscript", etc. See perform_dialog()
 */
qt_chooser_widget_rep::qt_chooser_widget_rep (command _cmd, string _type, bool _save)
 : qt_widget_rep (file_chooser), cmd (_cmd), save (_save),
   position (coord2 (0, 0)), size (coord2 (100, 100)), file ("")
{
  if (DEBUG_QT)
    cout << "qt_chooser_widget_rep::qt_chooser_widget_rep type=\""
         << type << "\" save=\"" << save << "\"" << LF;
  if (! set_type (_type))
    set_type ("generic");
}

qt_chooser_widget_rep::~qt_chooser_widget_rep() {}


void
qt_chooser_widget_rep::send (slot s, blackbox val) {

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
    case SLOT_STRING_INPUT:
      check_type<string>(val, s);
      if (DEBUG_QT) cout << "string input: " << open_box<string> (val) << LF;
      NOT_IMPLEMENTED
      break;
    case SLOT_INPUT_TYPE:
      check_type<string>(val, s);
      set_type (open_box<string> (val));
      break;
#if 0
    case SLOT_INPUT_PROPOSAL:
        //send_string (THIS, "default", val);
      NOT_IMPLEMENTED
      break;
#endif
    case SLOT_FILE:
        //send_string (THIS, "file", val);
      check_type<string>(val, s);
      if (DEBUG_QT) cout << "file: " << open_box<string> (val) << LF;
      file = open_box<string> (val);
      break;
    case SLOT_DIRECTORY:
      check_type<string>(val, s);
      directory = open_box<string> (val);
      directory = as_string (url_pwd () * url_system (directory));
      break;
      
    default:
      qt_widget_rep::send (s, val);
  }
  if (DEBUG_QT)
    cout << "qt_chooser_widget_rep: sent " << slot_name (s) 
         << "\t\tto widget\t"      << type_as_string() << LF;
}

blackbox
qt_chooser_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_chooser_widget_rep::query " << slot_name(s) << LF;
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
    {
      check_type_id<string> (type_id, s);
      if (DEBUG_QT) cout << "String: " << file << LF;
      return close_box<string> (file);
    }
    default:
      return qt_widget_rep::query (s, type_id);
  }
}

widget
qt_chooser_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT)
    cout << "qt_chooser_widget_rep::read " << slot_name(s) << LF;
  switch (s) {
    case SLOT_WINDOW:
      check_type_void (index, s);
      return this;
    case SLOT_FORM_FIELD:
      check_type<int> (index, s);
      return this;
    case SLOT_FILE:
      check_type_void (index, s);
      return this;
    case SLOT_DIRECTORY:
      check_type_void (index, s);
      return this;
    default:
      return qt_widget_rep::read(s,index);
  }
}

widget
qt_chooser_widget_rep::plain_window_widget (string s, command q)
{
  win_title = s;
  quit      = q;
  return this;
}


bool
qt_chooser_widget_rep::set_type (const string& _type)
{
  if (_type == "directory") {
    type = _type;
    return true;
  } else if (_type == "generic") {
    nameFilter = to_qstring (translate ("Generic file"));
    type = _type;
    return true;
  }

  if (as_bool (call ("format?", _type))) {
    nameFilter = to_qstring (translate
                             (as_string (call ("format-get-name", _type))
                              * " file"));
  } else if (_type == "image") {
    nameFilter = to_qstring (translate ("Image file"));
  } else {
    if (DEBUG_STD)
      cout << "qt_chooser_widget: IGNORING unknown format " << _type << LF;
    return false;
  }

  nameFilter += " (";
  object ret = call ("format-get-suffixes*", _type);
  array<object> suffixes = as_array_object (ret);
  if (N(suffixes) > 0)
    defaultSuffix = to_qstring (as_string (suffixes[0]));
  for (int i = 0; i < N(suffixes); ++i)
    nameFilter += " *." + to_qstring (as_string (suffixes[i]));
  nameFilter += " )";
  
  type = _type;
  return true;
}



/*! Actually displays the dialog with all the options set.
 * Uses a native dialog on Mac/Win and opens a custom dialog with image preview
 * for other platforms.
 */
void
qt_chooser_widget_rep::perform_dialog () {
  QString caption = to_qstring (win_title);
  c_string tmp (directory * "/" * file);
  QString directory = QString::fromLocal8Bit (tmp);
  
#if (defined(Q_WS_MAC) || defined(Q_WS_WIN))
  QFileDialog* dialog = new QFileDialog (NULL, caption, directory, nameFilter);
#else
  QTMFileDialog*  dialog;
  QTMImageDialog* imgdialog= 0; // to avoid a dynamic_cast
  
  if (type == "image")
    dialog= imgdialog= new QTMImageDialog (NULL, caption, directory);
  else
    dialog= new QTMFileDialog (NULL, caption, directory);
#endif
  
  dialog->setViewMode (QFileDialog::Detail);
  if (type == "directory")
    dialog->setFileMode (QFileDialog::Directory);
  else if (type == "image" && !save)  // check !save just in case we support it
    dialog->setFileMode (QFileDialog::ExistingFile);
  else
    dialog->setFileMode (QFileDialog::AnyFile);

#if (QT_VERSION >= 0x040400)
  if (type != "directory") {
    dialog->setNameFilter (nameFilter);
    dialog->setDefaultSuffix (defaultSuffix);
  }
#endif

  dialog->updateGeometry();
  QSize   sz = dialog->sizeHint();
  QPoint pos = to_qpoint (position);
  QRect r;
  r.setSize (sz);
  r.moveCenter (pos);
  dialog->setGeometry (r);
  //dialog->setLabelText (QFileDialog::Accept, "Ok");  // why?
  
  QStringList fileNames;
  file = "#f";
  if (dialog->exec ()) {
    fileNames = dialog->selectedFiles();
    if (fileNames.count() > 0) {
      url u = url_system (scm_unquote (from_qstring (fileNames[0])));
        // FIXME: charset detection in to_qstring() (if that hack is still there)
        // fails sometimes, so we bypass it to force the proper (?) conversions here.
      //QByteArray arr   = to_qstring (as_string (u)).toLocal8Bit ();
      QByteArray arr   = utf8_to_qstring (cork_to_utf8 (as_string (u))).toLocal8Bit ();
      const char* cstr = arr.constData ();
      string localname = string ((char*) cstr);
      file = "(system->url " * scm_quote (localname) * ")";
      if (type == "image") {
#if !defined(Q_WS_MAC) && !defined(Q_WS_WIN)
        file = "(list " * file * imgdialog->getParamsAsString () * ")";
#else
        QPixmap pic (fileNames[0]);
        string params;
          // HACK: which value should we choose here?
        int ww = (get_current_editor()->get_page_width () / PIXEL) / 3;
        int  w = min (ww, pic.width());
        int  h = ((double) pic.height() / (double) pic.width()) * (double) w;
        params << "\"" << from_qstring (QString ("%1px").arg (w)) << "\" "
               << "\"" << from_qstring (QString ("%1px").arg (h)) << "\" "
               << "\"" << "" << "\" "  // xps ??
               << "\"" << "" << "\"";   // yps ??
        file = "(list " * file * params * ")";
#endif
      } else {
      }
    }
  }
  
  delete dialog;
  
  cmd ();
  if (!is_nil (quit)) quit ();

}
