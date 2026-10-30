
/******************************************************************************
 * MODULE     : QTMMimeData.cpp
 * DESCRIPTION: QT Texmacs customized QMimeData class
 * COPYRIGHT  : (C) 2026  Philippe Joyez, Gregoire Lecerf
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMMimeData.hpp"
#include "locale.hpp"
#include "Editor/edit_main.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "qt_gui.hpp"
#include <QClipboard>
#include <QMimeData>
#include <QByteArray>
#include <QImage>
#include <QCoreApplication>

string selection_encode (string lan, string s);
tree get_default_clipboard_verbatim ();
tree get_default_clipboard_html ();
tree get_default_clipboard_latex ();
string get_default_clipboard_image ();
string get_default_clipboard_image_format ();

QTMMimeData::QTMMimeData (tree _sel, string _s):
  sel (_sel), t (copy (_sel[1])), s (_s) {
  // sel is tuple ("texmacs", selection_tree, mode, lan)
  tree sel2 = copy (sel);
  mode= as_string (sel2[2]);
  lan= as_string (sel2[3]);
}

QStringList 
QTMMimeData::formats () const {
  // Register all MIME types we can place on the clipboar.
  // Decreasing priority order.
  static QStringList l0 = {
    "application/x-texmacs-clipboard",
    "application/x-texmacs-pid"
  };
  static QStringList l1 = {  
    "text/html",
    "application/x-tex",
    "text/x-tex",
    "text/plain;charset=utf-8", // always asked (by Qt?), even if not defined
    "text/plain"
  };
  QStringList ret= l0;
  string image= get_default_clipboard_image ();
  string image_format= locase_all (get_default_clipboard_image_format ());
  if (image != "") {
    ret += QString ("application/pdf");
    if (image_format == "svg")
      ret += QString ("image/svg+xml");
    if (qt_supports ("x." * image_format))
      ret += QString ("application/x-qt-image");
    ret += QString ("image/png");
  }
  else ret += l1;
  return ret;
}

static inline QByteArray
to_qbytearray (const string& s) {
  c_string cs (s);
  QByteArray rawdata (cs, N(s));
  return rawdata;
}

static string
_convert (string from, string fm_from, string fm_to) {
  if (fm_from == fm_to) return from;
  url f= url_temp ("." * fm_from), t= url_temp ("." * fm_to);
  save_string (f, from);
  call_scm_converter (f, t);
  string to;
  load_string (t, to, false);
  if (exists (to)) remove (to);
  if (exists (from)) remove (from);
  return to;
}

static string
_convert_to_pdf (string from, string fm_from) {
  if (fm_from == "pdf") return from;
  url f= url_temp ("." * fm_from), t= url_temp (".pdf");
  save_string (f, from);
  qt_image_to_pdf (f, t);
  string to;
  load_string (t, to, false);
  if (exists (to)) remove (to);
  if (exists (from)) remove (from);
  return to;
}

static string
_convert_pdf_to_png (string from) {
  url f= url_temp (".pdf"), t= url_temp (".png");
  save_string (f, from);
  image_to_png (f, t);
  string to;
  load_string (t, to, false);
  if (exists (to)) remove (to);
  if (exists (from)) remove (from);
  return to;
}

QVariant 
#if QT_VERSION >= 0x060000
QTMMimeData::retrieveData (const QString &qmimeType, QMetaType type) const 
#else
QTMMimeData::retrieveData (const QString &qmimeType, QVariant::Type type) const
#endif
{
  (void) type;
  string mimeType = from_qstring (qmimeType);
  if (DEBUG_QT)
    debug_qt << "QTMMimeData::retrieveData, " << mimeType << LF;
  if (mimeType == "application/x-texmacs-pid") {
    QString pid_str;
    pid_str.setNum (QCoreApplication::applicationPid ());
    return pid_str.toLatin1();
  }
  if (mimeType == "application/x-qt-mime-type-name") {
    QByteArray customTypes = "application/x-texmacs";
    customTypes.append ('\0');
    return customTypes;
  }
  if (mimeType == "application/x-texmacs-clipboard")
    return to_qbytearray (s);
  if (mimeType == "text/plain" ||
      mimeType == "text/plain;charset=utf-8") {
    tree txt= get_default_clipboard_verbatim ();
    string sv= tree_to_generic (txt, "verbatim-snippet");
    sv= selection_encode (lan, sv);
    c_string _sv (sv);
    string enc = get_preference ("texmacs->verbatim:encoding");
    if (enc == "auto")
      enc = get_locale_charset ();
    QString qs= (enc == "utf-8" || enc == "UTF-8") ?
      QString::fromUtf8 (_sv, N(sv)) : QString::fromLatin1 (_sv, N(sv));
    if (mimeType == "text/plain;charset=utf-8")
      return qs.toUtf8 ();
    else
      return qs;
  }
  if (mimeType == "text/html") {
    tree t= get_default_clipboard_html ();
    object obj= call ("texmacs->html*", tree_to_stree (t));
    string sh= as_string (call ("serialize-html", obj));
    return to_qbytearray (selection_encode (lan, sh));
  }
  if (mimeType == "application/x-tex" || mimeType == "text/x-tex") {
    tree t= get_default_clipboard_latex ();
    string st= tree_to_generic (t, "latex-snippet");
    st= selection_encode (lan, st);
    string enc = get_preference ("texmacs->latex:encoding"); 
    if (enc == "utf-8" || enc == "UTF-8" || enc == "cork")
      return utf8_to_qstring (st);
    else {
      c_string _st;
      return QString::fromLatin1 (_st, N(st));
    }
  }
  string image= get_default_clipboard_image ();
  string image_format= locase_all (get_default_clipboard_image_format ());
  if (mimeType == "application/pdf")
    return to_qbytearray (_convert_to_pdf (image, image_format));
  if (mimeType == "image/svg+xml") {
    string svg;
    if (image_format == "svg")
      svg= image;
    else {
      string pdf= _convert_to_pdf (image, image_format);
      svg= _convert (pdf, "pdf", "svg");
    }
    return to_qbytearray (svg);
  }
  if (mimeType == "application/x-qt-image") {
    c_string _fm (image_format);
    QImage im; im.loadFromData (to_qbytearray (image), _fm);
    return QVariant::fromValue (im);
  }
  if (mimeType == "image/png") {
    string png;
    if (image_format == "png")
      png= image;
    else {
      string pdf= _convert_to_pdf (image, image_format);
      png= _convert_pdf_to_png (pdf);
    }
    return to_qbytearray (png);
  }
  if (DEBUG_QT)
    debug_qt << "QTMMimeData::retrieveData, unsupported MimeType "
	     << mimeType << LF;
  return QMimeData::retrieveData (qmimeType, type);
}
