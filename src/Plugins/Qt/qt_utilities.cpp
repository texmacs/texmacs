
/******************************************************************************
* MODULE     : qt_utilities.cpp
* DESCRIPTION: Utilities for QT
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMStyle.hpp"
#include "qt_utilities.hpp"
#include <time.h>

#include <QImage>
#include <QPrinter>
#include <QPainter>
#include <QCoreApplication>
#include <QLocale>
#include <QDateTime>
#include <QTextCodec>
#include <QHash>
#include <QStringList>
#include <QKeySequence>

#include <QPrinter>
#include <QPrintDialog>
#include <QImageReader>

#include "colors.hpp"

#include "dictionary.hpp"
#include "converter.hpp"
#include "language.hpp"
#include "scheme.hpp"
#include "wencoding.hpp"

#include "qt_gui.hpp"    // gui_maximal_extents()

#ifdef USE_GS
#include "Ghostscript/gs_utilities.hpp"
#endif

#define SCREEN_PIXEL (PIXEL)

/******************************************************************************
 * Debugging
 ******************************************************************************/

void
qt_dump (QObject* obj, int indent) {
  if (obj == NULL) { cout << "NULL\n"; return; }
  for (int j = 0;j < indent; ++j) cout << "  ";
  cout << from_qstring (obj->metaObject()->className()) << ":\n";
  foreach (QObject* child , obj->children()) {
    qt_dump (child, indent+1);
  }
}

tm_ostream&
operator << (tm_ostream& out, QRect rect) {
  return out << "(" << rect.x() << "," << rect.y() << ","
  << rect.width() << "," << rect.height() << ")";
}

/******************************************************************************
 * Conversion of data types
 ******************************************************************************/

QFont
to_qfont (int style, QFont font) {
  if (style & WIDGET_STYLE_MINI) {  // Use smaller text font
    int fs = as_int (get_preference ("gui:mini-fontsize", QTM_MINI_FONTSIZE));
    font.setPointSize (qt_zoom (fs > 0 ? fs : QTM_MINI_FONTSIZE));
  }
  if (style & WIDGET_STYLE_MONOSPACED) {  // Use monospaced font
    font.setFixedPitch (true);        //FIXME: ignored for fonts in QActions
#if (QT_VERSION >= 0x040800)
    font.setStyleHint (QFont::Monospace);
#endif
  }
  if (style & WIDGET_STYLE_GREY)      // use grey text font
    font.setWeight (QFont::Light);    // FIXME: this is only an approximation
  if (style & WIDGET_STYLE_PRESSED)   // Button is currently pressed
    {}
  if (style & WIDGET_STYLE_INERT)     // Only render, don't associate any action
    {}
  if (style & WIDGET_STYLE_BUTTON)    // Render button as standard button
    {}
  if (style & WIDGET_STYLE_BOLD)
    font.setBold(true);
  return font;
}

/*! Try to convert a TeXmacs lenght (em, px, w, h) into a QSize.
 
 This uses the widget's current size to compute relative sizes as specified
 with "FFw", where FF is the string representation of a double.
 A value of "1w" should not affect the widget size.
 
 FIXME: does 1w mean 100% of the contents' size or 100% of the available size?
 */
QSize
qt_decode_length (string width, string height,
                  const QSize& ref, const QFontMetrics& fm) {
  QSize size = ref;

    // Width as a function of the default width
  if (ends (width, "w") && is_double (width (0, N(width) - 1))) {
    double x = as_double (width (0, N(width) - 1));
    size.rwidth() *= x;
  }
    // Width as a function of the default height
  else if (ends (width, "h") && is_double (width (0, N(width) - 1))) {
    double y = as_double (width (0, N(width) - 1));
    size.rwidth() = y * size.height();
  }
    // Absolute EM units
  else if (ends (width, "em") && is_double (width (0, N(width) - 2))) {
    double x = as_double (width (0, N(width) - 2));
    size.setWidth(x * fm.width("M")); 
  }
    // Absolute pixel units 
  else if (ends (width, "px") && is_double (width (0, N(width) - 2))) {
    double x = as_double (width (0, N(width) - 2));
    size.setWidth(x);
  }

    // Height as a function of the default width
  if (ends (height, "w") && is_double (height (0, N(height) - 1))) {
    double x = as_double (height (0, N(height) - 1));
    size.rheight() = x * size.width();
  }
    // Height as a function of the default height
  else if (ends (height, "h") && is_double (width (0, N(width) - 1))) {
    double y = as_double (height (0, N(height) - 1));
    size.rheight() *= y;
  }
  else if (ends (height, "em") && is_double (height (0, N(height) - 2))) {
    double y = as_double (height (0, N(height) - 2));
    size.setHeight(y * fm.width("M")); 
  }
  else if (ends (height, "px") && is_double (height (0, N(height) - 2))) {
    double y = as_double (height (0, N(height) - 2));
    size.setHeight(y);
  }
  return size;
}

// used only by to_qkeysequence
static string
conv_sub (const string& ks) {
  string r(ks);
  r = replace (r, "S-", "Shift+");
  r = replace (r, "A-", "Alt+");
  //r = replace (r, "K-", "");
#ifdef Q_WS_MAC
  r = replace (r, "C-", "Meta+");
  r = replace (r, "M-", "Ctrl+");
#else
  r = replace (r, "C-", "Ctrl+");
  r = replace (r, "M-", "Meta+");
#endif
  array<string> a = tokenize (r, " ");
  for (int i = 0; i < N(a); ++i) {
    int pos = -1, tmp = 0, n = N(a[i]);
    while (tmp < n && (tmp = search_forwards ("+", tmp, a[i])) != -1)
      pos = tmp++;
    if (pos != -1 && n > pos+1) {
      if (is_locase (a[i][pos+1]))
        a[i] = a[i](0, pos) * upcase_all (a[i] (pos, n));
      else if (is_upcase (a[i][pos+1])) {
        // HACK: don't prepend Shift for function keys F1, F2...
        if (n>pos+2 && a[i][pos+1] == 'F' && as_int (a[i][pos+2]) > 0)
          ;
        else
          a[i] = a[i](0, pos) * "+Shift" * upcase_all (a[i] (pos, n));
      }
    }
  }
  return recompose (a, ",");
}

QKeySequence
to_qkeysequence (string ks) {
  string r (conv_sub (ks));
  if (DEBUG_QT && N(r) > 0) {
    QKeySequence qks (to_qstring (r));
    debug_qt << "ks: " << ks << " --> " << r << " --> "
             << qks.toString (QKeySequence::NativeText).toLatin1().data() << LF;
    return qks;
  }
  return QKeySequence (to_qstring (r));
}


tm_ostream& operator << (tm_ostream& out, coord4 c) {
    out << "[" << c.x1 << "," << c.x2 << ","
    << c.x3 << "," << c.x4 << "]";
    return out;
}

tm_ostream& operator << (tm_ostream& out, coord2 c) {
    out << "[" << c.x1 << "," << c.x2 << "]";
    return out;
}

coord4
from_qrect (const QRect & rect) {
  SI c1, c2, c3, c4;
  c1= rect.x() * SCREEN_PIXEL;
  c2= -(rect.y() + rect.height()) * SCREEN_PIXEL;       
  c3= (rect.x() + rect.width()) * SCREEN_PIXEL;
  c4= -rect.y() * SCREEN_PIXEL;
  return coord4 (c1, c2, c3, c4);
}

/*! Transforms a rectangle given by its lower left and upper right corners
 into one given by its upper left and width/height */
QRect
to_qrect (const coord4 & p) {
  float c= 1.0/SCREEN_PIXEL;
  return QRect (p.x1*c, -p.x4*c,
                (p.x3-p.x1+SCREEN_PIXEL-1)*c, (p.x4-p.x2+SCREEN_PIXEL-1)*c);
}

coord2
from_qsize (const QSize & s) {
  return coord2 (s.width() * SCREEN_PIXEL, s.height() * SCREEN_PIXEL);
}

QSize
to_qsize (const coord2 & p) {
  float c= 1.0/SCREEN_PIXEL;
  return QSize (p.x1*c, p.x2*c);
}

QSize
to_qsize (const SI& w, const SI& h) {
  float c= 1.0/SCREEN_PIXEL;
  return QSize (w*c, h*c);
}

coord2
from_qpoint (const QPoint & pt) {
  return coord2 (pt.x() * SCREEN_PIXEL, -pt.y() * SCREEN_PIXEL);
}

/*! Transforms texmacs coordinates, with origin at the lower left corner, into
 Qt coordinates, with origin at the upper left corner */
QPoint
to_qpoint (const coord2 & p) {
  float c= 1.0/SCREEN_PIXEL;
  return QPoint (p.x1*c, -p.x2*c);
}

array<string>
from_qstringlist(const QStringList& l) {
  array<string> tl (l.size());
  for(QStringList::const_iterator it = l.begin(); it != l.end(); ++it)
    tl << from_qstring(*it);
  return tl;
}

QStringList
to_qstringlist (array<string> l) {
  QStringList ql;
  for(int i=0; i<N(l); ++i)
    ql << to_qstring(l[i]);
  return ql;
}

/* HACK!!! Something has to be done wrt. to internal encoding: most of the times
 it's cork, but often it's utf8. For instance when the title is built in a tmfs
 title handler in scheme, it is sent to us as an utf8 string. Should we convert
 before? Another example are items in the go-menu: file names are internally
 stored using the os 8-bit encoding (UTF-8 on linux/Mac OS locale code page on windows),
 but we assume that strings are sent to us for display in
 widgets as cork and thus display the wrong encoding.
 
 It gets tricky soon, so for the time being we use this hack.
 */
QString //uses heuristics
to_qstring (const string& s) {
  if (looks_utf8 (s) && !(looks_ascii (s) || looks_universal (s)))
    return utf8_to_qstring (s);
  else
    return utf8_to_qstring (cork_to_utf8 (s));
}

string
from_qstring (const QString &s) {
  return utf8_to_cork (from_qstring_utf8 (s));
}

QString
latin1_to_qstring (const string& s) {
  c_string p (s);
  QString nss= QString::fromLatin1 (p, N(s));
  return nss;
}

QString
utf8_to_qstring (const string& s) {
  c_string p (s);
  QString nss= QString::fromUtf8 (p, N(s));
  return nss;
}

string
from_qstring_utf8 (const QString &s) {
  QByteArray arr= s.toUtf8 ();
  const char* cstr= arr.constData ();
  return string ((char*) cstr);
}

// This should provide better lookup times
static QHash<QString, QColor> _NamedColors;

/*!
  Takes either an hexadecimal RGB color, as in #e3a1ff, or a named color
  as those defined in src/Graphics/Renderer/ * _colors.hpp and returns a QColor
 */
QColor
to_qcolor (const string& col) {
  QString _col = to_qstring(col);
  if (_NamedColors.contains (_col))
    return _NamedColors[_col];
  color c= named_color (col);
  if (c == 0 && locase_all (col) != "black") {
    if(DEBUG_QT_WIDGETS)
      debug_widgets << "to_qcolor(" << col << "): "
        << "name is not defined.\n";
    return QColor(100,100,100);  // FIXME? 
  }
  QColor _c= to_qcolor (c);
  _NamedColors.insert(_col, _c);
  return _c;
}

/*! Returns a color encoded as a string with hexadecimal RGB values, 
 as in #e3a1ff 
 */
string
from_qcolor (const QColor& col) {
  return from_qstring (col.name ());
}

QColor
to_qcolor(color c) {
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  if (get_reverse_colors ()) reverse (r, g, b);
  return QColor (r, g, b, a);
}

color
to_color (const QColor& c) {
  int r, g, b, a;
  c.getRgb (&r, &g, &b, &a);
  if (get_reverse_colors ()) reverse (r, g, b);
  return rgb_color (r, g, b, a);
}




/******************************************************************************
 * Image conversion
 ******************************************************************************/

bool
qt_supports (url u) {
  static QList<QByteArray> formats = QImageReader::supportedImageFormats();
/*  if (DEBUG_CONVERT) {
	  debug_convert <<"QT valid formats:";
	  foreach (QString _format, formats) debug_convert <<", "<< from_qstring(_format);
	  debug_convert <<LF;
  }	*/  
  string suf=suffix (u);
  bool ans = (bool) formats.contains((QByteArray) as_charp(suf));
  //if (DEBUG_CONVERT) {debug_convert <<"QT valid format:"<<((ans)?"yes":"no")<<LF;}
  return ans;
}

bool
qt_image_size (url image, int& w, int& h) {// w, h in points
  if (DEBUG_CONVERT) debug_convert << "qt_image_size :" <<LF;
  QImage im= QImage (utf8_to_qstring (concretize (image)));
  if (im.isNull ()) {
      convert_error << "Cannot read image file '" << image << "'"
      << " in qt_image_size" << LF;
      w= 35; h= 35;
	  return false;
  }
  else {
    w= (int) rint ((((double) im.width ())*2834)/im.dotsPerMeterX());
    h= (int) rint ((((double) im.height())*2834)/im.dotsPerMeterY());
    if (DEBUG_CONVERT) debug_convert <<"QT dotsPerMeter: "
        <<w<<" x "<<h<<LF;
    return true;      
  }
}

void
qt_convert_image (url image, url dest, int w, int h) {// w, h in pixels
  if (DEBUG_CONVERT) debug_convert << "qt_convert_image " << image << " -> "<<dest<<LF;
  QImage im (utf8_to_qstring (concretize (image)));
  if (im.isNull ())
    convert_error << "Cannot read image file '" << image << "'"
    << " in qt_convert_image" << LF;
  else {
    if (w > 0 && h > 0)
      im= im.scaled (w, h, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
    im.scaled (w, h).save (utf8_to_qstring (concretize (dest)));
  }
}

void
qt_image_to_pdf (url image, url outfile, int w_pt, int h_pt, int dpi) {
// use a QPrinter to output raster images to eps or pdf
// dpi is the maximum dpi : the image will either be dowsampled to that dpi
// or the actual dpi will be lower  
  if (DEBUG_CONVERT) debug_convert << "qt_image_to_eps_or_pdf " << image << " -> "<<outfile<<LF;
  QPrinter printer;
  printer.setOrientation(QPrinter::Portrait);
  if (suffix(outfile)=="eps") {
#if (QT_VERSION >= 0x050000)
    //note that PostScriptFormat is gone in Qt5. a substitute?: http://soft.proindependent.com/eps/
    cout << "TeXmacs] warning: PostScript format no longer supported in Qt5\n";
    printer.setOutputFormat(QPrinter::PdfFormat);
#else    
    printer.setOutputFormat(QPrinter::PostScriptFormat);
#endif
  }
  else printer.setOutputFormat(QPrinter::PdfFormat);
  printer.setFullPage(true);
  if (!dpi) dpi=96; 
  printer.setResolution(dpi);
  printer.setOutputFileName(utf8_to_qstring (concretize (outfile)));
  QImage im (utf8_to_qstring (concretize (image)));
  if (im.isNull ()) {
    convert_error << "Cannot read image file '" << image << "'"
    << " in qt_image_to_pdf" << LF;
  // load the "?" image?
  }
  else {
/*  if (DEBUG_CONVERT) debug_convert << "size asked " << w_pt << "x"<<h_pt
  << " at " << maximum dpi <<" dpi"<<LF
  << "dpi set: " << printer.resolution() <<LF;
*/
    if (dpi > 0 && w_pt > 0 && h_pt > 0) {
	    printer.setPaperSize(QSizeF(w_pt, h_pt), QPrinter::Point); // in points

      // w_pt and h_pt are dimensions in points (and there are 72 points per inch)
      int ww = w_pt * dpi / 72;
      int hh = h_pt * dpi / 72;
      if ((ww < im.width ()) ||( hh < im.height ())) //downsample if possible to reduce file size
	      im= im.scaled (ww, hh, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
  	  else // image was too small, reduce dpi accordingly to fill page
        printer.setResolution((int) (dpi*im.width())/(double)ww);
      if (DEBUG_CONVERT) debug_convert << "dpi asked: "<< dpi <<" ; actual dpi set: " << printer.resolution() <<LF;
	  }
	  else printer.setPaperSize(QSizeF(im.width (), im.height ()), QPrinter::DevicePixel);
    QPainter p;
    p.begin(&printer);
    p.drawImage(0, 0, im);
    p.end();
    }
}

void qt_image_to_eps(url image, url outfile, int w_pt, int h_pt, int dpi) {
  qt_image_to_pdf(image, outfile, w_pt, h_pt, dpi);};

/* not in use anymore : now use a Qt printer that outputs ps.

void
qt_image_to_eps (url image, url eps, int w_pt, int h_pt, int dpi) {
  if (DEBUG_CONVERT) debug_convert << "qt_image_to_eps " << image << " -> "<<eps<<LF;
  string r= qt_image_to_eps (image, w_pt, h_pt, dpi);
  save_string (eps, r);
}

string
qt_image_to_eps (url image, int w_pt, int h_pt, int dpi) {
  if (DEBUG_CONVERT) debug_convert << "in qt_image_to_eps"<<LF; 

  static const char* d= "0123456789ABCDEF";
  QImage im (utf8_to_qstring (concretize (image)));
  string r;
  if (im.isNull ())
    convert_error << "Cannot read image file '" << image << "'"
    << " in qt_image_to_eps" << LF;
  else {
    bool alpha= im.hasAlphaChannel ();
    if (dpi > 0 && w_pt > 0 && h_pt > 0) {
      int ww= w_pt * dpi / 72;
      int hh= h_pt * dpi / 72;
      if (ww < im.width () || hh < im.height ()) {
        im= im.scaled (ww, hh, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
      }
    }
    string sw= as_string (im.width ());
    string sh= as_string (im.height ());
    r << "%!PS-Adobe-3.0 EPSF-3.0\n%%Creator: TeXmacs\n%%BoundingBox: 0 0 "
    << sw << " " << sh
    << "\n\n% Created by qt_image_to_eps ()\n\n%%BeginProlog\nsave\n"
    << "countdictstack\nmark\nnewpath\n/showpage {} def\n/setpagedevice "
    << "{pop} def\n%%EndProlog\n%%Page 1 1\n"
    << "/max { 2 copy lt { exch } if pop } bind def\n"
    << "/ImageWidth " << sw
    << " def\n/ImageHeight " << sh << " def\nImageWidth ImageHeight max "
    << "ImageWidth ImageHeight max scale\n\n/ImageDatas\n\tcurrentfile\n\t"
    << "<< /Filter /ASCIIHexDecode >>\n\t/ReusableStreamDecode\n\tfilter\n";
    
    int v, i= 0, j= 0, k= 0, l= 0;
    string mask;
    for (j= 0; j < im.height (); j++) {
      for (i=0; i < im.width (); i++) {
        l++;
        QRgb p= im.pixel (i, j);
        v= qRed (p);
        r << d [(v >> 4)] << d [v % 16];
        v= qGreen (p);
        r << d [(v >> 4)] << d [v % 16];
        v= qBlue (p);
        r << d [(v >> 4)] << d [v % 16];
        if (l > 12) {
          r << "\n";
          l= 0;
        }
      }
     if (alpha) {
        v= 0;
        for (i=0; i < im.width (); i++) {
          v+= (qAlpha (im.pixel (i, j)) == 0) << (3 - i % 4);
          if (i % 4 == 3 || i + 1 == im.width ()) {
            mask << d[v];
            v= 0;
            k++;
              // Padding of the image data mask
            if (i + 1 == im.width () && k % 2 == 1) {
              mask << d[0];
              k++;
            }
              // Code layout
            if (k >= 78) {
              mask << "\n";
              k= 0;
            }
          }
        }
      }
    }
    r << ">\ndef\n\n";
    
    if (alpha) {
      r << "/MaskDatas\n\tcurrentfile\n\t<< /Filter /ASCIIHexDecode >>\n"
      << "\t/ReusableStreamDecode\n\tfilter\n"
      << mask
      << ">\ndef\n\n"
      << "/TheMask\n<<\n\t/ImageType\t1\n\t/Width\t\tImageWidth\n\t/Height\t"
      << "\tImageHeight\n\t/BitsPerComponent 1\n\t/Decode [ 0 1 ]\n\t"
      << "/ImageMatrix [ ImageWidth 0 0 ImageWidth neg 0 ImageHeight ]\n\t"
      << "/DataSource MaskDatas\n>> def\n\n";
    }
    r << "/TheImage\n<<\n\t/ImageType\t1\n\t/Width\t\tImageWidth\n\t/Height\t"
    << "\tImageHeight\n\t/BitsPerComponent 8\n\t/Decode [ 0 1 0 1 0 1 ]\n\t"
    << "/ImageMatrix [ ImageWidth 0 0 ImageWidth neg 0 ImageHeight ]\n\t"
    << "/DataSource ImageDatas\n>> def\n\n"
    << "/DeviceRGB setcolorspace\n";
    if (alpha) {
      r << "<<\n\t/ImageType 3\n\t/InterleaveType 3\n\t/DataDict TheImage\n"
      << "\t/MaskDict TheMask\n>>";
    }
    else {
      r << "\tTheImage";
    }
    r << "\nimage\nshowpage\n%%Trailer\ncleartomark\ncountdictstack\n"
    << "exch sub { end } repeat\nrestore\n%%EOF\n";
  }
  return r;
}
*/

void
qt_image_data (url image, int& w, int&h, string& data, string& palette, string& mask) {
  // debug_convert << "in qt_image_data"<<LF; 
  (void) palette;
  QImage im (utf8_to_qstring (concretize (image)));
  if (im.isNull ()) {
    convert_error << "Cannot read image file '" << image << "'"
    << " in qt_image_data" << LF;
    return;
  }
  w=  im.width ();
  h=  im.height ();    
  data = string ((w*h)*3);
  mask = string (w*h); 
  int i= 0, j= 0, k= 0, l= 0;
  for (j= 0; j < im.height (); j++) {
    for (i=0; i < im.width (); i++) {
      QRgb p= im.pixel (i, j);
      data[l++] = qRed (p);
      data[l++] = qGreen (p);
      data[l++] = qBlue (p);
      mask[k++] = qAlpha (p);
    }
  }
}

QPixmap
as_pixmap (const QImage& im) {
  QPixmap pm (im.size ());
#if (QT_VERSION >= 0x040700)
  pm.convertFromImage (im);
#else
  pm.fromImage (im);
#endif
  return pm;
}


/******************************************************************************
 * Stuff related to widgets
 ******************************************************************************/

QString
parse_tm_style (int style) {
  QString sheet;
  if (style & WIDGET_STYLE_MINI) {  // Use smaller text font
    int fs = as_int (get_preference ("gui:mini-fontsize", QTM_MINI_FONTSIZE));
    sheet += QString("font-size: %1pt;").arg (fs > 0 ? fs : QTM_MINI_FONTSIZE);
    sheet += QString("padding: 1px;");
  }
  if (style & WIDGET_STYLE_MONOSPACED)  // Use monospaced font
    sheet += "font-family: \"monospace\";";
  if (style & WIDGET_STYLE_GREY)      // Use grey text font
    sheet += "color: #414141;";
  if (style & WIDGET_STYLE_PRESSED)   // Button is currently pressed
    sheet += "";
  if (style & WIDGET_STYLE_INERT)     // Only render, don't associate any action
    sheet += "color: #414141;";
  if (style & WIDGET_STYLE_BUTTON)    // Render button as standard button
    sheet += "";
  if (style & WIDGET_STYLE_CENTERED)  // Use centered text
    sheet += "text-align: center;";
  if (style & WIDGET_STYLE_BOLD)
    sheet += "font-weight: bold;";
  if (DEBUG_QT_WIDGETS)
    sheet += "border:1px solid rgb(255, 0, 0);";
  return sheet;
}

void
qt_apply_tm_style (QWidget* qwid, int style) {
  QString sheet = "* {" + parse_tm_style (style) + "}";
  qwid->setStyleSheet (sheet);
  qwid->setEnabled (! (style & WIDGET_STYLE_INERT));
}

void
qt_apply_tm_style (QWidget* qwid, int style, color c) {
  int r,g,b,a;
  get_rgb_color (c, r, g, b, a);
  a = a*100/255;
  QString sheet = "* {" + parse_tm_style (style)
  + QString("color: rgba(%1, %2, %3, %4%);").arg(r).arg(g).arg(b).arg(a)
  + "} ";

#ifdef Q_WS_MAC
    /* Disabled QLabels are not greyed out (at least in MacOS, since Qt 4.7.2), 
     see: https://bugreports.qt-project.org/browse/QTBUG-19008
     For consistency we set the disabled color for all widgets.
     */
  sheet += " :disabled { color: #7F7F7F; }";
#endif
  qwid->setEnabled (! (style & WIDGET_STYLE_INERT));
  qwid->setStyleSheet (sheet);
}


QString
qt_translate (const string& s) {
  string in_lan= get_input_language ();
  string out_lan= get_output_language ();
  return to_qstring(tm_var_encode (translate (s, in_lan, out_lan)));
}

string
qt_application_directory () {
  return string (QCoreApplication::applicationDirPath().toLatin1().constData());
  // This is used to set $TEXMACS_PATH
  // in Windows TeXmacs cannot run if this path contains unicode characters
  // apparently because Guile uses standard narrow char api to load its modules => patch Guile?.  
  // return from_qstring (QCoreApplication::applicationDirPath ());
}

string
qt_get_date (string lan, string fm) {
  QDateTime localtime = QDateTime::currentDateTime();
  if (fm == "") {
    if ((lan == "british") || (lan == "english") || (lan == "american"))
      fm = "MMMM d, yyyy";
    else if (lan == "german")
      fm = "d. MMMM yyyy";
    else if (lan == "chinese" || lan == "japanese" ||
             lan == "korean" || lan == "taiwanese")
    {
      string y = as_string(localtime.date().year());
      string m = as_string(localtime.date().month());
      string d = as_string(localtime.date().day());
      if (lan == "korean")
        return y * "<#b144> " * m * "<#c6d4> " * d * "<#c77c>";
      return y * "<#5e74>" * m * "<#6708>" * d * "<#65e5>";
    }
    else fm = "d MMMM yyyy";
  }
  else if (fm[0] == '%') {
    char buf[64];
    time_t ti;
    time (&ti);
    strftime (buf, sizeof(buf), as_charp(fm), ::localtime(&ti));
    return buf;
  }
  QLocale loc = QLocale (to_qstring (language_to_locale(lan)));
#if (QT_VERSION >= 0x040400)
  QString date = loc.toString (localtime, to_qstring (fm));
#else
  QString date = localtime.toString (to_qstring (fm));
#endif
  return from_qstring (date);
}

string
qt_pretty_time (int t) {
  QDateTime dt= QDateTime::fromTime_t (t);
  QString s= dt.toString ();
  return from_qstring (s);
}

#ifndef _MBD_EXPERIMENTAL_PRINTER_WIDGET  // this is in qt_printer_widget

#define PAPER(fmt)  case QPrinter::fmt : return "fmt"
static string 
qt_papersize_to_string (QPrinter::PaperSize sz) {
  switch (sz) {
      PAPER (A0) ;
      PAPER (A1) ;
      PAPER (A2) ;
      PAPER (A3) ;
      PAPER (A4) ;
      PAPER (A5) ;
      PAPER (A6) ;
      PAPER (A7) ;
      PAPER (A8) ;
      PAPER (A9) ;
      PAPER (B0) ;
      PAPER (B1) ;
      PAPER (B2) ;
      PAPER (B3) ;
      PAPER (B4) ;
      PAPER (B5) ;
      PAPER (B6) ;
      PAPER (B7) ;
      PAPER (B8) ;
      PAPER (B9) ;
      PAPER (B10) ;      
      PAPER (Letter) ;
    default:
      return "A4";
  }
}
#undef PAPER

bool 
qt_print (bool& to_file, bool& landscape, string& pname, url& filename, 
          string& first, string& last, string& paper_type) {
  static QPrinter *qprinter = NULL;
  if (!qprinter) {
    qprinter = new QPrinter;
  }
  QPrintDialog pdialog(qprinter);
  if (pdialog.exec() == QDialog::Accepted) {
    to_file = !(qprinter->outputFileName().isNull());
    pname = from_qstring( qprinter->printerName() );
    filename = from_qstring( qprinter->outputFileName() );
    landscape = (qprinter->orientation() == QPrinter::Landscape);
    paper_type = qt_papersize_to_string(qprinter->paperSize());
    if (qprinter->printRange() == QPrinter::PageRange) {
      first = qprinter->fromPage(); 
      last = qprinter->toPage(); 
    }
    //cout << "Printer :" << pname << LF;
    //cout << "File :" << filename << LF;
    return true;
  }
  return false;
}

#endif //(not defined) _MBD_EXPERIMENTAL_PRINTER_WIDGET


#ifdef OS_MACOS

// Additional utilities for MACOS
// this part has to be at the end because it imports CoreFoundation definitions
// which interfere with TeXmacs and QT types...

#define extend CFextend // avoid name clashes...
#include <CoreFoundation/CoreFoundation.h>
#undef extend


// HACK: this function is needed on MacOS when dropping URLS
// which could not correspond to standard Unix paths

QString fromNSUrl(const QUrl &url) {
  QString localFileQString = url.toLocalFile();
  // [pzion 20150805] Work around
  // https://bugreports.qt.io/browse/QTBUG-40449
  if ( localFileQString.startsWith("/.file/id=") )
  {
    CFStringRef relCFStringRef =
      CFStringCreateWithCString(kCFAllocatorDefault,
                                localFileQString.toUtf8().constData(),
                                kCFStringEncodingUTF8);
    CFURLRef relCFURL =
      CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
                                    relCFStringRef,
                                    kCFURLPOSIXPathStyle,
                                    false); // isDirectory
    CFErrorRef error = 0;
    CFURLRef absCFURL =
      CFURLCreateFilePathURL(kCFAllocatorDefault, relCFURL, &error);
    if ( !error )
    {
      static const CFIndex maxAbsPathCStrBufLen = 4096;
      char absPathCStr[maxAbsPathCStrBufLen];
      if ( CFURLGetFileSystemRepresentation(absCFURL,
                                            true, // resolveAgainstBase
                                            reinterpret_cast<UInt8 *>( &absPathCStr[0] ),
                                            maxAbsPathCStrBufLen))
      {
        localFileQString = QString( absPathCStr );
      }
    }
    CFRelease( absCFURL );
    CFRelease( relCFURL );
    CFRelease( relCFStringRef );
  }
  return localFileQString;
}
#endif // OS_MACOS

