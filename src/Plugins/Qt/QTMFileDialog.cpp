
/******************************************************************************
* MODULE     : QTMFileDialog.cpp
* DESCRIPTION: QT file choosers
* COPYRIGHT  : (C) 2009 David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMFileDialog.hpp"
#include <QPainter>
#include <QLineEdit>
#include <QIntValidator>
#include <QMimeData>
#include <QUrl>
#include <QDrag>
#include <QDragEnterEvent>
#include <QDragLeaveEvent>
#include <QDropEvent>
#include "file.hpp"
#include "sys_utils.hpp"
#include "qt_utilities.hpp"
#include "qt_gui.hpp"
#include "analyze.hpp"
#include "dictionary.hpp"
#include "image_files.hpp"

QTMFileDialog::QTMFileDialog (QWidget * parent, const QString & caption, 
                              const QString & directory, const QString & filter)
  : QDialog (parent) 
{
  setWindowTitle (caption);
  hbox= new QHBoxLayout (this);
  hbox->setContentsMargins (0, 0, 0, 0);
  file= new QMyFileDialog (0, caption, directory, filter);
  hbox->addWidget (file);
  setLayout (hbox);
  setAcceptDrops(true);
  connect(file, SIGNAL(accepted()), this, SLOT(accept()));
  connect(file, SIGNAL(finished(int)), this, SLOT(done(int)));
  connect(file, SIGNAL(rejected()), this, SLOT(reject()));
}

void QTMFileDialog::dragEnterEvent(QDragEnterEvent *event)
{
	event->acceptProposedAction();
}

void QTMFileDialog::dragMoveEvent(QDragMoveEvent *event)
{
	event->acceptProposedAction();
}

void QTMFileDialog::dropEvent(QDropEvent *event)
{
	const QMimeData *mimeData = event->mimeData();
	
	foreach (QString format, mimeData->formats()) {
		if (format == "text/uri-list") {
			file->selectFile(mimeData->urls().at(0).toLocalFile());
			break;
		}
	}
	
	event->acceptProposedAction();
}

void QTMFileDialog::dragLeaveEvent(QDragLeaveEvent *event)
{
	event->accept();
}


static QWidget*
simple_input (string s, QLineEdit* ledit, QWidget* parent= 0) {
  QWidget* widget= new QWidget (parent);
  QHBoxLayout* layout= new QHBoxLayout (widget);
  layout->setContentsMargins (0, 0, 0, 0);
//  string in_lan= get_input_language ();
//  string out_lan= get_output_language ();
//  QLabel* label= new QLabel (to_qstring (tm_var_encode (translate (s, in_lan, out_lan))), parent);
  QLabel* label= new QLabel (to_qstring (s), parent);
  layout->addWidget (label);
  layout->addWidget (ledit);
  widget->setLayout (layout);
  return widget;
}

QTMImagePreview::QTMImagePreview (QWidget* parent)
  : QWidget (parent) {
  QVBoxLayout* vbox= new QVBoxLayout (this);
  vbox->addStretch ();
  image= new QLabel (this);
  image->setMinimumWidth (100);
  image->setAlignment (Qt::AlignCenter);
  vbox->addWidget (image);
  vbox->addSpacing (10);
  wid= new QLineEdit (this);
  vbox->addWidget (simple_input ("Width:", wid, this));
  hei= new QLineEdit (this);
  vbox->addWidget (simple_input ("Height:", hei, this));
  xps= new QLineEdit (this);
  vbox->addWidget (simple_input ("X-position:", xps, this));
  yps= new QLineEdit (this);
  vbox->addWidget (simple_input ("Y-position:", yps, this));
  vbox->addStretch ();
  vbox->addStretch ();
  setLayout (vbox);
  setMinimumWidth (175);
  setMaximumWidth (225);
  setImage (0);
}

void 
QTMImagePreview::setImage (const QString& file) { 	  //generate thumbnail
BEGIN_SLOT
  QImage img;
  wid->setText ("");
  hei->setText ("");
  xps->setText ("");
  yps->setText ("");

  string localname= from_qstring_utf8 (file);
  url image_url= url_system (localname);
  if (DEBUG_CONVERT) debug_convert<<"image preview :["<<image_url<<"]"<<LF;
  if (!(as_string(image_url)=="") && !is_directory(image_url) && exists(image_url) ){
    url temp= url_temp (".png");
    int w_pt, h_pt;
    double w, h;
    image_size (image_url, w_pt, h_pt);
    if (w_pt*h_pt !=0) { //necessary if gs returns h=v=0 (for instance 0-size pdf)
      wid->setText (QString::number (w_pt) + "pt");
      hei->setText (QString::number (h_pt) + "pt");
      if (w_pt > h_pt) {
        w= 98;
        h= ceil (h_pt*98/w_pt);
      } else {
        w= ceil (w_pt*98/h_pt);
        h= 98;
      }
      // generate thumbnail:
      image_to_png (image_url, temp, w, h);
      img.load (utf8_to_qstring (as_string (temp)));
      remove (temp);
    }
  }

  if (img.isNull()) {
    QImage vide (100, 100, QImage::Format_RGB32);
    QPainter painter;
    painter.begin (&vide);
    painter.fillRect (0, 0, 100, 100, Qt::white);
    QPen ThinBlack (Qt::black);
    ThinBlack.setWidth (0);
    ThinBlack.setStyle (Qt::SolidLine);
    painter.setPen (ThinBlack);
    painter.drawLine (0, 0, 99, 99);
    painter.drawLine (0, 99, 99, 0);
    painter.drawRect (0, 0, 99, 99);
    painter.end ();
    image->setPixmap(QPixmap::fromImage(vide));
  }
  else
    image->setPixmap (QPixmap::fromImage (img.scaled (98, 98, Qt::KeepAspectRatio, Qt::FastTransformation)));
END_SLOT
}

QTMImageDialog::QTMImageDialog (QWidget* parent, const QString& caption, const QString& directory, const QString& filter)
  : QTMFileDialog (parent, caption, directory, filter)
{
  preview= new QTMImagePreview (this);
  hbox->insertWidget(0, preview);
  connect(file, SIGNAL(currentChanged (const QString&)), preview, SLOT(setImage(const QString&)));
}

string
QTMImageDialog::getParamsAsString () {
  string params;
  params << "\"" << from_qstring (preview->wid->text ()) << "\" ";
  params << "\"" << from_qstring (preview->hei->text ()) << "\" ";
  params << "\"" << from_qstring (preview->xps->text ()) << "\" ";
  params << "\"" << from_qstring (preview->yps->text ()) << "\"";
  return params;
}
