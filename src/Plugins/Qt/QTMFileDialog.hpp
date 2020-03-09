
/******************************************************************************
* MODULE     : QTMFileDialog.hpp
* DESCRIPTION: QT file choosers
* COPYRIGHT  : (C) 2009 David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMFILEDIALOG_HPP
#define QTMFILEDIALOG_HPP

#include <QFileDialog>
#include <QLabel>
#include <QLineEdit>
#include <QHBoxLayout>
#include "string.hpp"

class QMyFileDialog : public QFileDialog
{
  Q_OBJECT
public:
  QMyFileDialog (QWidget* parent= 0,
                 const QString & caption = QString(),
                 const QString & directory = QString(),
                 const QString & filter = QString())
  : QFileDialog(parent, caption, directory, filter) { }
};

class QTMFileDialog : public QDialog
{
  Q_OBJECT

protected:
  QHBoxLayout* hbox;
  QFileDialog* file;
  void dragEnterEvent(QDragEnterEvent* event);
  void dragMoveEvent(QDragMoveEvent* event);
  void dragLeaveEvent(QDragLeaveEvent* event);
  void dropEvent(QDropEvent* event);

public:
  QTMFileDialog (QWidget* parent= 0,
                 const QString& caption = QString(),
                 const QString& directory = QString(),
                 const QString & filter = QString());
  QStringList selectedFiles () { return file->selectedFiles (); };
#if (defined(Q_OS_MAC) && (QT_VERSION >= 0x040500))
  void setOptions (QFileDialog::Options opts) { file->setOptions (opts); };
#endif
  void setAcceptMode (QFileDialog::AcceptMode mode) { file->setAcceptMode(mode); }
  void setViewMode (QFileDialog::ViewMode mode) { file->setViewMode (mode); }
  void setFileMode (QFileDialog::FileMode mode) { file->setFileMode (mode); }
#if (QT_VERSION >= 0x040400)
  void setNameFilter (const QString& filter) { file->setNameFilter (filter); }
  void setNameFilters (const QStringList& filters) { file->setNameFilters (filters); }
#endif
  void setDefaultSuffix (const QString& suffix) { file->setDefaultSuffix (suffix); }
  void setLabelText (QFileDialog::DialogLabel label, const QString& text) { file->setLabelText (label, text); }
};

class QTMImagePreview : public QWidget
{
  Q_OBJECT

  QLabel *image;

public:
  QLineEdit* wid;
  QLineEdit* hei;
  QLineEdit* xps;
  QLineEdit* yps;

public slots:
  void setImage (const QString&);

public:
  QTMImagePreview (QWidget* parent= 0);
};

class QTMImageDialog : public QTMFileDialog
{
  Q_OBJECT

  QTMImagePreview* preview;

public:
  QTMImageDialog (QWidget* parent= 0,
                  const QString& caption = QString (),
                  const QString& directory = QString (),
                  const QString& filter = QString ());
  string getParamsAsString ();
};

#endif // defined QTMFILEDIALOG_HPP

