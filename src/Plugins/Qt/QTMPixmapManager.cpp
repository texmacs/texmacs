/******************************************************************************
* MODULE   : QTMPixmapManager.cpp
* DESCRIPTION: A Qt6 utility class to manage the loading of pixmaps and icons
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMPixmapManager.hpp"
#include "QTMSVGIconEngine.hpp"
#include "qt_picture.hpp"

#if QT_VERSION >= 0x060000

#include "sys_utils.hpp"

QTMPixmapManager::QTMPixmapManager(QString path) : mPath(path) {}

void QTMPixmapManager::loadAll() {
  // We prefer SVG icons over PNG icons and XPM icons.
  // However we use XPM icons as fallback in case 
  // the SVG icons are not available.
  loadAll(QStringList() << "*.svg");
  loadAll(QStringList() << "*.xpm");

  // Compatibility with old icons (for some plugins to work)
  loadAllOld(QStringList() << "*_x4.png");
  loadAllOld(QStringList() << "*_x2.png");
  loadAllOld(QStringList() << "*.png");
  loadAllOld(QStringList() << "*.xpm");
}

void QTMPixmapManager::loadAll(QStringList filters) {
  if (mPath.isEmpty()) {
    // set the default path to the TeXmacs pixmaps
    string tmpath = get_env("TEXMACS_PATH");
    mPath = QString::fromUtf8(&tmpath[0], N(tmpath)) + "/misc/pixmaps";
  }
  QDirIterator it(mPath + "/light", filters, QDir::Files, QDirIterator::Subdirectories);
  while (it.hasNext()) {
    load(it.next(), false);
  }
  QDirIterator it_dark(mPath + "/dark", filters, QDir::Files, QDirIterator::Subdirectories);
  while (it_dark.hasNext()) {
    load(it_dark.next(), true);
  }
}

void QTMPixmapManager::loadAllOld(QStringList filters) {
  if (mPath.isEmpty()) {
    // set the default path to the TeXmacs pixmaps
    string tmpath = get_env("TEXMACS_PATH");
    mPath = QString::fromUtf8(&tmpath[0], N(tmpath)) + "/misc/pixmaps";
  }
  QDirIterator it(mPath + "/modern", filters, QDir::Files, QDirIterator::Subdirectories);
  while (it.hasNext()) {
    QString res = load(it.next(), false);
    if (!res.isEmpty()) {
      computeDarkVersion(res);
    }
  }
  QDirIterator it_dark(mPath + "/traditional", filters, QDir::Files, QDirIterator::Subdirectories);
  while (it_dark.hasNext()) {
    load(it_dark.next(), false);
    QString res = load(it_dark.next(), true);
    if (!res.isEmpty()) {
      computeDarkVersion(res);
    }
  }
}

QString QTMPixmapManager::load(QString path, bool is_dark) {
  // Look for the right map
  QMap<QString, QIcon> *icons = &mIcons;
  if (is_dark) {
    icons = &mIconsDark;
  }
  
  // Get the name of the icon
  QString name = QFileInfo(path).baseName();
  name = name.replace(QRegularExpression("_x[24]$"), "");
  name = name.replace(".dark", "");

  // If icon does not exist, create it
  if (!icons->contains(name)) {
    QIcon icon;
    if (path.endsWith(".svg")) {
      icon = QIcon(new QTMSVGIconEngine(path));
      icon.addPixmap(icon.pixmap(64, QIcon::Normal, QIcon::On));
      icon.addPixmap(icon.pixmap(64, QIcon::Disabled, QIcon::On));
      icon.addPixmap(icon.pixmap(64, QIcon::Active, QIcon::On));
      icon.addPixmap(icon.pixmap(64, QIcon::Selected, QIcon::On));
    } else {
      QPixmap pixmap(path);
      if (pixmap.isNull()) {
        return "";
      }
      icon = QIcon(pixmap);
      icon.addPixmap(pixmap, QIcon::Disabled);
      icon.addPixmap(pixmap, QIcon::Active);
      icon.addPixmap(pixmap, QIcon::Selected);
    }
    (*icons)[name] = icon;
    return name;
  }
  return "";
}

void QTMPixmapManager::computeDarkVersion(QString name) {
  if (mIconsDark.contains(name)) {
    return;
  }
  if (!mIcons.contains(name)) {
    return;
  }
  if (mIcons[name].availableSizes().isEmpty()) {
    std_warning << "Icon seems to be corrupted: " << name.toUtf8().constData() << LF;
    return;
  }

  // get the maximum size of the icon
  int maxSizeIndex = 0;
  int maxSizeSquare = 0;
  for (int i = 0; i < mIcons[name].availableSizes().size(); i++) {
    QSize size = mIcons[name].availableSizes()[i];
    int sizeSquare = size.width() * size.height();
    if (sizeSquare > maxSizeSquare) {
      maxSizeSquare = sizeSquare;
      maxSizeIndex = i;
    }
  }

  // create the dark version of the icon
  QImage image = mIcons[name].pixmap(mIcons[name].availableSizes()[maxSizeIndex]).toImage();
  invert_colors (image);
  saturate (image);

  // create the icon
  QPixmap pixmap = QPixmap::fromImage(image);
  QIcon icon = QIcon(pixmap);
  icon.addPixmap(pixmap, QIcon::Disabled);
  icon.addPixmap(pixmap, QIcon::Active);
  icon.addPixmap(pixmap, QIcon::Selected);
  mIconsDark[name] = icon;
}

#endif // QT_VERSION >= 0x060000
