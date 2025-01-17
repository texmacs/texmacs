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

#if QT_VERSION >= 0x060000

#include "sys_utils.hpp"

QTMPixmapManager::QTMPixmapManager(QString path) : mPath(path) {}

void QTMPixmapManager::loadAll() {
  // We prefer SVG icons over PNG icons and XPM icons.
  // However we use XPM icons as fallback in case 
  // the SVG icons are not available.
  loadAll(QStringList() << "*.svg");
  //loadAll(QStringList() << "*_x4.png");
  //loadAll(QStringList() << "*_x2.png");
  //loadAll(QStringList() << "*.png");
  loadAll(QStringList() << "*.xpm");
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

void QTMPixmapManager::load(QString path, bool is_dark) {
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
      icon = QIcon(pixmap);
      icon.addPixmap(pixmap, QIcon::Disabled);
      icon.addPixmap(pixmap, QIcon::Active);
      icon.addPixmap(pixmap, QIcon::Selected);
    }
    (*icons)[name] = icon;
    return;
  }
}

#endif // QT_VERSION >= 0x060000
