/******************************************************************************
* MODULE   : QTMPixmapManager.hpp
* DESCRIPTION: A Qt6 utility class to manage the loading of pixmaps and icons
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TEXMACS_QTMPIXMAPMANAGER_HPP
#define TEXMACS_QTMPIXMAPMANAGER_HPP

#include <QApplication>

#if QT_VERSION >= 0x060000

#include <QPixmap>
#include <QIcon>
#include <QDir>
#include <QMap>
#include <QDirIterator>
#include <QRegularExpression>
#include <QFontDatabase>

#include "gui.hpp"
#include "url.hpp"
#include "string.hpp"

/**
 * @brief The `QTMPixmapManager` class is a utility class to manage the
 * loading of pixmaps and icons
 * 
 * The `QTMPixmapManager` class is designed to manage the loading and 
 * retrieval of icon images for the TeXmacs application. It provides 
 * functionality to access these icons asynchronously using Qt's 
 * concurrency features.
 * 
 * All the icons with the suffix '.dark' in their name are considered 
 * dark mode icons.
 * 
 * To retrieve an icon asynchronously, you can use the `getIcon` 
 * method and set the icon on a button or another widget when 
 * it's available:
 * ```
  QTMPixmapManager pixmap_manager("path/to/all/icons");
  QPushButton *b = new QPushButton();
  b.setIcon (pixmap_manager.getIcon("open"));
 * ```
 */
class QTMPixmapManager {

public:
  /**
   * @brief Construct a new QTMPixmapManager object
   * 
   * @param path The path to the directory containing the icons.
   * If empty, the $TEXMACS_PATH/misc/pixmaps directory is used.
   */
  QTMPixmapManager(QString path="");
  
  /**
   * @brief Load all icons from the directory
   * 
   * This method loads all icons from the directory specified 
   * in the constructor.
   */
  void loadAll();
  
  /**
   * @brief Load all icons from the directory matching the given filters
   * 
   * This method loads all icons from the directory specified in the 
   * constructor that match the given filters.
   * 
   * @param filters The list of filters to match the icons
   */
  void loadAll(QStringList filters);

  /**
   * @brief Load all old icons from the directory matching the given filters
   * 
   * This method loads all old icons from the directory specified in the 
   * constructor that match the given filters.
   * 
   * @param filters The list of filters to match the icons
   */
  void loadAllOld(QStringList filters);
  
  /**
   * @brief Load an icon from the given path
   * 
   * This method loads an icon from the given path and adds it to 
   * the icon map.
   * 
   * @param path The path to the icon file
   */
  QString load(QString path, bool is_dark=false);

  /**
   * @brief Compute the dark version of an icon
   * 
   * This method computes the dark version of a light icon.
   * 
   * @param name The name of the icon
   */
  void computeDarkVersion(QString name);
  
  /**
   * @brief Get the icon map
   * 
   * This method returns the icon map for the current style.
   * 
   * @return const auto& The icon map
   */
  inline const QMap<QString, QIcon> &icons() const {
    if (occurs ("dark", tm_style_sheet)) {
      return mIconsDark;
    }
    return mIcons;
  }
  
  /**
   * @brief Get an icon by name
   * 
   * This method retrieves an icon by name. If the icon is not found, 
   * a warning is printed to the console.
   * 
   * @param name The name of the icon
   * @return QIcon The icon future
   */
  inline QIcon getIcon(QString name) const {
    name = name.replace(QRegularExpression("\\.xpm$"), "");
    if (icons().contains(name)) {
      return icons()[name];
    }
    std_warning << "Icon not found: " << name.toUtf8().constData() << LF;
    return {};
  }
  
  /**
   * @brief Get an icon by name
   * 
   * This method retrieves an icon by name. If the icon is not found, 
   * a warning is printed to the console.
   * 
   * @param name The name of the icon
   * @return QIcon The icon future
   */
  inline QIcon getIcon(string name) const {
    return getIcon(QString::fromUtf8(&name[0], N(name)));
  }
  
  /**
   * @brief Get an icon by name
   * 
   * This method retrieves an icon by name. If the icon is not found, 
   * a warning is printed to the console.
   * 
   * @param name The name of the icon
   * @return QIcon The icon future
   */
  inline QIcon getIcon(url name) const {
    return getIcon(as_string(name));
  }

private:
  QString mPath;
  QMap<QString, QIcon> mIcons, mIconsDark;
};

#endif // QT_VERSION >= 0x060000
#endif // TEXMACS_QTMPIXMAPMANAGER_HPP
