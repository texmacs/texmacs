
/******************************************************************************
* MODULE     : QTMIconManager.hpp
* DESCRIPTION: A Qt6 utility class to manage icons
* COPYRIGHT  : (C) 2024 Liza Belos, 2025 Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TEXMACS_QTMICONMANAGER_HPP
#define TEXMACS_QTMICONMANAGER_HPP

#include <QApplication>

#if QT_VERSION >= 0x060000

#include <QIcon>
#include <QMap>
#include "url.hpp"
#include "gui.hpp"

class QTMIconManager {

public:
  QTMIconManager () {};
  
  QIcon getIcon (url file_name);

  static inline bool is_dark_mode () {
    return occurs ("dark", tm_style_sheet); }

private:
  QMap<QString, QIcon> icon_table, dark_icon_table;
  
  inline const QMap<QString, QIcon>& icon_cache () {
    return is_dark_mode () ? dark_icon_table : icon_table; }
};

#endif // QT_VERSION >= 0x060000
#endif // TEXMACS_QTMICONMANAGER_HPP
