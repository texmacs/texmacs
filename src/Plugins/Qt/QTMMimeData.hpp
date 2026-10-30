
/******************************************************************************
 * MODULE     : QTMMimeData.hpp
 * DESCRIPTION: QT Texmacs customized QMimeData class
 * COPYRIGHT  : (C) 2026  Philippe Joyez, Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree.hpp"
#include <QMimeData>

/* This on-demand mechanism is used only for the "default" selection format;
 * When a specific format is specified (e.g. "Copy to html" command), a
 * dedicated faster mechanism is used, where the desired data is provided
 * beforehand to the clipboard, using the standard QMimeData object.
 */

class QTMMimeData : public QMimeData {
  tree sel;
  string mode;
  string lan;
  tree t;
  string s;
public:
  QTMMimeData (tree _sel, string _s);
protected:
#if QT_VERSION >= 0x060000
  QVariant retrieveData (const QString& mimeType,
			 QMetaType type) const override ;
#else
  QVariant retrieveData (const QString& mimeType,
			 QVariant::Type type) const override;
#endif
  QStringList formats () const override;
};
