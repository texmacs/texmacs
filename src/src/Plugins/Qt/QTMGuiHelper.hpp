/******************************************************************************
 * MODULE     : QTMGuiHelper.hpp
 * DESCRIPTION: QT Gui helper class
 * COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license and comes WITHOUT
 * ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
 * If you don't have this file, write to the Free Software Foundation, Inc.,
 * 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 ******************************************************************************/

#ifndef QTMGUIHELPER_HPP
#define QTMGUIHELPER_HPP

#include "qt_gui.hpp"
#include <QObject>

class QTMGuiHelper : public QObject {
	Q_OBJECT
	
	qt_gui_rep *gui;
	
public:
	QTMGuiHelper(qt_gui_rep *_gui) : QObject(), gui(_gui) {};
	
	public slots:
	void doUpdate();
};


#endif
