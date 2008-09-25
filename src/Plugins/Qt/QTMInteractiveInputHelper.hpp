/******************************************************************************
 * MODULE     : QTMInteractiveInputHelper.hpp
 * DESCRIPTION: QT Interactive Input Helper class
 * COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license and comes WITHOUT
 * ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
 * If you don't have this file, write to the Free Software Foundation, Inc.,
 * 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 ******************************************************************************/

#ifndef QTMINTERACTIVEINPUTHELPER_HPP
#define QTMINTERACTIVEINPUTHELPER_HPP
 
//#include "qt_other_widgets.hpp" 

#include <QObject>

class qt_tm_widget_rep;

class QTMInteractiveInputHelper : public QObject {
	Q_OBJECT
	
	qt_tm_widget_rep *wid;
	
public:
	QTMInteractiveInputHelper(qt_tm_widget_rep *_wid) : QObject(), wid(_wid) { };
	
	
public slots:
	void doit();
	
};

#endif