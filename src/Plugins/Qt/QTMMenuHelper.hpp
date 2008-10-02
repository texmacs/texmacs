/******************************************************************************
 * MODULE     : QTMMenuHelper.hpp
 * DESCRIPTION: QT Texmacs menu helper classes
 * COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license and comes WITHOUT
 * ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
 * If you don't have this file, write to the Free Software Foundation, Inc.,
 * 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 ******************************************************************************/

#ifndef QTMMENUHELPER_HPP
#define QTMMENUHELPER_HPP

#include "message.hpp"
#include "promise.hpp"

#include <QObject>
#include <QAction>
#include <QMenu>

class QTMCommand : public QObject {
	
	Q_OBJECT
	
	command_rep *cmd;
	
public:
	QTMCommand(command_rep *_cmd) : cmd(_cmd) { 	INC_COUNT_NULL(cmd);   };
	~QTMCommand() { DEC_COUNT_NULL(cmd); };
	
public slots:
	void apply() {
		if (cmd) cmd->apply();
	}
	
	
};



class QTMLazyMenu : public QMenu {

	Q_OBJECT
	
	promise_rep<widget> *pm;
	bool forced;

public:
	QTMLazyMenu(promise_rep<widget> *_pm) : pm(_pm), forced(false) { 
	 INC_COUNT_NULL(pm); 
		QObject::connect(this,SIGNAL(aboutToShow()),this,SLOT(force()));
	};
	~QTMLazyMenu() { DEC_COUNT_NULL(pm); }

	public slots:
	void force();
};


#endif
