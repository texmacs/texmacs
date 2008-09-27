/******************************************************************************
 * MODULE     : QTMWidget.hpp
 * DESCRIPTION: QT Texmacs widget class
 * COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license and comes WITHOUT
 * ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
 * If you don't have this file, write to the Free Software Foundation, Inc.,
 * 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 ******************************************************************************/

#ifndef QTMWIDGET_HPP
#define QTMWIDGET_HPP

#include "qt_simple_widget.hpp" 
#include <QVariant>
#include <QWidget>

class QTMWidget : public QWidget {
	Q_OBJECT

public:
	QTMWidget(simple_widget_rep *_wid) : QWidget() { 
	  setFocusPolicy(Qt::StrongFocus);
	//	setBackgroundRole(QPalette::Window);
//		setAutoFillBackground(true);
		setAutoFillBackground(false);
//		setAttribute (Qt::WA_OpaquePaintEvent);
		setAttribute (Qt::WA_NoSystemBackground);

	  setProperty("texmacs_widget", QVariant::fromValue((void*)_wid));  
	};

	
	simple_widget_rep *tm_widget() { 
		QVariant v= property("texmacs_widget");
		return (simple_widget_rep *)(v.canConvert<void *>() ? v.value<void *>() : NULL);
	}
	
private slots:
  void postponedUpdate(QRect r);	
	
protected:	
  virtual void paintEvent ( QPaintEvent * event );
  virtual void keyPressEvent ( QKeyEvent * event );
  virtual void mousePressEvent ( QMouseEvent * event );
  virtual void mouseReleaseEvent ( QMouseEvent * event );
	virtual void mouseMoveEvent ( QMouseEvent * event );

};

#endif