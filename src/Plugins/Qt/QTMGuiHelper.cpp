
/******************************************************************************
 * MODULE     : QTMGuiHelper.cpp
 * DESCRIPTION: QT Gui helper class. Infrastructure for delayed menu installation 
 * COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMGuiHelper.hpp"
#include "qt_tm_widget.hpp"
#include "scheme.hpp"

void
QTMGuiHelper::aboutToShowMainMenu() {
    //cout << "Show :" << menu_count << LF;
  menu_count++;
}

void 
QTMGuiHelper::aboutToHideMainMenu() {
  menu_count--;
    //cout << "Hide :" << menu_count << " " << N(waiting_widgets) <<  LF;
  if (menu_count <= 0) {
    menu_count = 0;
    QTimer::singleShot (0, the_gui->gui_helper, SLOT (doPopWaitingWidgets ()));
  }
}

void 
QTMGuiHelper::doPopWaitingWidgets() {
  if (!is_nil(waiting_widgets)) {
    if (DEBUG_QT)
      cout << "Installing postponed menu" << LF;
    waiting_widgets->item->install_main_menu();
    waiting_widgets = waiting_widgets->next;
  }
}

void
QTMGuiHelper::emitTmSlotRefresh () {
  emit tmSlotRefresh();
}


/******************************************************************************
 * QTMRefreshWidget
 ******************************************************************************/

QTMRefreshWidget::QTMRefreshWidget (string _tmwid)
: QWidget (), tmwid (_tmwid), curobj (false), cur (), cache (widget ()) 
{   
  QObject::connect(the_gui->gui_helper, SIGNAL(tmSlotRefresh()), 
                   this, SLOT(doRefresh()));
  doRefresh();
}


widget make_menu_widget (object wid);


bool
QTMRefreshWidget::recompute () {
  string s= "'(vertical (link " * tmwid * "))";
  eval ("(lazy-initialize-force)");
  //cout << "Recompute " << tmwid << "\n";
  object xwid= call ("menu-expand", eval (s));
  //cout << "xwid= " << xwid << "\n";
  if (cache->contains (xwid)) {
    //if (curobj == xwid) cout << "Same " << s << "\n";
    if (curobj == xwid) return false;
    curobj= xwid;
    cur   = cache [xwid];
    //cout << "Cached " << s << "\n";
    return true;
  }
  else {
    curobj= xwid;
    //cout << "Compute " << s << "\n";
    object uwid= eval (s);
    //cout << "uwid= " << uwid << "\n";
    cur= make_menu_widget (uwid);
    //cout << "cur= " << cur << "\n";
    cache (xwid)= cur;
    return true;
  }
}

void 
QTMRefreshWidget::doRefresh() {
  if (recompute()) {
    if (layout()) {
      QLayoutItem* item;
      while ((item = layout()->takeAt(0)) != 0) {		
        if (item->widget()) {
          layout()->removeWidget(item->widget());
          delete item->widget();
        }	
        delete item;
      }
      delete layout();
    }
    setLayout(concrete(cur)->as_qlayoutitem()->layout());
  }
}
