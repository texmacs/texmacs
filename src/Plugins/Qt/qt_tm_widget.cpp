
/******************************************************************************
 * MODULE     : qt_tm_widget.cpp
 * DESCRIPTION: The main TeXmacs widget for the Qt GUI
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include <QtGui>

#include "analyze.hpp"

#include "qt_tm_widget.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"
#include "qt_menu.hpp"    // to_qmenu()
#include "qt_gui.hpp"

#include "qt_basic_widgets.hpp"
#include "qt_simple_widget.hpp"
#include "qt_window_widget.hpp"
#include "QTMWindow.hpp"
#include "QTMStyle.hpp"      // qtstyle()
#include "QTMGuiHelper.hpp"  // needed to connect()
#include "QTMInteractivePrompt.hpp"
#include "QTMInteractiveInputHelper.hpp"

int menu_count = 0;
list<qt_tm_widget_rep*> waiting_widgets;

void
replaceActions (QWidget* dest, QWidget* src) {
    //NOTE: the parent hierarchy of the actions is not modified while installing
    //      the menu in the GUI (see qt_menu.cpp for this memory management 
    //      policy)
  dest->setUpdatesEnabled(false);
  QList<QAction *> list = dest->actions();
  while (!list.isEmpty()) {
    QAction* a= list.takeFirst();
    dest->removeAction (a);
  }
  list = src->actions();
  while (!list.isEmpty()) {
    QAction* a= list.takeFirst();
    dest->addAction (a);
  }
  dest->setUpdatesEnabled(true);
}

void
replaceButtons(QToolBar* dest, QWidget* src) {
  replaceActions (dest, src);
  dest->setUpdatesEnabled(false);
  QList<QObject*> list= dest->children();
  for (int i=0; i<list.count(); i++) {
    QToolButton* button= qobject_cast<QToolButton*> (list[i]);
    if (button) {
      button->setPopupMode (QToolButton::InstantPopup);
      button->setStyle( qtmstyle() );
    }
  }
  dest->setUpdatesEnabled(true);
}

void QTMInteractiveInputHelper::doit() {
  wid->do_interactive_prompt();
}

/******************************************************************************
 * qt_tm_widget_rep
 ******************************************************************************/


qt_tm_widget_rep::qt_tm_widget_rep(int mask, command _quit):
qt_view_widget_rep (new QTMWindow (this)), helper (this), quit(_quit)
{
  // decode mask
  visibility[0] = (mask & 1)  == 1;  // header
  visibility[1] = (mask & 2)  == 2;  // main
  visibility[2] = (mask & 4)  == 4;  // mode
  visibility[3] = (mask & 8)  == 8;  // focus
  visibility[4] = (mask & 16) == 16; // user
  visibility[5] = (mask & 32) == 32; // footer
  
  // general setup for main window
  
  QMainWindow* mw= tm_mainwindow ();
  mw->setStyle (qtmstyle ());
  mw->menuBar()->setStyle (qtmstyle ()); 

  
  // there is a bug in the early implementation of toolbars in Qt 4.6
  // which has been fixed in 4.6.2 (at least)
  // this is why we change dimension of icons
	
#if (defined(Q_WS_MAC)&&(QT_VERSION>=QT_VERSION_CHECK(4,6,0))&&(QT_VERSION<QT_VERSION_CHECK(4,6,2)))
  mw->setIconSize (QSize (22, 30));  
#else
  mw->setIconSize (QSize (17, 17));
#endif
  mw->setFocusPolicy (Qt::NoFocus);
  
  
  // central widget
  
  QStackedWidget* tw = new QStackedWidget ();
  tw->setObjectName("stacked widget"); // to easily find this object

  // status bar
  
  QStatusBar* bar= new QStatusBar(mw);
  leftLabel= new QLabel ("Welcome to TeXmacs", mw);
  rightLabel= new QLabel ("Booting", mw);
  leftLabel->setFrameStyle (QFrame::NoFrame);
  rightLabel->setFrameStyle (QFrame::NoFrame);
  {
    QFont f=  leftLabel->font();
    f.setPixelSize(12);
    leftLabel->setFont(f);
    rightLabel->setFont(f);
  }
  bar->addWidget (leftLabel);
  bar->addPermanentWidget (rightLabel);
  bar->setStyle (qtmstyle ());
  
  // NOTE (mg): the following setMinimumWidth command disable automatic 
  // enlarging of the status bar and consequently of the main window due to 
  // long messages in the left label. I found this strange solution here
  // http://www.archivum.info/qt-interest@trolltech.com/2007-05/01453/Re:-QStatusBar-size.html
  // The solution if due to Martin Petricek. He adds:
  //    The docs says: If minimumSize() is set, the minimum size hint will be ignored.
  //    Probably the minimum size hint was size of the lengthy message and
  //    internal layout was enlarging the satusbar and the main window
  //    Maybe the notice about QLayout that is at minimumSizeHint should be
  //    also at minimumSize, didn't notice it first time and spend lot of time
  //    trying to figure this out :)
  
  bar->setMinimumWidth(2);
  mw->setStatusBar (bar);
  
  
  // toolbars
  

#ifdef Q_WS_MAC

  QWidget *cw= new QWidget ();
  QBoxLayout *bl = new QBoxLayout(QBoxLayout::TopToBottom, cw);
  bl->setContentsMargins(2,2,2,2);
  bl->setSpacing(0);
  cw->setLayout(bl);
  bl->addWidget(tw);
  
  mw->setCentralWidget(cw);
  
  mw->setUnifiedTitleAndToolBarOnMac(true);
  mainToolBar= mw->addToolBar ("main toolbar");
  modeToolBar = new QToolBar("mode toolbar");
  bl->insertWidget(0, modeToolBar);
  focusToolBar = new QToolBar("focus toolbar");
  bl->insertWidget(1, focusToolBar);
  userToolBar = new QToolBar("user toolbar");
  bl->insertWidget(2, userToolBar);
  
#else
  mw->setCentralWidget(tw);
  
  mainToolBar= mw->addToolBar ("main toolbar");
  mw->addToolBarBreak ();
  modeToolBar= mw->addToolBar ("mode toolbar");
  mw->addToolBarBreak ();
  focusToolBar= mw->addToolBar ("focus toolbar");
  mw->addToolBarBreak ();
  userToolBar= mw->addToolBar ("user toolbar");
  mw->addToolBarBreak ();
  
#endif
  
#if 0
  QScrollArea* sa= new QTMScrollArea (this);
  sa->setParent (mw);
  sa->setBackgroundRole (QPalette::Dark);
  sa->setAlignment (Qt::AlignCenter);
  sa->setFocusPolicy (Qt::NoFocus);
  
  mw->setCentralWidget (sa);
#endif
  
  
//  mainToolBar->setStyle (qtmstyle ());
  modeToolBar->setStyle (qtmstyle ());
  focusToolBar->setStyle (qtmstyle ());
  userToolBar->setStyle (qtmstyle ());
  
  focusToolBar->setIconSize(QSize(14,14));
  
  updateVisibility();
	
  
    // handles visibility
    // at this point all the toolbars are empty so we avoid showing them
    // same for the menu bar if we are not on the Mac (where we do not have
    // other options)
  
  mainToolBar->setVisible (false);
  modeToolBar->setVisible (false);
  focusToolBar->setVisible (false);
  userToolBar->setVisible (false);
  tm_mainwindow()->statusBar()->setVisible (true);
#ifndef Q_WS_MAC
  tm_mainwindow()->menuBar()->setVisible (false);
#endif  
}

qt_tm_widget_rep::~qt_tm_widget_rep () {
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::~qt_tm_widget_rep\n";
  
  
    // clear any residual waiting menu installation
  waiting_widgets = remove(waiting_widgets, this);
  
    // we must detach the QTMWidget canvas from the Qt widget hierarchy otherwise
    // it will be destroyed when the view member of this object is deallocated
    // this is another problem related to our choice of letting qt_widget own its
    // underlying QWidget.
  
  QTMWidget *canvas = tm_canvas();
  QStackedWidget* tw= tm_centralwidget();
  if (canvas) {
    tw->removeWidget(canvas);
    canvas->setParent(NULL);
    QTMWidget::all_widgets.remove(canvas);
  }
}

void qt_tm_widget_rep::updateVisibility()
{
  mainToolBar->setVisible (visibility[1] && visibility[0]);
  modeToolBar->setVisible (visibility[2] && visibility[0]);
  focusToolBar->setVisible (visibility[3] && visibility[0]);
  userToolBar->setVisible (visibility[4] && visibility[0]);
  tm_mainwindow()->statusBar()->setVisible (visibility[5]);
  tm_mainwindow()->menuBar()->setVisible (visibility[0]);
}


void
qt_tm_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::send " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_INVALIDATE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      if (DEBUG_QT)
        cout << "Invalidating rect " << rectangle(p.x1,p.x2,p.x3,p.x4) << LF;
      qt_renderer_rep* ren = (qt_renderer_rep*)get_renderer (this);
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (ren && canvas) {
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;    
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        canvas->invalidate_rect (x1,y2,x2,y1);
      }
    }
      break;
    case SLOT_INVALIDATE_ALL:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (DEBUG_QT)
        cout << "Invalidating all"<<  LF;
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (canvas) canvas->invalidate_all ();
    }
      break;
      
    case SLOT_EXTENTS:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      QRect rect = to_qrect (p);
        //NOTE: rect.topLeft is ignored since it is always (0,0)
      tm_canvas() -> setExtents(rect);
#if 0
        //cout << "p= " << p << "\n";
      QSize sz= to_qrect (p).size ();
      QSize ws= tm_scrollarea () -> size ();
      sz.setHeight (max (sz.height (), ws.height () - 4));
        //FIXME: the above adjustment is not very nice and useful only in papyrus 
        //       mode. When setting the size we should ask the GUI of some 
        //       preferred max size and set that without post-processing.
        //      tm_canvas () -> setFixedSize (sz);
      tm_canvas() -> setExtentsSize(sz);
#endif
    }
      break;
    case SLOT_HEADER_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[0] = f;
      updateVisibility();
    }
      break;
    case SLOT_MAIN_ICONS_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[1] = f;
      updateVisibility();
    }
      break;
    case SLOT_MODE_ICONS_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[2] = f;
      updateVisibility();
    }
      break;
    case SLOT_FOCUS_ICONS_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[3] = f;
      updateVisibility();
    }
      break;
    case SLOT_USER_ICONS_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[4] = f;
      updateVisibility();
    }
      break;
    case SLOT_FOOTER_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[5] = f;
      updateVisibility();
    }
      break;
      
    case SLOT_LEFT_FOOTER:
    {
      TYPE_CHECK (type_box (val) == type_helper<string>::id);
      string msg= open_box<string> (val);
      leftLabel->setText (to_qstring (tm_var_encode (msg)));
      leftLabel->update ();
    }
      break;
    case SLOT_RIGHT_FOOTER:
    {
      TYPE_CHECK (type_box (val) == type_helper<string>::id);
      string msg= open_box<string> (val);
      rightLabel->setText (to_qstring (tm_var_encode (msg)));
      rightLabel->update ();
    }
      break;
      
    case SLOT_SCROLL_POSITION:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      QPoint pt= to_qpoint (p);
      if (DEBUG_QT)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n ";
      tm_scrollarea()->setOrigin(pt);
    }
      break;
      
    case SLOT_SCROLLBARS_VISIBILITY:
        // ignore this: qt handles scrollbars independently
        //                send_int (THIS, "scrollbars", val);
      break;
      
    case SLOT_INTERACTIVE_MODE:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      if (open_box<bool> (val) == true) {
        QTimer::singleShot (0, &helper, SLOT (doit ()));
          // do_interactive_prompt ();
      }
    }
      break;
      
    case SLOT_SHRINKING_FACTOR:
      TYPE_CHECK (type_box (val) == type_helper<int>::id);
      if (QTMWidget* tmw= qobject_cast<QTMWidget*> (tm_canvas())) {
        int new_sf = open_box<int> (val);
        if (DEBUG_QT) cout << "New shrinking factor :" << new_sf << LF;
        tmw->tm_widget()->handle_set_shrinking_factor (new_sf);
      }
      break;
      
    case SLOT_FILE:
    {
      TYPE_CHECK (type_box (val) == type_helper<string>::id);
      string file = open_box<string> (val);
      if (DEBUG_QT) cout << "File: " << file << LF;
#if (QT_VERSION >= 0x040400)
      view->window()->setWindowFilePath(to_qstring(file));
#endif
    }
      break;
      
      
    default:
      qt_view_widget_rep::send (s, val);
  }
}


blackbox
qt_tm_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::query " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_SCROLL_POSITION:
    {
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt= tm_canvas()->origin;
      if (DEBUG_QT)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }
      
    case SLOT_EXTENTS:
    {
      TYPE_CHECK (type_id == type_helper<coord4>::id);
      QRect rect= tm_canvas()->extents;
      coord4 c= from_qrect (rect);
      if (DEBUG_QT) cout << "Canvas geometry " << rect << LF;
      return close_box<coord4> (c);
    }
      
    case SLOT_VISIBLE_PART:
    {
      TYPE_CHECK (type_id == type_helper<coord4>::id);
      QSize sz = tm_canvas()->QAbstractScrollArea::viewport()->size();
        //sz.setWidth(sz.width()-2);
      QPoint pos = tm_canvas()->backing_pos;
      coord4 c = from_qrect(QRect(pos,sz));
      if (DEBUG_QT) 
        cout << "Visible Region " << c << LF;
      return close_box<coord4> (c);
    }
      
    case SLOT_USER_ICONS_VISIBILITY:
      TYPE_CHECK (type_id == type_helper<bool>::id);
      return close_box<bool> (visibility[4]);
      
    case SLOT_FOCUS_ICONS_VISIBILITY:
      TYPE_CHECK (type_id == type_helper<bool>::id);
      return close_box<bool> (visibility[3]);
      
    case SLOT_MODE_ICONS_VISIBILITY:
      TYPE_CHECK (type_id == type_helper<bool>::id);
      return close_box<bool> (visibility[2]);
      
    case SLOT_MAIN_ICONS_VISIBILITY:
      TYPE_CHECK (type_id == type_helper<bool>::id);
      return close_box<bool> (visibility[1]);
      
    case SLOT_HEADER_VISIBILITY:
      TYPE_CHECK (type_id == type_helper<bool>::id);
      return close_box<bool> (visibility[0]);
      
    case SLOT_FOOTER_VISIBILITY:
      TYPE_CHECK (type_id == type_helper<bool>::id);
      return close_box<bool> (visibility[5]);
      
    case SLOT_INTERACTIVE_INPUT:
      TYPE_CHECK (type_id == type_helper<string>::id);
      return close_box<string>
      (((qt_input_text_widget_rep*) int_input.rep) -> text);
        // return close_box<string> ("FIXME");
      
    case SLOT_INTERACTIVE_MODE:
      TYPE_CHECK (type_id == type_helper<bool>::id);
      return close_box<bool> (false); // FIXME: who needs this info?
      
    default:
      return qt_view_widget_rep::query (s, type_id);
  }
}

widget
qt_tm_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT) cout << "[qt_tm_widget_rep] ";
  return qt_view_widget_rep::read (s, index);
}


void
qt_tm_widget_rep::install_main_menu () {
  widget tmp = main_menu_widget;
  main_menu_widget = waiting_main_menu_widget;
  QMenu* m= to_qmenu (main_menu_widget);
  if (m) {
    {
      QMenuBar *dest = tm_mainwindow()->menuBar();
      QWidget *src = m;
      replaceActions(dest,src);
      QList<QAction*> list = dest->actions();
      for (int i= 0; i < list.count(); i++) {
        QAction* a= list[i];
        if (a->menu()) {
          QObject::connect(a->menu(), SIGNAL(aboutToShow()),
                           the_gui->gui_helper, SLOT(aboutToShowMainMenu()));
          QObject::connect(a->menu(), SIGNAL(aboutToHide()),
                           the_gui->gui_helper, SLOT(aboutToHideMainMenu()));
        }
      }
        //      tm_mainwindow()->menuBar()->macUpdateMenuBar();
    }
    updateVisibility();
  }
}

void
qt_tm_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::write " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_CANVAS:
    {
      check_type_void (index, "SLOT_CANVAS");
      QTMWidget* new_canvas= qobject_cast<QTMWidget*>(((qt_view_widget_rep*) w.rep)->view);
      QTMWidget* old_canvas= tm_canvas();
      QStackedWidget* tw= tm_centralwidget();
      if (new_canvas && (new_canvas != old_canvas) ) {
        tw->addWidget(new_canvas);
        tw->removeWidget(old_canvas);
        QTMWidget::all_widgets.insert(new_canvas);
        if (old_canvas) {
          old_canvas->setParent(NULL);
          QTMWidget::all_widgets.remove(old_canvas);
        }
        new_canvas->setFocusPolicy (Qt::StrongFocus);
        new_canvas->setFocus ();
        
      }
    }
      break;
      
    case SLOT_MAIN_MENU:
      check_type_void (index, "SLOT_MAIN_MENU");
    {
      waiting_main_menu_widget = w;
      if (menu_count <=0) {
        install_main_menu();
      } else { 
          // menu interaction ongoing.
          // postpone menu installation when the menu interaction is done
        if (DEBUG_QT)
          cout << "Main menu is busy: postponing menu installation" << LF;
        if (!contains(waiting_widgets,this))
          waiting_widgets << this;
      }
    }
      break;
      
    case SLOT_MAIN_ICONS:
      check_type_void (index, "SLOT_MAIN_ICONS");
    {
        //cout << "widget :" << (void*)w.rep << LF;
      main_icons_widget = w;
      QMenu* m= to_qmenu (w);
      replaceButtons (mainToolBar, m);
      updateVisibility();
    }
      break;
      
    case SLOT_MODE_ICONS:
      check_type_void (index, "SLOT_MODE_ICONS");
    {   
      mode_icons_widget = w;
      QMenu* m= to_qmenu (w);
      replaceButtons (modeToolBar, m);
      updateVisibility();
    }
      break;
      
    case SLOT_FOCUS_ICONS:
      check_type_void (index, "SLOT_FOCUS_ICONS");
    {   
      focus_icons_widget = w;
      QMenu* m= to_qmenu (w);
      replaceButtons (focusToolBar, m);
      updateVisibility();
    }
      break;
      
    case SLOT_USER_ICONS:
      check_type_void (index, "SLOT_USER_ICONS");
    {   
      user_icons_widget = w;
      QMenu* m= to_qmenu (w);
      replaceButtons (userToolBar, m);
      updateVisibility();
    }
      break;
      
    case SLOT_INTERACTIVE_PROMPT:
      check_type_void (index, "SLOT_INTERACTIVE_PROMPT");
      int_prompt= concrete (w);
      break;
      
    case SLOT_INTERACTIVE_INPUT:
      check_type_void (index, "SLOT_INTERACTIVE_INPUT");
      int_input= concrete (w);
      break;
      
    default:
      qt_view_widget_rep::write (s, index, w);
  }
}

widget
qt_tm_widget_rep::plain_window_widget (string s) {
    // creates a decorated window with name s and contents w
  widget w= qt_view_widget_rep::plain_window_widget (s);
    // to manage correctly retain counts
  qt_window_widget_rep* wid= (qt_window_widget_rep*) (w.rep);
  return wid;
}

#if 0
void
qt_tm_widget_rep::do_interactive_prompt () {
  QStringList items;
  QString label= to_qstring (((qt_text_widget_rep*) int_prompt.rep)->str);
  qt_input_text_widget_rep* it = (qt_input_text_widget_rep*) (int_input.rep);
  for (int j=0; j < N(it->def); j++)
    items << to_qstring(it->def[j]);
  bool ok;
  QString item =
  QInputDialog::getItem (NULL, "Interactive Prompt", label,
                         items, 0, true, &ok );
  if (ok && !item.isEmpty()) {
    ((qt_input_text_widget_rep*) int_input.rep) -> text=
    scm_quote (from_qstring (item));
    ((qt_input_text_widget_rep*) int_input.rep) -> cmd ();
  }
}
#elif 1
void
qt_tm_widget_rep::do_interactive_prompt () {
  QStringList items;
  QString label= to_qstring (tm_var_encode (((qt_text_widget_rep*) int_prompt.rep)->str));
  qt_input_text_widget_rep* it = (qt_input_text_widget_rep*) (int_input.rep);
  if ( N(it->def) == 0) {
    items << "";
  } else {
    for (int j=0; j < N(it->def); j++) {
      items << to_qstring(it->def[j]);
    }
  }
  QDialog d (0, Qt::Sheet);
  QVBoxLayout* vl = new QVBoxLayout(&d);
  
  QHBoxLayout *hl = new QHBoxLayout();
  
  QLabel *lab = new QLabel (label,&d);
  QComboBox *cb = new QComboBox(&d);
  cb -> setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLength);
  cb -> setEditText (items[0]);
  int minlen = 0;
  for(int j=0; j < items.count(); j++) {
    cb -> addItem (items[j]);
    int c = items[j].count();
    if (c > minlen) minlen = c;
  }
  cb -> setMinimumContentsLength (minlen>50 ? 50 : (minlen < 2 ? 10 : minlen));
  cb -> setEditable (true);
    // apparently the following flag prevents Qt from substituting an history item
    // for an input when they differ only from the point of view of case (upper/lower)
    // eg. if the history contains aAAAAa and you type AAAAAA then the combo box
    // will retain the string aAAAAa
  cb->setDuplicatesEnabled(true); 
  cb->completer()->setCaseSensitivity(Qt::CaseSensitive);
  
  lab -> setBuddy (cb);
  hl -> addWidget (lab);
  hl -> addWidget (cb);
  vl -> addLayout (hl);
  
  if (ends (it->type, "file") || it->type == "directory") {
      // autocompletion
    QCompleter *completer = new QCompleter(&d);
    QDirModel *dirModel = new QDirModel(&d);
    completer->setModel(dirModel);
    cb->setCompleter(completer);
  }
  
  {
    QDialogButtonBox* buttonBox =
    new QDialogButtonBox (QDialogButtonBox::Ok | QDialogButtonBox::Cancel,
                          Qt::Horizontal, &d);
    QObject::connect (buttonBox, SIGNAL (accepted()), &d, SLOT (accept()));
    QObject::connect (buttonBox, SIGNAL (rejected()), &d, SLOT (reject()));
    vl -> addWidget (buttonBox);
  }
    //  d.setLayout (vl);
  
  QRect wframe = view->window()->frameGeometry();
  QPoint pos = QPoint(wframe.x()+wframe.width()/2,wframe.y()+wframe.height()/2);
  
  d.setWindowTitle("Interactive Prompt");
  d.updateGeometry();
  QSize sz = d.sizeHint();
  QRect r; r.setSize(sz);
  r.moveCenter(pos);
  d.setGeometry(r);
  
  
  int result = d.exec ();
  if (result == QDialog::Accepted) {
    QString item = cb->currentText();
    ((qt_input_text_widget_rep*) int_input.rep) -> text=
    scm_quote (from_qstring (item));
    ((qt_input_text_widget_rep*) int_input.rep) -> cmd ();
  } else {
      //    ((qt_input_text_widget_rep*) int_input.rep) -> text="#f";
  }
}
#else

void
qt_tm_widget_rep::do_interactive_prompt () {
	QString label = to_qstring (tm_var_encode (((qt_text_widget_rep*) int_prompt.rep)->str));
	QStringList items;
  qt_input_text_widget_rep* it = (qt_input_text_widget_rep*) (int_input.rep);
  if ( N(it->def) == 0)
		items << "";
  else for (int j=0; j < N(it->def); j++)
		items << to_qstring(it->def[j]);
  
	QTMInteractivePrompt _prompt(label, items, to_qstring(it->type), tm_mainwindow());
	
	if (_prompt.exec() == QDialog::Accepted) {
		QString text = _prompt.currentText();
    ((qt_input_text_widget_rep*) int_input.rep) -> text = scm_quote (from_qstring (text));
    ((qt_input_text_widget_rep*) int_input.rep) -> cmd ();
  } else {
      //    ((qt_input_text_widget_rep*) int_input.rep) -> text="#f";
  }
}

#endif