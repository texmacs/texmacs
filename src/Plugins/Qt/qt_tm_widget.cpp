
/******************************************************************************
 * MODULE     : qt_tm_widget.cpp
 * DESCRIPTION: The main TeXmacs input widget and its embedded counterpart.
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
#include "qt_gui.hpp"

#include "qt_dialogues.hpp"
#include "qt_simple_widget.hpp"
#include "qt_window_widget.hpp"
#include "QTMWindow.hpp"
#include "QTMStyle.hpp"      // qtstyle()
#include "QTMGuiHelper.hpp"  // needed to connect()
#include "QTMInteractivePrompt.hpp"
#include "QTMInteractiveInputHelper.hpp"


#ifdef Q_WS_MAC
#define UNIFIED_TOOLBAR
// enable the unified toolbar style on the mac. To work properly this requires
// a modification of the widget hierarchy of the main window.
#endif

int menu_count = 0;
list<qt_tm_widget_rep*> waiting_widgets;

void
replaceActions (QWidget* dest, QWidget* src) {
    //NOTE: the parent hierarchy of the actions is not modified while installing
    //      the menu in the GUI (see qt_menu.hpp for this memory management 
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
  dest->setUpdatesEnabled(false);
  bool visible = dest->isVisible();
  if (visible) dest->hide(); //TRICK: to avoid flicker of the dest widget
  replaceActions (dest, src);
  QList<QObject*> list= dest->children();
  for (int i=0; i<list.count(); i++) {
    QToolButton* button= qobject_cast<QToolButton*> (list[i]);
    if (button) {
      button->setPopupMode (QToolButton::InstantPopup);
      button->setStyle( qtmstyle() );
    }
  }
  if (visible) dest->show(); //TRICK: see above
  dest->setUpdatesEnabled(true);
}

void QTMInteractiveInputHelper::commit(int result) {
  if (wid) {
    if (result == QDialog::Accepted) {
      QString item = "#f";
      QComboBox *cb = sender()->findChild<QComboBox*>("input");
      if (cb) {
        item = cb->currentText();
      }      
      static_cast<qt_input_text_widget_rep*>(wid->int_input.rep)->text= from_qstring (item);
      static_cast<qt_input_text_widget_rep*>(wid->int_input.rep)->cmd ();      
    }
  }
  sender()->deleteLater();
}


/******************************************************************************
* qt_tm_widget_rep
******************************************************************************/

static void
tweak_iconbar_size (QSize& sz) {
  if (sz.height () >= 24) {
    sz.setWidth (sz.width () + 2);
    sz.setHeight (sz.height () + 6);
  }
  else if (sz.height () >= 20) {
    sz.setHeight (sz.height () + 2);
  }
  else if (sz.height () >= 16) {
    sz.setHeight (sz.height () + 2);
  }
}

qt_tm_widget_rep::qt_tm_widget_rep(int mask, command _quit)
 : qt_window_widget_rep (new QTMWindow (0, this), _quit), helper (this), 
   full_screen(false)
{
  type = texmacs_widget;  // FIXME: remove this whole "type" thing

  main_widget = tm_new<qt_simple_widget_rep>();
  
  // decode mask
  visibility[0] = (mask & 1)  == 1;  // header
  visibility[1] = (mask & 2)  == 2;  // main
  visibility[2] = (mask & 4)  == 4;  // mode
  visibility[3] = (mask & 8)  == 8;  // focus
  visibility[4] = (mask & 16) == 16; // user
  visibility[5] = (mask & 32) == 32; // footer
  visibility[6] = (mask & 64) == 64; // side tools #0
  
  // general setup for main window
  
  QMainWindow* mw= mainwindow ();
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
  bar->addWidget (leftLabel, 1);
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
  
  mainToolBar  = new QToolBar ("main toolbar", mw);
  modeToolBar  = new QToolBar ("mode toolbar", mw);
  focusToolBar = new QToolBar ("focus toolbar", mw);
  userToolBar  = new QToolBar ("user toolbar", mw);
  
  sideDock     = new QDockWidget ("Side tools", 0);
    // Wrap the dock in a "virtual" window widget to have clicks report the right position
  dock_window_widget = tm_new<qt_window_widget_rep>(sideDock, command());
  
  mainToolBar->setStyle (qtmstyle ());
  modeToolBar->setStyle (qtmstyle ());
  focusToolBar->setStyle (qtmstyle ());
  userToolBar->setStyle (qtmstyle ());
  sideDock->setStyle(qtmstyle());
  
  {
    // set proper sizes for icons
    QPixmap *pxm = the_qt_renderer()->xpm_image ("tm_new.xpm");
    QSize sz = (pxm ? pxm->size() : QSize(24,24));
    tweak_iconbar_size (sz);
    mainToolBar->setIconSize (sz);
    pxm = the_qt_renderer()->xpm_image ("tm_section.xpm");
    sz = (pxm ? pxm->size() : QSize(20,20));
    tweak_iconbar_size (sz);
    modeToolBar->setIconSize(sz);
    pxm = the_qt_renderer()->xpm_image ("tm_add.xpm");
    sz = (pxm ? pxm->size() : QSize(16,16));
    tweak_iconbar_size (sz);
    focusToolBar->setIconSize(sz);
  }  
  
  QWidget *cw= new QWidget();
  cw->setObjectName("central widget");  // this is important for styling toolbars.
  
    // The main layout
  
  QVBoxLayout *bl = new QVBoxLayout(cw);
  bl->setContentsMargins(0,1,0,0);
  bl->setSpacing(0);
  cw->setLayout(bl);
  bl->addWidget(concrete(main_widget)->as_qwidget());  // force creation of QWidget
  
  mw->setCentralWidget(cw);
  
#ifdef UNIFIED_TOOLBAR

  mw->setUnifiedTitleAndToolBarOnMac(true);
    
  //WARNING: dumbToolBar is the toolbar installed on the top area of the
  //main widget which is  then unified in the title bar. 
  //to overcome some limitations of the unified toolbar implementation we
  //install the real toolbars as widgets in this toolbar.
  
  dumbToolBar = mw->addToolBar("dumb toolbar");
  dumbToolBar->setMinimumHeight(30);

  //these are the actions related to the various toolbars to be installed in
  //the dumb toolbar.
  
  mainToolBarAction = dumbToolBar->addWidget(mainToolBar);
  modeToolBarAction = NULL;

  
  // A ruler
  rulerWidget = new QWidget(cw);
  rulerWidget->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Fixed);
  rulerWidget->setMinimumHeight(1);
  rulerWidget->setBackgroundRole(QPalette::Mid);
  // FIXME: how to use 112 (active) and 146 (passive)
  rulerWidget->setVisible(false);
  rulerWidget->setAutoFillBackground(true);
  // rulerWidget = new QLabel("pippo", cw);
  
    // A second ruler (this one always visible) to separate from the canvas.
  QWidget* r2 = new QWidget(mw);
  r2->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Fixed);
  r2->setMinimumHeight(1);
  r2->setBackgroundRole(QPalette::Mid);
  r2->setVisible(true);
  r2->setAutoFillBackground(true);

  bl->insertWidget(0, modeToolBar);
  bl->insertWidget(1, rulerWidget);
  bl->insertWidget(2, focusToolBar);
  bl->insertWidget(3, userToolBar);
  bl->insertWidget(4, r2);
  
    //mw->setContentsMargins (-2, -2, -2, -2);  // Why this?
  bar->setContentsMargins (0, 1, 0, 1);

#else
  mw->addToolBar (mainToolBar);
  mw->addToolBarBreak ();
  mw->addToolBar (modeToolBar);
  mw->addToolBarBreak ();
  mw->addToolBar (focusToolBar);
  mw->addToolBarBreak ();
  mw->addToolBar (userToolBar);
 // mw->addToolBarBreak ();
#endif

  sideDock->setAllowedAreas(Qt::AllDockWidgetAreas);
  sideDock->setFeatures(QDockWidget::DockWidgetMovable |
                        QDockWidget::DockWidgetFloatable);
  sideDock->setFloating(false);
  mw->addDockWidget(Qt::RightDockWidgetArea, sideDock);
  
    // FIXME? add DockWidgetClosable and connect the close signal
    // to the scheme code
    //  QObject::connect(sideDock, SIGNAL(closeEvent()), 
    //                   someHelper, SLOT(call_scheme_hide_side_tools()));  

  
  // handles visibility
  // at this point all the toolbars are empty so we avoid showing them
  // same for the menu bar if we are not on the Mac (where we do not have
  // other options)
  
  mainToolBar->setVisible (false);
  modeToolBar->setVisible (false);
  focusToolBar->setVisible (false);
  userToolBar->setVisible (false);
  sideDock->setVisible (false);
  mainwindow()->statusBar()->setVisible (true);
#ifndef Q_WS_MAC
  mainwindow()->menuBar()->setVisible (false);
#endif  
}

qt_tm_widget_rep::~qt_tm_widget_rep () {
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::~qt_tm_widget_rep of widget" << type_as_string() << LF;
  
    // clear any residual waiting menu installation
  waiting_widgets = remove(waiting_widgets, this);
}

/*! FIXME: should we overwrite the previous quit command?
 */
widget
qt_tm_widget_rep::plain_window_widget (string title, command quit) {
  (void) quit;
  qwid->setWindowTitle (to_qstring (title));
  return this;
}

void
qt_tm_widget_rep::update_visibility() {
#define XOR(exp1,exp2) (((!exp1) && (exp2)) || ((exp1) && (!exp2)))

  bool old_mainVisibility = mainToolBar->isVisible();
  bool old_modeVisibility = modeToolBar->isVisible();
  bool old_focusVisibility = focusToolBar->isVisible();
  bool old_userVisibility = userToolBar->isVisible();
  bool old_sideVisibility = sideDock->isVisible();
  bool old_statusVisibility = mainwindow()->statusBar()->isVisible();

  bool new_mainVisibility = visibility[1] && visibility[0];
  bool new_modeVisibility = visibility[2] && visibility[0];
  bool new_focusVisibility = visibility[3] && visibility[0];
  bool new_userVisibility = visibility[4] && visibility[0];
  bool new_statusVisibility = visibility[5];
  bool new_sideVisibility = visibility[6];
  
  if ( XOR(old_mainVisibility,  new_mainVisibility) )
    mainToolBar->setVisible (new_mainVisibility);
  if ( XOR(old_modeVisibility,  new_modeVisibility) )
    modeToolBar->setVisible (new_modeVisibility);
  if ( XOR(old_focusVisibility,  new_focusVisibility) )
    focusToolBar->setVisible (new_focusVisibility);
  if ( XOR(old_userVisibility,  new_userVisibility) )
    userToolBar->setVisible (new_userVisibility);
  if ( XOR(old_sideVisibility,  new_sideVisibility) )
    sideDock->setVisible (new_sideVisibility);
  if ( XOR(old_statusVisibility,  new_statusVisibility) )
    mainwindow()->statusBar()->setVisible (new_statusVisibility);

#ifndef Q_WS_MAC
  bool old_menuVisibility = mainwindow()->menuBar()->isVisible();
  bool new_menuVisibility = visibility[0];

  if ( XOR(old_menuVisibility,  new_menuVisibility) )
    mainwindow()->menuBar()->setVisible (new_menuVisibility);
#endif

//#if 0
#ifdef UNIFIED_TOOLBAR

  // do modifications only if needed to reduce flicker
  if ( XOR(old_mainVisibility,  new_mainVisibility) ||
      XOR(old_modeVisibility,  new_modeVisibility) )
  {
    // ensure that the topmost visible toolbar is always unified on Mac
    // (actually only for main and mode toolbars, unifying focus is not
    // appropriate)
    
    QBoxLayout *bl = qobject_cast<QBoxLayout*>(mainwindow()->centralWidget()->layout());
    
    if (modeToolBarAction)
      modeToolBarAction->setVisible(modeToolBar->isVisible());
    mainToolBarAction->setVisible(mainToolBar->isVisible());
    
    //WARNING: jugglying around bugs in Qt unified toolbar implementation
    //do not try to change the order of the following operations....
    
    if (mainToolBar->isVisible()) {       
      bool tmp = modeToolBar->isVisible();
      dumbToolBar->removeAction(modeToolBarAction);
      dumbToolBar->addAction(mainToolBarAction);
      bl->insertWidget(0, rulerWidget);
      bl->insertWidget(0, modeToolBar);
      mainToolBarAction->setVisible(true);
      rulerWidget->setVisible(true);
      modeToolBar->setVisible(tmp);
      if (modeToolBarAction)
        modeToolBarAction->setVisible(tmp);
      dumbToolBar->setVisible(true);
    } else { 
      dumbToolBar->removeAction(mainToolBarAction);
      if (modeToolBar->isVisible()) {
        bl->removeWidget(rulerWidget);
        rulerWidget->setVisible(false);
        bl->removeWidget(modeToolBar);
        if (modeToolBarAction == NULL) {
          modeToolBarAction = dumbToolBar->addWidget(modeToolBar);
        } else {
          dumbToolBar->addAction(modeToolBarAction);
        }
        dumbToolBar->setVisible(true);
      } else {
        dumbToolBar->setVisible(false);
        dumbToolBar->removeAction(modeToolBarAction);
      }
    }
  }
#endif // UNIFIED_TOOLBAR
#undef XOR
}

widget
qt_tm_widget_rep::read(slot s, blackbox index) {
  widget ret;
  
  switch (s) {
    case SLOT_CANVAS:
      check_type_void (index, s);
      ret = main_widget;
      break;
      
    default:
      return qt_window_widget_rep::read(s, index);
  }
  
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::read " << slot_name (s) << "\t\tfor widget\t" 
         << type_as_string() << LF;
  
  return ret;
}

void
qt_tm_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_INVALIDATE:
    case SLOT_INVALIDATE_ALL:
    case SLOT_EXTENTS:
    case SLOT_SCROLL_POSITION:
    case SLOT_SHRINKING_FACTOR:
    case SLOT_MOUSE_GRAB:
      main_widget->send(s, val);
      break;
      
    case SLOT_HEADER_VISIBILITY:
    {
      check_type<bool>(val, s);
      visibility[0] = open_box<bool> (val);
      update_visibility();
    }
      break;
    case SLOT_MAIN_ICONS_VISIBILITY:
    {
      check_type<bool>(val, s);
      visibility[1] = open_box<bool> (val);
      update_visibility();
    }
      break;
    case SLOT_MODE_ICONS_VISIBILITY:
    {
      check_type<bool>(val, s);
      visibility[2] = open_box<bool> (val);
      update_visibility();
    }
      break;
    case SLOT_FOCUS_ICONS_VISIBILITY:
    {
      check_type<bool>(val, s);
      visibility[3] = open_box<bool> (val);
      update_visibility();
    }
      break;
    case SLOT_USER_ICONS_VISIBILITY:
    {
      check_type<bool>(val, s);
      visibility[4] = open_box<bool> (val);
      update_visibility();
    }
      break;

    case SLOT_FOOTER_VISIBILITY:
    {
      check_type<bool>(val, s);
      visibility[5] = open_box<bool> (val);
      update_visibility();
    }
      break;
 
    case SLOT_SIDE_TOOLS_VISIBILITY:
    {
      check_type<bool>(val, s);
      visibility[6] = open_box<bool> (val);
      update_visibility();
    }
      break;

    case SLOT_LEFT_FOOTER:
    {
      check_type<string>(val, s);
      string msg = open_box<string> (val);
      leftLabel->setText (to_qstring (tm_var_encode (msg)));
      leftLabel->update ();
    }
      break;
    case SLOT_RIGHT_FOOTER:
    {
      check_type<string>(val, s);
      string msg= open_box<string> (val);
      rightLabel->setText (to_qstring (tm_var_encode (msg)));
      rightLabel->update ();
    }
      break;
      
    case SLOT_SCROLLBARS_VISIBILITY:
        // ignore this: qt handles scrollbars independently
        //                send_int (THIS, "scrollbars", val);
      break;
      
    case SLOT_INTERACTIVE_MODE:
    {
      check_type<bool>(val, s);

      if (open_box<bool> (val) == true) {
        prompt = new QTMInteractivePrompt (int_prompt, int_input);
        mainwindow()->statusBar()->removeWidget(leftLabel);
        mainwindow()->statusBar()->removeWidget(rightLabel);
        mainwindow()->statusBar()->addWidget(prompt, 1);
        prompt->start();
      } else {
        if (prompt) prompt->end();
        mainwindow()->statusBar()->removeWidget(prompt);
        mainwindow()->statusBar()->addWidget(leftLabel);
        mainwindow()->statusBar()->addPermanentWidget(rightLabel);
        leftLabel->show();
        rightLabel->show();
        delete prompt;
        prompt = NULL;
      }
    }
      break;
      
    case SLOT_FILE:
    {
      check_type<string>(val, s);
      string file = open_box<string> (val);
      if (DEBUG_QT) cout << "File: " << file << LF;
#if (QT_VERSION >= 0x040400)
      mainwindow()->setWindowFilePath(to_qstring(tm_var_encode(file)));
#endif
    }
      break;
      
    case SLOT_DESTROY:
    {  
      ASSERT (is_nil (val), "type mismatch");
      if (!is_nil (quit))
        quit ();
      needs_update ();
    }
      break;
    default:
      qt_window_widget_rep::send (s, val);
      return;
  }
  
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep: sent " << slot_name (s) 
         << "\t\tto widget\t"      << type_as_string() << LF;
}

blackbox
qt_tm_widget_rep::query (slot s, int type_id) {
  if ((DEBUG_QT) && (s != SLOT_RENDERER))
    cout << "qt_tm_widget_rep: queried " << slot_name(s)
         << "\t\tto widget\t" << type_as_string() << LF;
  
  switch (s) {
    case SLOT_SCROLL_POSITION:
    case SLOT_EXTENTS:
    case SLOT_VISIBLE_PART:
    case SLOT_SHRINKING_FACTOR:
      return main_widget->query(s, type_id);

    case SLOT_HEADER_VISIBILITY:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (visibility[0]);
      
    case SLOT_MAIN_ICONS_VISIBILITY:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (visibility[1]);
    
    case SLOT_MODE_ICONS_VISIBILITY:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (visibility[2]);

    case SLOT_FOCUS_ICONS_VISIBILITY:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (visibility[3]);      

    case SLOT_USER_ICONS_VISIBILITY:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (visibility[4]);
      
    case SLOT_FOOTER_VISIBILITY:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (visibility[5]);

    case SLOT_SIDE_TOOLS_VISIBILITY:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (visibility[6]);
      
    case SLOT_INTERACTIVE_INPUT:
    {
      check_type_id<string> (type_id, s);
      qt_input_text_widget_rep* w = 
        static_cast<qt_input_text_widget_rep*>(int_input.rep);
      if (w->ok)
        return close_box<string>(scm_quote(w->text));
      else
        return close_box<string>("#f");
    }

    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
        // Skip title and toolbars
      QPoint pt = QPoint(mainwindow()->geometry().x(), mainwindow()->geometry().y());
        //cout << "wpos: " << pt.x() << ", " << pt.y() << LF;
      return close_box<coord2> (from_qpoint (pt));
    }

    case SLOT_INTERACTIVE_MODE:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (false); // FIXME: who needs this info?
      
    default:
      return qt_window_widget_rep::query (s, type_id);
  }
}

void
qt_tm_widget_rep::install_main_menu () {
  main_menu_widget = waiting_main_menu_widget;
  QMenu* m= concrete (main_menu_widget)->get_qmenu();
  if (m) {
      // REMARK: We do not want the menubar shared across windows as suggested  
      // in http://doc.qt.nokia.com/4.7/qmainwindow.html#menuBar
      // e.g. :
      //
      //     QMenuBar *dest = new QMenuBar(0);
      //     mainwindow()->setMenuBar(dest);
      //
      // as the default behavior on MacOS. The main reason is that in TeXmacs
      // different windows can have different main menus so that it is indeed
      // appropriate to change the main menu as the window focus changes. 
      // So we kindly ask to each window to give us its own menu and we install
      // there our actions.
      // So we do:
    
    QMenuBar *dest = mainwindow()->menuBar();
    
      // and everything is fine.
      // Also please note that we have to do the replacement and not simply 
      // install the menu returned by get_qmenu() since the main menu there 
      // could contain some default items appropriate for the given OS (like the
      // service menu on MacOS) which are not present in our menu widget.
    
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
  }
}

void
qt_tm_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::write " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_SCROLLABLE:
    {
      check_type_void (index, s);

      QLayout* l = centralwidget()->layout();
      l->removeWidget(canvas());
      delete canvas();  // remember: only windows delete QWidgets.
      concrete(main_widget)->qwid = 0;
      main_widget = w;
      concrete(main_widget)->as_qwidget();  // force (re)creation of QWidget 
      if (canvas()) { // if the passed widget wasn't empty... (while switching buffers it is)
        l->addWidget(canvas());
        canvas()->show();
        canvas()->setFocusPolicy(Qt::StrongFocus);
        canvas()->setFocus();
      }
    }
      break;
      
    case SLOT_MAIN_MENU:
      check_type_void (index, s);
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
      check_type_void (index, s);
    {
        //cout << "widget :" << (void*)w.rep << LF;
      main_icons_widget = w;
      QMenu* m= concrete (w)->get_qmenu();
      replaceButtons (mainToolBar, m);
      update_visibility();
    }
      break;
      
    case SLOT_MODE_ICONS:
      check_type_void (index, s);
    {   
      mode_icons_widget = w;
      QMenu* m= concrete (w)->get_qmenu();
      replaceButtons (modeToolBar, m);
      update_visibility();
    }
      break;
      
    case SLOT_FOCUS_ICONS:
      check_type_void (index, s);
    {   
      focus_icons_widget = w;
      QMenu* m= concrete (w)->get_qmenu();
      replaceButtons (focusToolBar, m);
      update_visibility();
    }
      break;
      
    case SLOT_USER_ICONS:
      check_type_void (index, s);
    {   
      user_icons_widget = w;
      QMenu* m= concrete (w)->get_qmenu();
      replaceButtons (userToolBar, m);
      update_visibility();
    }
      break;
      
    case SLOT_SIDE_TOOLS:
      check_type_void (index, s);
    {
      side_tools_widget = w;
      QWidget* new_qwidget = concrete (w)->as_qwidget();
      QWidget* old_qwidget = sideDock->widget();
      delete old_qwidget;
      sideDock->setWidget (new_qwidget); 
      update_visibility();
      new_qwidget->show();
    }
      break;
      
    case SLOT_INTERACTIVE_PROMPT:
      check_type_void (index, s);
      int_prompt= concrete (w);
      break;
      
    case SLOT_INTERACTIVE_INPUT:
      check_type_void (index, s);
      int_input= concrete (w);
      break;

    default:
      qt_window_widget_rep::write (s, index, w);
  }
}

void
qt_tm_widget_rep::set_full_screen(bool flag) {
  full_screen = flag;
  QWidget *win = mainwindow()->window();  
  if (win) {
    if (flag ) {
      // remove the borders from some widgets
      scrollarea()->setFrameShape(QFrame::NoFrame);
#ifdef UNIFIED_TOOLBAR
      //HACK: we disable unified toolbar since otherwise
      //  the application will crash when we return in normal mode
      // (bug in Qt? present at least with 4.7.1)
      mainwindow()->setUnifiedTitleAndToolBarOnMac(false);
      mainwindow()->centralWidget()->layout()->setContentsMargins(0,0,0,0);
#endif
//      mainwindow()->window()->setContentsMargins(0,0,0,0);
      //win->showFullScreen();
       win->setWindowState(win->windowState() ^ Qt::WindowFullScreen);
    } else {
      bool cache = visibility[0];
      visibility[0] = false;
      update_visibility();
//      win->showNormal();
      win->setWindowState(win->windowState() ^ Qt::WindowFullScreen);

      visibility[0] = cache;
      update_visibility();
      // reset the borders of some widgets
      scrollarea()->setFrameShape(QFrame::Box);
#ifdef UNIFIED_TOOLBAR
      mainwindow()->centralWidget()->layout()->setContentsMargins(2,2,2,2);
      //HACK: we reenable unified toolbar (see above HACK) 
      //  the application will crash return in normal mode
      mainwindow()->setUnifiedTitleAndToolBarOnMac(true);
#endif
    }
  }
  
  scrollarea()->setHorizontalScrollBarPolicy(flag ? Qt::ScrollBarAlwaysOff : Qt::ScrollBarAsNeeded);
  scrollarea()->setVerticalScrollBarPolicy(flag ? Qt::ScrollBarAlwaysOff : Qt::ScrollBarAsNeeded);
}


/******************************************************************************
 * qt_tm_embedded_widget_rep
 * FIXME: this might make more sense as a child of qt_simple_widget_rep
 ******************************************************************************/

qt_tm_embedded_widget_rep::qt_tm_embedded_widget_rep (command _quit) 
  : qt_view_widget_rep(new QTMWidget(0, 0), embedded_tm_widget), quit(_quit) {
      //static_cast<QTMWidget*>(qwid)->set_tm_widget(this);
}

qt_tm_embedded_widget_rep::~qt_tm_embedded_widget_rep () { }

void
qt_tm_embedded_widget_rep::send (slot s, blackbox val) {

  switch (s) {
    case SLOT_DESTROY:
    {  
      ASSERT (is_nil (val), "type mismatch");
      if (!is_nil (quit))
        quit ();
      needs_update ();
    }
      break;
      
    default:
      qt_view_widget_rep::send (s, val);
      return;
  }
  if (DEBUG_QT)
    cout << "qt_tm_embedded_widget_rep: sent " << slot_name (s) 
         << "\t\tto widget\t" << type_as_string() << LF;  
}

blackbox
qt_tm_embedded_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_tm_embedded_widget_rep::query " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_HEADER_VISIBILITY:
    case SLOT_MAIN_ICONS_VISIBILITY:
    case SLOT_MODE_ICONS_VISIBILITY:
    case SLOT_FOCUS_ICONS_VISIBILITY:
    case SLOT_USER_ICONS_VISIBILITY:
    case SLOT_FOOTER_VISIBILITY:
    case SLOT_SIDE_TOOLS_VISIBILITY:
        // FIXME: decide what to do with all these for embedded widgets.      
      check_type_id<bool> (type_id, s);
      return close_box<bool> (false);

    default:
      return qt_view_widget_rep::query (s, type_id);
  }
}

widget
qt_tm_embedded_widget_rep::read(slot s, blackbox index) {
  widget ret;
  
  switch (s) {
    case SLOT_CANVAS:
      check_type_void (index, s);
      ret = widget(this);
      break;
    default:
      return qt_view_widget_rep::read(s, index);
  }
  
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::read " << slot_name (s) 
         << "\t\tfor widget\t" << type_as_string() << LF;
  
  return ret;
}

void
qt_tm_embedded_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_QT)
    cout << "qt_tm_embedded_widget_rep::write " << slot_name (s) << LF;

  switch (s) {
        // Widget w is a qt_simple_widget_rep, with a QTMWidget as underlying
        // widget. We must discard the current QTMWidget and display the new.
        // Also, because upon construction we created a dummy QTMWidget without
        // an owning tm-widget, we set it here.
    case SLOT_SCROLLABLE:
    {
      check_type_void (index, s);
      qt_simple_widget_rep* wid = static_cast<qt_simple_widget_rep*>(w.rep);
      QTMWidget* new_widget     = static_cast<QTMWidget*>(wid->as_qwidget());
      if (new_widget) {
        delete canvas();
        qwid = 0;
        new_widget->setFocusPolicy (Qt::StrongFocus);
        new_widget->setFocus ();
        qwid = new_widget;
      } else {
        FAILED("Attempt to set an invalid scrollable widget for a qt_tm_embedded_widget");
      }
    }
      break;
      
    case SLOT_MAIN_MENU:
    case SLOT_MAIN_ICONS:
    case SLOT_MODE_ICONS:
    case SLOT_FOCUS_ICONS:
    case SLOT_USER_ICONS:
    case SLOT_SIDE_TOOLS:
    case SLOT_INTERACTIVE_INPUT:
        // FIXME: decide what to do with these for embedded widgets.
    default:
      qt_view_widget_rep::write (s, index, w);
  }
}
