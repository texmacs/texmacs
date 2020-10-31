
/******************************************************************************
 * MODULE     : qt_tm_widget.cpp
 * DESCRIPTION: The main TeXmacs input widget and its embedded counterpart.
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include <QToolBar>
#include <QToolButton>
#include <QDialog>
#include <QComboBox>
#include <QStatusBar>
#include <QDockWidget>
#include <QMainWindow>
#include <QMenuBar>
#include <QLayoutItem>

#include "config.h"
#include "analyze.hpp"
#include "scheme.hpp"

#include "qt_tm_widget.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"
#include "qt_gui.hpp"
#include "qt_picture.hpp"

#include "qt_dialogues.hpp"
#include "qt_simple_widget.hpp"
#include "qt_window_widget.hpp"
#include "qt_menu.hpp"
#include "QTMWindow.hpp"
#include "QTMStyle.hpp"      // qtstyle()
#include "QTMGuiHelper.hpp"  // needed to connect()
#include "QTMInteractivePrompt.hpp"
#include "QTMInteractiveInputHelper.hpp"

int menu_count = 0;  // zero if no menu is currently being displayed
list<qt_tm_widget_rep*> waiting_widgets;

static void
replaceActions (QWidget* dest,  QList<QAction*>* src) {
  //NOTE: the parent hierarchy of the actions is not modified while installing
  //      the menu in the GUI (see qt_menu.hpp for this memory management
  //      policy)
  if (src == NULL || dest == NULL)
    FAILED ("replaceActions expects valid objects");
  dest->setUpdatesEnabled (false);
  QList<QAction *> list = dest->actions();
  for (int i = 0; i < list.count(); i++) {
    QAction* a = list[i];
    dest->removeAction (a);
  }
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    dest->addAction(a);
  }
  dest->setUpdatesEnabled (true);
}

static void
replaceButtons (QToolBar* dest, QList<QAction*>* src) {
  if (src == NULL || dest == NULL)
    FAILED ("replaceButtons expects valid objects");
  dest->setUpdatesEnabled (false);
  bool visible = dest->isVisible();
  if (visible) dest->hide(); //TRICK: to avoid flicker of the dest widget
  replaceActions (dest, src);
  QList<QObject*> list = dest->children();
  for (int i = 0; i < list.count(); ++i) {
    QToolButton* button = qobject_cast<QToolButton*> (list[i]);
    if (button) {
      button->setPopupMode (QToolButton::InstantPopup);
      button->setStyle (qtmstyle());
    }
  }
  if (visible) dest->show(); //TRICK: see above
  dest->setUpdatesEnabled (true);
}

void
QTMInteractiveInputHelper::commit (int result) {
  if (wid && result == QDialog::Accepted) {
    QString  item = "#f";
    QComboBox* cb = sender()->findChild<QComboBox*> ("input");
    if (cb)  item = cb->currentText();
    static_cast<qt_input_text_widget_rep*>(wid->int_input.rep)->input =
      from_qstring (item);
    static_cast<qt_input_text_widget_rep*>(wid->int_input.rep)->cmd ();
  }
  sender()->deleteLater();
}


/******************************************************************************
* qt_tm_widget_rep
******************************************************************************/

qt_tm_widget_rep::qt_tm_widget_rep(int mask, command _quit)
 : qt_window_widget_rep (new QTMWindow (0), "popup", _quit), helper (this),
   prompt (NULL), full_screen (false)
{
  type = texmacs_widget;

  main_widget = concrete (::glue_widget (true, true, 1, 1));
  
  // decode mask
  visibility[0] = (mask & 1)   == 1;   // header
  visibility[1] = (mask & 2)   == 2;   // main
  visibility[2] = (mask & 4)   == 4;   // mode
  visibility[3] = (mask & 8)   == 8;   // focus
  visibility[4] = (mask & 16)  == 16;  // user
  visibility[5] = (mask & 32)  == 32;  // footer
  visibility[6] = (mask & 64)  == 64;  // side tools #0
  visibility[7] = (mask & 128) == 128; // bottom tools

  // general setup for main window
  
  QMainWindow* mw= mainwindow ();
  mw->setStyle (qtmstyle ());
  mw->menuBar()->setStyle (qtmstyle ());

#if (defined(MACOS_QT_MENU))
  mw->menuBar()->setNativeMenuBar(false);
#endif
  
  // there is a bug in the early implementation of toolbars in Qt 4.6
  // which has been fixed in 4.6.2 (at least)
  // this is why we change dimension of icons
  
#if (defined(Q_OS_MAC)&&(QT_VERSION>=QT_VERSION_CHECK(4,6,0))&&(QT_VERSION<QT_VERSION_CHECK(4,6,2)))
  mw->setIconSize (QSize (22, 30));  
#else
  mw->setIconSize (QSize (17, 17));
#endif
  mw->setFocusPolicy (Qt::NoFocus);
  
  // status bar
  
  QStatusBar* bar= new QStatusBar(mw);
  leftLabel= new QLabel (qt_translate ("Welcome to TeXmacs"), mw);
  rightLabel= new QLabel (qt_translate ("Booting"), mw);
  leftLabel->setFrameStyle (QFrame::NoFrame);
  rightLabel->setFrameStyle (QFrame::NoFrame);
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
  
  bar->setMinimumWidth (2);
#ifdef Q_OS_LINUX
  int min_h= (int) floor (28 * retina_scale);
  bar->setMinimumHeight (min_h);
#else
#if (QT_VERSION >= 0x050000)
  int min_h= (int) floor (24 * retina_scale);
  bar->setMinimumHeight (min_h);
#else
  if (retina_scale > 1.0) {
    int min_h= (int) floor (20 * retina_scale);
    bar->setMinimumHeight (min_h);
  }
#endif
#endif
  mw->setStatusBar (bar);
 
  // toolbars
  
  mainToolBar   = new QToolBar ("main toolbar", mw);
  modeToolBar   = new QToolBar ("mode toolbar", mw);
  focusToolBar  = new QToolBar ("focus toolbar", mw);
  userToolBar   = new QToolBar ("user toolbar", mw);
  
  bottomTools   = new QDockWidget ("bottom tools", mw);
  sideTools     = new QDockWidget ("side tools", 0);
    // HACK: Wrap the dock in a "fake" window widget (last parameter = true) to
    // have clicks report the right position.
  static int cnt=0;
  string dock_name = "dock:" * as_string(cnt++);
  dock_window_widget = tm_new<qt_window_widget_rep> (sideTools, dock_name,
                                                     command(), true);
  
  mainToolBar->setStyle (qtmstyle ());
  modeToolBar->setStyle (qtmstyle ());
  focusToolBar->setStyle (qtmstyle ());
  userToolBar->setStyle (qtmstyle ());
  sideTools->setStyle (qtmstyle ());
  bottomTools->setStyle (qtmstyle ());
  
  {
    // set proper sizes for icons
    QImage *pxm = xpm_image ("tm_new.xpm");
    QSize sz = (pxm ? pxm->size() : QSize (24, 24));
    tweak_iconbar_size (sz);
    mainToolBar->setIconSize (sz);
    pxm = xpm_image ("tm_section.xpm");
    sz = (pxm ? pxm->size() : QSize (20, 20));
    tweak_iconbar_size (sz);
    modeToolBar->setIconSize (sz);
    pxm = xpm_image ("tm_add.xpm");
    sz = (pxm ? pxm->size() : QSize (16, 16));
    tweak_iconbar_size (sz);
    focusToolBar->setIconSize (sz);
  }

  // Why we need fixed height:
  // The height of the toolbar is actually determined by the font height.
  // And the font height is not fixed. If the height of the toolbar is not
  // fixed, the stretching of it will make the document area floating and
  // triggers the re-rendering of the full document.
  //
  // NOTICE: setFixedHeight must be after setIconSize
  // TODO: the size of the toolbar should be calculated dynamically
#if (QT_VERSION >= 0x050000)
  int toolbarHeight= 30;
  mainToolBar->setFixedHeight (toolbarHeight + 8);
  modeToolBar->setFixedHeight (toolbarHeight + 4);
  focusToolBar->setFixedHeight (toolbarHeight);
#else
#ifndef Q_OS_MAC
  int toolbarHeight= 30 * retina_icons;
  mainToolBar->setFixedHeight (toolbarHeight + 8);
  modeToolBar->setFixedHeight (toolbarHeight + 4);
  focusToolBar->setFixedHeight (toolbarHeight);  
#endif
#endif
  
  QWidget *cw= new QWidget();
  cw->setObjectName("central widget");  // this is important for styling toolbars.
  
    // The main layout
  
  QVBoxLayout *bl = new QVBoxLayout (cw);
  bl->setContentsMargins (0, 1, 0, 0);
  bl->setSpacing (0);
  cw->setLayout (bl);
  QWidget* q = main_widget->as_qwidget(); // force creation of QWidget
  q->setParent (qwid); // q->layout()->removeWidget(q) will reset the parent to this
  bl->addWidget (q);
  
  mw->setCentralWidget (cw);

#ifdef UNIFIED_TOOLBAR

  if (use_unified_toolbar) {
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
    bl->insertWidget(5, r2);

    //mw->setContentsMargins (-2, -2, -2, -2);  // Why this?
    bar->setContentsMargins (0, 1, 0, 1);
  }
  else {
    mw->addToolBar (mainToolBar);
    mw->addToolBarBreak ();
    mw->addToolBar (modeToolBar);
    mw->addToolBarBreak ();
    mw->addToolBar (focusToolBar);
    mw->addToolBarBreak ();
    mw->addToolBar (userToolBar);
    mw->addToolBarBreak ();
  }

#else
  mw->addToolBar (mainToolBar);
  mw->addToolBarBreak ();
  mw->addToolBar (modeToolBar);
  mw->addToolBarBreak ();
  mw->addToolBar (focusToolBar);
  mw->addToolBarBreak ();
  mw->addToolBar (userToolBar);
  mw->addToolBarBreak ();
#endif

  sideTools->setAllowedAreas (Qt::AllDockWidgetAreas);
  sideTools->setFeatures (QDockWidget::DockWidgetMovable |
                         QDockWidget::DockWidgetFloatable);
  sideTools->setFloating (false);
  mw->addDockWidget (Qt::RightDockWidgetArea, sideTools);

  bottomTools->setAllowedAreas (Qt::BottomDockWidgetArea);
  bottomTools->setFeatures (QDockWidget::NoDockWidgetFeatures);
  bottomTools->setFloating (false);
  bottomTools->setTitleBarWidget (new QWidget()); // Disables title bar
  bottomTools->setMinimumHeight (10);             // Avoids warning
  bottomTools->setContentsMargins (3, 6, 3, -2);  // Hacks hacks hacks... :(
  mw->addDockWidget (Qt::BottomDockWidgetArea, bottomTools);

  
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
  sideTools->setVisible (false);
  bottomTools->setVisible (false);
  mainwindow()->statusBar()->setVisible (true);
#ifndef Q_OS_MAC
  mainwindow()->menuBar()->setVisible (false);
#endif  
  QPalette pal;
  QColor bgcol (160, 160, 160); // same as tm_background
  pal.setColor (QPalette::Mid, bgcol);
  mainwindow()->setPalette(pal);
}

qt_tm_widget_rep::~qt_tm_widget_rep () {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_tm_widget_rep::~qt_tm_widget_rep of widget "
                  << type_as_string() << LF;
  
    // clear any residual waiting menu installation
  waiting_widgets = remove(waiting_widgets, this);
}

void
qt_tm_widget_rep::tweak_iconbar_size (QSize& sz) {
#ifdef Q_OS_LINUX
  if (sz.height () >= 24) {
    sz.setWidth (sz.width () + 2);
    sz.setHeight (sz.height () + 8);
  }
  else if (sz.height () >= 20) {
    sz.setWidth (sz.width () + 1);
    sz.setHeight (sz.height () + 4);
  }
  else if (sz.height () >= 16) {
    sz.setHeight (sz.height () + 4);
  }
#else
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
#endif
  //sz.setHeight ((int) floor (sz.height () * retina_scale + 0.5));
}


/*! Return ourselves as a window widget.
 \param name A unique identifier for the window (e.g. "TeXmacs:3")
 */
widget
qt_tm_widget_rep::plain_window_widget (string name, command _quit) {
  (void) _quit; // The widget already has a command. Don't overwrite. 
  orig_name = name;
  return this;
}

void
qt_tm_widget_rep::update_visibility () {
#define XOR(exp1,exp2) (((!exp1) && (exp2)) || ((exp1) && (!exp2)))

  bool old_mainVisibility = mainToolBar->isVisible();
  bool old_modeVisibility = modeToolBar->isVisible();
  bool old_focusVisibility = focusToolBar->isVisible();
  bool old_userVisibility = userToolBar->isVisible();
  bool old_sideVisibility = sideTools->isVisible();
  bool old_bottomVisibility = bottomTools->isVisible();
  bool old_statusVisibility = mainwindow()->statusBar()->isVisible();

  bool new_mainVisibility = visibility[1] && visibility[0];
  bool new_modeVisibility = visibility[2] && visibility[0];
  bool new_focusVisibility = visibility[3] && visibility[0];
  bool new_userVisibility = visibility[4] && visibility[0];
  bool new_statusVisibility = visibility[5];
  bool new_sideVisibility = visibility[6];
  bool new_bottomVisibility = visibility[7];
  
  if ( XOR(old_mainVisibility,  new_mainVisibility) )
    mainToolBar->setVisible (new_mainVisibility);
  if ( XOR(old_modeVisibility,  new_modeVisibility) )
    modeToolBar->setVisible (new_modeVisibility);
  if ( XOR(old_focusVisibility,  new_focusVisibility) )
    focusToolBar->setVisible (new_focusVisibility);
  if ( XOR(old_userVisibility,  new_userVisibility) )
    userToolBar->setVisible (new_userVisibility);
  if ( XOR(old_sideVisibility,  new_sideVisibility) )
    sideTools->setVisible (new_sideVisibility);
  if ( XOR(old_bottomVisibility,  new_bottomVisibility) )
    bottomTools->setVisible (new_bottomVisibility);
  if ( XOR(old_statusVisibility,  new_statusVisibility) )
    mainwindow()->statusBar()->setVisible (new_statusVisibility);

#ifndef Q_OS_MAC
  bool old_menuVisibility = mainwindow()->menuBar()->isVisible();
  bool new_menuVisibility = visibility[0];

  if ( XOR(old_menuVisibility,  new_menuVisibility) )
    mainwindow()->menuBar()->setVisible (new_menuVisibility);
#endif

//#if 0
#ifdef UNIFIED_TOOLBAR

  // do modifications only if needed to reduce flicker
  if (use_unified_toolbar &&
      (XOR(old_mainVisibility,  new_mainVisibility) ||
       XOR(old_modeVisibility,  new_modeVisibility) ))
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
  {
    QFont f = leftLabel->font();
    int fs = as_int (get_preference ("gui:mini-fontsize", QTM_MINI_FONTSIZE));
    f.setPointSize (qt_zoom (fs > 0 ? fs : QTM_MINI_FONTSIZE));
    leftLabel->setFont(f);
    rightLabel->setFont(f);
  }
}

widget
qt_tm_widget_rep::read(slot s, blackbox index) {
  widget ret;
  
  switch (s) {
    case SLOT_CANVAS:
      check_type_void (index, s);
      ret = abstract (main_widget);
      break;
      
    default:
      return qt_window_widget_rep::read(s, index);
  }
  
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_tm_widget_rep::read " << slot_name (s)
                  << "\t\tfor widget\t" << type_as_string() << LF;
  
  return ret;
}

void
qt_tm_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_INVALIDATE:
    case SLOT_INVALIDATE_ALL:
    case SLOT_EXTENTS:
    case SLOT_SCROLL_POSITION:
    case SLOT_ZOOM_FACTOR:
    case SLOT_MOUSE_GRAB:
      main_widget->send(s, val);
      return;
    case SLOT_KEYBOARD_FOCUS:
    {
      check_type<bool> (val, s);
      bool focus = open_box<bool> (val);
      if (focus && canvas() && !canvas()->hasFocus())
        canvas()->setFocus (Qt::OtherFocusReason);
    }
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
    case SLOT_BOTTOM_TOOLS_VISIBILITY:
    {
      check_type<bool>(val, s);
      visibility[7] = open_box<bool> (val);
      update_visibility();
    }
      break;

    case SLOT_LEFT_FOOTER:
    {
      check_type<string>(val, s);
      string msg = open_box<string> (val);
      leftLabel->setText (to_qstring (msg));
      leftLabel->update ();
    }
      break;
    case SLOT_RIGHT_FOOTER:
    {
      check_type<string>(val, s);
      string msg= open_box<string> (val);
      rightLabel->setText (to_qstring (msg));
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
        mainwindow()->statusBar()->removeWidget (leftLabel);
        mainwindow()->statusBar()->removeWidget (rightLabel);
        mainwindow()->statusBar()->addWidget (prompt, 1);
        prompt->start();
      } else {
        if (prompt) prompt->end();
        mainwindow()->statusBar()->removeWidget (prompt);
        mainwindow()->statusBar()->addWidget (leftLabel);
        mainwindow()->statusBar()->addPermanentWidget (rightLabel);
        leftLabel->show();
        rightLabel->show();
        prompt->deleteLater();
        prompt = NULL;
      }
    }
      break;
    case SLOT_FILE:
    {
      check_type<string>(val, s);
      string file = open_box<string> (val);
      if (DEBUG_QT_WIDGETS) debug_widgets << "\tFile: " << file << LF;
#if (QT_VERSION >= 0x040400)
      mainwindow()->setWindowFilePath (utf8_to_qstring (file));
#endif
    }
      break;
    case SLOT_POSITION:
    {
      check_type<coord2>(val, s);
      coord2 p= open_box<coord2> (val);
      mainwindow()->move (to_qpoint (p));
    }
      break;
    case SLOT_SIZE:
    {
      check_type<coord2>(val, s);
      coord2 p= open_box<coord2> (val);
      mainwindow()->resize (to_qsize (p));
    }
      break;
    case SLOT_DESTROY:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (!is_nil (quit))
        quit ();
      the_gui->need_update ();
    }
      break;
    case SLOT_FULL_SCREEN:
    {
      check_type<bool> (val, s);
      set_full_screen(open_box<bool> (val));
    }
      break;
    default:
      qt_window_widget_rep::send (s, val);
      return;
  }
  
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_tm_widget_rep: sent " << slot_name (s) 
                  << "\t\tto widget\t"      << type_as_string() << LF;
}

blackbox
qt_tm_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_tm_widget_rep: queried " << slot_name(s)
                  << "\t\tto widget\t" << type_as_string() << LF;
  
  switch (s) {
    case SLOT_SCROLL_POSITION:
    case SLOT_EXTENTS:
    case SLOT_VISIBLE_PART:
    case SLOT_ZOOM_FACTOR:
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
    case SLOT_BOTTOM_TOOLS_VISIBILITY:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (visibility[7]);
      
    case SLOT_INTERACTIVE_INPUT:
    {
      check_type_id<string> (type_id, s);
      qt_input_text_widget_rep* w = 
        static_cast<qt_input_text_widget_rep*> (int_input.rep);
      if (w->ok)
        return close_box<string> (scm_quote (w->input));
      else
        return close_box<string> ("#f");
    }

    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (from_qpoint (mainwindow()->pos()));
    }
      
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (from_qsize (mainwindow()->size()));
    }

    case SLOT_INTERACTIVE_MODE:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (prompt && prompt->isActive());

    default:
      return qt_window_widget_rep::query (s, type_id);
  }
}

void
qt_tm_widget_rep::install_main_menu () {
  if (main_menu_widget == waiting_main_menu_widget) return;
  main_menu_widget = waiting_main_menu_widget;
  QList<QAction*>* src = main_menu_widget->get_qactionlist();
  if (!src) return;
  QMenuBar* dest = mainwindow()->menuBar();
  dest->clear();
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    if (a->menu()) {
      //TRICK: Mac native QMenuBar accepts only menus which are already populated
      // this will cause a problem for us, since menus are lazy and populated only after triggering
      // this is the reason we add a dummy action before inserting the menu
      a->menu()->addAction("native menubar trick");
      dest->addAction(a->menu()->menuAction());
      QObject::connect (a->menu(),         SIGNAL (aboutToShow()),
                        the_gui->gui_helper, SLOT (aboutToShowMainMenu()));
      QObject::connect (a->menu(),         SIGNAL (aboutToHide()),
                        the_gui->gui_helper, SLOT (aboutToHideMainMenu()));
    }
  }
}

void
qt_tm_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_tm_widget_rep::write " << slot_name (s) << LF;
  
  switch (s) {
        // Widget w is usually a qt_simple_widget_rep, with a QTMWidget as
        // underlying widget. We must discard the current main_widget and
        // display the new. But while switching buffers the widget w is a
        // glue_widget, so we may not just use canvas() everywhere.
    case SLOT_SCROLLABLE:
    {
      check_type_void (index, s);
      
      QWidget* q = main_widget->qwid;
      q->hide();
      QLayout* l = centralwidget()->layout();
      l->removeWidget(q);
      
      q = concrete(w)->as_qwidget();   // force creation of the new QWidget
      l->addWidget(q);
      /* " When you use a layout, you do not need to pass a parent when
       constructing the child widgets. The layout will automatically reparent
       the widgets (using QWidget::setParent()) so that they are children of 
       the widget on which the layout is installed " */
      main_widget = concrete (w);
        // canvas() now returns the new QTMWidget (or 0)
      
      if (scrollarea())   // Fix size to draw margins around.
        scrollarea()->surface()->setSizePolicy (QSizePolicy::Fixed,
                                                QSizePolicy::Fixed);
      send_keyboard_focus (abstract (main_widget));
    }
      break;
      
    case SLOT_MAIN_MENU:
      check_type_void (index, s);
    {
      waiting_main_menu_widget = concrete (w);
      if (menu_count <= 0)
        install_main_menu();
      else if (!contains (waiting_widgets, this))
          // menu interaction ongoing, postpone new menu installation until done
        waiting_widgets << this;
    }
      break;
      
    case SLOT_MAIN_ICONS:
      check_type_void (index, s);
    {
      main_icons_widget = concrete (w);
      QList<QAction*>* list = main_icons_widget->get_qactionlist();
      if (list) {
        replaceButtons (mainToolBar, list);
        update_visibility();
      }
    }
      break;
      
    case SLOT_MODE_ICONS:
      check_type_void (index, s);
    {
      mode_icons_widget = concrete (w);
      QList<QAction*>* list = mode_icons_widget->get_qactionlist();
      if (list) {
        replaceButtons (modeToolBar, list);
        update_visibility();
      }
    }
      break;
      
    case SLOT_FOCUS_ICONS:
      check_type_void (index, s);
    {
      focus_icons_widget = concrete (w);
      QList<QAction*>* list = focus_icons_widget->get_qactionlist();
      if (list) {
        replaceButtons (focusToolBar, list);
        update_visibility();
      }
    }
      break;
      
    case SLOT_USER_ICONS:
      check_type_void (index, s);
    {   
      user_icons_widget = concrete (w);
      QList<QAction*>* list = user_icons_widget->get_qactionlist();
      if (list) {
        replaceButtons (userToolBar, list);
        update_visibility();
      }
    }
      break;
      
    case SLOT_SIDE_TOOLS:
      check_type_void (index, s);
    {
      side_tools_widget = concrete (w);
      QWidget* new_qwidget = side_tools_widget->as_qwidget();
      QWidget* old_qwidget = sideTools->widget();
      if (old_qwidget) old_qwidget->deleteLater();
      sideTools->setWidget (new_qwidget);
      update_visibility();
      new_qwidget->show();
    }
      break;

    case SLOT_BOTTOM_TOOLS:
      check_type_void (index, s);
    {   
      bottom_tools_widget = concrete (w);
      QWidget* new_qwidget = bottom_tools_widget->as_qwidget();
      QWidget* old_qwidget = bottomTools->widget();
      if (old_qwidget) old_qwidget->deleteLater();
      bottomTools->setWidget (new_qwidget);
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
      QPalette pal;
      pal.setColor(QPalette::Mid, Qt::black);
      mainwindow()->setPalette(pal);
#ifdef UNIFIED_TOOLBAR
      if (use_unified_toolbar) {
        //HACK: we disable unified toolbar since otherwise
        //  the application will crash when we return to normal mode
        // (bug in Qt? present at least with 4.7.1)
        mainwindow()->setUnifiedTitleAndToolBarOnMac(false);
        mainwindow()->centralWidget()->layout()->setContentsMargins(0,0,0,0);
      }
#endif
//      mainwindow()->window()->setContentsMargins(0,0,0,0);
      //win->showFullScreen();
       win->setWindowState(win->windowState() | Qt::WindowFullScreen);
    }
    else {
      QPalette pal;
      QColor bgcol (160, 160, 160); // same as tm_background
      pal.setColor (QPalette::Mid, bgcol);
      mainwindow()->setPalette(pal);
      bool cache = visibility[0];
      visibility[0] = false;
      update_visibility();
//      win->showNormal();
      win->setWindowState(win->windowState() & ~Qt::WindowFullScreen);

      visibility[0] = cache;
      update_visibility();
#ifdef UNIFIED_TOOLBAR
      if (use_unified_toolbar) {
        mainwindow()->centralWidget()->layout()->setContentsMargins (0,1,0,0);
        //HACK: we reenable unified toolbar (see above HACK) 
        //  the application will crash when we return to normal mode
        mainwindow()->setUnifiedTitleAndToolBarOnMac(true);
      }
#endif
    }
  }
  
  scrollarea()->setHorizontalScrollBarPolicy(flag ? Qt::ScrollBarAlwaysOff : Qt::ScrollBarAsNeeded);
  scrollarea()->setVerticalScrollBarPolicy(flag ? Qt::ScrollBarAlwaysOff : Qt::ScrollBarAsNeeded);
}


/******************************************************************************
 * qt_tm_embedded_widget_rep
 ******************************************************************************/

qt_tm_embedded_widget_rep::qt_tm_embedded_widget_rep (command _quit) 
 : qt_widget_rep (embedded_tm_widget), quit (_quit) {
  main_widget = ::glue_widget (true, true, 1, 1);
}

void
qt_tm_embedded_widget_rep::send (slot s, blackbox val) {

  switch (s) {
    case SLOT_INVALIDATE:
    case SLOT_INVALIDATE_ALL:
    case SLOT_EXTENTS:
    case SLOT_SCROLL_POSITION:
    case SLOT_ZOOM_FACTOR:
    case SLOT_MOUSE_GRAB:
      main_widget->send(s, val);
      return;

       /// FIXME: decide what to do with these for embedded widgets
    case SLOT_HEADER_VISIBILITY:
    case SLOT_MAIN_ICONS_VISIBILITY:    
    case SLOT_MODE_ICONS_VISIBILITY:
    case SLOT_FOCUS_ICONS_VISIBILITY:
    case SLOT_USER_ICONS_VISIBILITY:
    case SLOT_FOOTER_VISIBILITY:
    case SLOT_SIDE_TOOLS_VISIBILITY:
    case SLOT_BOTTOM_TOOLS_VISIBILITY:
    case SLOT_LEFT_FOOTER:
    case SLOT_RIGHT_FOOTER:
    case SLOT_SCROLLBARS_VISIBILITY:
    case SLOT_INTERACTIVE_MODE:
    case SLOT_FILE:
      break;
 
    case SLOT_DESTROY:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (!is_nil (quit))
        quit ();
      the_gui->need_update ();
    }
      break;
      
    default:
      qt_widget_rep::send (s, val);
      return;
  }
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_tm_embedded_widget_rep: sent " << slot_name (s) 
                  << "\t\tto widget\t" << type_as_string() << LF;  
}

blackbox
qt_tm_embedded_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_tm_embedded_widget_rep::query " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_IDENTIFIER:
    {
      if (qwid) {
        widget_rep* wid = qt_window_widget_rep::widget_from_qwidget(qwid);
        if (wid) return wid->query (s, type_id);
      }
      return close_box<int>(0);
    }

    case SLOT_SCROLL_POSITION:
    case SLOT_EXTENTS:
    case SLOT_VISIBLE_PART:
    case SLOT_ZOOM_FACTOR:
    case SLOT_POSITION:
    case SLOT_SIZE:
      if (!is_nil (main_widget))
        return main_widget->query (s, type_id);
      else
        return qt_widget_rep::query (s, type_id);
        /// FIXME: decide what to do with these for embedded widgets
    case SLOT_HEADER_VISIBILITY:
    case SLOT_MAIN_ICONS_VISIBILITY:
    case SLOT_MODE_ICONS_VISIBILITY:
    case SLOT_FOCUS_ICONS_VISIBILITY:
    case SLOT_USER_ICONS_VISIBILITY:
    case SLOT_FOOTER_VISIBILITY:
    case SLOT_SIDE_TOOLS_VISIBILITY:
    case SLOT_BOTTOM_TOOLS_VISIBILITY:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (false);

    default:
      return qt_widget_rep::query (s, type_id);
  }
}

widget
qt_tm_embedded_widget_rep::read (slot s, blackbox index) {
  widget ret;
  
  switch (s) {
    case SLOT_CANVAS:
      check_type_void (index, s);
      ret = main_widget;
      break;
    default:
      return qt_widget_rep::read(s, index);
  }
  
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_tm_widget_rep::read " << slot_name (s) 
                  << "\t\tfor widget\t" << type_as_string() << LF;
  
  return ret;
}

void
qt_tm_embedded_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_tm_embedded_widget_rep::write " << slot_name (s) << LF;

  switch (s) {
      // Widget w is a qt_simple_widget_rep, with a QTMWidget as underlying
      // widget. We must discard the current QTMWidget and display the new.
      // see qt_tm_widget_rep::write()
    case SLOT_SCROLLABLE:
    {
      check_type_void (index, s);
      main_widget = w;
    }
      break;
        /// FIXME: decide what to do with these for embedded widgets
    case SLOT_MAIN_MENU:
    case SLOT_MAIN_ICONS:
    case SLOT_MODE_ICONS:
    case SLOT_FOCUS_ICONS:
    case SLOT_USER_ICONS:
    case SLOT_SIDE_TOOLS:
    case SLOT_BOTTOM_TOOLS:
    case SLOT_INTERACTIVE_INPUT:
    case SLOT_INTERACTIVE_PROMPT:
    default:
      qt_widget_rep::write (s, index, w);
  }
}

QWidget*
qt_tm_embedded_widget_rep::as_qwidget() {
  qwid = new QWidget();
  QVBoxLayout* l = new QVBoxLayout();
  l->setContentsMargins (0,0,0,0);
  qwid->setLayout (l);
  l->addWidget (concrete(main_widget)->as_qwidget());
  return qwid;
}

QLayoutItem*
qt_tm_embedded_widget_rep::as_qlayoutitem () {
  return new QWidgetItem(as_qwidget());
}
