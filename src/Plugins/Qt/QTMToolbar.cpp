/******************************************************************************
* MODULE     : QTMTToolbar.cpp
* DESCRIPTION: Custom toolbar for TeXmacs, that can scroll on Android.
* COPYRIGHT  : (C) 2025 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMToolbar.hpp"
#include "QTMStyle.hpp"
#include "gui.hpp"

#include <QToolButton>
#include <QMenu>
#include <QWidgetAction>
#ifdef OS_ANDROID
#include <QScroller>
#include <QScrollBar>
#endif

#define QTMTOOLBAR_MARGIN 2

QTMToolbar::QTMToolbar (const QString& title, QSize iconSize, QWidget* parent)
  : QToolBar (title, parent) {

  if (tm_style_sheet == "") setStyle (qtmstyle ());
  
  if (!iconSize.isNull()) {
    setIconSize (iconSize);
    setFixedHeight (iconSize.height() + QTMTOOLBAR_MARGIN * 2);
  }

  setMovable (false);

#ifdef OS_ANDROID
  mScrollArea = new QScrollArea (this);
  addWidget (mScrollArea);

  mLayout = new QHBoxLayout (mScrollArea);
  QWidget* w = new QWidget ();
  w->setLayout (mLayout);
  mScrollArea->setWidget (w);

  // don't expand the layout to fill the scroll area
  mLayout->setSizeConstraint (QLayout::SetMinimumSize);

  mScrollArea->setWidgetResizable (true);
  mScrollArea->setVerticalScrollBarPolicy (Qt::ScrollBarAlwaysOff);
  mScrollArea->verticalScrollBar()->setDisabled (true);
  mScrollArea->setHorizontalScrollBarPolicy (Qt::ScrollBarAlwaysOff);
  mScrollArea->horizontalScrollBar()->setDisabled (true);
  mScrollArea->setFrameShape (QFrame::NoFrame);
  
  mLayout->setContentsMargins (0, 0, 0, 0);
  mLayout->setSpacing (0);

  QScrollerProperties properties = QScroller::scroller(mScrollArea)->scrollerProperties();
  properties.setScrollMetric(QScrollerProperties::VerticalOvershootPolicy, QScrollerProperties::OvershootAlwaysOff);
  properties.setScrollMetric(QScrollerProperties::HorizontalOvershootPolicy, QScrollerProperties::OvershootAlwaysOff);
  QScroller::scroller(mScrollArea)->setScrollerProperties(properties);

  QScroller::grabGesture (mScrollArea, QScroller::LeftMouseButtonGesture);
#endif
}

QTMToolbar::~QTMToolbar () {
}

void QTMToolbar::replaceActions (QList<QAction*>* src) {
  if (src == NULL)
    FAILED ("replaceActions expects valid objects");
  setUpdatesEnabled (false);
#ifdef OS_ANDROID
  while (mLayout->count() > 0) {
    QWidget* w = mLayout->itemAt(0)->widget();
    mLayout->removeWidget(w);
    delete w;
  }
#else
  clear ();
#endif
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);
}

void QTMToolbar::replaceButtons (QList<QAction*>* src) {
  if (src == NULL)
    FAILED ("replaceButtons expects valid objects");
  setUpdatesEnabled (false);
  #ifdef OS_ANDROID
  while (mLayout->count() > 0) {
    QWidget* w = mLayout->itemAt(0)->widget();
    mLayout->removeWidget(w);
    delete w;
  }
#else
  clear ();
#endif
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);
}

void QTMToolbar::addAction (QAction* action) {
  if (action->isSeparator()) {
#ifdef OS_ANDROID
    actionWidget = new QWidget (this);
    actionWidget->setMinimumWidth (QTMTOOLBAR_MARGIN);
    actionWidget->setMinimumHeight (iconSize().height() + QTMTOOLBAR_MARGIN * 2);
#else
    QToolBar::addSeparator();
    return;
#endif
  }

  
  // create the tool button
  QWidget *actionWidget = nullptr;
  
  if (qobject_cast<QWidgetAction*> (action)) {
    actionWidget = qobject_cast<QWidgetAction*> (action)->requestWidget(this);
  }

  if (!actionWidget) {
    actionWidget = new QToolButton (this);
    if (tm_style_sheet == "") {
      actionWidget->setStyle (qtmstyle ());
    }
    ((QToolButton*)actionWidget)->setDefaultAction (action);
  }

  QToolButton* button = qobject_cast<QToolButton*> (actionWidget);
  if (button) {

    // if the action contains a icon, set a fixed icon size
    if (!action->icon().isNull()) {
      button->setIconSize (iconSize());
    }
    
    // if the action is a menu, the tool button should be a menu button
    if (action->menu()) {
      button->setPopupMode (QToolButton::InstantPopup);
    }
    
    // if the action contains only text, add a margin to the button
    if (action->icon().isNull()) {
      button->setToolButtonStyle (Qt::ToolButtonTextOnly);
      button->setContentsMargins (QTMTOOLBAR_MARGIN, QTMTOOLBAR_MARGIN, QTMTOOLBAR_MARGIN, QTMTOOLBAR_MARGIN);
    }
    
    // if the fixed height is lower than the required height, set the fixed height
    int requiredHeight = button->sizeHint().height() + QTMTOOLBAR_MARGIN * 2;
    if (height() < requiredHeight) {
      setFixedHeight (requiredHeight);
    }

  }

  // add the button to the toolbar, and on Android to the scrollable layout
#ifdef OS_ANDROID
  mLayout->addWidget (button);
#else
  QToolBar::addWidget (actionWidget);
#endif
}

#ifdef OS_ANDROID
void QTMToolbar::removeAction (QAction* action) {
  for (int i = 0; i < mLayout->count(); i++) {
    QToolButton* button = qobject_cast<QToolButton*> (mLayout->itemAt(i)->widget());
    if (button && button->defaultAction() == action) {
      mLayout->removeWidget (button);
      button->deleteLater();
      break;
    }
  }
}

void QTMToolbar::clear () {
  while (mLayout->count() > 0) {
    QWidget* w = mLayout->itemAt(0)->widget();
    mLayout->removeWidget(w);
    delete w;
  }
}
#endif