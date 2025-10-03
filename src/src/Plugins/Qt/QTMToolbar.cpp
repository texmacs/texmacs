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
#include "QTMWidget.hpp"
#include "gui.hpp"

#include <QToolButton>
#include <QMenu>
#include <QWidgetAction>

#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
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

#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  mScrollArea = new QScrollArea (this);
  addWidget (mScrollArea);

  mLayout = new QHBoxLayout (mScrollArea);
  QWidget* w = new QWidget (mScrollArea);
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
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  while (mLayout->count() > 0) {
    QWidget* w = mLayout->itemAt(0)->widget();
    mLayout->removeWidget(w);
    delete w;
  }
#else
  clear ();
#endif
  addSeparator();
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  addRightSpacer();
#endif
}

void QTMToolbar::replaceButtons (QList<QAction*>* src) {
  if (src == NULL)
    FAILED ("replaceButtons expects valid objects");
  setUpdatesEnabled (false);
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  while (mLayout->count() > 0) {
    QWidget* w = mLayout->itemAt(0)->widget();
    mLayout->removeWidget(w);
    delete w;
  }
#else
  clear ();
#endif
  addSeparator();
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  addRightSpacer();
#endif
}

#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
void QTMToolbar::addSeparator () {
  QWidget* spacer = new QWidget (this);
  spacer->setStyleSheet ("background: transparent;");
  spacer->setFixedWidth (10);
  mLayout->addWidget (spacer);
}

void QTMToolbar::addRightSpacer () {
  // a a spacer that will push the buttons to the left
  QWidget* spacer = new QWidget (this);
  spacer->setStyleSheet ("background: transparent;");
  spacer->setSizePolicy (QSizePolicy::Expanding, QSizePolicy::Preferred);
  mLayout->addWidget (spacer);
}
#endif

void QTMToolbar::addAction (QAction* action) {
  
  // create the tool button
  QWidget *actionWidget = nullptr;
  
  if (action->isSeparator()) {
    addSeparator();
    return;
  }

  if (qobject_cast<QWidgetAction*> (action)) {
    actionWidget = qobject_cast<QWidgetAction*> (action)->requestWidget(this);
  }

  if (!actionWidget) {
    actionWidget = new QToolButton (this);
    if (tm_style_sheet == "") {
      cout << "Setting style for action widget" << LF;
      actionWidget->setStyle (qtmstyle ());
    } else {
      // the background should be transparent when not hovered
      //actionWidget->setStyleSheet ("QToolButton:not(:hover) { background: transparent; }");
    }
    ((QToolButton*)actionWidget)->setDefaultAction (action);
  }

  QToolButton* button = qobject_cast<QToolButton*> (actionWidget);
  if (button) {

    // if the action contains a icon, set a fixed icon size
    if (!action->icon().isNull()) {
      button->setIconSize (iconSize());
    }
    
    // on click finish, set the focus to the last focused widget
    connect (button, &QToolButton::clicked, []() {
      QTMWidget::setFocusToLast();
    });

    // if the action is a menu, the tool button should be a menu button
    if (action->menu()) {
      button->setPopupMode (QToolButton::InstantPopup);
      connect (action->menu(), &QMenu::aboutToHide, [button]() {
        QTMWidget::setFocusToLast();
      });
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
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  mLayout->addWidget (button);
#else
  QToolBar::addWidget (actionWidget);
#endif
}

#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
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
