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
#include <QFrame>
#include <QScrollBar>
#include <QEvent>

#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
#include <QScroller>
#include <QScrollerProperties>
#endif

#define QTMTOOLBAR_MARGIN 2

QTMToolbar::QTMToolbar (const QString& title, QSize iconSize, QWidget* parent)
  : QToolBar (title, parent)
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  , mScrollArea(nullptr)
  , mLayout(nullptr)
  , mLeftBtn(nullptr)
  , mRightBtn(nullptr)
#endif
{  
  if (!iconSize.isNull()) {
    setIconSize (iconSize);
    setFixedHeight (iconSize.height() + QTMTOOLBAR_MARGIN * 2);
  }

  setMovable (false);

#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  mLeftBtn = new QToolButton (this);
  mLeftBtn->setText (QString::fromUtf8("<"));
  mLeftBtn->setToolTip (tr("Scroll left"));
  mLeftBtn->setAutoRepeat (true);
  mLeftBtn->setAutoRepeatDelay (250);
  mLeftBtn->setAutoRepeatInterval (50);
  connect (mLeftBtn, &QToolButton::clicked, [this]() { scrollBy (-scrollStep()); });
  mLeftAct = addWidget (mLeftBtn);

  mScrollArea = new QScrollArea (this);
  mScrollArea->setFrameShape (QFrame::NoFrame);
  mScrollArea->setWidgetResizable (true);
  mScrollArea->setVerticalScrollBarPolicy (Qt::ScrollBarAlwaysOff);
  mScrollArea->setHorizontalScrollBarPolicy (Qt::ScrollBarAlwaysOff);

  QWidget* w = new QWidget (mScrollArea);
  mLayout = new QHBoxLayout (w);
  mLayout->setSizeConstraint (QLayout::SetMinimumSize);
  mLayout->setContentsMargins (0, 0, 0, 0);
  mLayout->setSpacing (0);
  w->setLayout (mLayout);
  mScrollArea->setWidget (w);
  //w->setStyleSheet ("background: transparent;");
  w->setAttribute (Qt::WA_TranslucentBackground);

  QScrollerProperties props = QScroller::scroller(mScrollArea->viewport())->scrollerProperties();
  props.setScrollMetric(QScrollerProperties::VerticalOvershootPolicy,   QScrollerProperties::OvershootAlwaysOff);
  props.setScrollMetric(QScrollerProperties::HorizontalOvershootPolicy, QScrollerProperties::OvershootAlwaysOff);
  QScroller::scroller(mScrollArea->viewport())->setScrollerProperties(props);
  QScroller::grabGesture (mScrollArea->viewport(), QScroller::LeftMouseButtonGesture);

  addWidget (mScrollArea);

  mRightBtn = new QToolButton (this);
  mRightBtn->setText (QString::fromUtf8(">"));
  mRightBtn->setToolTip (tr("Scroll right"));
  mRightBtn->setAutoRepeat (true);
  mRightBtn->setAutoRepeatDelay (250);
  mRightBtn->setAutoRepeatInterval (50);
  connect (mRightBtn, &QToolButton::clicked, [this]() { scrollBy (+scrollStep()); });
  mRightAct = addWidget (mRightBtn);

  mScrollArea->viewport()->installEventFilter (this);
  w->installEventFilter (this);
  connect (mScrollArea->horizontalScrollBar(), &QScrollBar::valueChanged, this, &QTMToolbar::updateNavButtons);
  
  updateNavButtons();
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
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  addRightSpacer();
  updateNavButtons();
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
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
  addRightSpacer();
  updateNavButtons();
#endif
}

#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
void QTMToolbar::addSeparator () {
  QWidget* spacer = new QWidget (this);
  spacer->setFixedWidth (10);
  mLayout->addWidget (spacer);

  spacer = new QWidget (this);
  spacer->setFixedWidth (1);
  spacer->setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Preferred);
  spacer->setStyleSheet ("background: rgba(128,128,128,10%);");
  mLayout->addWidget (spacer);

  spacer = new QWidget (this);
  spacer->setFixedWidth (10);
  mLayout->addWidget (spacer);
}

void QTMToolbar::addSmallSeparator () {
  QWidget* spacer = new QWidget (this);
  spacer->setFixedWidth (3);
  mLayout->addWidget (spacer);
}

void QTMToolbar::addRightSpacer () {
  // a a spacer that will push the buttons to the left
  QWidget* spacer = new QWidget (this);
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
  } else {
#ifdef ENABLE_EXPERIMENTAL_TOOLBAR
    addSmallSeparator();
#endif
  }

  if (qobject_cast<QWidgetAction*> (action)) {
    actionWidget = qobject_cast<QWidgetAction*> (action)->requestWidget(this);
  }

  if (!actionWidget) {
    actionWidget = new QToolButton (this);
    ((QToolButton*)actionWidget)->setDefaultAction (action);
  }

  QToolButton* button = qobject_cast<QToolButton*> (actionWidget);
  if (button) {

    // if the action contains a icon, set a fixed icon size
    if (!action->icon().isNull()) {
      button->setIconSize (iconSize());
    }
    
#if QT_VERSION >= 0x060000
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
#else
    (void) button;
    if (action->menu()) {
      button->setPopupMode (QToolButton::InstantPopup);
    }
#endif
    
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
  mLayout->addWidget (actionWidget);
  updateNavButtons();
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
  updateNavButtons();
}

void QTMToolbar::clear () {
  while (mLayout->count() > 0) {
    QWidget* w = mLayout->itemAt(0)->widget();
    mLayout->removeWidget(w);
    delete w;
  }
  updateNavButtons();
}

int QTMToolbar::scrollStep () const {
  int byIcon = iconSize().isValid() ? iconSize().width() : 64;
  return byIcon;
}

void QTMToolbar::scrollBy (int dx) {
  if (!mScrollArea) return;
  QScrollBar* h = mScrollArea->horizontalScrollBar();
  if (!h) return;
  int v = h->value();
  int nv = qBound(h->minimum(), v + dx, h->maximum());
  if (nv != v) h->setValue(nv);
}

void QTMToolbar::updateNavButtons () {
  if (!mScrollArea || !mLeftBtn || !mRightBtn || !mLeftAct || !mRightAct) return;

  QWidget* content = mScrollArea->widget();
  if (!content) {
    mLeftBtn->setVisible(false);
    mRightBtn->setVisible(false);
    return;
  }

  const int contentW  = content->sizeHint().width();
  const int viewportW = mScrollArea->viewport()->width();

  const bool needScroll = contentW > viewportW;

  if (!needScroll) {
    mLeftAct->setVisible(false);
    mRightAct->setVisible(false);
    return;
  }
  
  QScrollBar* h = mScrollArea->horizontalScrollBar();
  const bool atLeft  = (h->value() <= h->minimum());
  const bool atRight = (h->value() >= h->maximum());

  mLeftAct->setVisible(!atLeft);
  mRightAct->setVisible(!atRight);
}

bool QTMToolbar::eventFilter (QObject* watched, QEvent* event) {
  if (!mScrollArea) return false;
  if (watched == mScrollArea->viewport() || watched == mScrollArea->widget()) {
    if (event->type() == QEvent::Resize) {
      updateNavButtons();
    }
    else if (event->type() == QEvent::Wheel) {
      // use vertical wheel to scroll horizontally
      QWheelEvent* we = static_cast<QWheelEvent*> (event);
      QPoint pixelDelta = we->pixelDelta();
      QPoint angleDelta = we->angleDelta();
      
      if (pixelDelta.y() != 0) {
        int dx = -pixelDelta.y();
        scrollBy (dx);
        return true;
      }
      if (angleDelta.y() != 0) {
        int dx = -angleDelta.y() / 120 * scrollStep();
        scrollBy (dx);
        return true;
      }
    }
  }
  return false;
}
#endif
