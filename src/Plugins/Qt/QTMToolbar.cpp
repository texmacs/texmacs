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
#include <QScroller>
#include <QScrollerProperties>

#define QTMTOOLBAR_MARGIN 2

QTMToolbar::QTMToolbar (const QString& title, QSize iconSize, QWidget* parent)
  : QToolBar (title, parent)
  , mScrollArea(nullptr)
  , mLayout(nullptr)
  , mLeftBtn(nullptr)
  , mRightBtn(nullptr)
  , mCurrentMenu(nullptr)
{  

  // strong focus
  setFocusPolicy (Qt::StrongFocus);

  if (!iconSize.isNull()) {
    setIconSize (iconSize);
    setFixedHeight (iconSize.height() + QTMTOOLBAR_MARGIN * 2);
  }

  setMovable (false);

  if (tmapp()->useNewToolbar()) {
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
  }
}

QTMToolbar::~QTMToolbar () {
}

void QTMToolbar::replaceActions (QList<QAction*>* src) {
  if (src == NULL)
    FAILED ("replaceActions expects valid objects");
  setUpdatesEnabled (false);

  if (tmapp()->useNewToolbar()) {
    while (mLayout->count() > 0) {
      QWidget* w = mLayout->itemAt(0)->widget();
      mLayout->removeWidget(w);
      delete w;
    }
  } else {
    clear ();
    addSeparator ();
  }
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);
  if (tmapp()->useNewToolbar()) {
    addRightSpacer();
    updateNavButtons();
  }
}

void QTMToolbar::replaceButtons (QList<QAction*>* src) {
  if (src == NULL)
    FAILED ("replaceButtons expects valid objects");
  setUpdatesEnabled (false);
  if (tmapp()->useNewToolbar()) {
    while (mLayout->count() > 0) {
      QWidget* w = mLayout->itemAt(0)->widget();
      mLayout->removeWidget(w);
      delete w;
    }
  } else {
    clear ();
    addSeparator ();
  }
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);
  if (tmapp()->useNewToolbar()) {
    addRightSpacer();
    updateNavButtons();
  }
}

void QTMToolbar::addSeparator () {
  if (!tmapp()->useNewToolbar()) {
    QToolBar::addSeparator();
    return;
  }
  QWidget* spacer = new QWidget (this);
  spacer->setFixedWidth (10);
  mLayout->addWidget (spacer);

  spacer = new QWidget (this);
  spacer->setFixedWidth (1);
  spacer->setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Preferred);
  spacer->setObjectName ("toolbarSeparator");
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

void QTMToolbar::addAction (QAction* action) {
  
  // create the tool button
  QWidget *actionWidget = nullptr;
  
  if (action->isSeparator()) {
    addSeparator();
    return;
  } else if (tmapp()->useNewToolbar()) {
    addSmallSeparator();
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
      QMenu *actionMenu = action->menu();
      connect (actionMenu, &QMenu::aboutToShow, [this, actionMenu]() {
        mCurrentMenu = actionMenu;
      });
      connect (actionMenu, &QMenu::aboutToHide, [this, actionMenu]() {
        if (mCurrentMenu == actionMenu) {
          mCurrentMenu = nullptr;
          QTMWidget::setFocusToLast();
        }
      });
      actionMenu->installEventFilter (this);
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
  if (tmapp()->useNewToolbar()) {
    mLayout->addWidget (actionWidget);
    updateNavButtons();
  } else {
    QToolBar::addWidget (actionWidget);
  }
}

void QTMToolbar::removeAction (QAction* action) {
  if (!tmapp()->useNewToolbar()) {
    QToolBar::removeAction(action);
    return;
  }
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
  if (!tmapp()->useNewToolbar()) {
    QToolBar::clear();
    return;
  }
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

void QTMToolbar::setRightActVisible (bool v) {
  if (v) {
    mRightAct->setEnabled(true);
    //mRightBtn->setText (QString::fromUtf8(">"));
    mRightBtn->setStyleSheet ("");
  } else {
    mRightAct->setEnabled(false);
    //mRightBtn->setText (QString::fromUtf8(""));
    mRightBtn->setStyleSheet ("color: transparent;");
  }
}

void QTMToolbar::setLeftActVisible (bool v) {
  if (v) {
    mLeftAct->setEnabled(true);
    //mLeftBtn->setText (QString::fromUtf8("<"));
    mLeftBtn->setStyleSheet ("");
  } else {
    mLeftAct->setEnabled(false);
    //mLeftBtn->setText (QString::fromUtf8(""));
    mLeftBtn->setStyleSheet ("color: transparent;");
  }
}

void QTMToolbar::updateNavButtons () {
  if (!mScrollArea || !mLeftBtn || !mRightBtn || !mLeftAct || !mRightAct) return;

  QWidget* content = mScrollArea->widget();
  if (!content) {
    setLeftActVisible(false);
    setRightActVisible(false);
    return;
  }

  const int contentW  = content->sizeHint().width();
  const int viewportW = mScrollArea->viewport()->width();

  const bool needScroll = contentW > viewportW;

  if (!needScroll) {
    setLeftActVisible(false);
    setRightActVisible(false);
    return;
  }
  
  QScrollBar* h = mScrollArea->horizontalScrollBar();
  const bool atLeft  = (h->value() <= h->minimum());
  const bool atRight = (h->value() >= h->maximum());

  setLeftActVisible(!atLeft);
  setRightActVisible(!atRight);
}

bool QTMToolbar::eventFilter (QObject* watched, QEvent* event) {
  if (!tmapp()->useNewToolbar()) return false;
  if (!mScrollArea) return false;

  QMenu *menu = qobject_cast<QMenu*> (watched);
  if (menu && mCurrentMenu && mCurrentMenu == menu && event->type() == QEvent::MouseMove) {

    QToolButton *currentButton = nullptr;
    // look for the button that opened the menu
    for (int i = 0; i < mLayout->count(); i++) {
      QToolButton* button = qobject_cast<QToolButton*>(mLayout->itemAt(i)->widget());
      if (!button) continue;
      QAction* action = button->defaultAction();
      if (!action) continue;
      if (action->menu() == menu) {
        currentButton = button;
        break;
      }
    }

    if (!currentButton) return false;

    // look if the mouse is hovering a QToolButton. for that, get all the children of the QTMToolbar
    QPoint globalPos = QCursor::pos();
    for (int i = 0; i < mLayout->count(); i++) {
      QToolButton* button = qobject_cast<QToolButton*>(mLayout->itemAt(i)->widget());
      if (!button) continue;
      // get the action of the button
      QAction* action = button->defaultAction();
      if (!action) continue;
      if (!action->menu()) continue;
      if (action->menu() == menu) continue;
      // if the mouse is hovering the button
      QPoint buttonPos = button->mapFromGlobal(globalPos);
      if (button->rect().contains(buttonPos)) {
        qDebug() << "QTMToolbar: hovering button with menu, closing current menu";
        
        // send a mouse click event outside to close the current menu
        QPoint outsidePos = menu->mapToGlobal(QPoint(-9999, -9999));
        QMouseEvent mePress (QEvent::MouseButtonPress, QPoint(-9999, -9999), outsidePos, Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
        QCoreApplication::sendEvent (menu, &mePress);
        QMouseEvent meRelease (QEvent::MouseButtonRelease, QPoint(-9999, -9999), outsidePos, Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
        QCoreApplication::sendEvent (menu, &meRelease);

        // send a mouse click event to the hovered button (with globalPos)
        QPoint buttonLocalPos = button->mapFromGlobal(globalPos);
        QMouseEvent bePress (QEvent::MouseButtonPress, buttonLocalPos, globalPos, Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
        QCoreApplication::sendEvent (button, &bePress);
        QMouseEvent beRelease (QEvent::MouseButtonRelease, buttonLocalPos, globalPos, Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
        QCoreApplication::sendEvent (button, &beRelease);

        mCurrentMenu = action->menu();
        return true;
      }
    }
  }

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
