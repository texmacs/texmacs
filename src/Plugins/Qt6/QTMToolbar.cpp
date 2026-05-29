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

#if QT_VERSION >= 0x050000
#include <QToolButton>
#include <QMenu>
#include <QWidgetAction>
#include <QFrame>
#include <QScrollBar>
#include <QEvent>
#include <QScroller>
#include <QScrollerProperties>
#endif // QT_VERSION >= 0x050000

QTMToolbar::QTMToolbar (const QString& title, QSize iconSize, QWidget* parent)
  : QWidget (parent)
{
  setObjectName ("qtmToolbar");
  
#if QT_VERSION >= 0x050000
  mToolbarMargin= 4;
  mIconWidth= iconSize.width();
  mIconHeight= iconSize.height();
#ifdef OS_ANDROID
  mToolbarMargin= 8;
#endif
  setFocusPolicy (Qt::StrongFocus);

  if (!iconSize.isNull()) {
    setIconSize (iconSize);
  }

  QHBoxLayout* layout = new QHBoxLayout (this);
  layout->setContentsMargins (0, 0, 0, 0);
  layout->setSpacing (0);
  setLayout (layout);
  
  mLeftBtn = new QPushButton (this);
  mLeftBtn->setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Expanding);
  mLeftBtn->setText (QString::fromUtf8("<"));
  mLeftBtn->setObjectName ("toolbarLeftButton");
  connect (mLeftBtn, &QToolButton::clicked, [this]() { scrollBy (-scrollStep()); });
  layout->addWidget (mLeftBtn);

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

  QScrollerProperties props = QScroller::scroller(mScrollArea->viewport())->scrollerProperties();
  props.setScrollMetric(QScrollerProperties::VerticalOvershootPolicy,   QScrollerProperties::OvershootAlwaysOff);
  props.setScrollMetric(QScrollerProperties::HorizontalOvershootPolicy, QScrollerProperties::OvershootAlwaysOff);
  QScroller::scroller(mScrollArea->viewport())->setScrollerProperties(props);
  QScroller::grabGesture (mScrollArea->viewport(), QScroller::LeftMouseButtonGesture);

  layout->addWidget (mScrollArea);

  mRightBtn = new QPushButton (this);
  mRightBtn->setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Expanding);
  mRightBtn->setText (QString::fromUtf8(">"));
  mRightBtn->setObjectName ("toolbarRightButton");
  connect (mRightBtn, &QToolButton::clicked, [this]() { scrollBy (+scrollStep()); });
  layout->addWidget (mRightBtn);

  mScrollArea->viewport()->installEventFilter (this);
  w->installEventFilter (this);
  connect (mScrollArea->horizontalScrollBar(), &QScrollBar::valueChanged, this, &QTMToolbar::updateNavButtons);
  
  updateNavButtons();
  
#endif // QT_VERSION >= 0x050000
}

QTMToolbar::~QTMToolbar () {
}

void QTMToolbar::setToolbarMargin (int margin) {
  if (margin < 0) margin = 0;
  if (mToolbarMargin == margin) return;
  mToolbarMargin = margin;
  updateToolbarMetrics ();
}

void QTMToolbar::setIconWidth (int width) {
  if (width < 0) width = 0;
  if (mIconWidth == width) return;
  mIconWidth = width;
  updateToolbarMetrics ();
}

void QTMToolbar::setIconHeight (int height) {
  if (height < 0) height = 0;
  if (mIconHeight == height) return;
  mIconHeight = height;
  updateToolbarMetrics ();
}

void
QTMToolbar::updateToolbarMetrics () {
  QSize iconSize (mIconWidth, mIconHeight);
  int requiredHeight = mIconHeight > 0 ? mIconHeight + mToolbarMargin * 2 : 0;

  if (mLayout) {
    for (int i = 0; i < mLayout->count(); i++) {
      QWidget* actionWidget = mLayout->itemAt(i)->widget();
      QToolButton* button = qobject_cast<QToolButton*> (actionWidget);
      if (!button) continue;
      button->setIconSize (iconSize);
      if (button->icon().isNull()) {
        button->setContentsMargins (mToolbarMargin, mToolbarMargin,
                                    mToolbarMargin, mToolbarMargin);
      }
      requiredHeight = qMax (requiredHeight,
                             button->sizeHint().height() + mToolbarMargin * 2);
    }
  }

  if (requiredHeight > 0) {
    setFixedHeight (requiredHeight);
  }
}

void QTMToolbar::replaceActions (QList<QAction*>* src) {
#if QT_VERSION >= 0x050000
  if (src == NULL)
    FAILED ("replaceActions expects valid objects");
  setUpdatesEnabled (false);

  while (mLayout && mLayout->count() > 0) {
    QWidget* w = mLayout->itemAt(0)->widget();
    mLayout->removeWidget(w);
    w->hide();
    w->setParent(nullptr);
    w->deleteLater();
  }
  
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);
  
  addRightSpacer();
  updateNavButtons();
  
#endif // QT_VERSION >= 0x050000
}

void QTMToolbar::replaceButtons (QList<QAction*>* src) {
#if QT_VERSION >= 0x050000
  if (src == NULL)
    FAILED ("replaceButtons expects valid objects");
  setUpdatesEnabled (false);

  while (mLayout && mLayout->count() > 0) {
    QWidget* w = mLayout->itemAt(0)->widget();
    mLayout->removeWidget(w);
    w->hide();
    w->setParent(nullptr);
    w->deleteLater();
  }
  
  for (int i = 0; i < src->count(); i++) {
    QAction* a = (*src)[i];
    addAction(a);
  }
  setUpdatesEnabled (true);

  addRightSpacer();
  updateNavButtons();
  
#endif // QT_VERSION >= 0x050000
}

void QTMToolbar::addSeparator () {
#if QT_VERSION >= 0x050000

  if (!mLayout) return;

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
#else
  QToolBar::addSeparator();
#endif // QT_VERSION >= 0x050000
}

void QTMToolbar::addSmallSeparator () {
#if QT_VERSION >= 0x050000
  if (!mLayout) return;
  QWidget* spacer = new QWidget (this);
  spacer->setFixedWidth (3);
  mLayout->addWidget (spacer);
#endif
}

void QTMToolbar::addRightSpacer () {
#if QT_VERSION >= 0x050000
  if (!mLayout) return;
  // a a spacer that will push the buttons to the left
  QWidget* spacer = new QWidget (this);
  spacer->setSizePolicy (QSizePolicy::Expanding, QSizePolicy::Preferred);
  mLayout->addWidget (spacer);
#endif
}

void QTMToolbar::addAction (QAction* action) {
#if QT_VERSION >= 0x050000
  // create the tool button
  QWidget *actionWidget = nullptr;
  
  if (action->isSeparator()) {
    addSeparator();
    return;
  } else {
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
    button->setIconSize (QSize (mIconWidth, mIconHeight));
    
#if QT_VERSION >= 0x050000
    // on click finish, set the focus to the last focused widget
    connect (button, &QToolButton::clicked, []() {
      QTMWidget::setFocusToLast();
    });

    // if the action is a menu, the tool button should be a menu button
    if (action->menu()) {
      button->setPopupMode (QToolButton::InstantPopup);
      QMenu *actionMenu = action->menu();
      QPointer<QMenu> safeMenu = actionMenu;
      QPointer<QToolButton> safeButton = button;
      connect (actionMenu, &QMenu::aboutToShow, this, [this, safeMenu, safeButton]() {
        if (!safeMenu || !safeButton) return;
        mCurrentMenu = safeMenu;
        resetAllButtons(safeButton);
      });
      connect (actionMenu, &QMenu::aboutToHide, this, [this, safeMenu, safeButton]() {
        if (!safeMenu) return;
        if (mCurrentMenu == safeMenu) {
          mCurrentMenu = nullptr;
          QTMWidget::setFocusToLast();
        }
        if (!safeButton) return;
        QMetaObject::invokeMethod (this, [this, safeButton]() {
          if (!safeButton) return;
          resetButton(safeButton);
        }, Qt::QueuedConnection);
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
      button->setContentsMargins (mToolbarMargin, mToolbarMargin,
                                  mToolbarMargin, mToolbarMargin);
    }
    
    // if the fixed height is lower than the required height, set the fixed height
    int requiredHeight = button->sizeHint().height() + mToolbarMargin * 2;
    if (height() < requiredHeight) {
     // setFixedHeight (requiredHeight);
    }
  }
  
  // add the button to the toolbar, and on Android to the scrollable layout
  if (mLayout) {
    actionWidget->setSizePolicy (QSizePolicy::Preferred, QSizePolicy::Expanding);
    mLayout->addWidget (actionWidget);
    updateNavButtons();
  }
#else
  QToolBar::addAction (action);
#endif // QT_VERSION >= 0x050000
}

void QTMToolbar::removeAction (QAction* action) {
#if QT_VERSION >= 0x050000
  if (!mLayout) {
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
#else
  QToolBar::removeAction(action);
#endif // QT_VERSION >= 0x050000
}

void QTMToolbar::clear () {
#if QT_VERSION >= 0x050000
  if (!mLayout) {
    //QToolBar::clear();
    return;
  }
  while (mLayout->count() > 0) {
    QWidget* w = mLayout->itemAt(0)->widget();
    mLayout->removeWidget(w);
    w->hide();
    w->setParent(nullptr);
    w->deleteLater();
  }
  updateNavButtons();
#else
  QToolBar::clear();
#endif // QT_VERSION >= 0x050000
}

int QTMToolbar::scrollStep () const {
#if QT_VERSION >= 0x050000
  ///int byIcon = iconSize().isValid() ? iconSize().width() : 64;
  int byIcon = 64;
  return byIcon;
#else
  return 0;
#endif
}

void QTMToolbar::scrollBy (int dx) {
#if QT_VERSION >= 0x050000
  if (!mScrollArea) return;
  QScrollBar* h = mScrollArea->horizontalScrollBar();
  if (!h) return;
  int v = h->value();
  int nv = qBound(h->minimum(), v + dx, h->maximum());
  if (nv != v) h->setValue(nv);
#endif
}

void QTMToolbar::setRightActVisible (bool v) {
#if QT_VERSION >= 0x050000
  if (!mRightBtn) return;
  if (v) {
    mRightBtn->setEnabled(true);
    mRightBtn->setText (QString::fromUtf8(">"));
    mRightBtn->setProperty("tmenabled", true);
  } else {
    mRightBtn->setEnabled(false);
    mRightBtn->setText ("");
    mRightBtn->setProperty("tmenabled", false);
  }
#endif
}

void QTMToolbar::setLeftActVisible (bool v) {
#if QT_VERSION >= 0x050000
  if (!mLeftBtn) return;
  if (v) {
    mLeftBtn->setEnabled(true);
    mLeftBtn->setText (QString::fromUtf8("<"));
    mLeftBtn->setProperty("tmenabled", true);
  } else {
    mLeftBtn->setEnabled(false);
    mLeftBtn->setText ("");
    mLeftBtn->setProperty("tmenabled", false);
  }
#endif // QT_VERSION >= 0x050000
}

void QTMToolbar::updateNavButtons () {
#if QT_VERSION >= 0x050000
  if (!mScrollArea || !mLeftBtn || !mRightBtn) return;

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
#endif // QT_VERSION >= 0x050000
}

bool QTMToolbar::eventFilter (QObject* watched, QEvent* event) {
#if QT_VERSION >= 0x050000
  if (!mScrollArea || !mLayout) return false;

#ifndef OS_ANDROID
  QMenu *menu = qobject_cast<QMenu*> (watched);
  if (menu && mCurrentMenu == menu && event->type() == QEvent::MouseMove) {

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
#endif

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
#endif // QT_VERSION >= 0x050000
  return false;
}

QList<QTMToolbar*> QTMToolbar::getAllToolbarsFromMainWindow () const {
  QList<QTMToolbar*> toolbars;
#if QT_VERSION >= 0x050000
  QWidget* mainWindow = parentWidget(); // todo
  if (!mainWindow) return toolbars;
  QList<QToolBar*> allToolbars = mainWindow->findChildren<QToolBar*>();
  for (QToolBar* tb : allToolbars) {
    QTMToolbar* tmtb = qobject_cast<QTMToolbar*>(tb);
    if (tmtb) {
      toolbars.append(tmtb);
    }
  }
#endif // QT_VERSION >= 0x050000
  return toolbars;
}

QList<QToolButton*> QTMToolbar::getAllButtonsFromAllToolbars () const {
  QList<QToolButton*> buttons;
#if QT_VERSION >= 0x050000
  QList<QTMToolbar*> toolbars = getAllToolbarsFromMainWindow();
  for (QTMToolbar* tb : toolbars) {
    if (!tb->mLayout) continue;
    for (int i = 0; i < tb->mLayout->count(); i++) {
      QToolButton* button = qobject_cast<QToolButton*>(tb->mLayout->itemAt(i)->widget());
      if (button) {
        buttons.append(button);
      }
    }
  }
#endif // QT_VERSION >= 0x050000
  return buttons;
}

void QTMToolbar::resetAllButtons(QToolButton* except) {
#if QT_VERSION >= 0x050000
  QList<QToolButton*> buttons = getAllButtonsFromAllToolbars();
  for (QToolButton* button : buttons) {
    if (button == except) continue;
    resetButton(button);
  }
#endif // QT_VERSION >= 0x050000
}

void QTMToolbar::resetButton(QToolButton* button) {
#if QT_VERSION >= 0x050000
  if (!button) return;
  button->setDown (false);
  button->setAttribute(Qt::WA_UnderMouse, false);
#endif // QT_VERSION >= 0x050000
}

/*
QMenu* QTMToolbar::currentMenu () const {
  QWidget *activePopup = QApplication::activePopupWidget();
  if (!activePopup) return nullptr;
  QMenu *menu = qobject_cast<QMenu*> (activePopup);
  if (!menu) return nullptr;
  return menu;
}
  */
void QTMToolbar::setIconSize(const QSize& size) {
  if (size.width() >= 0) mIconWidth = size.width();
  if (size.height() >= 0) mIconHeight = size.height();
  updateToolbarMetrics ();
}