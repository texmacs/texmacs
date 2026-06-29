/******************************************************************************
* MODULE     : QTMResponsiveTabWidget.cpp
* DESCRIPTION: A responsive tab widget that can adapts to different 
               screen sizes and orientations.
* COPYRIGHT  : (C) 2026 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMResponsiveTabWidget.hpp"
#include "QTMMainTabWindow.hpp"

#include "string.hpp"
#include "boot.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QStackedWidget>
#include <QListWidget>
#include <QPushButton>
#include <QLabel>
#include <QGraphicsDropShadowEffect>
#include <QMouseEvent>
#include <QCursor>
#include <QResizeEvent>
#include <QSizeGrip>
#include <QFontMetrics>
#include <QGridLayout>
#include <QIcon>
#include <QScrollArea>
#include <QScroller>

#include <functional>

int get_default_responsive_mode() {
  string mode= get_user_preference ("gui:responsive tab mode", "default");
  if (mode == "top") return 0;
  if (mode == "side") return 1;
  if (mode == "mobile") return 2;
  if (mode == "grid") return 3;

#ifdef OS_ANDROID
  return 2;
#endif

  return 1;
}

QTMHorizontalTextTabBar::QTMHorizontalTextTabBar(QWidget* parent) 
  : QTabBar(parent) {
  setMovable(true);
  setCursor(Qt::PointingHandCursor);

  connect(this, &QTabBar::currentChanged, 
          this, &QTMHorizontalTextTabBar::updateLabelColors);
  connect(this, &QTabBar::tabMoved, 
          this, &QTMHorizontalTextTabBar::onTabMovedNoArgs);
}

void QTMHorizontalTextTabBar::onTabMovedNoArgs() {
  updateLabelColors(currentIndex());
}

int QTMHorizontalTextTabBar::addCustomTab(const QIcon &icon, 
                                          const QString &text) {
  int index = QTabBar::addTab("");

  QWidget* vwidget = new QWidget(this);
  vwidget->setObjectName("ResponsiveTabContainer");
  vwidget->setAttribute(Qt::WA_TransparentForMouseEvents);
  
  QVBoxLayout* vlayout = new QVBoxLayout(vwidget);
  vlayout->setContentsMargins(0, 0, 0, 0);
  vlayout->setSpacing(0);

  QWidget *hwidget = new QWidget(vwidget);
  hwidget->setObjectName("ResponsiveTabInnerContainer");
  
  QHBoxLayout* hlayout = new QHBoxLayout(hwidget);
  hlayout->setContentsMargins(10, 0, 10, 0);

  QLabel* iconLabel = new QLabel(hwidget);
  iconLabel->setObjectName("ResponsiveTabIcon");
  if (!icon.isNull()) iconLabel->setPixmap(icon.pixmap(18, 18));
  iconLabel->setFixedSize(18, 18);
  iconLabel->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
  hlayout->addWidget(iconLabel);
  hlayout->addStretch();

  QLabel* textLabel = new QLabel(text, hwidget);
  textLabel->setObjectName("ResponsiveTabLabel");
  textLabel->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  hlayout->addWidget(textLabel);

  vlayout->addStretch();
  vlayout->addWidget(hwidget);
  vlayout->addStretch();

  hwidget->setMinimumWidth(110);
  vwidget->setFixedHeight(35);

  setTabButton(index, QTabBar::LeftSide, vwidget);
  updateLabelColors(currentIndex());
  return index;
}

void QTMHorizontalTextTabBar::setCustomTabIcon(int index, const QIcon &icon) {
  if (icon.isNull()) return;
  QPointer<QWidget> vwidget = tabButton(index, QTabBar::LeftSide);
  if (vwidget) {
    QPointer<QLabel> iconLabel = vwidget->findChild<QLabel*>("ResponsiveTabIcon");
    if (iconLabel) {
      iconLabel->setPixmap(icon.pixmap(18, 18));
    }
  }
}

QSize QTMHorizontalTextTabBar::tabSizeHint(int index) const {
  QPointer<QWidget> btn = tabButton(index, QTabBar::LeftSide);
  if (btn) return btn->size();
  return QTabBar::tabSizeHint(index);
}

void QTMHorizontalTextTabBar::updateLabelColors(int index) {
  for (int i = 0; i < count(); ++i) {
    QPointer<QWidget> vwidget = tabButton(i, QTabBar::LeftSide);
    if (vwidget) {
      QPointer<QLabel> label = vwidget->findChild<QLabel*>("ResponsiveTabLabel");
      if (label) {
        label->setProperty("activeTab", i == index);
        label->style()->unpolish(label);
        label->style()->polish(label);
      }
    }
  }

  style()->unpolish(this);
  style()->polish(this);
}

QTMDraggableTopBar::QTMDraggableTopBar(QWidget* parent) 
  : QFrame(parent), mDraggable(false) {
  setObjectName("TopBar");
}

void QTMDraggableTopBar::mousePressEvent(QMouseEvent *event) {
  cout << "TopBar mousePressEvent, draggable: " << mDraggable 
       << ", button: " << event->button() << LF;
  if (mDraggable && event->button() == Qt::LeftButton) {
    mClickPos = event->pos();
  }
  QFrame::mousePressEvent(event);
}

void QTMDraggableTopBar::mouseMoveEvent(QMouseEvent *event) {
  if (mDraggable && (event->buttons() & Qt::LeftButton)) {
#if QT_VERSION >= 0x060000
    const QPoint globalPos = event->globalPosition().toPoint();
#else
    const QPoint globalPos = event->globalPos();
#endif
    cout << "Dragging window, cursor pos: " << globalPos.x()
         << ", " << globalPos.y() << LF;
    window()->move(QCursor::pos() - mClickPos);
  }
  QFrame::mouseMoveEvent(event);
}

QTMResponsiveTabWidget::QTMResponsiveTabWidget(QWidget *parent)
  : QWidget(parent), mCurrentGridCols(-1), mMobileViewingContent(true),
    mIsUpdating(false), mWindowFusion(false), mIsResizing(false),
    mCurrentMode(-1), mCurrentDepth(-1) {

  setAttribute(Qt::WA_StyledBackground, true);

  mAddTabBtn = new QPushButton("+ Nouveau", this);
  mAddTabBtn->setObjectName("AddTabBtn");
  mAddTabBtn->hide();

  mTabBar = new QTMHorizontalTextTabBar(this);
  mTabBar->setObjectName("ResponsiveTabWidgetBar");
  mListWidget = new QListWidget(this);
  mListWidget->setObjectName("ResponsiveTabWidgetList");
  
  mContentStack = new QStackedWidget(this);
  mContentStack->setObjectName("ContentStack");

  mGridContainer = new QWidget(this);
  mGridContainer->setObjectName("GridContainer");
  mGridLayout = new QGridLayout(mGridContainer);
  mGridLayout->setContentsMargins(10, 10, 10, 10);
  mGridLayout->setSpacing(20);
  mGridContainer->hide();

  mTopBar = new QTMDraggableTopBar(this);
  mTopLayout = new QHBoxLayout(mTopBar);
  mTopLayout->setContentsMargins(0, 0, 0, 0);
  mTopLayout->setSpacing(0);

  mTopLayout->addWidget(mAddTabBtn);
  mTopLayout->addStretch();

  QVBoxLayout* mainLayout = new QVBoxLayout(this);
  mainLayout->setContentsMargins(0, 0, 0, 0);
  mainLayout->setSpacing(0);

  mDynamicLayout = new QBoxLayout(QBoxLayout::TopToBottom);
  mDynamicLayout->setContentsMargins(0, 0, 0, 0);
  mDynamicLayout->setSpacing(0); 

  mDynamicLayout->addWidget(mListWidget);
  mDynamicLayout->addWidget(mContentStack, 1);
  mDynamicLayout->addWidget(mGridContainer, 1);

  mainLayout->addWidget(mTopBar);
  mainLayout->addLayout(mDynamicLayout);

  mSizeGrip = new QSizeGrip(this);
  mSizeGrip->setObjectName("ResponsiveTabSizeGrip");
  mSizeGrip->hide();

  connect(mTabBar, &QTabBar::currentChanged, 
          this, &QTMResponsiveTabWidget::onTabSelected);
  connect(mListWidget, &QListWidget::itemClicked, 
      this, &QTMResponsiveTabWidget::onListItemClicked);
  connect(mTabBar, &QTabBar::tabMoved, 
          this, &QTMResponsiveTabWidget::onTabMoved);
  connect(mAddTabBtn, &QPushButton::clicked, 
          this, &QTMResponsiveTabWidget::newTabRequested);

  int default_mode= get_default_responsive_mode ();
  setProperty("tmmode", default_mode);
  mMobileViewingContent= (default_mode != 2);
  if (default_mode == 2 && mListWidget) mListWidget->clearSelection();
  applyMode(default_mode);  
}

QTMResponsiveTabWidget::~QTMResponsiveTabWidget() {
  if (QTMMainTabWindow* mainTab = qobject_cast<QTMMainTabWindow*>(window())) {
    mainTab->unregisterBackButtonProvider(this);
  }
}

void QTMResponsiveTabWidget::showEvent(QShowEvent* event) {
  QWidget::showEvent(event);
  updateNestingVisuals();
  reevaluateBackButton();
}

void QTMResponsiveTabWidget::hideEvent(QHideEvent* event) {
  QWidget::hideEvent(event);
  if (QTMMainTabWindow* mainTab = qobject_cast<QTMMainTabWindow*>(window())) {
    mainTab->setBackButtonProviderVisible(this, false);
  }
}

void QTMResponsiveTabWidget::updateNestingVisuals() {
    int depth = 0;
    QWidget* p = this->parentWidget();
    while (p) {
        if (qobject_cast<QTMResponsiveTabWidget*>(p)) depth++;
        p = p->parentWidget();
    }

    if (depth == mCurrentDepth) return;
    mCurrentDepth = depth;

    setObjectName(QString("QTMResponsiveTabWidgetDepth%1").arg(depth));

    style()->unpolish(this);
    style()->polish(this);

    QList<QWidget*> childrenToPolish = { 
        mTopBar, mTabBar, mContentStack, mGridContainer, mListWidget 
    };
    
    for (QWidget* child : childrenToPolish) {
        if (child) {
            child->style()->unpolish(child);
            child->style()->polish(child);
        }
    }

    if (mTabBar && mTabBar->count() > 0) {
        if (auto* customTabBar = qobject_cast<QTMHorizontalTextTabBar*>(mTabBar)) {
            customTabBar->updateLabelColors(customTabBar->currentIndex());
        }
    }

    update();
}

void QTMResponsiveTabWidget::addTab(QWidget* widget, const QString& title, 
                                    const QIcon& icon) {

  if (!widget) return;

  bool containsResponsive = false;
  bool strechToTop = false;
  QList<QWidget*> allChildren = widget->findChildren<QWidget*>();
  for (QWidget* child : allChildren) {
    if (qobject_cast<QTMResponsiveTabWidget*>(child)) {
      containsResponsive = true;
      break;
    }
    if (child->property("stretchToTop").toBool()) {
      strechToTop = true;
    }
  }
  
  if (!qobject_cast<QTMResponsiveTabWidget*>(widget) && !containsResponsive) {
    QWidget *top = new QWidget(this);
    top->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QVBoxLayout *layout = new QVBoxLayout(top);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(widget);
    if (strechToTop) {
      layout->addStretch();
    }
    top->setLayout(layout);

  
    // we are going to do better : a vertical scroll !
    QScrollArea *scroll = new QScrollArea(this);
    scroll->setWidgetResizable(true);
    scroll->setFrameShape(QFrame::NoFrame);
    scroll->setWidget(top);
    scroll->viewport()->setAttribute(Qt::WA_AcceptTouchEvents, true);

    QScroller *scroller = QScroller::scroller(scroll->viewport());
    scroller->grabGesture(scroll->viewport(), QScroller::LeftMouseButtonGesture);
    QScrollerProperties props = scroller->scrollerProperties();
    props.setScrollMetric(QScrollerProperties::MousePressEventDelay, 0.15);
    scroller->setScrollerProperties(props);

    widget = scroll;
  }

  mPages.append(widget);

  if (mCurrentMode == 3) {
    int cols = (width() > 1700) ? 3 : 2;
    int i = mPages.size() - 1;
    if (mGridLayout) {
      mGridLayout->addWidget(widget, i / cols, i % cols);
      widget->show();
    }
  } else {
    if (mContentStack) {
        mContentStack->addWidget(widget);
    }
  }

  if (mTabBar) mTabBar->addCustomTab(icon, title);
  if (mListWidget) {
      QListWidgetItem* item = new QListWidgetItem(icon, title);
      mListWidget->addItem(item);
  }
}

void QTMResponsiveTabWidget::setTabIcon(int index, const QIcon& icon) {
  if (mTabBar && index >= 0 && index < count()) {
      mTabBar->setCustomTabIcon(index, icon);
  }
  if (mListWidget && index >= 0 && index < mListWidget->count()) {
      QListWidgetItem* item = mListWidget->item(index);
      if (item) item->setIcon(icon);
  }
}

int QTMResponsiveTabWidget::count() const { 
    return mTabBar ? mTabBar->count() : 0; 
}

void QTMResponsiveTabWidget::setCurrentIndex(int index) {
  if (mTabBar && index >= 0 && index < count()) {
      mTabBar->setCurrentIndex(index);
  }
}

int QTMResponsiveTabWidget::currentIndex() const { 
  return mTabBar ? mTabBar->currentIndex() : -1; 
}

void QTMResponsiveTabWidget::setAddButtonVisible(bool visible) { 
  if (mAddTabBtn) mAddTabBtn->setVisible(visible); 
}

void QTMResponsiveTabWidget::setDraggable(bool draggable) { 
  if (mTopBar) mTopBar->setDraggable(draggable); 
}

void QTMResponsiveTabWidget::applyMode(int mode) {
  mCurrentMode = mode;
  
  if (mTabBar) mTabBar->hide();
  if (mListWidget) mListWidget->hide();
  if (mContentStack) mContentStack->hide();
  if (mGridContainer) mGridContainer->hide();

  if (mTopLayout && mTabBar) mTopLayout->removeWidget(mTabBar);
  if (mDynamicLayout && mTabBar) mDynamicLayout->removeWidget(mTabBar);

  if (mode == 3) {
    int cols = (width() > 1700) ? 3 : 2;
    for (int i = 0; i < mPages.size(); ++i) {
      if (QWidget* w = mPages[i]) {
        if (mContentStack) mContentStack->removeWidget(w);
        if (mGridLayout) {
            mGridLayout->removeWidget(w);
            mGridLayout->addWidget(w, i / cols, i % cols);
        }
        w->show();
      }
    }
  } else {
    for (int i = 0; i < mPages.size(); ++i) {
      if (QWidget* w = mPages[i]) {
        if (mGridLayout) mGridLayout->removeWidget(w);
        if (mContentStack) {
            mContentStack->removeWidget(w);
            mContentStack->insertWidget(i, w);
        }
      }
    }
    if (!mPages.isEmpty() && mContentStack && mTabBar) {
      mContentStack->setCurrentIndex(mTabBar->currentIndex());
    }
  }

  if (mode == 3) {
    if (mGridContainer) mGridContainer->show();
    mMobileViewingContent = true;
  }
  else if (mode == 0) {
    if (mTopLayout && mTabBar) mTopLayout->insertWidget(2, mTabBar);
    if (mDynamicLayout) mDynamicLayout->setDirection(QBoxLayout::TopToBottom);
    if (mTabBar) {
        mTabBar->setShape(QTabBar::RoundedNorth);
        mTabBar->show();
    }
    if (mContentStack) mContentStack->show();
    mMobileViewingContent = true;
  }
  else if (mode == 1) {
    if (mDynamicLayout && mTabBar) {
        mDynamicLayout->insertWidget(0, mTabBar, 0, Qt::AlignTop); 
        mDynamicLayout->setDirection(QBoxLayout::LeftToRight);
    }
    if (mTabBar) {
        mTabBar->setShape(QTabBar::RoundedWest);
        mTabBar->show();
    }
    if (mContentStack) mContentStack->show();
    mMobileViewingContent = true;
  }
  else if (mode == 2) {
    if (mDynamicLayout) mDynamicLayout->setDirection(QBoxLayout::TopToBottom);
    if (mMobileViewingContent) {
      if (mContentStack) mContentStack->show();
    } else {
      if (mListWidget) mListWidget->show();
    }
  }
  
  if (mTabBar) mTabBar->updateGeometry();
  reevaluateBackButton();
}

void QTMResponsiveTabWidget::onTabSelected(int index) {
  if (index < 0 || mIsUpdating) return;
  mIsUpdating = true;
  if (mContentStack) mContentStack->setCurrentIndex(index);
  if (mListWidget) mListWidget->setCurrentRow(index);
  mIsUpdating = false;
  reevaluateBackButton();
}

void QTMResponsiveTabWidget::onListItemClicked(QListWidgetItem *item) {
  if (!mListWidget) return;
  onListSelected(mListWidget->row(item));
}

void QTMResponsiveTabWidget::onListSelected(int index) {
  if (index < 0 || mIsUpdating) return;
  mIsUpdating = true;
  if (mContentStack) mContentStack->setCurrentIndex(index);
  if (mTabBar) mTabBar->setCurrentIndex(index);
  mIsUpdating = false;

  if (mCurrentMode == 2) {
    mMobileViewingContent = true;
    applyMode(2);
  } else {
    reevaluateBackButton();
  }
}

void QTMResponsiveTabWidget::onBackClicked() {
  goBack();
}

void QTMResponsiveTabWidget::onTabMoved(int from, int to) {
  if (mIsUpdating) return;
  mIsUpdating = true;

  mPages.move(from, to);

  if (mContentStack) {
      QWidget* widget = mContentStack->widget(from);
      if (widget) {
          mContentStack->removeWidget(widget);
          mContentStack->insertWidget(to, widget);
      }
  }

  if (mListWidget) {
      QListWidgetItem* item = mListWidget->takeItem(from);
      if (item) {
          mListWidget->insertItem(to, item);
      }
  }

  if (mContentStack && mTabBar) mContentStack->setCurrentIndex(mTabBar->currentIndex());
  if (mListWidget && mTabBar) mListWidget->setCurrentRow(mTabBar->currentIndex());
  mIsUpdating = false;
}

QTMResponsiveTabWidget* QTMResponsiveTabWidget::getParentTabWidget() const {
  QWidget* p = this->parentWidget();
  while (p) {
    if (QTMResponsiveTabWidget* tab = qobject_cast<QTMResponsiveTabWidget*>(p)) {
      return tab;
    }
    p = p->parentWidget();
  }
  return nullptr;
}

bool QTMResponsiveTabWidget::needsBackButton() const {
  if (mContentStack && !mContentStack->isHidden()) {
    if (QWidget* currentContent = mContentStack->currentWidget()) {
      QList<QTMResponsiveTabWidget*> childTabs = 
          currentContent->findChildren<QTMResponsiveTabWidget*>();
      if (QTMResponsiveTabWidget* directChild = 
              qobject_cast<QTMResponsiveTabWidget*>(currentContent)) {
        if (!childTabs.contains(directChild)) childTabs.append(directChild);
      }
      for (QTMResponsiveTabWidget* child : childTabs) {
        if (child->needsBackButton()) return true;
      }
    }
  }
  
  if (mCurrentMode == 3) {
    for (const QPointer<QWidget>& ptr : mPages) {
      if (QWidget* w = ptr) {
        QList<QTMResponsiveTabWidget*> childTabs = 
            w->findChildren<QTMResponsiveTabWidget*>();
        if (QTMResponsiveTabWidget* directChild = 
                qobject_cast<QTMResponsiveTabWidget*>(w)) {
          if (!childTabs.contains(directChild)) childTabs.append(directChild);
        }
        for (QTMResponsiveTabWidget* child : childTabs) {
          if (child->needsBackButton()) return true;
        }
      }
    }
  }

  return (mCurrentMode == 2 && mMobileViewingContent);
}

void QTMResponsiveTabWidget::reevaluateBackButton() {
  QTMResponsiveTabWidget* parentTab = getParentTabWidget();
  if (parentTab) {
    parentTab->reevaluateBackButton();
  } else {
    if (QTMMainTabWindow* mainTab = qobject_cast<QTMMainTabWindow*>(window())) {
      mainTab->registerBackButtonProvider(
          this, std::bind(&QTMResponsiveTabWidget::onBackClicked, this));
      mainTab->setBackButtonProviderVisible(this, needsBackButton());
    }
  }
}

bool QTMResponsiveTabWidget::goBack() {
  if (mContentStack && !mContentStack->isHidden()) {
    if (QWidget* currentContent = mContentStack->currentWidget()) {
      QList<QTMResponsiveTabWidget*> childTabs = 
          currentContent->findChildren<QTMResponsiveTabWidget*>();
      if (QTMResponsiveTabWidget* directChild = 
              qobject_cast<QTMResponsiveTabWidget*>(currentContent)) {
        if (!childTabs.contains(directChild)) childTabs.append(directChild);
      }
      for (QTMResponsiveTabWidget* child : childTabs) {
        if (child->goBack()) {
          reevaluateBackButton();
          return true;
        }
      }
    }
  }
  
  if (mCurrentMode == 3) {
    for (const QPointer<QWidget>& ptr : mPages) {
      if (QWidget* w = ptr) {
        QList<QTMResponsiveTabWidget*> childTabs = 
            w->findChildren<QTMResponsiveTabWidget*>();
        if (QTMResponsiveTabWidget* directChild = 
                qobject_cast<QTMResponsiveTabWidget*>(w)) {
          if (!childTabs.contains(directChild)) childTabs.append(directChild);
        }
        for (QTMResponsiveTabWidget* child : childTabs) {
          if (child->goBack()) {
            reevaluateBackButton();
            return true;
          }
        }
      }
    }
  }

  if (mCurrentMode == 2 && mMobileViewingContent) {
    mMobileViewingContent = false;
    if (mListWidget) {
        mListWidget->clearSelection();
        mListWidget->setCurrentRow(-1);
    }
    applyMode(2);
    return true;
  }
  return false;
}

void QTMResponsiveTabWidget::changeEvent(QEvent *event) {
    QWidget::changeEvent(event);
    
    // Recalculate depth immediately if this widget is moved to a new parent
    if (event->type() == QEvent::ParentChange) {
        updateNestingVisuals();
    reevaluateBackButton();
    }
}