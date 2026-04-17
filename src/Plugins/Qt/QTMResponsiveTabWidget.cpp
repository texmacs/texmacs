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

#include "string.hpp"

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

QTMHorizontalTextTabBar::QTMHorizontalTextTabBar(QWidget* parent) 
  : QTabBar(parent) {
  setMovable(true);
  setCursor(Qt::PointingHandCursor);

  connect(this, &QTabBar::currentChanged, 
          this, &QTMHorizontalTextTabBar::updateLabelColors);
  connect(this, &QTabBar::tabMoved, 
          this, [this]() { updateLabelColors(currentIndex()); });
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

  if (!icon.isNull()) {
    QLabel* iconLabel = new QLabel(hwidget);
    iconLabel->setObjectName("ResponsiveTabIcon");
    iconLabel->setPixmap(icon.pixmap(18, 18));
    iconLabel->setFixedSize(18, 18);
    iconLabel->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
    hlayout->addWidget(iconLabel);
    hlayout->addStretch();
  }

  QLabel* textLabel = new QLabel(text, hwidget);
  textLabel->setObjectName("ResponsiveTabLabel");
  textLabel->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  hlayout->addWidget(textLabel);

  vlayout->addStretch();
  vlayout->addWidget(hwidget);
  vlayout->addStretch();

  hwidget->setFixedWidth(110);
  vwidget->setFixedHeight(35);

  setTabButton(index, QTabBar::LeftSide, vwidget);
  updateLabelColors(currentIndex());
  return index;
}

QSize QTMHorizontalTextTabBar::tabSizeHint(int index) const {
  QWidget* btn = tabButton(index, QTabBar::LeftSide);
  if (btn) return btn->size();
  return QTabBar::tabSizeHint(index);
}

void QTMHorizontalTextTabBar::updateLabelColors(int index) {
  for (int i = 0; i < count(); ++i) {
    QWidget* vwidget = tabButton(i, QTabBar::LeftSide);
    if (vwidget) {
      QLabel* label = vwidget->findChild<QLabel*>("ResponsiveTabLabel");
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
    cout << "Dragging window, cursor pos: " << event->globalPos().x() 
         << ", " << event->globalPos().y() << LF;
    window()->move(QCursor::pos() - mClickPos);
  }
  QFrame::mouseMoveEvent(event);
}

QTMResponsiveTabWidget::QTMResponsiveTabWidget(QWidget *parent)
  : QWidget(parent), mMobileViewingContent(true), mIsUpdating(false),
    mWindowFusion(false), mIsResizing(false), mCurrentMode(-1),
    mCurrentDepth(-1), mCurrentGridCols(-1) {

  setAttribute(Qt::WA_StyledBackground, true);
  setMinimumSize(320, 300);

  mBackBtn = new QPushButton("← Retour", this);
  mBackBtn->setObjectName("BackButton");
  mBackBtn->hide();

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

  mMinBtn = new QPushButton("—", this);
  mMaxBtn = new QPushButton("◻", this);
  mCloseBtn = new QPushButton("✕", this);
  mMinBtn->setObjectName("WinMinBtn");
  mMaxBtn->setObjectName("WinMaxBtn");
  mCloseBtn->setObjectName("WinCloseBtn");

  mMinBtn->hide();
  mMaxBtn->hide();
  mCloseBtn->hide();

  connect(mMinBtn, &QPushButton::clicked, this, &QWidget::showMinimized);
  connect(mMaxBtn, &QPushButton::clicked, 
          [this]() { isMaximized() ? showNormal() : showMaximized(); });
  connect(mCloseBtn, &QPushButton::clicked, this, &QWidget::close);

  mTopLayout->addWidget(mBackBtn);
  mTopLayout->addWidget(mAddTabBtn);
  mTopLayout->addStretch();
  mTopLayout->addWidget(mMinBtn);
  mTopLayout->addWidget(mMaxBtn);
  mTopLayout->addWidget(mCloseBtn);

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
        this, [this](QListWidgetItem *item) {
            onListSelected(mListWidget->row(item));
        });
  connect(mBackBtn, &QPushButton::clicked, 
          this, &QTMResponsiveTabWidget::onBackClicked);
  connect(mTabBar, &QTabBar::tabMoved, 
          this, &QTMResponsiveTabWidget::onTabMoved);
  connect(mAddTabBtn, &QPushButton::clicked, 
          this, &QTMResponsiveTabWidget::newTabRequested);

#ifdef OS_ANDROID
  mMobileViewingContent = false;
  mListWidget->clearSelection();
  applyMode(2);
#else
  applyMode(1);
#endif
  
  if (parent && parent->metaObject()->className() == QString("QTMPlainWindow")) {
    //setWindowFusion(true);
    //setDraggable(true);
  }
}

void QTMResponsiveTabWidget::showEvent(QShowEvent* event) {
  QWidget::showEvent(event);
  updateNestingVisuals();
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

                                        bool containsResponsive = false;
  QList<QWidget*> allChildren = widget->findChildren<QWidget*>();
  for (QWidget* child : allChildren) {
    if (qobject_cast<QTMResponsiveTabWidget*>(child)) {
      containsResponsive = true;
      break;
    }
  }
  
  if (!qobject_cast<QTMResponsiveTabWidget*>(widget) && !containsResponsive) {
    QWidget *top = new QWidget(this);
    QVBoxLayout *layout = new QVBoxLayout(top);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(widget);
    layout->addStretch();
    top->setLayout(layout);
    widget = top;
  }

  mPages.append(widget);

  if (mCurrentMode == 3) {
    int cols = (width() > 1700) ? 3 : 2;
    int i = mPages.size() - 1;
    mGridLayout->addWidget(widget, i / cols, i % cols);
    widget->show();
  } else {
    mContentStack->addWidget(widget);
  }

  mTabBar->addCustomTab(icon, title);
  QListWidgetItem* item = new QListWidgetItem(icon, title);
  mListWidget->addItem(item);
}

int QTMResponsiveTabWidget::count() const { return mTabBar->count(); }

void QTMResponsiveTabWidget::setCurrentIndex(int index) {
  if (index >= 0 && index < count()) mTabBar->setCurrentIndex(index);
}

int QTMResponsiveTabWidget::currentIndex() const { 
  return mTabBar->currentIndex(); 
}

void QTMResponsiveTabWidget::setAddButtonVisible(bool visible) { 
  mAddTabBtn->setVisible(visible); 
}

void QTMResponsiveTabWidget::setDraggable(bool draggable) { 
  mTopBar->setDraggable(draggable); 
}

void QTMResponsiveTabWidget::setWindowFusion(bool fusion) {
  mWindowFusion = fusion;
  mMinBtn->setVisible(fusion);
  mMaxBtn->setVisible(fusion);
  mCloseBtn->setVisible(fusion);
  mSizeGrip->setVisible(fusion);

  if (fusion) {
    window()->setWindowFlags(window()->windowFlags() | 
                             Qt::FramelessWindowHint | Qt::Window);    
  } else {
    window()->setWindowFlags(window()->windowFlags() & 
                             ~Qt::FramelessWindowHint & ~Qt::Window);
  }
}

void QTMResponsiveTabWidget::applyMode(int mode) {
  mCurrentMode = mode;
  
  mTabBar->hide();
  mListWidget->hide();
  mContentStack->hide();
  mGridContainer->hide();

  mTopLayout->removeWidget(mTabBar);
  mDynamicLayout->removeWidget(mTabBar);

  if (mode == 3) {
    int cols = (width() > 1700) ? 3 : 2;
    for (int i = 0; i < mPages.size(); ++i) {
      QWidget* w = mPages[i];
      mContentStack->removeWidget(w);
      mGridLayout->removeWidget(w);
      mGridLayout->addWidget(w, i / cols, i % cols);
      w->show();
    }
  } else {
    for (int i = 0; i < mPages.size(); ++i) {
      QWidget* w = mPages[i];
      mGridLayout->removeWidget(w);
      mContentStack->removeWidget(w);
      mContentStack->insertWidget(i, w);
    }
    if (!mPages.isEmpty()) {
      mContentStack->setCurrentIndex(mTabBar->currentIndex());
    }
  }

  if (mode == 3) {
    mGridContainer->show();
    mMobileViewingContent = true;
  }
  else if (mode == 0) {
    mTopLayout->insertWidget(2, mTabBar);
    mDynamicLayout->setDirection(QBoxLayout::TopToBottom);
    mTabBar->setShape(QTabBar::RoundedNorth);
    mTabBar->show();
    mContentStack->show();
    mMobileViewingContent = true;
  }
  else if (mode == 1) {
    mDynamicLayout->insertWidget(0, mTabBar);
    mDynamicLayout->setDirection(QBoxLayout::LeftToRight);
    mTabBar->setShape(QTabBar::RoundedWest);
    mTabBar->show();
    mContentStack->show();
    mMobileViewingContent = true;
  }
  else if (mode == 2) {
    mDynamicLayout->setDirection(QBoxLayout::TopToBottom);
    if (mMobileViewingContent) {
      mContentStack->show();
    } else {
      mListWidget->show();
    }
  }
  
  mTabBar->updateGeometry();
  reevaluateBackButton();
}

void QTMResponsiveTabWidget::onTabSelected(int index) {
  if (index < 0 || mIsUpdating) return;
  mIsUpdating = true;
  mContentStack->setCurrentIndex(index);
  mListWidget->setCurrentRow(index);
  mIsUpdating = false;
  reevaluateBackButton();
}

void QTMResponsiveTabWidget::onListSelected(int index) {
  if (index < 0 || mIsUpdating) return;
  mIsUpdating = true;
  mContentStack->setCurrentIndex(index);
  mTabBar->setCurrentIndex(index);
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

  QWidget* widget = mContentStack->widget(from);
  mContentStack->removeWidget(widget);
  mContentStack->insertWidget(to, widget);

  QListWidgetItem* item = mListWidget->takeItem(from);
  mListWidget->insertItem(to, item);

  mContentStack->setCurrentIndex(mTabBar->currentIndex());
  mListWidget->setCurrentRow(mTabBar->currentIndex());
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
  if (QWidget* currentContent = mContentStack->currentWidget()) {
    if (!mContentStack->isHidden()) {
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
    for (QWidget* w : mPages) {
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

  return (mCurrentMode == 2 && mMobileViewingContent);
}

void QTMResponsiveTabWidget::reevaluateBackButton() {
  QTMResponsiveTabWidget* parentTab = getParentTabWidget();
  if (parentTab) {
    mBackBtn->hide();
    parentTab->reevaluateBackButton();
  } else {
    if (needsBackButton()) mBackBtn->show();
    else mBackBtn->hide();
  }
}

bool QTMResponsiveTabWidget::goBack() {
  if (!mContentStack->isHidden()) {
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
    for (QWidget* w : mPages) {
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

  if (mCurrentMode == 2 && mMobileViewingContent) {
    mMobileViewingContent = false;
    mListWidget->clearSelection();
    mListWidget->setCurrentRow(-1);
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
    }
}