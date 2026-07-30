#include "QTMSettingWidgets.hpp"
#include <QPainter>
#include <QMouseEvent>
#include <QResizeEvent>

#define RESPONSIVE_WIDTH_THRESHOLD 10

QTMSwitchControl::QTMSwitchControl(QWidget* parent) : QAbstractButton(parent) {
  setCheckable(true);
  setFixedSize(44, 24);
  setCursor(Qt::PointingHandCursor);
  setProperty("isSettingWidget", true);
}

void QTMSwitchControl::paintEvent(QPaintEvent*) {
  QPainter p(this);
  p.setRenderHint(QPainter::Antialiasing);

  QRect trackRect(2, 6, width() - 4, 16);
  QRect thumbRect(0, 4, 20, 20);
  p.setPen(Qt::NoPen);

  if (isChecked()) {
    p.setBrush(QColor("#2C6080"));
    p.drawRoundedRect(trackRect, 8, 8);
    p.setBrush(QColor("#E0E0E0"));
    thumbRect.moveLeft(width() - 22);
  } else {
    p.setBrush(QColor("#404040"));
    p.drawRoundedRect(trackRect, 8, 8);
    p.setBrush(QColor("#808080"));
  }

  p.setPen(QPen(QColor(0, 0, 0, 80), 1));
  p.drawEllipse(thumbRect);
}

QTMSettingCheckbox::QTMSettingCheckbox(QWidget* parent) : QWidget(parent) {
  setAttribute(Qt::WA_StyledBackground, true); 
  setCursor(Qt::PointingHandCursor);
  setProperty("isSettingWidget", true);

  mLayout = new QBoxLayout(QBoxLayout::LeftToRight, this);
  mLayout->setSpacing(8);
  mLayout->setContentsMargins(4, 2, 4, 2);

  mLabel = new QLabel(this);
  // align text to the right and vertically centered
  mLabel->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  mSwitch = new QTMSwitchControl(this);
  mSwitchLayout = new QVBoxLayout();
  mSwitchLayout->setContentsMargins(0, 0, 0, 0);
  mSwitchLayout->addWidget(mSwitch);
  mSwitchLayout->addSpacerItem(new QSpacerItem(0, 0, QSizePolicy::Minimum, QSizePolicy::Expanding));
  mSwitchWrapper = new QWidget(this);
  mSwitchWrapper->setLayout(mSwitchLayout);

  mLayout->addWidget(mLabel);
  mLayout->addWidget(mSwitchWrapper);
  mLayout->addSpacerItem(new QSpacerItem(0, 0, QSizePolicy::Minimum, QSizePolicy::Expanding));

  connect(mSwitch, &QTMSwitchControl::toggled, this, &QTMSettingCheckbox::toggled);
  setVerticalLayout(needVerticalLayout());
}

void QTMSettingCheckbox::setDescriptionText(const QString& text) {
  if (!mLabel) return;
  mLabel->setText(text);
  setVerticalLayout(needVerticalLayout());
}

bool QTMSettingCheckbox::isChecked() const {
  return mSwitch ? mSwitch->isChecked() : false;
}

void QTMSettingCheckbox::setChecked(bool checked) {
  if (!mSwitch) return;
  mSwitch->setChecked(checked);
}

void QTMSettingCheckbox::mouseReleaseEvent(QMouseEvent*) {
  if (!mSwitch) return;
  mSwitch->toggle();
}

void QTMSettingCheckbox::resizeEvent(QResizeEvent* event) {
  QWidget::resizeEvent(event);
  setVerticalLayout(needVerticalLayout());
}

bool QTMSettingCheckbox::needVerticalLayout() const {
  if (mLayout == nullptr || mLabel == nullptr || mSwitch == nullptr) return false;

  int margins = mLayout->contentsMargins().left() + mLayout->contentsMargins().right();
  int spacing = mLayout->spacing();
  int labelWidth = mLabel->sizeHint().width();
  int switchWidth = mSwitch->sizeHint().width();
  int requiredWidth = labelWidth + switchWidth + spacing + margins + 24;
  if (requiredWidth < RESPONSIVE_WIDTH_THRESHOLD) requiredWidth = RESPONSIVE_WIDTH_THRESHOLD;
  return width() > 0 && width() < requiredWidth;
}

void QTMSettingCheckbox::setVerticalLayout(bool vertical) {
  if (mLayout == nullptr || mLabel == nullptr || mSwitch == nullptr || mSwitchWrapper == nullptr) return;

  if (vertical) {
    mLayout->setDirection(QBoxLayout::TopToBottom);
    mLayout->setAlignment(mLabel, Qt::AlignLeft);
    mLayout->setAlignment(mSwitchWrapper, Qt::AlignLeft);
    mSwitch->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  } else {
    mLayout->setDirection(QBoxLayout::LeftToRight);
    mLayout->setAlignment(mLabel, Qt::AlignLeft | Qt::AlignVCenter);
    mLayout->setAlignment(mSwitchWrapper, Qt::AlignLeft | Qt::AlignVCenter);
    mSwitch->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  }
}


class MyComboBox : public QComboBox {
public:
  using QComboBox::QComboBox;
  QSize sizeHint() const override {
    QSize size = QComboBox::sizeHint();
    size.setWidth(qMax(size.width() + 32, 180));
    return size;
  }
};

QTMSettingSelect::QTMSettingSelect(QWidget* parent) : QWidget(parent) {
  setAttribute(Qt::WA_StyledBackground, true); 
  setProperty("isSettingWidget", true);
  mLayout = new QBoxLayout(QBoxLayout::LeftToRight, this);
  mLayout->setSpacing(8);
  mLayout->setContentsMargins(4, 2, 4, 2);

  mLabel = new QLabel(this);

  mCombo = new MyComboBox(this);
  if (mCombo) mCombo->setCursor(Qt::PointingHandCursor);
  if (mCombo) mCombo->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);

  // mLayout->addSpacerItem(new QSpacerItem(0, 0, QSizePolicy::Expanding, QSizePolicy::Minimum));
  mLayout->addWidget(mLabel);
  mLayout->addWidget(mCombo);
  mLayout->addSpacerItem(new QSpacerItem(0, 0, QSizePolicy::Expanding, QSizePolicy::Minimum));

  connect(mCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), 
          this, &QTMSettingSelect::currentIndexChanged);
  setVerticalLayout(needVerticalLayout());
}

void QTMSettingSelect::setDescriptionText(const QString& text) {
  if (!mLabel) return;
  mLabel->setText(text);
  setVerticalLayout(needVerticalLayout());
}

QString QTMSettingSelect::currentText() const {
  return mCombo ? mCombo->currentText() : QString();
}

int QTMSettingSelect::findText(const QString& text, Qt::MatchFlags flags) const {
  return mCombo ? mCombo->findText(text, flags) : -1;
}

void QTMSettingSelect::setEditable(bool editable) {
  if (!mCombo) return;
  mCombo->setEditable(editable);
}

void QTMSettingSelect::addItems(const QStringList& texts) {
  if (!mCombo) return;
  mCombo->addItems(texts);
  setVerticalLayout(needVerticalLayout());
}

int QTMSettingSelect::currentIndex() const {
  return mCombo->currentIndex();
}

void QTMSettingSelect::setCurrentIndex(int index) {
  if (!mCombo) return;
  mCombo->setCurrentIndex(index);
}

void QTMSettingSelect::resizeEvent(QResizeEvent* event) {
  QWidget::resizeEvent(event);
  setVerticalLayout(needVerticalLayout());
}

bool QTMSettingSelect::needVerticalLayout() const {
  if (mLayout == nullptr || mLabel == nullptr || mCombo == nullptr) return false;

  int margins = mLayout->contentsMargins().left() + mLayout->contentsMargins().right();
  int spacing = mLayout->spacing();
  int labelWidth = mLabel->sizeHint().width();
  int comboWidth = qMax(mCombo->width(), 180);
  int requiredWidth = labelWidth + comboWidth + spacing + margins + 24;
  if (requiredWidth < RESPONSIVE_WIDTH_THRESHOLD) requiredWidth = RESPONSIVE_WIDTH_THRESHOLD;
  return width() > 0 && width() < requiredWidth;
}

void QTMSettingSelect::setVerticalLayout(bool vertical) {
  if (mLayout == nullptr || mLabel == nullptr || mCombo == nullptr) return;

  if (vertical) {
    mLayout->setDirection(QBoxLayout::TopToBottom);
    mLayout->setAlignment(mLabel, Qt::AlignLeft);
    mLabel->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
    mLayout->setAlignment(mCombo, Qt::AlignLeft);
    mCombo->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
  } else {
    mLayout->setDirection(QBoxLayout::LeftToRight);
    mLayout->setAlignment(mLabel, Qt::AlignLeft | Qt::AlignVCenter);
    mLabel->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    mLayout->setAlignment(mCombo, Qt::AlignLeft | Qt::AlignVCenter);
    mCombo->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
  }
}

QTMSettingTitle::QTMSettingTitle(QWidget* parent) : QWidget(parent) {
  setAttribute(Qt::WA_StyledBackground, true); 
  setProperty("isSettingWidget", true);

  mLayout = new QBoxLayout(QBoxLayout::LeftToRight, this);
  mLabel = new QLabel(this);
  mLabel->setObjectName("setting-title-label");
  mLayout->addWidget(mLabel);
  mLayout->setAlignment(mLabel, Qt::AlignLeft);
}

void QTMSettingTitle::setTitleText(const QString& text) {
  if (!mLabel) return;
  mLabel->setText(text);
}

QTMSettingWrapper::QTMSettingWrapper(QWidget *wrapped, QWidget* parent) : QWidget(parent) {
  setAttribute(Qt::WA_StyledBackground, true); 
  setProperty("isSettingWidget", true);

  mLayout = new QBoxLayout(QBoxLayout::LeftToRight, this);
  mWrapped = wrapped;
  if (mWrapped) {
    mWrapped->setParent(this);
    mLayout->addWidget(mWrapped);
  }
}

QTMSettingGroup::QTMSettingGroup(QWidget* parent)
  : QWidget(parent), mOuterMargin(5), mContentItems(0) {
  setAttribute(Qt::WA_StyledBackground, true);
  setObjectName("setting-group");

  setProperty("stretchToTop", true);

  mOuterLayout = new QVBoxLayout(this);
  mOuterLayout->setSpacing(0);
  setOuterMargin(mOuterMargin);

  mWrap = new QWidget(this);
  mWrap->setObjectName("setting-group-wrap");
  mOuterLayout->addWidget(mWrap);

  mLayout = new QVBoxLayout(mWrap);
  mTitle = new QTMSettingTitle(mWrap);
  mTitle->setObjectName("setting-group-title");


  mLayout->setSpacing(4);
  mLayout->setContentsMargins(8, 8, 8, 8);
  mLayout->addWidget(mTitle);

  setVisible(false);
}

int QTMSettingGroup::outerMargin() const {
  return mOuterMargin;
}

void QTMSettingGroup::setOuterMargin(int margin) {
  if (margin < 0) margin = 0;
  mOuterMargin = margin;
  if (mOuterLayout)
    mOuterLayout->setContentsMargins(margin, margin, margin, margin);
  updateResponsiveLayout();
}

void QTMSettingGroup::setTitleText(const QString& text) {
  if (!mTitle) return;
  QString formattedText = text;
  formattedText.replace(" -> ", " → ");
  mTitle->setTitleText(formattedText);
}

void QTMSettingGroup::resizeEvent(QResizeEvent* event) {
  QWidget::resizeEvent(event);
  updateResponsiveLayout();
}

void QTMSettingGroup::updateResponsiveLayout() {
  QWidget *parentWidget = this->parentWidget();
  if (!parentWidget) return;

  const QList<QTMSettingCheckbox*> checkboxes = parentWidget->findChildren<QTMSettingCheckbox*>();
  const QList<QTMSettingSelect*> selects = parentWidget->findChildren<QTMSettingSelect*>();

  bool forceVertical = false;
  for (QTMSettingCheckbox* checkbox : checkboxes) {
    if (checkbox && checkbox->needVerticalLayout()) {
      forceVertical = true;
      break;
    }
  }

  if (!forceVertical) {
    for (QTMSettingSelect* select : selects) {
      if (select && select->needVerticalLayout()) {
        forceVertical = true;
        break;
      }
    }
  }

  for (QTMSettingCheckbox* checkbox : checkboxes) {
    if (checkbox) checkbox->setVerticalLayout(forceVertical);
  }
  for (QTMSettingSelect* select : selects) {
    if (select) select->setVerticalLayout(forceVertical);
  }
}

void QTMSettingGroup::addItem(QLayoutItem* item) {
  if (!mLayout || !item) return;
  if (QWidget* widget = item->widget()) {
    if (widget->parentWidget() != contentWidget())
      widget->setParent(contentWidget());
    
    // is this a setting widget ? if not, we need to add a padding
    if (!widget->property("isSettingWidget").toBool()) {
      mLayout->addWidget(new QTMSettingWrapper(widget, contentWidget()));
    } else {
      mLayout->addItem(item);
    }
  }
  else if (QLayout* layout = item->layout()) {
    QWidget* container = new QWidget(contentWidget());
    container->setLayout(layout);
    mLayout->addWidget(new QTMSettingWrapper(container, contentWidget()));
  }
  else {
    // todo
    mLayout->addItem(item);
  }

  if (item->widget() != nullptr || item->layout() != nullptr)
    mContentItems++;

  setVisible(mContentItems > 0);
  updateResponsiveLayout();
  //QTMSettingSelect::synchronizeLabelWidths(this->parentWidget());
  //QTMSettingSelect::synchronizeComboBoxWidths(this->parentWidget());
  synchronizeSizes();
}

void QTMSettingGroup::synchronizeSizes() {
  // list all left and right widgets and synchronize their sizes
  QWidget* parentWidget = this;
  if (!parentWidget) return;

  // list all QTMSettingWidget
  QList<QWidget*> allWidgets = parentWidget->findChildren<QWidget*>();
  QList<QTMSettingWidget*> settingWidgets;
  for (QWidget* widget : allWidgets) {
    if (QTMSettingWidget* settingWidget = dynamic_cast<QTMSettingWidget*>(widget)) {
      settingWidgets.append(settingWidget);
    }
  }

  QList<QWidget*> leftWidgets, rightWidgets;
  for (QTMSettingWidget* settingWidget : settingWidgets) {
    if (!settingWidget) continue;
    if (QWidget* left = settingWidget->leftWidget()) {
      leftWidgets.append(left);
    }
    if (QWidget* right = settingWidget->rightWidget()) {
      rightWidgets.append(right);
    }
  }

  // synchronize left widgets
  int maxLeftWidth = 0;
  for (QWidget* left : leftWidgets) {
    if (!left) continue;
    maxLeftWidth = qMax(maxLeftWidth, left->sizeHint().width());
  }

  for (QWidget* left : leftWidgets) {
    if (!left) continue;
    left->setMinimumWidth(maxLeftWidth);
  }

  // synchronize right widgets
  int maxRightWidth = 0;
  for (QWidget* right : rightWidgets) {
    if (!right) continue;
    maxRightWidth = qMax(maxRightWidth, right->sizeHint().width());
  }

  for (QWidget* right : rightWidgets) {
    if (!right) continue;
    right->setMinimumWidth(maxRightWidth);
  }

  
}