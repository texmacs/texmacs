#include "QTMSettingWidgets.hpp"
#include <QPainter>
#include <QMouseEvent>

QTMSwitchControl::QTMSwitchControl(QWidget* parent) : QAbstractButton(parent) {
  setCheckable(true);
  setFixedSize(50, 28);
  setCursor(Qt::PointingHandCursor);
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

  QHBoxLayout* layout = new QHBoxLayout(this);

  mLabel = new QLabel(this);  
  mSwitch = new QTMSwitchControl(this);

  layout->addWidget(mLabel);
  layout->addStretch();
  layout->addWidget(mSwitch);

  connect(mSwitch, &QTMSwitchControl::toggled, this, &QTMSettingCheckbox::toggled);
}

void QTMSettingCheckbox::setDescriptionText(const QString& text) {
  if (!mLabel) return;
  mLabel->setText(text);
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

QTMSettingSelect::QTMSettingSelect(QWidget* parent) : QWidget(parent) {
  setAttribute(Qt::WA_StyledBackground, true); 
  QHBoxLayout* layout = new QHBoxLayout(this);

  mLabel = new QLabel(this);

  mCombo = new QComboBox(this);
  if (mCombo) mCombo->setCursor(Qt::PointingHandCursor);

  layout->addWidget(mLabel);
  layout->addWidget(mCombo);

  connect(mCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), 
          this, &QTMSettingSelect::currentIndexChanged);
}

void QTMSettingSelect::setDescriptionText(const QString& text) {
  if (!mLabel) return;
  mLabel->setText(text);
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
}

int QTMSettingSelect::currentIndex() const {
  return mCombo->currentIndex();
}

void QTMSettingSelect::setCurrentIndex(int index) {
  if (!mCombo) return;
  mCombo->setCurrentIndex(index);
}

QTMSettingGroup::QTMSettingGroup(QWidget* parent)
  : QWidget(parent), mOuterMargin(10), mContentItems(0) {
  setAttribute(Qt::WA_StyledBackground, true);
  setObjectName("setting-group");

  mOuterLayout = new QVBoxLayout(this);
  mOuterLayout->setSpacing(0);
  setOuterMargin(mOuterMargin);

  mWrap = new QWidget(this);
  mWrap->setObjectName("setting-group-wrap");
  mOuterLayout->addWidget(mWrap);

  mLayout = new QVBoxLayout(mWrap);
  mTitle = new QLabel(mWrap);
  mTitle->setObjectName("setting-group-title");

  mLayout->setSpacing(0);
  mLayout->setContentsMargins(0, 0, 0, 0);
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
}

void QTMSettingGroup::setTitleText(const QString& text) {
  if (!mTitle) return;
  mTitle->setText(text);
}

void QTMSettingGroup::addItem(QLayoutItem* item) {
  if (!mLayout || !item) return;
  if (QWidget* widget = item->widget()) {
    if (widget->parentWidget() != contentWidget())
      widget->setParent(contentWidget());
    mLayout->addItem(item);
  }
  else if (QLayout* layout = item->layout()) {
    mLayout->addLayout(layout);
  }
  else {
    mLayout->addItem(item);
  }

  if (item->widget() != nullptr || item->layout() != nullptr)
    mContentItems++;

  setVisible(mContentItems > 0);
}