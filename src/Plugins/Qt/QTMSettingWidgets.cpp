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
  mLabel->setText(text);
}

bool QTMSettingCheckbox::isChecked() const {
  return mSwitch->isChecked();
}

void QTMSettingCheckbox::setChecked(bool checked) {
  mSwitch->setChecked(checked);
}

void QTMSettingCheckbox::mouseReleaseEvent(QMouseEvent*) {
  mSwitch->toggle();
}

QTMSettingSelect::QTMSettingSelect(QWidget* parent) : QWidget(parent) {
  setAttribute(Qt::WA_StyledBackground, true); 
  QHBoxLayout* layout = new QHBoxLayout(this);

  mLabel = new QLabel(this);

  mCombo = new QComboBox(this);
  mCombo->setCursor(Qt::PointingHandCursor);

  layout->addWidget(mLabel);
  layout->addWidget(mCombo);

  connect(mCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), 
          this, &QTMSettingSelect::currentIndexChanged);
}

void QTMSettingSelect::setDescriptionText(const QString& text) {
  mLabel->setText(text);
}

QString QTMSettingSelect::currentText() const {
  return mCombo->currentText();
}

int QTMSettingSelect::findText(const QString& text, Qt::MatchFlags flags) const {
  return mCombo->findText(text, flags);
}

void QTMSettingSelect::setEditable(bool editable) {
  mCombo->setEditable(editable);
}

void QTMSettingSelect::addItems(const QStringList& texts) {
  mCombo->addItems(texts);
}

int QTMSettingSelect::currentIndex() const {
  return mCombo->currentIndex();
}

void QTMSettingSelect::setCurrentIndex(int index) {
  mCombo->setCurrentIndex(index);
}