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
  setFixedHeight(40);
  setCursor(Qt::PointingHandCursor);

  auto* layout = new QHBoxLayout(this);
  layout->setContentsMargins(10, 0, 10, 0);

  m_label = new QLabel(this);
  m_label->setStyleSheet("font-size: 15px; font-weight: bold; color: #E0E0E0; border: none; background: transparent;");
  
  m_switch = new QTMSwitchControl(this);

  layout->addWidget(m_label);
  layout->addStretch();
  layout->addWidget(m_switch);

  connect(m_switch, &QTMSwitchControl::toggled, this, &QTMSettingCheckbox::toggled);
}

void QTMSettingCheckbox::setText(const QString& text) {
  m_label->setText(text);
}

bool QTMSettingCheckbox::isChecked() const {
  return m_switch->isChecked();
}

void QTMSettingCheckbox::setChecked(bool checked) {
  m_switch->setChecked(checked);
}

void QTMSettingCheckbox::mouseReleaseEvent(QMouseEvent*) {
  m_switch->toggle();
}

QTMSettingSelect::QTMSettingSelect(QWidget* parent) : QWidget(parent) {
  setFixedHeight(40);

  auto* layout = new QHBoxLayout(this);
  layout->setContentsMargins(10, 0, 10, 0);

  m_label = new QLabel(this);
  m_label->setStyleSheet("font-size: 15px; font-weight: bold; color: #E0E0E0; border: none; background: transparent;");

  m_combo = new QComboBox(this);
  m_combo->setCursor(Qt::PointingHandCursor);
  m_combo->setStyleSheet(R"(
    QComboBox {
      background: #606060;
      color: #E0E0E0;
      selection-background-color: #004080;
      selection-color: #E0E0E0;
      padding: 4px 10px;
      border: 0px;
      border-radius: 5px;
      min-height: 24px;
      font-size: 14px;
    }
    QComboBox:focus { border: 1px solid #A0C0E0; }
    QComboBox::drop-down { background: transparent; border: 0px; }
    QComboBox::down-arrow { image: none; }
    QComboBox QAbstractItemView {
      background: #404040;
      color: #E0E0E0;
      selection-background-color: #2C6080;
      border: 1px solid #505050;
      outline: 0px;
    }
    QComboBox QAbstractItemView::item { padding: 8px; }
  )");

  layout->addWidget(m_label);
  layout->addStretch();
  layout->addWidget(m_combo);

  connect(m_combo, QOverload<int>::of(&QComboBox::currentIndexChanged), 
          this, &QTMSettingSelect::currentIndexChanged);
}

void QTMSettingSelect::setText(const QString& text) {
  m_label->setText(text);
}

QString QTMSettingSelect::currentText() const {
  return m_combo->currentText();
}

int QTMSettingSelect::findText(const QString& text, Qt::MatchFlags flags) const {
  return m_combo->findText(text, flags);
}

void QTMSettingSelect::setEditable(bool editable) {
  m_combo->setEditable(editable);
}

void QTMSettingSelect::addItems(const QStringList& texts) {
  m_combo->addItems(texts);
}

int QTMSettingSelect::currentIndex() const {
  return m_combo->currentIndex();
}

void QTMSettingSelect::setCurrentIndex(int index) {
  m_combo->setCurrentIndex(index);
}