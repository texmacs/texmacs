#ifndef QTMSETTINGWIDGET_HPP
#define QTMSETTINGWIDGET_HPP

#include <QWidget>
#include <QLabel>
#include <QAbstractButton>
#include <QComboBox>
#include <QHBoxLayout>
#include <QPointer>

#include "string.hpp"

class QTMSwitchControl : public QAbstractButton {
  Q_OBJECT
public:
  explicit QTMSwitchControl(QWidget* parent = nullptr);
protected:
  void paintEvent(QPaintEvent*) override;
};

class QTMSettingCheckbox : public QWidget {
  Q_OBJECT
public:
  explicit QTMSettingCheckbox(QWidget* parent = nullptr);
  
  void setDescriptionText(const QString& text);
  bool isChecked() const;
  void setChecked(bool checked);
  inline bool setCheckState(bool checked) { setChecked(checked); return checked; }

signals:
  void toggled(bool checked);

protected:
  void mouseReleaseEvent(QMouseEvent* event) override;

private:
  QPointer<QLabel> mLabel;
  QPointer<QTMSwitchControl> mSwitch;
};

class QTMSettingSelect : public QWidget {
  Q_OBJECT
public:
  explicit QTMSettingSelect(QWidget* parent = nullptr);
  
  void setDescriptionText(const QString& text);
  QString currentText() const;
  int findText(const QString& text, Qt::MatchFlags flags = Qt::MatchFlags(0)) const;
  
  void setEditable(bool editable);

  void addItems(const QStringList& texts);
  inline void addItemsAndResize(const QStringList& texts, string ww, string hh) { addItems(texts); } // todo

  int currentIndex() const;
  void setCurrentIndex(int index);
  
  QComboBox* comboBox() const { return mCombo ? mCombo.data() : nullptr; }

signals:
  void currentIndexChanged(int index);

private:
  QPointer<QLabel> mLabel;
  QPointer<QComboBox> mCombo;
};

#endif // QTMSETTINGWIDGET_HPP