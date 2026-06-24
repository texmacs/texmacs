#ifndef QTMSETTINGWIDGET_HPP
#define QTMSETTINGWIDGET_HPP

#include <QWidget>
#include <QLabel>
#include <QAbstractButton>
#include <QComboBox>
#include <QBoxLayout>
#include <QHBoxLayout>
#include <QVBoxLayout>
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
  void resizeEvent(QResizeEvent* event) override;

private:
  void updateResponsiveLayout();

private:
  QPointer<QBoxLayout> mLayout;
  QPointer<QLabel> mLabel;
  QPointer<QTMSwitchControl> mSwitch;
};

class QTMSettingSelect : public QWidget {
  Q_OBJECT
public:
  explicit QTMSettingSelect(QWidget* parent = nullptr);
  
  void setDescriptionText(const QString& text);
  QString currentText() const;
  int findText(const QString& text, Qt::MatchFlags flags = Qt::MatchFlags()) const;
  
  void setEditable(bool editable);

  void addItems(const QStringList& texts);
  inline void addItemsAndResize(const QStringList& texts, string, string) { addItems(texts); } // todo

  int currentIndex() const;
  void setCurrentIndex(int index);
  
  QComboBox* comboBox() const { return mCombo ? mCombo.data() : nullptr; }

signals:
  void currentIndexChanged(int index);

protected:
  void resizeEvent(QResizeEvent* event) override;

private:
  void updateResponsiveLayout();

private:
  QPointer<QBoxLayout> mLayout;
  QPointer<QLabel> mLabel;
  QPointer<QComboBox> mCombo;
};

class QTMSettingTitle : public QWidget {
  Q_OBJECT

public:
  explicit QTMSettingTitle(QWidget* parent = nullptr);
  void setTitleText(const QString& text);

private:
  QPointer<QBoxLayout> mLayout;
  QPointer<QLabel> mLabel;

};

class QTMSettingWrapper : public QWidget {
  Q_OBJECT

public:
  explicit QTMSettingWrapper(QWidget *wrapped, QWidget* parent = nullptr);

private:
  QPointer<QBoxLayout> mLayout;
  QPointer<QWidget> mWrapped;
};

class QTMSettingGroup : public QWidget {
  Q_OBJECT
  Q_PROPERTY(int outerMargin READ outerMargin WRITE setOuterMargin)
public:
  explicit QTMSettingGroup(QWidget* parent = nullptr);

  void setTitleText(const QString& text);
  void addItem(QLayoutItem* item);
  QWidget* contentWidget() const { return mWrap ? mWrap.data() : const_cast<QTMSettingGroup*>(this); }
  int outerMargin() const;
  void setOuterMargin(int margin);

private:
  QPointer<QTMSettingTitle> mTitle;
  QPointer<QWidget> mWrap;
  QPointer<QVBoxLayout> mOuterLayout;
  QPointer<QVBoxLayout> mLayout;
  int mOuterMargin;
  int mContentItems;
};

#endif // QTMSETTINGWIDGET_HPP