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

class QTMSettingWidget {

public:
  virtual inline QPointer<QWidget> leftWidget() const {
    return nullptr;
  }

  virtual inline QPointer<QWidget> rightWidget() const {
    return nullptr;
  }

};

class QTMSwitchControl : public QAbstractButton {
  Q_OBJECT
public:
  explicit QTMSwitchControl(QWidget* parent = nullptr);
protected:
  void paintEvent(QPaintEvent*) override;
};

class QTMSettingCheckbox : public QWidget, public QTMSettingWidget {
  Q_OBJECT
public:
  explicit QTMSettingCheckbox(QWidget* parent = nullptr);
  
  void setDescriptionText(const QString& text);
  bool isChecked() const;
  void setChecked(bool checked);
  inline bool setCheckState(bool checked) { setChecked(checked); return checked; }
  bool needVerticalLayout() const;
  void setVerticalLayout(bool vertical);
  QLabel* descriptionLabel() const { return mLabel ? mLabel.data() : nullptr; }

  inline QPointer<QWidget> leftWidget() const override {
    return mLabel ? mLabel.data() : nullptr;
  }

  inline QPointer<QWidget> rightWidget() const override {
    return mSwitchWrapper ? mSwitchWrapper.data() : nullptr;
  }
  
signals:
  void toggled(bool checked);

protected:
  void mouseReleaseEvent(QMouseEvent* event) override;
  void resizeEvent(QResizeEvent* event) override;

private:
  QPointer<QBoxLayout> mLayout;
  QPointer<QLabel> mLabel;
  QPointer<QVBoxLayout> mSwitchLayout;
  QPointer<QWidget> mSwitchWrapper;
  QPointer<QTMSwitchControl> mSwitch;
};

class QTMSettingSelect : public QWidget, public QTMSettingWidget {
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
  bool needVerticalLayout() const;
  void setVerticalLayout(bool vertical);
  QLabel* descriptionLabel() const { return mLabel ? mLabel.data() : nullptr; }
  
  QComboBox* comboBox() const { return mCombo ? mCombo.data() : nullptr; }
  
  inline QPointer<QWidget> leftWidget() const override {
    return mLabel ? mLabel.data() : nullptr;
  }

  inline QPointer<QWidget> rightWidget() const override {
    return mCombo ? mCombo.data() : nullptr;
  }

signals:
  void currentIndexChanged(int index);

protected:
  void resizeEvent(QResizeEvent* event) override;

private:
  QPointer<QBoxLayout> mLayout;
  QPointer<QLabel> mLabel;
  QPointer<QComboBox> mCombo;
};

class QTMSettingTitle : public QWidget, public QTMSettingWidget {
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

  void synchronizeSizes();

protected:
  void resizeEvent(QResizeEvent* event) override;

private:
  void updateResponsiveLayout();

private:
  QPointer<QTMSettingTitle> mTitle;
  QPointer<QWidget> mWrap;
  QPointer<QVBoxLayout> mOuterLayout;
  QPointer<QVBoxLayout> mLayout;
  int mOuterMargin;
  int mContentItems;
};

#endif // QTMSETTINGWIDGET_HPP