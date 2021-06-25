/******************************************************************************
 * MODULE     : QTMPrintDialog.hpp
 * DESCRIPTION: 
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

// Actual code follows the pasted one. Life's sad without uic...

/********************************************************************************
 ** Form generated from reading UI file 'QTMPrintDialog.ui'
 **
 ** Created: Sat Dec 18 20:23:41 2010
 **      by: Qt User Interface Compiler version 4.7.1
 **
 ** WARNING! All changes made in this file will be lost when recompiling UI file!
 ********************************************************************************/

#ifndef UI_QTMPRINTDIALOG_H
#define UI_QTMPRINTDIALOG_H

#include <QtGlobal>
#include <QtCore/QLocale>
#include <QtCore/QVariant>
#if (QT_VERSION >= 0x050000)
#include <QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QRadioButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QVBoxLayout>
#else
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QDialog>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QRadioButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#endif

QT_BEGIN_NAMESPACE

class Ui_QTMPrintDialog
{
public:
  QVBoxLayout *verticalLayout;
  QGridLayout *gridLayout_3;
  QComboBox *printerCombo;
  QLabel *label_3;
  QLineEdit *copiesInput;
  QCheckBox *collatedCheck;
  QHBoxLayout *horizontalLayout_2;
  QGridLayout *gridLayout_2;
  QLabel *label_7;
  QRadioButton *allPagesRadio;
  QRadioButton *rangePagesRadio;
  QLineEdit *fromPageInput;
  QLabel *label_4;
  QLineEdit *toPageInput;
  QCheckBox *evenPagesCheck;
  QCheckBox *oddPagesCheck;
  QFrame *line;
  QHBoxLayout *horizontalLayout;
  QSpacerItem *horizontalSpacer_2;
  QGridLayout *gridLayout;
  QLabel *paperSizeLabel;
  QComboBox *paperSizeCombo;
  QLabel *label_2;
  QComboBox *orientationCombo;
  QLabel *resolutionLabel;
  QComboBox *resolutionCombo;
  QSpacerItem *horizontalSpacer;
  QHBoxLayout *horizontalLayout_3;
  QSpacerItem *horizontalSpacer_3;
  QCheckBox *duplexCheck;
  QSpacerItem *horizontalSpacer_5;
  QCheckBox *fitToPageCheck;
  QSpacerItem *horizontalSpacer_4;
  QGridLayout *gridLayout_4;
  QLabel *label_5;
  QComboBox *pagesPerSideCombo;
  QLabel *label_6;
  QComboBox *orderPagesCombo;
  QFrame *line_2;
  QCheckBox *blackWhiteCheck;
  QDialogButtonBox *buttonBox;
  
  void setupUi(QDialog *QTMPrintDialog)
  {
    if (QTMPrintDialog->objectName().isEmpty())
      QTMPrintDialog->setObjectName(QString::fromUtf8("QTMPrintDialog"));
    QTMPrintDialog->setWindowModality(Qt::WindowModal);
    QTMPrintDialog->resize(527, 475);
    QTMPrintDialog->setMinimumSize(QSize(527, 475));
    QTMPrintDialog->setWindowOpacity(1);
    QTMPrintDialog->setLocale(QLocale(QLocale::English, QLocale::UnitedStates));
    QTMPrintDialog->setSizeGripEnabled(true);
    QTMPrintDialog->setModal(true);
    verticalLayout = new QVBoxLayout(QTMPrintDialog);
    verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
    gridLayout_3 = new QGridLayout();
    gridLayout_3->setObjectName(QString::fromUtf8("gridLayout_3"));
    printerCombo = new QComboBox(QTMPrintDialog);
    printerCombo->setObjectName(QString::fromUtf8("printerCombo"));
    printerCombo->setMinimumSize(QSize(331, 26));
    
    gridLayout_3->addWidget(printerCombo, 0, 0, 1, 3);
    
    label_3 = new QLabel(QTMPrintDialog);
    label_3->setObjectName(QString::fromUtf8("label_3"));
    
    gridLayout_3->addWidget(label_3, 1, 0, 1, 1);
    
    copiesInput = new QLineEdit(QTMPrintDialog);
    copiesInput->setObjectName(QString::fromUtf8("copiesInput"));
    copiesInput->setMaximumSize(QSize(81, 16777215));
    
    gridLayout_3->addWidget(copiesInput, 1, 1, 1, 1);
    
    collatedCheck = new QCheckBox(QTMPrintDialog);
    collatedCheck->setObjectName(QString::fromUtf8("collatedCheck"));
    collatedCheck->setEnabled(false);
    collatedCheck->setChecked(true);
    
    gridLayout_3->addWidget(collatedCheck, 1, 2, 1, 1);
    
    
    verticalLayout->addLayout(gridLayout_3);
    
    horizontalLayout_2 = new QHBoxLayout();
    horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
    gridLayout_2 = new QGridLayout();
    gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
    label_7 = new QLabel(QTMPrintDialog);
    label_7->setObjectName(QString::fromUtf8("label_7"));
    
    gridLayout_2->addWidget(label_7, 0, 0, 1, 1);
    
    allPagesRadio = new QRadioButton(QTMPrintDialog);
    allPagesRadio->setObjectName(QString::fromUtf8("allPagesRadio"));
    allPagesRadio->setChecked(true);
    
    gridLayout_2->addWidget(allPagesRadio, 0, 1, 1, 1);
    
    rangePagesRadio = new QRadioButton(QTMPrintDialog);
    rangePagesRadio->setObjectName(QString::fromUtf8("rangePagesRadio"));
    
    gridLayout_2->addWidget(rangePagesRadio, 1, 1, 1, 1);
    
    fromPageInput = new QLineEdit(QTMPrintDialog);
    fromPageInput->setObjectName(QString::fromUtf8("fromPageInput"));
    QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    sizePolicy.setHorizontalStretch(0);
    sizePolicy.setVerticalStretch(0);
    sizePolicy.setHeightForWidth(fromPageInput->sizePolicy().hasHeightForWidth());
    fromPageInput->setSizePolicy(sizePolicy);
    fromPageInput->setMaximumSize(QSize(81, 22));
    
    gridLayout_2->addWidget(fromPageInput, 1, 2, 1, 1);
    
    label_4 = new QLabel(QTMPrintDialog);
    label_4->setObjectName(QString::fromUtf8("label_4"));
    
    gridLayout_2->addWidget(label_4, 1, 3, 1, 1);
    
    toPageInput = new QLineEdit(QTMPrintDialog);
    toPageInput->setObjectName(QString::fromUtf8("toPageInput"));
    toPageInput->setMaximumSize(QSize(81, 22));
    
    gridLayout_2->addWidget(toPageInput, 1, 4, 1, 1);
    
    evenPagesCheck = new QCheckBox(QTMPrintDialog);
    evenPagesCheck->setObjectName(QString::fromUtf8("evenPagesCheck"));
    evenPagesCheck->setChecked(true);
    
    gridLayout_2->addWidget(evenPagesCheck, 0, 2, 1, 1);
    
    oddPagesCheck = new QCheckBox(QTMPrintDialog);
    oddPagesCheck->setObjectName(QString::fromUtf8("oddPagesCheck"));
    oddPagesCheck->setChecked(true);
    
    gridLayout_2->addWidget(oddPagesCheck, 0, 4, 1, 1);
    
    
    horizontalLayout_2->addLayout(gridLayout_2);
    
    
    verticalLayout->addLayout(horizontalLayout_2);
    
    line = new QFrame(QTMPrintDialog);
    line->setObjectName(QString::fromUtf8("line"));
    line->setFrameShape(QFrame::HLine);
    line->setFrameShadow(QFrame::Sunken);
    
    verticalLayout->addWidget(line);
    
    horizontalLayout = new QHBoxLayout();
    horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
    horizontalSpacer_2 = new QSpacerItem(30, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);
    
    horizontalLayout->addItem(horizontalSpacer_2);
    
    gridLayout = new QGridLayout();
    gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
    paperSizeLabel = new QLabel(QTMPrintDialog);
    paperSizeLabel->setObjectName(QString::fromUtf8("paperSizeLabel"));
    
    gridLayout->addWidget(paperSizeLabel, 0, 0, 1, 1);
    
    paperSizeCombo = new QComboBox(QTMPrintDialog);
    paperSizeCombo->setObjectName(QString::fromUtf8("paperSizeCombo"));
    paperSizeCombo->setMinimumSize(QSize(200, 26));
    
    gridLayout->addWidget(paperSizeCombo, 0, 1, 1, 1);
    
    label_2 = new QLabel(QTMPrintDialog);
    label_2->setObjectName(QString::fromUtf8("label_2"));
    
    gridLayout->addWidget(label_2, 1, 0, 1, 1);
    
    orientationCombo = new QComboBox(QTMPrintDialog);
    orientationCombo->setObjectName(QString::fromUtf8("orientationCombo"));
    orientationCombo->setMinimumSize(QSize(200, 26));
    
    gridLayout->addWidget(orientationCombo, 1, 1, 1, 1);
    
    resolutionLabel = new QLabel(QTMPrintDialog);
    resolutionLabel->setObjectName(QString::fromUtf8("resolutionLabel"));
    
    gridLayout->addWidget(resolutionLabel, 2, 0, 1, 1);
    
    resolutionCombo = new QComboBox(QTMPrintDialog);
    resolutionCombo->setObjectName(QString::fromUtf8("resolutionCombo"));
    resolutionCombo->setMinimumSize(QSize(200, 26));
    
    gridLayout->addWidget(resolutionCombo, 2, 1, 1, 1);
    
    
    horizontalLayout->addLayout(gridLayout);
    
    horizontalSpacer = new QSpacerItem(30, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);
    
    horizontalLayout->addItem(horizontalSpacer);
    
    
    verticalLayout->addLayout(horizontalLayout);
    
    horizontalLayout_3 = new QHBoxLayout();
    horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
    horizontalSpacer_3 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);
    
    horizontalLayout_3->addItem(horizontalSpacer_3);
    
    duplexCheck = new QCheckBox(QTMPrintDialog);
    duplexCheck->setObjectName(QString::fromUtf8("duplexCheck"));
    duplexCheck->setEnabled(false);
    duplexCheck->setMinimumSize(QSize(0, 31));
    duplexCheck->setLocale(QLocale(QLocale::English, QLocale::UnitedStates));
    
    horizontalLayout_3->addWidget(duplexCheck);
    
    horizontalSpacer_5 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);
    
    horizontalLayout_3->addItem(horizontalSpacer_5);
    
    fitToPageCheck = new QCheckBox(QTMPrintDialog);
    fitToPageCheck->setObjectName(QString::fromUtf8("fitToPageCheck"));
    fitToPageCheck->setChecked(true);
    
    horizontalLayout_3->addWidget(fitToPageCheck);
    
    horizontalSpacer_4 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);
    
    horizontalLayout_3->addItem(horizontalSpacer_4);
    
    
    verticalLayout->addLayout(horizontalLayout_3);
    
    gridLayout_4 = new QGridLayout();
    gridLayout_4->setObjectName(QString::fromUtf8("gridLayout_4"));
    label_5 = new QLabel(QTMPrintDialog);
    label_5->setObjectName(QString::fromUtf8("label_5"));
    
    gridLayout_4->addWidget(label_5, 0, 0, 1, 1);
    
    pagesPerSideCombo = new QComboBox(QTMPrintDialog);
    pagesPerSideCombo->setObjectName(QString::fromUtf8("pagesPerSideCombo"));
    
    gridLayout_4->addWidget(pagesPerSideCombo, 0, 1, 1, 1);
    
    label_6 = new QLabel(QTMPrintDialog);
    label_6->setObjectName(QString::fromUtf8("label_6"));
    
    gridLayout_4->addWidget(label_6, 1, 0, 1, 1);
    
    orderPagesCombo = new QComboBox(QTMPrintDialog);
    orderPagesCombo->setObjectName(QString::fromUtf8("orderPagesCombo"));
    orderPagesCombo->setMinimumSize(QSize(211, 26));
    
    gridLayout_4->addWidget(orderPagesCombo, 1, 1, 1, 1);
    
    
    verticalLayout->addLayout(gridLayout_4);
    
    line_2 = new QFrame(QTMPrintDialog);
    line_2->setObjectName(QString::fromUtf8("line_2"));
    line_2->setFrameShape(QFrame::HLine);
    line_2->setFrameShadow(QFrame::Sunken);
    
    verticalLayout->addWidget(line_2);
    
    blackWhiteCheck = new QCheckBox(QTMPrintDialog);
    blackWhiteCheck->setObjectName(QString::fromUtf8("blackWhiteCheck"));
    blackWhiteCheck->setEnabled(false);
    blackWhiteCheck->setChecked(true);
    
    verticalLayout->addWidget(blackWhiteCheck);
    
    buttonBox = new QDialogButtonBox(QTMPrintDialog);
    buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
    buttonBox->setOrientation(Qt::Horizontal);
    buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
    
    verticalLayout->addWidget(buttonBox);
    
#ifndef QT_NO_SHORTCUT
    label_3->setBuddy(copiesInput);
    label_7->setBuddy(allPagesRadio);
    label_4->setBuddy(toPageInput);
    paperSizeLabel->setBuddy(paperSizeCombo);
    label_2->setBuddy(orientationCombo);
    resolutionLabel->setBuddy(resolutionCombo);
    label_5->setBuddy(pagesPerSideCombo);
    label_6->setBuddy(orderPagesCombo);
#endif // QT_NO_SHORTCUT
    QWidget::setTabOrder(printerCombo, copiesInput);
    QWidget::setTabOrder(copiesInput, allPagesRadio);
    QWidget::setTabOrder(allPagesRadio, rangePagesRadio);
    QWidget::setTabOrder(rangePagesRadio, fromPageInput);
    QWidget::setTabOrder(fromPageInput, toPageInput);
    QWidget::setTabOrder(toPageInput, evenPagesCheck);
    QWidget::setTabOrder(evenPagesCheck, oddPagesCheck);
    QWidget::setTabOrder(oddPagesCheck, collatedCheck);
    QWidget::setTabOrder(collatedCheck, paperSizeCombo);
    QWidget::setTabOrder(paperSizeCombo, orientationCombo);
    QWidget::setTabOrder(orientationCombo, resolutionCombo);
    QWidget::setTabOrder(resolutionCombo, duplexCheck);
    QWidget::setTabOrder(duplexCheck, fitToPageCheck);
    QWidget::setTabOrder(fitToPageCheck, pagesPerSideCombo);
    QWidget::setTabOrder(pagesPerSideCombo, orderPagesCombo);
    QWidget::setTabOrder(orderPagesCombo, blackWhiteCheck);
    QWidget::setTabOrder(blackWhiteCheck, buttonBox);
    
    retranslateUi(QTMPrintDialog);
    QObject::connect(buttonBox, SIGNAL(accepted()), QTMPrintDialog, SLOT(accept()));
    QObject::connect(buttonBox, SIGNAL(rejected()), QTMPrintDialog, SLOT(reject()));
    
    QMetaObject::connectSlotsByName(QTMPrintDialog);
  } // setupUi
  
  void retranslateUi(QDialog *QTMPrintDialog)
  {
#if (QT_VERSION >= 0x050000)
    QTMPrintDialog->setWindowTitle(QApplication::translate("QTMPrintDialog", "Print", 0));
    label_3->setText(QApplication::translate("QTMPrintDialog", "Copies:", 0));
    copiesInput->setText(QApplication::translate("QTMPrintDialog", "1", 0));
    collatedCheck->setText(QApplication::translate("QTMPrintDialog", "Collated", 0));
    label_7->setText(QApplication::translate("QTMPrintDialog", "Pages:", 0));
    allPagesRadio->setText(QApplication::translate("QTMPrintDialog", "All", 0));
    rangePagesRadio->setText(QApplication::translate("QTMPrintDialog", "From:", 0));
    fromPageInput->setText(QApplication::translate("QTMPrintDialog", "1", 0));
    label_4->setText(QApplication::translate("QTMPrintDialog", "To:", 0));
    toPageInput->setText(QApplication::translate("QTMPrintDialog", "1", 0));
    evenPagesCheck->setText(QApplication::translate("QTMPrintDialog", "Even", 0));
    oddPagesCheck->setText(QApplication::translate("QTMPrintDialog", "Odd", 0));
    paperSizeLabel->setText(QApplication::translate("QTMPrintDialog", "Paper size:", 0));
    label_2->setText(QApplication::translate("QTMPrintDialog", "Orientation:", 0));
    resolutionLabel->setText(QApplication::translate("QTMPrintDialog", "Resolution:", 0));
    duplexCheck->setText(QApplication::translate("QTMPrintDialog", "Print on both sides", 0));
    fitToPageCheck->setText(QApplication::translate("QTMPrintDialog", "Fit to page", 0));
    label_5->setText(QApplication::translate("QTMPrintDialog", "Pages per side:", 0));
    pagesPerSideCombo->clear();
    pagesPerSideCombo->insertItems(0, QStringList()
                                   << QApplication::translate("QTMPrintDialog", "1", 0)
                                   << QApplication::translate("QTMPrintDialog", "2", 0)
                                   << QApplication::translate("QTMPrintDialog", "4", 0)
                                   << QApplication::translate("QTMPrintDialog", "6", 0)
                                   << QApplication::translate("QTMPrintDialog", "9", 0)
                                   << QApplication::translate("QTMPrintDialog", "16", 0)
                                   );
    label_6->setText(QApplication::translate("QTMPrintDialog", "Order:", 0));
    blackWhiteCheck->setText(QApplication::translate("QTMPrintDialog", "Print in black and white", 0));
#else
    QTMPrintDialog->setWindowTitle(QApplication::translate("QTMPrintDialog", "Print", 0, QApplication::UnicodeUTF8));
    label_3->setText(QApplication::translate("QTMPrintDialog", "Copies:", 0, QApplication::UnicodeUTF8));
    copiesInput->setText(QApplication::translate("QTMPrintDialog", "1", 0, QApplication::UnicodeUTF8));
    collatedCheck->setText(QApplication::translate("QTMPrintDialog", "Collated", 0, QApplication::UnicodeUTF8));
    label_7->setText(QApplication::translate("QTMPrintDialog", "Pages:", 0, QApplication::UnicodeUTF8));
    allPagesRadio->setText(QApplication::translate("QTMPrintDialog", "All", 0, QApplication::UnicodeUTF8));
    rangePagesRadio->setText(QApplication::translate("QTMPrintDialog", "From:", 0, QApplication::UnicodeUTF8));
    fromPageInput->setText(QApplication::translate("QTMPrintDialog", "1", 0, QApplication::UnicodeUTF8));
    label_4->setText(QApplication::translate("QTMPrintDialog", "To:", 0, QApplication::UnicodeUTF8));
    toPageInput->setText(QApplication::translate("QTMPrintDialog", "1", 0, QApplication::UnicodeUTF8));
    evenPagesCheck->setText(QApplication::translate("QTMPrintDialog", "Even", 0, QApplication::UnicodeUTF8));
    oddPagesCheck->setText(QApplication::translate("QTMPrintDialog", "Odd", 0, QApplication::UnicodeUTF8));
    paperSizeLabel->setText(QApplication::translate("QTMPrintDialog", "Paper size:", 0, QApplication::UnicodeUTF8));
    label_2->setText(QApplication::translate("QTMPrintDialog", "Orientation:", 0, QApplication::UnicodeUTF8));
    resolutionLabel->setText(QApplication::translate("QTMPrintDialog", "Resolution:", 0, QApplication::UnicodeUTF8));
    duplexCheck->setText(QApplication::translate("QTMPrintDialog", "Print on both sides", 0, QApplication::UnicodeUTF8));
    fitToPageCheck->setText(QApplication::translate("QTMPrintDialog", "Fit to page", 0, QApplication::UnicodeUTF8));
    label_5->setText(QApplication::translate("QTMPrintDialog", "Pages per side:", 0, QApplication::UnicodeUTF8));
    pagesPerSideCombo->clear();
    pagesPerSideCombo->insertItems(0, QStringList()
                                   << QApplication::translate("QTMPrintDialog", "1", 0, QApplication::UnicodeUTF8)
                                   << QApplication::translate("QTMPrintDialog", "2", 0, QApplication::UnicodeUTF8)
                                   << QApplication::translate("QTMPrintDialog", "4", 0, QApplication::UnicodeUTF8)
                                   << QApplication::translate("QTMPrintDialog", "6", 0, QApplication::UnicodeUTF8)
                                   << QApplication::translate("QTMPrintDialog", "9", 0, QApplication::UnicodeUTF8)
                                   << QApplication::translate("QTMPrintDialog", "16", 0, QApplication::UnicodeUTF8)
                                   );
    label_6->setText(QApplication::translate("QTMPrintDialog", "Order:", 0, QApplication::UnicodeUTF8));
    blackWhiteCheck->setText(QApplication::translate("QTMPrintDialog", "Print in black and white", 0, QApplication::UnicodeUTF8));
#endif
  } // retranslateUi
  
};

namespace Ui {
  class QTMPrintDialog: public Ui_QTMPrintDialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_QTMPRINTDIALOG_H


#ifndef QTMPRINTDIALOG_HPP
#define QTMPRINTDIALOG_HPP

#include <QDialog>
//#include "ui_QTMPrintdialog.h"   // life's sad without uic...
#include "QTMPrinterSettings.hpp"

class QPrinter;

/*!
 * Our own print dialog. This implements a subset of the options available in
 * the native or QT dialogs, with the purpose of not having "dead" options in
 * them (i.e. options we don't implement). This cannot be helped since the
 * native print dialogs are not configurable and expect features we cannot
 * implement: how to know if under MacOS the user selected "send per email"?
 */
class QTMPrintDialog : public QDialog, protected Ui::QTMPrintDialog
{
  Q_OBJECT
  
public:
  QTMPrintDialog(QTMPrinterSettings* s, QDialog* parent=0);
  void setupUi(QDialog *dia);

public slots:
  void accept();
  void reject();
  
protected slots:
  void updatePrinterCapabilities();
  // Auto connections:
  void on_allPagesRadio_clicked(bool);
  void on_rangePagesRadio_clicked(bool);
  void on_copiesInput_textChanged(const QString& );
  void on_fromPageInput_textChanged(const QString& );
  void on_toPageInput_textChanged(const QString& );
  void on_oddPagesCheck_stateChanged(int state);
  void on_evenPagesCheck_stateChanged(int state);
  
protected:
  QTMPrinterSettings* _settings;
};

#endif // QTMPRINTDIALOG_HPP
