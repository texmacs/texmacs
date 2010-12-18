/******************************************************************************
 * MODULE     : QTMPrintDialog.cpp
 * DESCRIPTION: 
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMPrintDialog.hpp"
#include <QPrinterInfo>

/*!
 * Constructs a QTMPrintDialog and sets up the connections that allow for 
 * asynchronous loading of printer configuration:
 * QTMPrinterSettings will emit QTMPrinterSettings::doneReading() after a call to 
 * QTMPrinterSettings::readFromSystemConfig() terminates, that is, after the 
 * command executed is done.
 *
 * @see QTMPrinterSettings 
 *
 * @todo Read the current printer settings from TeXmacs.
 */
QTMPrintDialog::QTMPrintDialog(QTMPrinterSettings* s, QDialog* parent) 
  : QDialog(parent), _settings(s) {
    
  setupUi(this);
    
  QObject::connect(_settings, SIGNAL(doneReading()), 
                   this, SLOT(updatePrinterCapabilities()));
  QObject::connect(printerCombo, SIGNAL(currentIndexChanged(const QString)),
                   _settings, SLOT(startReadingSystemConfig(const QString&)));
    

}


/*!
 * Updates the controls which depend on the capabilities of the selected
 * printer, that is: available paper sizes, color printing, and duplex printing.
 * To this purpose we use QTMPrinterSettings::getChoices().
 *
 * @fixme How can we translate the output from the system config? Must we?
 */
void
QTMPrintDialog::updatePrinterCapabilities() {
  int idx = 0;
  
  paperSizeCombo->clear();
  paperSizeCombo->addItems(_settings->getChoices(QTMPrinterSettings::PageSize, idx));
  paperSizeCombo->setCurrentIndex(idx);
  paperSizeLabel->setEnabled(paperSizeCombo->count() > 1);
  paperSizeCombo->setEnabled(paperSizeCombo->count() > 1);

  resolutionCombo->clear();
  resolutionCombo->addItems(_settings->getChoices(QTMPrinterSettings::Resolution, idx));
  resolutionCombo->setCurrentIndex(idx);
  resolutionLabel->setEnabled(resolutionCombo->count() > 1);
  resolutionCombo->setEnabled(resolutionCombo->count() > 1);

  if (_settings->getChoices(QTMPrinterSettings::Duplex, idx).size() > 1) {
    duplexCheck->setEnabled(true);
  } else {
    duplexCheck->setEnabled(false);
    duplexCheck->setChecked(false);
  }
  
  blackWhiteCheck->setEnabled(_settings->getChoices(QTMPrinterSettings::ColorModel, 
                                                    idx).size() > 1);
}

/*!
 * @todo Translate strings?
 * @fixme QPrinterInfo::availablePrinters() does not return the spool names, but
 * the printer names, which are useless to print with lpr.
 */
void
QTMPrintDialog::setupUi(QDialog *dia) {

  Ui::QTMPrintDialog::setupUi(dia);
  
  foreach (QString printerName, _settings->availablePrinters())
    printerCombo->addItem(printerName);
  
  if(printerCombo->count() == 0)
    printerCombo->addItem("No printers available"); // TODO hide everything else

  // We use the QVariant field to retrieve the correct value later.
  orientationCombo->addItem("Portrait", QTMPrinterSettings::Portrait);
  orientationCombo->addItem("Landscape", QTMPrinterSettings::Landscape);
  orientationCombo->addItem("Reverse portrait", QTMPrinterSettings::ReversePortrait);
  orientationCombo->addItem("Reverse landscape", QTMPrinterSettings::ReverseLandscape);
  
  // We use the QVariant field to retrieve the correct value later.
  orderPagesCombo->addItem("Left-Right, Top-Bottom", QTMPrinterSettings::LR_TB);
  orderPagesCombo->addItem("Right-Left, Top-Bottom", QTMPrinterSettings::RL_TB);
  orderPagesCombo->addItem("Top-Bottom, Left-Right", QTMPrinterSettings::TB_LR);
  orderPagesCombo->addItem("Top-Bottom, Right-Left", QTMPrinterSettings::TB_RL);
  
  // Force reading the printer settings for the first printer in the list. 
  _settings->startReadingSystemConfig(printerCombo->currentText());
  
}

/*!
 * Stores the values from the dialog into the QTMPrinterSettings object after the
 * user presses the "Accept" button.
 */
void 
QTMPrintDialog::accept() {
  
  _settings->printerName   = printerCombo->currentText();
  _settings->copyCount     = copiesInput->text().toInt();
  _settings->collateCopies = collatedCheck->isChecked();
  
  if (allPagesRadio->isChecked())
    _settings->firstPage = _settings->lastPage = 0;
  else {
    _settings->firstPage   = fromPageInput->text().toInt();
    _settings->lastPage    = toPageInput->text().toInt();  
  }
  _settings->printOddPages  = oddPagesCheck->isChecked();
  _settings->printEvenPages = evenPagesCheck->isChecked();
  _settings->paperSize      = paperSizeCombo->currentText();
  _settings->orientation    = (QTMPrinterSettings::PageOrientation)
            orientationCombo->itemData(orderPagesCombo->currentIndex()).toInt();
  _settings->duplex         = duplexCheck->isChecked();
  _settings->blackWhite     = blackWhiteCheck->isChecked();
  _settings->pagesPerSide   = pagesPerSideCombo->currentText().toInt();
  _settings->pagesOrder     = (QTMPrinterSettings::PagePrintingOrder) 
             orderPagesCombo->itemData(orderPagesCombo->currentIndex()).toInt();
  _settings->fitToPage      = fitToPageCheck->isChecked();
  _settings->blackWhite     = blackWhiteCheck->isChecked();
  
  QDialog::accept();
}

/*!
 *
 */
void QTMPrintDialog::reject()
{
  QDialog::reject();
}


/////////////////// Some cosmetic stuff follows


/*!
 * Disable the From: and To: fields in the dialog.
 *
 * @note We use this signal because toggled() would be fired by our call to
 *       setChecked inside on_*PageInput_textChanged()
 */
void QTMPrintDialog::on_allPagesRadio_clicked(bool on)
{
  if(on) {
    fromPageInput->setText("");
    toPageInput->setText("");
  }
  allPagesRadio->setChecked(on);
}

/*!
 * Enable the From: and To: fields in the dialog.
 */
void QTMPrintDialog::on_rangePagesRadio_clicked(bool on)
{
  if (on) {
    int f = (_settings->firstPage < 1) ? 1 : _settings->firstPage;
    int l = (_settings->lastPage < 1) ? 1 : _settings->lastPage;
    fromPageInput->setText(QString("%1").arg(f));
    toPageInput->setText(QString("%1").arg(l));
  }
  rangePagesRadio->setChecked(on);
}

void QTMPrintDialog::on_copiesInput_textChanged(const QString& text)
{
  (void) text;
  collatedCheck->setEnabled(text.toInt() > 1);
}

void QTMPrintDialog::on_fromPageInput_textChanged(const QString& text)
{
  (void) text;
  if (allPagesRadio->isChecked())
    rangePagesRadio->setChecked(true);
}

void QTMPrintDialog::on_toPageInput_textChanged(const QString& text) {
  (void) text;
  if (allPagesRadio->isChecked())
    rangePagesRadio->setChecked(true);
}

void QTMPrintDialog::on_oddPagesCheck_stateChanged(int state) {
  evenPagesCheck->setEnabled(state == Qt::Checked);
}

void QTMPrintDialog::on_evenPagesCheck_stateChanged(int state) {
  oddPagesCheck->setEnabled(state == Qt::Checked);
}
