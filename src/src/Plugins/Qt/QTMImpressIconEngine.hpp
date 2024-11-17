/******************************************************************************
* MODULE   : QTMImpressIconEngine.hpp
* DESCRIPTION: A Qt6 QIconEngine for the TeXmacs Document Icons
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMIMPRESSICONENGINE_HPP
#define QTMIMPRESSICONENGINE_HPP

#include <QApplication>

#if QT_VERSION >= 0x060000

#include <QIconEngine>

class qt_simple_widget_rep;

/**
 * @brief The `QTMImpressIconEngine` class is a custom `QIconEngine` 
 * designed to render teXmacs document inside a QIcon (or a QPixmap).
 * 
 * To use this class, simply `QIcon(new QTMImpressIconEngine(widget));`.
 */
class QTMImpressIconEngine : public QIconEngine {

public:
  /**
   * @brief Construct a new QTMImpressIconEngine object
   * 
   * @param w The `qt_simple_widget_rep` object to render
   */
  QTMImpressIconEngine (qt_simple_widget_rep* w);

  /**
   * @brief Paint the icon on the given `QPainter` object
   * 
   * @param painter The `QPainter` object to paint on
   * @param rect The rectangle to paint the icon in
   * @param mode Ignored
   * @param state Ignored
   * 
   * @see QIconEngine::paint
   */
  void paint (QPainter *painter, const QRect &rect, QIcon::Mode mode, QIcon::State state) override;

  /**
   * @brief Create a `QPixmap` object from the icon
   * 
   * @param size The size of the icon
   * @param mode Ignored
   * @param state Ignored
   * @return QPixmap The rendered icon
   * 
   * @see QIconEngine::pixmap
   */
  QPixmap pixmap (const QSize &size, QIcon::Mode mode, QIcon::State state) override;

  /**
   * @brief Get the actual size of the icon
   * 
   * @param size The size of the icon
   * @param mode Ignored
   * @param state Ignored
   * @return QSize The actual size of the icon
   * 
   * @see QIconEngine::actualSize
   */
  virtual QSize actualSize (const QSize &size,
			    QIcon::Mode mode, QIcon::State state) override;

  /**
   * @brief Clone the `QTMImpressIconEngine` object
   * 
   * @return QIconEngine* A new `QTMImpressIconEngine` object
   * 
   * @see QIconEngine::clone
   */
  virtual QIconEngine *clone() const override;

private:
  qt_simple_widget_rep *wid;
  QSize iconSize;
};

#endif

#endif
