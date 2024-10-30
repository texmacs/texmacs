/******************************************************************************
* MODULE   : QTMSVGIconEngine.hpp
* DESCRIPTION: A Qt6 QIconEngine for the TeXmacs SVG icons
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMSVGICONENGINE_HPP
#define QTMSVGICONENGINE_HPP

#include <QApplication>

#if QT_VERSION >= 0x060000

#include "config.h"

#include <QSvgRenderer>
#include <QIconEngine>
#include <QPainter>

/**
 * @brief The `QTMSVGIconEngine` class is a custom `QIconEngine` designed 
 * to render SVG icons for the TeXmacs application.
 * 
 * Since TeXmacs SVG files do not have a `viewBox` attribute, the class
 * calculates the viewbox based on the default size of the SVG and adds
 * a small margin to prevent clipping of the SVG content when rendered.
 * 
 * To use this class, simply `QIcon(new QTMSVGIconEngine(path));`
 * where `path` is the path to the SVG file. You do not have to manage
 * the memory of the `QTMSVGIconEngine` object, as the `QIcon` object
 * will take care of it.
 */
class QTMSVGIconEngine : public QIconEngine {

public:
  /**
   * @brief Construct a new QTMSVGIconEngine object
   * 
   * @param path The path to the SVG file
   */
  QTMSVGIconEngine(QString path);

  /**
   * @brief Paint the SVG icon on the given `QPainter` object
   * 
   * @param painter The `QPainter` object to paint on
   * @param rect The rectangle to paint the icon in
   * @param mode Ignored
   * @param state Ignored
   * 
   * @see QIconEngine::paint
   */
  void paint(QPainter *painter, const QRect &rect, QIcon::Mode mode, QIcon::State state) override;

  /**
   * @brief Create a `QPixmap` object from the SVG icon
   * 
   * @param size The size of the pixmap to create
   * @param mode Ignored
   * @param state Ignored
   * @return QPixmap The created pixmap object
   * 
   * @see QIconEngine::pixmap
   */
  QPixmap pixmap(const QSize &size, QIcon::Mode mode, QIcon::State state) override;

  /**
   * @brief Return the size of the SVG icon when requested the given size
   * 
   * @param size The requested size
   * @param mode Ignored
   * @param state Ignored
   * 
   * @return QSize The size of the icon when rendered at the requested size
   */
  QSize actualSize(const QSize &size, QIcon::Mode mode = QIcon::Normal, QIcon::State state = QIcon::Off) override;

  /**
   * @brief Clone the `QTMSVGIconEngine` object
   * 
   * @return QIconEngine* A new `QTMSVGIconEngine` object
   * 
   * @see QIconEngine::clone
   */
  QIconEngine *clone() const override;

private:
  QString mPath;
  QSvgRenderer mSvgRenderer;
};

#endif // QT_VERSION >= 0x060000
#endif // QTMSVGICONENGINE_HPP