
/******************************************************************************
* MODULE     : pdf_hummus_renderer.hpp
* DESCRIPTION: Renderer for printing pdf graphics using the PDFHummus library
* COPYRIGHT  : (C) 2012  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PDF_HUMMUS_RENDERER_H
#define PDF_HUMMUS_RENDERER_H

#include "renderer.hpp"
#include "gui.hpp"
#include "hashmap.hpp"
#include "url.hpp"

renderer pdf_hummus_renderer (url pdf_file_name, int dpi, int nr_pages= 1,
                              string page_type= "a4", bool landscape= false,
                              double paper_w= 21.0, double paper_h= 29.7);
		  
void hummus_pdf_image_size (url image, int& w, int& h);

#endif // ifdef PDF_HUMMUS_RENDERER_H
