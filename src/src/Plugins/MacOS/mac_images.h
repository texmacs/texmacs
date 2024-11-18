
/******************************************************************************
* MODULE     : mac_images.h
* DESCRIPTION: interface with the MacOSX image conversion facilities
* COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MAC_IMAGES_H
#define MAC_IMAGES_H

#include "config.h"
#include "file.hpp"

#if !defined(QTTEXMACS) || AC_QT_MAJOR_VERSION < 6
bool mac_supports (url img_file) ;
bool mac_image_size (url img_file, int& w, int& h) ;
void mac_image_to_png (url img_file, url png_file, int w, int h) ;
void mac_ps_to_pdf (url ps_file, url pdf_file) ;
#endif

#endif // MAC_IMAGES_H
