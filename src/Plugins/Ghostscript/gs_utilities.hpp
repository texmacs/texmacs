
/******************************************************************************
* MODULE     : gs_utilities.hpp
* DESCRIPTION: Utilities for GhostScript
* COPYRIGHT  : (C) 2010 David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GS_UTILITIES_HPP
#define GS_UTILITIES_HPP

#include "tm_configure.hpp"
#ifdef USE_GS

#include "url.hpp"

string gs_prefix ();
string eps_device ();
bool gs_supports (url image);
void gs_image_size (url image, int& w_pt, int& h_pt);
bool gs_PDFimage_size (url image, int& w_pt, int& h_pt);
bool gs_to_png (url image, url png, int w_px, int h_px);
void gs_to_eps (url image, url eps);
void gs_to_pdf (url image, url pdf, int w_pt, int h_pt); //notice reversed dimensions order !
void gs_to_pdf (url doc, url pdf, bool landsc, double paper_h, double paper_w);
bool gs_PDF_EmbedAllFonts (url image, url pdf);
void gs_to_ps (url doc, url ps, bool landsc, double paper_h, double paper_w);
bool gs_check (url doc);
string pdf_version (url image);
string pdf_version ();
string default_pdf_version ();
void tm_gs (url image);

#endif // USE_GS

#endif // GS_UTILITIES_HPP

