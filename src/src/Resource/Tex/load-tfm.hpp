
/******************************************************************************
* MODULE     : load-tfm.h
* DESCRIPTION: load TeX-font-metric file
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef LOAD_TFM_H
#define LOAD_TFM_H
#include "file.hpp"
#include "tex.hpp"

tex_font_metric load_tfm (url file_name, string family, int size);

#endif // defined LOAD_TFM_H
