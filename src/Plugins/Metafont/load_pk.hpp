
/******************************************************************************
* MODULE     : load_pk.h
* DESCRIPTION: load pk files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef LOAD_PK_H
#define LOAD_PK_H
#include "file.hpp"
#include "tex_files.hpp"
#include "load_tfm.hpp"
#include "bitmap_font.hpp"

struct pk_loader {
  url             file_name;
  string          input_s;
  int             input_pos;
  tex_font_metric tfm;
  int             dpi;

  HN   inputbyte;
  HN   flagbyte; 
  HN   bitweight; 
  HN   dynf;
  HN   repeatcount;
  long remainder;
  bool real_func_flag;
  int  bc;
  int  ec;
  
  /* for lazy unpacking */
  int*  char_pos;
  HN*   char_flag;
  bool* unpacked;
  
  pk_loader (url pk_file_name, tex_font_metric tfm, int dpi);
  HI pkbyte ();
  SI pkquad ();
  SI pktrio ();
  SI pkduo  ();
  HI getnyb ();
  bool getbit ();
  HN pkpackednum ();  
  HN rest ();
  HN realfunc ();
  HN handlehuge (HN i, HN k);
  void unpack (glyph& gl);
  glyph* load_pk ();
};

#endif // defined LOAD_PK_H
