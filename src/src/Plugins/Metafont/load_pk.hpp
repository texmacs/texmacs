
/******************************************************************************
* MODULE     : load_pk.h
* DESCRIPTION: load pk files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
