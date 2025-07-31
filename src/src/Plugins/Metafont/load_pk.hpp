
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

  N16  inputbyte;
  N16  flagbyte; 
  N16  bitweight; 
  N16  dynf;
  N16  repeatcount;
  long remainder;
  bool real_func_flag;
  int  bc;
  int  ec;
  
  /* for lazy unpacking */
  int*  char_pos;
  N16*  char_flag;
  bool* unpacked;

  pk_loader (url pk_file_name, tex_font_metric tfm, int dpi);
  Z16 pkbyte ();
  Z32 pkquad ();
  Z32 pktrio ();
  Z32 pkduo  ();
  Z16 getnyb ();
  bool getbit ();
  N16 pkpackednum ();  
  N16 rest ();
  N16 realfunc ();
  N16 handlehuge (N16 i, N16 k);
  void unpack (glyph& gl);
  glyph* load_pk ();
};

#endif // defined LOAD_PK_H
