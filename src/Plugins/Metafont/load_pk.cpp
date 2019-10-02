
/******************************************************************************
* MODULE     : load_pk.cpp
* DESCRIPTION: load pk files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
* This program is a modified version of the program 'pk2bm' by Piet Tutelaers.
* It also uses parts of 'dvips' by Tomas Rokicki
******************************************************************************/

#include "load_tex.hpp"
#include "tm_timer.hpp"
#include "renderer.hpp" // for PIXEL

typedef short HI;

/******************************************************************************
* Initialize the pk_loader
******************************************************************************/

pk_loader::pk_loader (url pk_file_name, tex_font_metric tfm2, int dpi2):
  file_name (pk_file_name), tfm (tfm2), dpi (dpi2),
  inputbyte (0), flagbyte (0), bitweight (0), dynf (0),
  repeatcount (0), remainder (0), real_func_flag (true),
  bc (tfm->bc), ec (tfm->ec),
  char_pos(0), char_flag(0), unpacked(0)
{
  (void) load_string (pk_file_name, input_s, true);
  input_pos= 0;
}

/******************************************************************************
* File handling
******************************************************************************/

HI
pk_loader::pkbyte () {
  if (input_pos == N(input_s)) {
    failed_error << "pk file= " << file_name << "\n";
    FAILED ("unexpected eof in pk file");
  }
  return (HI) ((QN) input_s [input_pos++]);
}

SI
pk_loader::pkduo () {
  SI i;

  i = pkbyte ();
  if (i > 127) i -= 256;
  i = i * 256 + pkbyte ();
  return i;
}

SI
pk_loader::pktrio () {
  SI i;

  i = pkbyte ();
  i = i * 256 + pkbyte ();
  i = i * 256 + pkbyte ();
  return i;
}

SI
pk_loader::pkquad () {
  SI i;

  i = pkbyte ();
  if (i > 127) i -= 256;
  i = i * 256 + pkbyte ();
  i = i * 256 + pkbyte ();
  i = i * 256 + pkbyte ();
  return i;
}

/******************************************************************************
* Get a nybble, bit, and packed word from the packed data structure.
******************************************************************************/

HI
pk_loader::getnyb () {
  HN temp;
  if (bitweight == 0) {
    bitweight = 16; 
    inputbyte = pkbyte ();
    temp = inputbyte >> 4;
  } else {
    bitweight = 0;
    temp = inputbyte & 15;
  }
  return temp;
} 

bool
pk_loader::getbit ()
{
  bitweight >>= 1; 
  if (bitweight == 0) {
    inputbyte = pkbyte ();
    bitweight = 128;
  } 
  return (inputbyte & bitweight);
}

HN
pk_loader::pkpackednum () {
  HN i, j; 
  i = getnyb (); 
  if (i == 0) {
    do { j = getnyb (); i++; } while (! (j != 0)); 
    if (i > 3) return handlehuge (i, j);
    while (i > 0) {
      j= j*16 + getnyb (); 
      i--; 
    } 
    return (j-15 + (13-dynf)*16 + dynf); 
  }
  else if (i <= dynf) return i; 
  else if (i < 14) return ((i-dynf-1)*16+ getnyb()+ dynf+ 1); 
  else {
    if (i == 14) repeatcount = pkpackednum (); 
    else repeatcount = 1; 
    return realfunc ();
  } 
} 

HN
pk_loader::rest () {
  HN i;

  if (remainder < 0) {
    remainder = - remainder;
    return 0;
  } else if (remainder > 0) {
    if (remainder > 4000) {
      remainder = 4000 - remainder;
      return (4000);
    }
    i = remainder;
    remainder = 0;
    real_func_flag = true;
    return i;
  }
  failed_error << "pk file= " << file_name << "\n";
  FAILED ("unexpected situation");
  return 0;
}

HN
pk_loader::handlehuge (HN i , HN k) {
  long j = k;
  
  while (i) {
    j = (j << 4L) + getnyb();
    i--;
  }
  remainder = j - 15 + (13 - dynf) * 16 + dynf;
  real_func_flag = false;
  return (rest ());
}

HN
pk_loader::realfunc () {
  if (real_func_flag) return pkpackednum ();
  else return rest ();
}

/******************************************************************************
* Writing to a character
******************************************************************************/

struct char_bitstream {
  glyph& gl;
  int x, y;
  QN* pos;
  int bit;

  char_bitstream (glyph& gl2):
    gl (gl2), x(0), y(0), pos (gl->raster), bit (0) {}
  void write (int num, int times=1, int repeat=0) {
    int i, j;
    for (i=0; i<times; i++) {
      (*pos) += (num<<bit);
      bit= (bit+1)&7;
      if (bit==0) pos++;
      x++;
      if (x==gl->width) {
	x=0; y++;
	while (repeat>0) {
	  for (j=0; j<gl->width; j++) {
	    (*pos) += ((gl->get_1 (j, y-1))<<bit);
	    bit= (bit+1)&7;
	    if (bit==0) pos++;
	  }
	  y++;
	  repeat--;
	}
      }
    }
  }
};

/******************************************************************************
* Unpacking a character bitmap
******************************************************************************/

void
pk_loader::unpack (glyph& gl) { 
  SI i, j;
  HN wordweight;
  HI rowsleft; 
  bool turnon;
  HI hbit;
  HN count; 
  char_bitstream bit_out (gl);

  real_func_flag = true;
  dynf = flagbyte / 16; 
  turnon = flagbyte & 8; 

  if (dynf == 14) {
    bitweight = 0 ; 
    for (j=0; j<gl->height; j++)
      for (i=0; i<gl->width; i++)
	bit_out.write (getbit ()? 1: 0);
  }

  else {
    rowsleft = gl->height; 
    hbit = gl->width; 
    repeatcount = 0; 
    wordweight = 16;
    bitweight = 0;
    while (rowsleft > 0) {
      count = realfunc (); 
      bit_out.write (turnon? 1: 0, count, repeatcount);

      while (count != 0) {
	if ((count < wordweight) && (count < hbit)) {
	  hbit -= count;
	  wordweight -= count; 
	  count = 0; 
	} 
	else if ((count >= hbit) && (hbit <= wordweight)) {
	  rowsleft -= repeatcount + 1; 
	  repeatcount = 0; 
	  count -= hbit; 
	  hbit = gl->width; 
	  wordweight = 16; 
	} 
	else {
	  count -= wordweight; 
	  hbit -= wordweight; 
	  wordweight = 16; 
	} 
      }
      turnon = ! turnon; 
    }
    if ((rowsleft != 0) || (hbit != gl->width)) {
      failed_error << "pk file= " << file_name << "\n";
      FAILED ("more bits than required while unpacking");
    }
  }
}

/******************************************************************************
* Reading the font
******************************************************************************/

glyph*
pk_loader::load_pk () {
  HI i;
  SI k;
  SI length = 0, startpos = 0;

  HI charcode= 0;
  SI cwidth;
  SI cheight;
  SI xoff;
  SI yoff;
  
  bench_start ("decode pk");
  glyph* fng= tm_new_array<glyph> (ec+1-bc);
  char_pos = tm_new_array<int> (ec+1-bc);
  unpacked = tm_new_array<bool> (ec+1-bc);
  char_flag = tm_new_array<HN> (ec+1-bc);
  for(i=0;i<ec+1-bc;i++) {
    char_pos[i] = 0;
    char_flag[i] = 0;
    unpacked[i] = true; // set to false for still unpacked glyphs
  }

  // Preamble
  if (pkbyte ()!=247) {
    failed_error << "pk file= " << file_name << "\n";
    FAILED ("bad pk file");
  }
  if (pkbyte ()!=89) {
    failed_error << "pk file= " << file_name << "\n";
    FAILED ("bad version of pk file");
  }
  for(i=pkbyte (); i>0; i--) (void) pkbyte (); /* creator of pkfile */
  (void) pkquad (); /* design size */
  k = pkquad ();    /* checksum    */
  k = pkquad ();    /* hppp        */
  k = pkquad ();    /* vppp	  */

  // The character definition
  while ((flagbyte=pkbyte ())!=245) {
    if (flagbyte < 240) {
      switch (flagbyte & 7) {
      case 0:
      case 1:
      case 2:
      case 3:
	length = (flagbyte & 7) * 256 + pkbyte ();
	charcode = pkbyte ();
	startpos = input_pos;
	i = pktrio ();  /* TFM width */
	i = pkbyte (); 	/* pixel width */
	break;
      case 4:
	length = pkbyte () * 256;
	length = length + pkbyte ();
	charcode = pkbyte ();
	startpos = input_pos;
	i = pktrio ();                  /* TFM width */
	i = pkbyte ();
	i = i * 256 + pkbyte ();        /* pixelwidth */
	break;
      case 5:
	failed_error << "pk file= " << file_name << "\n";
	failed_error << "last charcode= " << charcode << "\n";
	FAILED ("lost sync in pk file (character too big / status = 5)");
	break;
      case 6:
	failed_error << "pk file= " << file_name << "\n";
	failed_error << "last charcode= " << charcode << "\n";
	FAILED ("lost sync in pk file (character too big / status = 6)");
	break;
      case 7:
	length = pkquad ();
	charcode = pkquad ();
	startpos = input_pos;
	(void) pkquad ();		/* TFMwidth */
	                   /* pixelwidth = (pkquad () + 32768) >> 16; */
	(void) pkquad ();		/* pixelwidth */
	k = pkquad ();
      }
      /*
      cout << "---------------------------------------------------------------------------\n";
      cout << "Reading character " << charcode << "\n";
      cout << "---------------------------------------------------------------------------\n";
      */
      if (flagbyte & 4) { // the long and extended formats are bugged !
	if ((flagbyte & 7) == 7) { // extended format
	  cwidth = pkquad ();;
	  cheight = pkquad ();
	  xoff = pkquad ();
	  yoff = pkquad ();
	} else { // long format
	  cwidth = pkduo ();
	  cheight = pkduo ();
	  xoff = pkduo ();
	  yoff = pkduo ();
	}
      } else { // short format
	cwidth = pkbyte ();
	cheight = pkbyte ();
	xoff = pkbyte ();
	yoff = pkbyte ();
	if (xoff > 127) xoff -= 256;
	if (yoff > 127) yoff -= 256;
      }
      if ((cwidth > 0) && (cheight > 0) &&
	  (((QN) charcode) >= bc) && (((QN) charcode) <= ec)) {
	// cout << "---> unpacking " << charcode
	//     << " at " << input_pos 
	//     << " (start " << startpos << ", length " << length << ")!\n";
	glyph gl (cwidth, cheight, xoff, yoff);

	/* needed for lazy unpacking */
	char_pos[((QN) charcode)- bc] = input_pos;
	char_flag[((QN) charcode)- bc] = flagbyte;
	unpacked[((QN) charcode)- bc] = false;
	/* skip packed bitmap */
	input_pos = startpos + length;

	fng [((QN) charcode)- bc]= gl;
	// cout << "---> " << charcode << " done !\n";
	// cout << fng [((QN) charcode)- bc] << "\n";
      }
    }

    else {
      k = 0;
      switch (flagbyte) {
      case 243: k = pkbyte (); if (k > 127) k -= 256;
      case 242: k = k * 256 + pkbyte ();
      case 241: k = k * 256 + pkbyte ();
      case 240: k = k * 256 + pkbyte ();
	while (k-- > 0) i = pkbyte ();
	break;
      case 244: k = pkquad (); break;
      case 246: break;
      default :
	failed_error << "pk file= " << file_name << "\n";
	failed_error << "last charcode= " << charcode << "\n";
	failed_error << "flagbyte= " << flagbyte << "\n";
	FAILED ("lost sync in pk file");
      }
    }
  }

  int c;
  for (c=0; c<=ec-bc; c++)
    if (!is_nil (fng[c])) {
      SI design_size = tfm->design_size () >> 12;
      SI display_size= (((design_size*dpi)/72)*PIXEL) >> 8;
      double unit    = ((double) display_size) / ((double) (1<<20));
      SI lwidth= (SI) (((double) (tfm->w(c+bc))) * unit);
      fng[c]->lwidth= ((lwidth+(PIXEL>>1)) / PIXEL);
    }

  bench_cumul ("decode pk");
  return fng;
}
