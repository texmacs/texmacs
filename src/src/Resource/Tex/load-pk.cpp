
/******************************************************************************
* MODULE     : load-pk.cpp
* DESCRIPTION: load pk files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*******************************************************************************
* This program is a modified version of the program 'pk2bm' by Piet Tutelaers.
* It also uses parts of 'dvips' by Tomas Rokicki
******************************************************************************/

#include "string.hpp"
#include "file.hpp"
#include "tex.hpp"
#include "Tex/load-pk.hpp"

typedef short HI;
#define PIXEL 256

/******************************************************************************
* Initialize the pk_loader
******************************************************************************/

pk_loader::pk_loader (url pk_file_name, tex_font_metric tfm2, int dpi2):
  file_name (pk_file_name), tfm (tfm2), dpi (dpi2),
  inputbyte (0), flagbyte (0), bitweight (0), dynf (0),
  repeatcount (0), remainder (0), real_func_flag (true),
  bc (tfm->bc), ec (tfm->ec)
{
  (void) load_string (pk_file_name, input_s);
  input_pos= 0;
}

/******************************************************************************
* File handling
******************************************************************************/

HI
pk_loader::pkbyte () {
  if (input_pos == N(input_s)) {
    cerr << "\npk file= " << file_name << "\n";
    fatal_error ("unexpected eof in pk file", "pkbyte", "load-pk.cpp");
  }
  return (HI) ((QN) input_s [input_pos++]);
}

SI
pk_loader::pkduo () {
  register SI i;

  i = pkbyte ();
  if (i > 127) i -= 256;
  i = i * 256 + pkbyte ();
  return i;
}

SI
pk_loader::pktrio () {
  register SI i;

  i = pkbyte ();
  i = i * 256 + pkbyte ();
  i = i * 256 + pkbyte ();
  return i;
}

SI
pk_loader::pkquad () {
  register SI i;

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
  register HN i, j; 
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
  cerr << "\npk file= " << file_name << "\n";
  fatal_error ("unexpected situation", "rest", "load-pk.cpp");
  return 0; // Because of bug in certain versions of g++
}

HN
pk_loader::handlehuge (HN i , HN k) {
  register long j = k;
  
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
  bitmap_char& bmc;
  int x, y;
  QN* pos;
  int bit;

  char_bitstream (bitmap_char& bmc2):
    bmc (bmc2), x(0), y(0), pos (bmc->raster), bit (0) {}
  void write (int num, int times=1, int repeat=0) {
    int i, j;
    for (i=0; i<times; i++) {
      (*pos) += (num<<bit);
      bit= (bit+1)&7;
      if (bit==0) pos++;
      x++;
      if (x==bmc->width) {
	x=0; y++;
	while (repeat>0) {
	  for (j=0; j<bmc->width; j++) {
	    (*pos) += ((bmc->get_1 (j, y-1))<<bit);
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
pk_loader::unpack (bitmap_char& bmc) { 
  register SI i, j;
  register HN wordweight;
  HI rowsleft; 
  bool turnon;
  HI hbit;
  HN count; 
  char_bitstream bit_out (bmc);

  real_func_flag = true;
  dynf = flagbyte / 16; 
  turnon = flagbyte & 8; 

  if (dynf == 14) {
    bitweight = 0 ; 
    for (j=0; j<bmc->height; j++)
      for (i=0; i<bmc->width; i++)
	bit_out.write (getbit ()? 1: 0);
  }

  else {
    rowsleft = bmc->height; 
    hbit = bmc->width; 
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
	  hbit = bmc->width; 
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
    if ((rowsleft != 0) || (hbit != bmc->width)) {
      cerr << "\npk file= " << file_name << "\n";
      fatal_error ("more bits than required while unpacking",
		   "unpack", "load-pk.cpp");
    }
  }
}

/******************************************************************************
* Reading the font
******************************************************************************/

bitmap_char*
pk_loader::load_pk () {
  register HI i;
  register SI k;
  register SI length;

  register HI charcode= 0;
  register SI cwidth;
  register SI cheight;
  register SI xoff;
  register SI yoff;
  
  bitmap_char* bmf= new bitmap_char [ec+1-bc];

  // Preamble
  if (pkbyte ()!=247) {
    cerr << "\npk file= " << file_name << "\n";
    fatal_error ("bad pk file, expected pre", "load_pk", "load-pk.cpp");
  }
  if (pkbyte ()!=89) {
    cerr << "\npk file= " << file_name << "\n";
    fatal_error ("bad version of pk file", "load_pk", "load-pk.cpp");
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
	length = (flagbyte & 7) * 256 + pkbyte () - 4;
	charcode = pkbyte ();
	i = pktrio ();  /* TFM width */
	i = pkbyte (); 	/* pixel width */
	break;
      case 4:
	length = pkbyte () * 256;
	length = length + pkbyte () - 5;
	charcode = pkbyte ();
	i = pktrio ();                  /* TFM width */
	i = pkbyte ();
	i = i * 256 + pkbyte ();        /* pixelwidth */
	break;
      case 5:
	cerr << "\npk file= " << file_name << "\n";
	cerr << "last charcode= " << charcode << "\n";
	fatal_error ("lost sync in pk file (character too big / status = 5)",
		     "load_pk", "load-pk.cpp");
      case 6:
	cerr << "\npk file= " << file_name << "\n";
	cerr << "last charcode= " << charcode << "\n";
	fatal_error ("lost sync in pk file (character too big / status = 6)",
		     "load_pk", "load-pk.cpp");
      case 7:
	length = pkquad () - 12;
	charcode = pkquad ();
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
	// cout << "---> unpacking " << charcode << "!\n";
	bitmap_char bmc (cwidth, cheight, xoff, yoff);
	unpack (bmc);
	bmf [((QN) charcode)- bc]= bmc;
	// cout << "---> " << charcode << " done !\n";
	// cout << bmf [((QN) charcode)- bc] << "\n";
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
	cerr << "\npk file= " << file_name << "\n";
	cerr << "last charcode= " << charcode << "\n";
	cerr << "flagbyte= " << flagbyte << "\n";
	fatal_error ("lost sync in pk file", "load_pk", "load-pk.cpp");
      }
    }
  }

  register int c;
  for (c=bc; c<=ec; c++) {
    if (tfm->tag (c)==3) {
      if (tfm->bot(c)!=0) bmf[tfm->bot (c)]->status |= 1;
      if (tfm->top(c)!=0) bmf[tfm->top (c)]->status |= 2;
      if (tfm->mid(c)!=0) bmf[tfm->mid (c)]->status |= 3;
      if (tfm->rep(c)!=0) bmf[tfm->rep (c)]->status |= 3;
    }
  }
  for (c=0; c<=ec-bc; c++)
    if (!nil (bmf[c])) {
      if (bmf[c]->status != 0) bmf[c]->yoff= 0;
      SI design_size = tfm->design_size () >> 12;
      SI display_size= (((design_size*dpi)/72)*PIXEL) >> 8;
      double unit    = ((double) display_size) / ((double) (1<<20));
      SI lwidth= (SI) (((double) (tfm->w(c+bc))) * unit);
      bmf[c]->lwidth= ((lwidth+(PIXEL>>1)) / PIXEL);
    }

  return bmf;
}
