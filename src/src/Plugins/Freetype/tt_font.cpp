
/******************************************************************************
* MODULE     : tt_font.cpp
* DESCRIPTION: True Type fonts (using FreeType II)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "font.hpp"
#include "Freetype/free_type.hpp"
#include "Freetype/tt_file.hpp"

#ifdef USE_FREETYPE

/******************************************************************************
* True Type fonts
******************************************************************************/

struct tt_font_rep: font_rep {
  bitmap_metric bmm;
  bitmap_font   bmf;

  tt_font_rep (display dis, string name, string family, int size, int dpi);
  bool compute_bitmaps (string family, int size, int dpi);

  void get_extents (string s, text_extents& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (ps_device dev, string s, SI x, SI y);
  bitmap_char get_bitmap (string s);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

#define conv(x) ((SI) (((double) (x))*unit))

tt_font_rep::tt_font_rep (display dis, string name,
  string family, int size2, int dpi):
  font_rep (dis, name)
{
  size= size2;
  bool err= compute_bitmaps (family, size, dpi);
  if (err) {
    bmm= new bitmap_metric_rep (res_name, NULL, 0, -1);
    bmf= new bitmap_font_rep   (res_name, NULL, 0, -1);
    if (DEBUG_AUTO)
      cout << "TeXmacs] Font " << family << " " << size
	   << "pt at " << dpi << " dpi could not be loaded\n";
    
  }

  // get main font parameters
  text_extents ex;
  get_extents ("f", ex);
  y1= ex->y1;
  y2= ex->y2;
  display_size = y2-y1;
  design_size  = size << 8;

  // get character heights
  get_extents ("x", ex);
  yx           = ex->y4;

  // compute other heights
  yfrac        = yx >> 1;
  ysub_lo_base = -yx/3;
  ysub_hi_lim  = (5*yx)/6;
  ysup_lo_lim  = yx/2;
  ysup_lo_base = (5*yx)/6;
  ysup_hi_lim  = yx;
  yshift       = yx/6;

  // compute widths
  wpt          = (dpi*PIXEL)/72;
  wquad        = (wpt*design_size) >> 8;
  wline        = wquad/20;

  // get fraction bar parameters
  get_extents ("-", ex);
  yfrac= (ex->y3 + ex->y4) >> 1;

  // get space length
  get_extents (" ", ex);
  spc  = space ((3*(ex->x2-ex->x1))>>2, ex->x2-ex->x1, (ex->x2-ex->x1)<<1);
  extra= spc;
  sep  = wquad/10;

  // get_italic space
  get_extents ("f", ex);
  SI italic_spc= (ex->x4-ex->x3)-(ex->x2-ex->x1);
  slope= ((double) italic_spc) / ((double) display_size);
  if (slope<0.15) slope= 0.0;
}

/******************************************************************************
* Getting the bitmaps
******************************************************************************/

inline int tt_round (int l) { return ((l+0x400020) >> 6) - 0x10000; }
inline SI tt_si (int l) { return l<<2; }

bool
tt_font_rep::compute_bitmaps (string family, int size, int dpi) {
  if (bitmap_metric::instances -> contains (res_name)) {
    bmm= bitmap_metric (res_name);
    bmf= bitmap_font (res_name);
    return false;
  }

  if (ft_initialize ()) return true;
  if (DEBUG_AUTO)
    cout << "TeXmacs] Loading " << family << " " << size
	 << "pt at " << dpi << " dpi\n";
  url u= tt_font_find (family);
  if (is_none (u)) return true;
  char* _name= as_charp (concretize (u));
  FT_Face ft_face;
  if (ft_new_face (ft_library, _name, 0, &ft_face)) {
    delete[] _name; return true; }
  delete[] _name;
  if (ft_set_char_size (ft_face, 0, size<<6, dpi, dpi)) return true;

  int i;
  FT_UInt glyph_index;
  text_extents* T= new text_extents[256];
  bitmap_char * B= new bitmap_char [256];
  for (i=0; i<256; i++) {
    glyph_index= ft_get_char_index (ft_face, i);
    if (ft_load_glyph (ft_face, glyph_index, FT_LOAD_DEFAULT)) continue;
    FT_GlyphSlot slot= ft_face->glyph;
    if (ft_render_glyph (slot, ft_render_mode_mono)) continue;

    int w= slot->bitmap.width;
    int h= slot->bitmap.rows;
    int ox= tt_round (slot->metrics.horiBearingX);
    int oy= tt_round (slot->metrics.horiBearingY);
    int pitch= slot->bitmap.pitch;
    unsigned char *buf= slot->bitmap.buffer;
    if (pitch<0) buf -= pitch*h;
    int x, y;
    bitmap_char C (w, h, -ox, oy);
    for (y=0; y<h; y++) {
      for (x=0; x<w; x++) {
	unsigned char c= buf[x>>3];
	C->set_1 (x, y, (c >> (7-(x&7))) & 1);
      }
      buf += pitch;
    }
    B[i]= C;

    text_extents E;
    SI ww= w * PIXEL;
    SI hh= h * PIXEL;
    SI dx= tt_si (slot->metrics.horiBearingX);
    SI dy= tt_si (slot->metrics.horiBearingY);
    SI ll= tt_si (slot->metrics.horiAdvance);
    E->x1= 0;
    E->y1= dy - hh;
    E->x2= ll;
    E->y2= dy;
    E->x3= dx;
    E->y3= dy - hh;
    E->x4= dx + ww;
    E->y4= dy;
    T[i]= E;
  }
  bmm= new bitmap_metric_rep (res_name, T, 0, 255);
  bmf= new bitmap_font_rep   (res_name, B, 0, 255);
  return false;
}

/******************************************************************************
* Routines for font
******************************************************************************/

void
tt_font_rep::get_extents (string s, text_extents& ex) {
  if (N(s)==0) {
    ex->x1= ex->x3= ex->x2= ex->x4=0;
    ex->y3= ex->y1= 0; ex->y4= ex->y2= yx;
  }
  else {
    QN c= s[0];
    text_extents_struct* first= bmm->get (c);
    ex->x1= first->x1; ex->y1= first->y1;
    ex->x2= first->x2; ex->y2= first->y2;
    ex->x3= first->x3; ex->y3= first->y3;
    ex->x4= first->x4; ex->y4= first->y4;
    SI x= first->x2;

    int i;
    for (i=1; i<N(s); i++) {
      QN c= s[i];
      text_extents_struct* next= bmm->get (c);
      ex->x1= min (ex->x1, x+ next->x1); ex->y1= min (ex->y1, next->y1);
      ex->x2= max (ex->x2, x+ next->x2); ex->y2= max (ex->y2, next->y2);
      ex->x3= min (ex->x3, x+ next->x3); ex->y3= min (ex->y3, next->y3);
      ex->x4= max (ex->x4, x+ next->x4); ex->y4= max (ex->y4, next->y4);
      x += next->x2;
    }
  }
}

void
tt_font_rep::get_xpositions (string s, SI* xpos) {
  register int i, n= N(s);
  if (n == 0) return;
  
  register SI x= 0;
  for (i=0; i<N(s); i++) {
    text_extents_struct* next= bmm->get ((QN) s[i]);
    x += next->x2;
    xpos[i+1]= x;
  }
}

void
tt_font_rep::draw (ps_device dev, string s, SI x, SI y) {
  if (N(s)!=0) {
    int i;
    for (i=0; i<N(s); i++) {
      QN c= s[i];
      dev->draw (c, bmf, x, y);
      text_extents_struct* ex (bmm->get (c));
      x += ex->x2;
    }
  }
}

bitmap_char
tt_font_rep::get_bitmap (string s) {
  if (N(s)!=1) return font_rep::get_bitmap (s);
  int c= ((QN) s[0]);
  bitmap_char bmc= bmf->get (c);
  if (nil (bmc)) return font_rep::get_bitmap (s);
  return bmc;
}

/******************************************************************************
* Interface
******************************************************************************/

font
tt_font (display dis, string family, int size, int dpi) {
  string name= "tt:" * family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    new tt_font_rep (dis, name, family, size, dpi));
}

#else

font
tt_font (display dis, string family, int size, int dpi) {
  string name= "tt:" * family * as_string (size) * "@" * as_string(dpi);
  cerr << "\n\nFont name= " << name << "\n";
  fatal_error ("True type support was disabled", "tt_font");
}

#endif
