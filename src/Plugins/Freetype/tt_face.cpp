
/******************************************************************************
* MODULE     : tt_face.cpp
* DESCRIPTION: resources for true type faces, gliefs and metrics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "font.hpp"
#include "tt_face.hpp"
#include "tt_file.hpp"
#include "tm_timer.hpp"

#ifdef USE_FREETYPE

RESOURCE_CODE(tt_face);

/******************************************************************************
* Utilities
******************************************************************************/

inline int tt_round (int l) { return ((l+0x400020) >> 6) - 0x10000; }
inline SI tt_si (int l) { return l<<2; }

inline FT_UInt
decode_index (FT_Face face, int i) {
  if (i < 0xc000000) return ft_get_char_index (face, i);
  return i - 0xc000000;  
}

/******************************************************************************
* Freetype faces
******************************************************************************/

tt_face_rep::tt_face_rep (string name): rep<tt_face> (name) {
  bad_face= true;
  if (ft_initialize ()) return;
  if (DEBUG_VERBOSE)
    debug_fonts << "Loading True Type font " << name << "\n";
  url u= tt_font_find (name);
  if (is_none (u)) return;
  c_string _name (concretize (u));
  if (ft_new_face (ft_library, _name, 0, &ft_face)) {  return; }
  ft_select_charmap (ft_face, ft_encoding_adobe_custom);
  bad_face= false;
}

tt_face
load_tt_face (string name) {
  bench_start ("load tt face");
  tt_face face= make (tt_face, name, tm_new<tt_face_rep> (name));
  bench_cumul ("load tt face");
  return face;
}

/******************************************************************************
* Font metrics
******************************************************************************/

static metric error_metric;

tt_font_metric_rep::tt_font_metric_rep (
  string name, string family, int size2, int hdpi2, int vdpi2):
  font_metric_rep (name), size (size2), hdpi (hdpi2), vdpi (vdpi2), fnm (NULL)
{
  face= load_tt_face (family);
  bad_font_metric= face->bad_face ||
    ft_set_char_size (face->ft_face, 0, size<<6, hdpi, vdpi);
  if (bad_font_metric) return;

  error_metric->x1= error_metric->y1= 0;
  error_metric->x2= error_metric->y2= 0;
  error_metric->x3= error_metric->y3= 0;
  error_metric->x4= error_metric->y4= 0;
}

bool
tt_font_metric_rep::exists (int i) {
  if (face->bad_face) return false;
  if (fnm->contains (i)) return true;
  FT_UInt glyph_index= decode_index (face->ft_face, i);
  return glyph_index != 0;
}

metric&
tt_font_metric_rep::get (int i) {
  if (!face->bad_face && !fnm->contains(i)) {
    ft_set_char_size (face->ft_face, 0, size<<6, hdpi, vdpi);
    FT_UInt glyph_index= decode_index (face->ft_face, i);
    if (ft_load_glyph (face->ft_face, glyph_index, FT_LOAD_DEFAULT))
      return error_metric;
    FT_GlyphSlot slot= face->ft_face->glyph;
    if (ft_render_glyph (slot, ft_render_mode_mono)) return error_metric;
    metric_struct* M= tm_new<metric_struct> ();
    fnm(i)= (pointer) M;
    int w= slot->bitmap.width;
    int h= slot->bitmap.rows;
    SI ww= w * PIXEL;
    SI hh= h * PIXEL;
    SI xw= tt_si (slot->metrics.width);
    SI xh= tt_si (slot->metrics.height);
    SI dx= tt_si (slot->metrics.horiBearingX);
    SI dy= tt_si (slot->metrics.horiBearingY);
    SI ll= tt_si (slot->metrics.horiAdvance);
    (void) xw;
    M->x1= 0;
    M->y1= dy - xh;
    M->x2= ll;
    M->y2= dy;
    M->x3= dx;
    M->y3= dy - hh;
    M->x4= dx + ww;
    M->y4= dy;
    //cout << "Glyph " << i << " of " << res_name << "\n";
    //cout << "Logical : " << M->x1/PIXEL << ", " << M->y1/PIXEL
    //     << "; " << M->x2/PIXEL << ", " << M->y2/PIXEL << "\n";
    //cout << "Physical: " << M->x3/PIXEL << ", " << M->y3/PIXEL
    //     << "; " << M->x4/PIXEL << ", " << M->y4/PIXEL << "\n";
  }
  return *((metric*) ((void*) fnm [i]));
}

SI
tt_font_metric_rep::kerning (int left, int right) {
  if (face->bad_face || !FT_HAS_KERNING (face->ft_face)) return 0;
  FT_Vector k;
  FT_UInt l= decode_index (face->ft_face, left);
  FT_UInt r= decode_index (face->ft_face, right);
  ft_set_char_size (face->ft_face, 0, size<<6, hdpi, vdpi);
  if (ft_get_kerning (face->ft_face, l, r, FT_KERNING_DEFAULT, &k)) return 0;
  return tt_si (k.x);
}

font_metric
tt_font_metric (string family, int size, int hdpi, int vdpi) {
  string name= family * as_string (size) * "@" * as_string (hdpi);
  if (vdpi != hdpi) name << "x" << as_string (vdpi);
  return make (font_metric, name,
	       tm_new<tt_font_metric_rep> (name, family, size, hdpi, vdpi));
}

/******************************************************************************
* Font glyphs
******************************************************************************/

static glyph error_glyph;

tt_font_glyphs_rep::tt_font_glyphs_rep (
  string name, string family, int size2, int hdpi2, int vdpi2):
  font_glyphs_rep (name), size (size2),
  hdpi (hdpi2), vdpi (vdpi2), fng (glyph ())
{
  face= load_tt_face (family);
  bad_font_glyphs= face->bad_face ||
    ft_set_char_size (face->ft_face, 0, size<<6, hdpi, vdpi);
  if (bad_font_glyphs) return;
}

glyph&
tt_font_glyphs_rep::get (int i) {
  if (!face->bad_face && !fng->contains(i)) {
    ft_set_char_size (face->ft_face, 0, size<<6, hdpi, vdpi);
    FT_UInt glyph_index= decode_index (face->ft_face, i);
    if (ft_load_glyph (face->ft_face, glyph_index, FT_LOAD_DEFAULT))
      return error_glyph;
    FT_GlyphSlot slot= face->ft_face->glyph;
    if (ft_render_glyph (slot, ft_render_mode_mono)) return error_glyph;

    int w= slot->bitmap.width;
    int h= slot->bitmap.rows;
    int ox= tt_round (slot->metrics.horiBearingX);
    int oy= tt_round (slot->metrics.horiBearingY);
    int pitch= slot->bitmap.pitch;
    unsigned char *buf= slot->bitmap.buffer;
    if (pitch<0) buf -= pitch*h;
    int x, y;
    glyph G (w, h, -ox, oy);
    // mg:
    // the index variable is used by code who need the glyph_index for unicode characters
    // to locate the right glyph in the font file
    G->index = (face->ft_face->charmap &&
                face->ft_face->charmap->encoding == FT_ENCODING_UNICODE) ?
                  glyph_index : i;
    G->lwidth= (tt_si (slot->metrics.horiAdvance)+(PIXEL>>1))/PIXEL;

    for (y=0; y<h; y++) {
      for (x=0; x<w; x++) {
	unsigned char c= buf[x>>3];
	G->set_1 (x, y, (c >> (7-(x&7))) & 1);
      }
      buf += pitch;
    }
    //cout << "Glyph " << i << " of " << res_name << "\n";
    //cout << G << "\n";
    if (G->width * G->height == 0) G= error_glyph;
    fng(i)= G;
  }
  return fng(i);
}

font_glyphs
tt_font_glyphs (string family, int size, int hdpi, int vdpi) {
  string name=
    family * ":" * as_string (size) * "." * as_string (hdpi);
  if (vdpi != hdpi) name << "x" << as_string (vdpi);
  name << "tt";
  return make (font_glyphs, name,
	       tm_new<tt_font_glyphs_rep> (name, family, size, hdpi, vdpi));
}

#endif // USE_FREETYPE
