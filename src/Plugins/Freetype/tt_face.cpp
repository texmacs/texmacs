
/******************************************************************************
* MODULE     : tt_face.cpp
* DESCRIPTION: resources for true type faces, gliefs and metrics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "font.hpp"
#include "tt_face.hpp"
#include "Freetype/tt_file.hpp"

RESOURCE_CODE(tt_face);

inline int tt_round (int l) { return ((l+0x400020) >> 6) - 0x10000; }
inline SI tt_si (int l) { return l<<2; }

/******************************************************************************
* Freetype faces
******************************************************************************/

tt_face_rep::tt_face_rep (string name): rep<tt_face> (name) {
  bad_face= true;
  if (ft_initialize ()) return;
  if (DEBUG_AUTO)
    cout << "TeXmacs] Loading True Type font " << name << "\n";
  url u= tt_font_find (name);
  if (is_none (u)) return;
  char* _name= as_charp (concretize (u));
  if (ft_new_face (ft_library, _name, 0, &ft_face)) { delete[] _name; return; }
  delete[] _name;
  bad_face= false;
}

tt_face
load_tt_face (string name) {
  return make (tt_face, name, new tt_face_rep (name));
}

/******************************************************************************
* Font metrics
******************************************************************************/

static metric error_metric;

tt_font_metric_rep::tt_font_metric_rep (
  string name, string family, int size2, int dpi2):
  font_metric_rep (name), size (size2), dpi (dpi2)
{
  face= load_tt_face (family);
  bad_font_metric= face->bad_face ||
    ft_set_char_size (face->ft_face, 0, size<<6, dpi, dpi);
  if (bad_font_metric) return;

  error_metric->x1= error_metric->y1= 0;
  error_metric->x2= error_metric->y2= 0;
  error_metric->x3= error_metric->y3= 0;
  error_metric->x4= error_metric->y4= 0;

  int i;
  fnm = new metric [256];
  done= new bool   [256];
  for (i=0; i<256; i++) done[i]= false;
}

metric&
tt_font_metric_rep::get (int i) {
  if ((i<0) || (i>255)) return error_metric;
  if (!done[i]) {
    ft_set_char_size (face->ft_face, 0, size<<6, dpi, dpi);
    FT_UInt glyph_index= ft_get_char_index (face->ft_face, i);
    if (ft_load_glyph (face->ft_face, glyph_index, FT_LOAD_DEFAULT))
      return error_metric;
    FT_GlyphSlot slot= face->ft_face->glyph;
    if (ft_render_glyph (slot, ft_render_mode_mono)) return error_metric;
    metric& M= fnm[i];
    int w= slot->bitmap.width;
    int h= slot->bitmap.rows;
    SI ww= w * PIXEL;
    SI hh= h * PIXEL;
    SI dx= tt_si (slot->metrics.horiBearingX);
    SI dy= tt_si (slot->metrics.horiBearingY);
    SI ll= tt_si (slot->metrics.horiAdvance);
    M->x1= 0;
    M->y1= dy - hh;
    M->x2= ll;
    M->y2= dy;
    M->x3= dx;
    M->y3= dy - hh;
    M->x4= dx + ww;
    M->y4= dy;
    done[i]= true;
  }
  return fnm [i];
}

font_metric
tt_font_metric (string family, int size, int dpi) {
  string name= family * as_string (size) * "@" * as_string (dpi);
  return make (font_metric, name,
	       new tt_font_metric_rep (name, family, size, dpi));
}

/******************************************************************************
* Font glyphs
******************************************************************************/

static glyph error_glyph;

tt_font_glyphs_rep::tt_font_glyphs_rep (
  string name, string family, int size2, int dpi2):
  font_glyphs_rep (name), size (size2), dpi (dpi2)
{
  face= load_tt_face (family);
  bad_font_glyphs= face->bad_face ||
    ft_set_char_size (face->ft_face, 0, size<<6, dpi, dpi);
  if (bad_font_glyphs) return;

  int i;
  fng = new glyph [256];
  done= new bool  [256];
  for (i=0; i<256; i++) done[i]= false;
}

glyph&
tt_font_glyphs_rep::get (int i) {
  if ((i<0) || (i>255)) return error_glyph;
  if (!done[i]) {
    ft_set_char_size (face->ft_face, 0, size<<6, dpi, dpi);
    FT_UInt glyph_index= ft_get_char_index (face->ft_face, i);
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
    for (y=0; y<h; y++) {
      for (x=0; x<w; x++) {
	unsigned char c= buf[x>>3];
	G->set_1 (x, y, (c >> (7-(x&7))) & 1);
      }
      buf += pitch;
    }
    fng[i] = G;
    done[i]= true;
  }
  return fng [i];
}

font_glyphs
tt_font_glyphs (string family, int size, int dpi) {
  string name= family * as_string (size) * "@" * as_string (dpi);
  return make (font_glyphs, name,
	       new tt_font_glyphs_rep (name, family, size, dpi));
}
