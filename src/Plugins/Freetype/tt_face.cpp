
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
#include "sys_utils.hpp"

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

  FILE *font_file = texmacs_fopen (concretize (u), "r");
  if (!font_file) {
    debug_fonts << "Can't load " << name << LF;
    return;
  }
  ssize_t fsize = texmacs_fsize (font_file);
  if (fsize <= 0) {
    texmacs_fclose (font_file);
    debug_fonts << "Can't load " << name << LF; 
    return;
  }

  buffer = (FT_Byte*) malloc (fsize);
  ssize_t readed = texmacs_fread ((char*)buffer, fsize, font_file);
  if (readed != fsize) {
    free (buffer);
    buffer = nullptr;
    texmacs_fclose (font_file);
    debug_fonts << "Can't read " << name << LF;
    return;
  }
  texmacs_fclose(font_file);

  if (ft_new_memory_face (ft_library, buffer, fsize, 0, &ft_face)) {  
    debug_fonts << "Can't load font " << name << LF;
    free (buffer);
    buffer = nullptr;
    return; 
  }
  ft_select_charmap (ft_face, ft_encoding_adobe_custom);
  bad_face= false;
  buffer_size= (int) fsize;

  // the font file may contain an OpenType MATH table;
  // parse it from the buffer that we already hold in memory
  math_table= parse_mathtable (string ((const char*) buffer, (int) fsize));
  if (!is_nil (math_table) && DEBUG_VERBOSE) {
    debug_fonts << "Found MATH table for font " << name << "\n";
    dump_mathtable (debug_fonts, math_table);
  }
}

ot_gsub_map&
tt_face_rep::gsub_feature (string tag) {
  if (!gsub_features->contains (tag)) {
    ot_gsub_map m;
    if (buffer != nullptr)
      m= parse_gsub_feature (string ((const char*) buffer, buffer_size), tag);
    gsub_features (tag)= m;
  }
  return gsub_features (tag);
}

ot_gpos_kern
tt_face_rep::gpos_kern () {
  if (!gpos_kern_ready) {
    if (buffer != nullptr)
      gpos_kern_table=
        parse_gpos_kern (string ((const char*) buffer, buffer_size));
    gpos_kern_ready= true;
  }
  return gpos_kern_table;
}

tt_face_rep::~tt_face_rep () {
  std_warning << "tt_face_rep should not be deleted\n";
  if (ft_face) ft_done_face (ft_face);
  if (buffer) free (buffer);
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

// FT_MulFix: multiply by a 16.16 fixed point scale, rounding to nearest
static long
mul_fix (long a, long b) {
  int sign= 1;
  if (a < 0) { a= -a; sign= -sign; }
  if (b < 0) { b= -b; sign= -sign; }
  long long c= (((long long) a) * b + 0x8000) >> 16;
  return (sign > 0)? ((long) c): (-((long) c));
}

SI
tt_font_metric_rep::kerning (int left, int right) {
  if (face->bad_face) return 0;
  FT_UInt l= decode_index (face->ft_face, left);
  FT_UInt r= decode_index (face->ft_face, right);
  ft_set_char_size (face->ft_face, 0, size<<6, hdpi, vdpi);
  // OpenType fonts keep their kerning in GPOS and usually have no legacy
  // 'kern' table, which is the only one FreeType exposes
  ot_gpos_kern gk= face->gpos_kern ();
  if (!is_nil (gk) && !gk->empty ()) {
    int du= gk->get ((unsigned int) l, (unsigned int) r);
    if (du == 0) return 0;
    return tt_si ((int) mul_fix (du, face->ft_face->size->metrics.x_scale));
  }
  if (!FT_HAS_KERNING (face->ft_face)) return 0;
  FT_Vector k;
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
