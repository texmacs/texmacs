
/******************************************************************************
* MODULE     : free_type.cpp
* DESCRIPTION: Interface with Free Type II
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Freetype/free_type.hpp"
#include "dyn_link.hpp"

#ifdef USE_FREETYPE

static bool ft_initialized= false;
static bool ft_error      = true;

FT_Library ft_library;

FT_Error (*ft_init_freetype)  (FT_Library     *alibrary);
FT_Error (*ft_new_face)       (FT_Library     library,
			       const char*    filepathname,
			       FT_Long        face_index,
			       FT_Face*       aface);
FT_Error (*ft_set_char_size)  (FT_Face        face,
			       FT_F26Dot6     char_width,
			       FT_F26Dot6     char_height,
			       FT_UInt        horz_resolution,
			       FT_UInt        vert_resolution);
FT_UInt  (*ft_get_char_index) (FT_Face        face,
			       FT_ULong       charcode);
FT_Error (*ft_load_glyph)     (FT_Face        face,
			       FT_UInt        glyph_index,
			       FT_Int         load_flags);
FT_Error (*ft_render_glyph)   (FT_GlyphSlot   slot,
			       FT_Render_Mode render_mode);

typedef FT_Error (*glyph_renderer) (FT_GlyphSlot, FT_Render_Mode);

bool
ft_initialize () {
  if (ft_initialized) return ft_error;
  ft_initialized= true;
#ifdef LINKED_FREETYPE
  ft_init_freetype = FT_Init_FreeType;
  ft_new_face      = FT_New_Face;
  ft_set_char_size = FT_Set_Char_Size;
  ft_get_char_index= FT_Get_Char_Index;
  ft_load_glyph    = FT_Load_Glyph;
  ft_render_glyph  = (glyph_renderer) ((void*) FT_Render_Glyph);
  if (ft_init_freetype (&ft_library)) return true;
  if (DEBUG_AUTO) cout << "TeXmacs] With linked TrueType support\n";
#else
  int status= debug_off ();
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Init_FreeType" ,
			 (pointer&) ft_init_freetype);
  if (ft_init_freetype == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_New_Face"      ,
			 (pointer&) ft_new_face);
  if (ft_new_face == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Set_Char_Size" ,
			 (pointer&) ft_set_char_size);
  if (ft_set_char_size == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Get_Char_Index",
			 (pointer&) ft_get_char_index);
  if (ft_get_char_index == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Load_Glyph"    ,
			 (pointer&) ft_load_glyph);
  if (ft_load_glyph == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Render_Glyph"  ,
			 (pointer&) ft_render_glyph);
  if (ft_render_glyph == NULL) return true;
  debug_on (status);
  if (ft_init_freetype (&ft_library)) return true;
  if (DEBUG_AUTO) cout << "TeXmacs] Installed TrueType support\n";
#endif
  ft_error= false;
  return false;
}

bool
ft_present () {
  return !ft_initialize ();
}

#else

bool ft_initialize () { return true; }
bool ft_present () { return false; }

#endif
