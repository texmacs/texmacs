
/******************************************************************************
* MODULE     : free_type.hpp
* DESCRIPTION: Interface with Free Type II
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef FREE_TYPE_H
#define FREE_TYPE_H
#include "tree.hpp"

bool ft_initialize ();
bool ft_present ();

#ifdef USE_FREETYPE
#include <ft2build.h>
#include FT_FREETYPE_H 

extern FT_Library ft_library;

extern FT_Error (*ft_new_face)       (FT_Library     library,
				      const char*    filepathname,
				      FT_Long        face_index,
				      FT_Face*       aface);
extern FT_Error (*ft_set_char_size)  (FT_Face        face,
				      FT_F26Dot6     char_width,
				      FT_F26Dot6     char_height,
				      FT_UInt        horz_resolution,
				      FT_UInt        vert_resolution);
extern FT_UInt  (*ft_get_char_index) (FT_Face        face,
				      FT_ULong       charcode);
extern FT_Error (*ft_load_glyph)     (FT_Face        face,
				      FT_UInt        glyph_index,
				      FT_Int         load_flags);
extern FT_Error (*ft_render_glyph)   (FT_GlyphSlot   slot,
				      FT_Render_Mode render_mode);

#endif

#endif // FREE_TYPE_H
