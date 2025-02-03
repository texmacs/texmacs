
/******************************************************************************
* MODULE     : free_type.hpp
* DESCRIPTION: Interface with Free Type II
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
extern FT_Error (*ft_new_memory_face) (FT_Library library,
            const FT_Byte* file_base,
            FT_Long        file_size,
            FT_Long        face_index,
            FT_Face*       aface);
extern FT_Error (*ft_select_charmap) (FT_Face        face,
				      FT_Encoding    encoding);
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
extern FT_Error (*ft_get_kerning)    (FT_Face        face,
                                      FT_UInt        left_glyph,
                                      FT_UInt        right_glyph,
                                      FT_UInt        kern_mode,
                                      FT_Vector      *akerning);
extern FT_Error (*ft_done_face)      (FT_Face        face);

#endif

#endif // FREE_TYPE_H
