
/******************************************************************************
* MODULE     : load_tex.cpp
* DESCRIPTION: simultaneously load pk and tfm file and
*              generate them if they can't be found.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "load_tex.hpp"
#include "path.hpp"
#include "boot.hpp"
#include "Freetype/tt_file.hpp"
#include "Freetype/tt_face.hpp"

static int
mag (int dpi, int size, int dsize) {
  if ((size>=100) && (dsize<100)) dsize *= 100;
  if ((dsize>=100) && (size<100))  size *= 100;
#ifdef OS_WIN32
  return (size*((unsigned long int) dpi))/dsize;
#else
  return (size*((long long int) dpi))/dsize;
#endif
}

/******************************************************************************
* Loading tfm files
******************************************************************************/

bool
try_tfm (string family, int size, int osize, tex_font_metric& tfm) {
  string name_tfm = family * as_string (osize) * ".tfm";
  if (tex_font_metric::instances -> contains (name_tfm)) {
    tfm= tex_font_metric (name_tfm);
    return true;
  }
  string name= family * (size==0? string (""): as_string (size)) * ".tfm";
  if (DEBUG_STD) cout << "TeXmacs] Try tfm " << name << "\n";
  url u= resolve_tfm (name);
  if (is_none (u)) {
    if (exists (url ("$TEXMACS_HOME_PATH/fonts/error", name)))
      return false;
    system_wait ("Generating font file", name);
    make_tex_tfm (name);
    system_wait ("");
    u= resolve_tfm (name);
    if (is_none (u)) {
      reset_tfm_path ();
      u= resolve_tfm (name);
      if (is_none (u)) {
	save_string (url ("$TEXMACS_HOME_PATH/fonts/error", name), "");
	return false;
      }
    }
  }
  tfm= load_tfm (u, family, osize);
  if (size ==0) {
    size= ((tfm->header[1]+(1<<19))>>20);
    if (DEBUG_STD) cout << "TeXmacs] Design size = " << size << "\n";
  }
  if (size != osize)
    tfm->header[1]= mag (tfm->header[1], osize, size);
  return true;
}

bool
load_tex_tfm (string family, int size, int dsize, tex_font_metric& tfm) {
  if (try_tfm (family, size, size, tfm))
    return true;
  if (dsize != size)
    if (try_tfm (family, dsize, size, tfm))
      return true;
  if ((dsize != 10) && (size != 10))
    if (try_tfm (family, 10, size, tfm))
      return true;
  return false;
}

/******************************************************************************
* Loading pk files
******************************************************************************/

bool
try_pk (string family, int size, int dpi, int dsize,
	tex_font_metric& tfm, font_glyphs& pk)
{
  if (use_tt_fonts ()) {
    // Substitute by True Type font ?
    string tt_name= tt_find_name (family, size);
    if (tt_name != "") {
      if (font_glyphs::instances -> contains (tt_name))
	pk= font_glyphs (tt_name);
      else pk= tt_font_glyphs (tt_name, size, dpi);
      return true;
    }
  }

  // Open regular pk font
  string name_pk= family * as_string (size) * "." * as_string (dpi) * "pk";
  if (font_glyphs::instances -> contains (name_pk)) {
    pk = font_glyphs (name_pk);
    return true;
  }
  string size_name (dsize==0? string (""): as_string (size));
  string name (family * size_name * "." * as_string (dpi) * "pk");
  if (DEBUG_STD) cout << "TeXmacs] Open pk " << name << "\n";
  url u= resolve_pk (name);
  if (is_none (u)) {
    if (exists (url ("$TEXMACS_HOME_PATH/fonts/error", name)))
      return false;
    system_wait ("Generating font file", name);
    make_tex_pk (family * size_name, dpi,
		 as_int (get_setting ("DPI")), "localfont");
    system_wait ("");
    u= resolve_pk (name);
    if (is_none (u)) {
      reset_pk_path ();
      u= resolve_pk (name);
      if (is_none (u)) {
	save_string (url ("$TEXMACS_HOME_PATH/fonts/error", name), "");
	return false;
      }
    }
  }
  pk_loader pkl (u, tfm, dpi);
  pk= std_font_glyphs (name_pk, pkl.load_pk (), tfm->bc, tfm->ec);
  return true;
}

bool
load_tex_pk (string family, int size, int dpi, int dsize,
	     tex_font_metric& tfm, font_glyphs& pk) {
  if (try_pk (family, size, dpi, dsize, tfm, pk))
    return true;
  if ((dsize != size) && (dsize != 0))
    if (try_pk (family, dsize, mag (dpi, size, dsize), dsize, tfm, pk))
      return true;
  if ((dsize != 10) && (size != 10))
    if (try_pk (family, 10, mag (dpi, size, 10), dsize, tfm, pk))
      return true;
  return false;
}

/******************************************************************************
* Loading tfm and pk files
******************************************************************************/

void
load_tex (string family, int size, int dpi, int dsize,
	  tex_font_metric& tfm, font_glyphs& pk)
{
  if (DEBUG_AUTO) cout << "TeXmacs] loading " << family << size
		       << " at " << dpi << " dpi\n";
  if (load_tex_tfm (family, size, dsize, tfm) &&
      load_tex_pk (family, size, dpi, dsize, tfm, pk))
    return;
  if (load_tex_tfm ("cmr", size, 10, tfm) &&
      load_tex_pk ("cmr", size, dpi, 10, tfm, pk))
    return;
  string name= family * as_string (size) * "@" * as_string (dpi);
  cerr << "\nFatal error: I could not open " << name << "\n";
  cerr << "           : Tex seems not to be installed properly\n";
  cerr << "See routine: load_tex\n";
  cerr << "See file   : load-tex.cpp\n\n";
  exit (1);
}
