
/******************************************************************************
* MODULE     : load-tex.cpp
* DESCRIPTION: simultaneously load pk and tfm file and
*              generate them if they can't be found.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "path.hpp"
#include "tex.hpp"
#include "file.hpp"
#include "tex_files.hpp"
#include "Tex/load-pk.hpp"
#include "Tex/load-tfm.hpp"

RESOURCE_CODE(tex_font_metric);
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
* Opening the .pk and .tfm files and create them if necessary
******************************************************************************/

url
tfm_find (string family, int size, int dsize, int& status, int hashed=false) {
  string name= family * (dsize==0? string (""): as_string (size)) * ".tfm";
  if (DEBUG_STD && (!hashed))
    cout << "TeXmacs] Open tfm " << name << " try " << status << "\n";
  //cout << name << " --> "; flush (cout);
  url u= resolve_tfm (name);
  //cout << u << "\n";
  if (!is_none (u)) return u;
  if ((!hashed) && ((status&1)==1)) {
    reset_tfm_path ();
    return tfm_find (family, size, dsize, status, true);
  }
  if (DEBUG_STD) cout << "TeXmacs] Failed...\n";
  status++;
  if ((status==2) && (size==dsize)) status+=2;

  switch (status) {
  case 1:
  case 3:
  case 5:
  case 7:
    system_wait ("Generating font file", name);
    make_tex_tfm (name);
    system_wait ("");
    return tfm_find (family, size, dsize, status);

  case 2:
    return tfm_find (family, dsize, dsize, status);
  case 4:
  case 6:
    return tfm_find ("cmr", 10, 10, status);

  default:
    cerr << "\nFatal error: I could not open the file " << name << "\n";
    cerr << "           : Tex seems not to be installed properly\n";
    cerr << "See routine: tfm_find\n";
    cerr << "See file   : load-tex.cpp\n\n";
    exit (1);
  }
}

url
pk_find (string family, int size, int dpi, int dsize,
	 int& status, int hashed=false)
{
  string size_name (dsize==0? string (""): as_string (size));
  string name (family * size_name * "." * as_string (dpi) * "pk");
  if (DEBUG_STD && (!hashed))
    cout << "TeXmacs] Open pk  " << name << " try " << status << "\n";
  //cout << name << " --> "; flush (cout);
  url u= resolve_pk (name);
  //cout << u << "\n";
  if (!is_none (u)) return u;
  if ((!hashed) && ((status&1)==1)) {
    reset_pk_path ();
    return pk_find (family, size, dpi, dsize, status, true);
  }
  if (DEBUG_STD) cout << "TeXmacs] Failed...\n";
  status++;
  if ((status==2) && (size==dsize)) status+=2;

  switch (status) {
  case 1:
  case 3:
  case 5:
  case 7:
    system_wait ("Generating font file",
		 family * size_name * "." * as_string (dpi) * "pk");
    make_tex_pk (family * size_name, dpi,
		 as_int (get_setting ("DPI")), "localfont");
    system_wait ("");
    return pk_find (family, size, dpi, dsize, status);

  case 2:
    return pk_find (family, dsize, mag (dpi, size, dsize), dsize, status);
  case 4:
    return pk_find ("cmr", 10, mag (dpi, size, 10), 10, status);
  case 6:
    return pk_find ("cmr", 10, 600, 10, status);

  default:
    cerr << "\nFatal error: I could not open the file " << name << "\n";
    cerr << "           : Tex seems not to be installed properly\n";
    cerr << "See routine: pk_find\n";
    cerr << "See file   : load-tex.cpp\n\n";
    exit (1);
  }
}

/******************************************************************************
* Loading the .pk and .tfm resources
******************************************************************************/

bool
load_tex (string family, int size, int dpi,
	  tex_font_metric& tfm, bitmap_font& pk)
{
  int status_tfm  = 2;
  string name_tfm = family * as_string (size);
  int status_pk   = 2;
  string name_pk  = family * as_string (size) * "." * as_string (dpi) * "pk";
  if (tex_font_metric::instances -> contains (name_tfm) &&
      bitmap_font::instances -> contains (name_pk)) {
    tfm= tex_font_metric (name_tfm);
    pk = bitmap_font (name_pk);
    return true;
  }

  url tfm_name= url_none (), pk_name= url_none ();
  tfm_name= tfm_find (family, size, 0, status_tfm);
  if (status_tfm>=4) return false;
  if (tex_font_metric::instances -> contains (family * "0"))
    tfm= tex_font_metric (family * "0");
  else {
    if (DEBUG_AUTO) cout << "TeXmacs] Loading " << family << "0.tfm\n";
    tfm= load_tfm (tfm_name, family, 0);
  }
  int sz= ((tfm->header[1]+(1<<19))>>20);
  pk_name= pk_find (family, sz, mag (dpi, size, sz), 0, status_pk);
  if (status_pk>=4) return false;
  tfm_name= tfm_find (family, size, 0, status_tfm);
  if (status_tfm>=4) return false;

  if (tex_font_metric::instances -> contains (name_tfm))
    tfm= tex_font_metric (name_tfm);
  else {
    if (DEBUG_AUTO) cout << "TeXmacs] Loading " << name_tfm << ".tfm\n";
    tfm= load_tfm (tfm_name, family, size);
    tfm->header[1]= mag (tfm->header[1], size, sz);
  }
  if (bitmap_font::instances -> contains (name_pk))
    pk= bitmap_font (name_pk);
  else {
    if (DEBUG_AUTO) cout << "TeXmacs] Loading " << name_pk << "\n";
    pk_loader pkl (pk_name, tfm, dpi);
    pk= new bitmap_font_rep (name_pk, pkl.load_pk (), tfm->bc, tfm->ec);
  }
  return true;
}

void
load_tex (string family, int size, int dpi, int dsize,
	  tex_font_metric& tfm, bitmap_font& pk)
{
  /////////////////////////////////////////////////////////////////////////////
  // No design size
  /////////////////////////////////////////////////////////////////////////////

  if (DEBUG_AUTO)
    cout << "TeXmacs] Loading " << family << size << " at " << dpi << " dpi\n";
  if (DEBUG_STD)
    cout << "TeXmacs] Design size is " << dsize << "\n";
  if (dsize==0) {
    if (load_tex (family, size, dpi, tfm, pk)) return;
    else dsize= size;
  }

  /////////////////////////////////////////////////////////////////////////////
  // Test whether TeXmacs already failed to load TeX font before
  /////////////////////////////////////////////////////////////////////////////

  string err_s;
  int orig_size  = size;
  int status_tfm = 0;
  int status_pk  = 0;
  string name_err= family * as_string (size) * "." * as_string (dpi)*"dpi";
  if (!load_string (url ("$TEXMACS_HOME_PATH/fonts/error", name_err), err_s)) {
    path err_p = as_path (err_s);
    status_tfm = err_p->item; size= (status_tfm>=4)? 10: dsize;
    status_pk  = err_p->next->item; dpi = mag (dpi, orig_size, size);
  }

  /////////////////////////////////////////////////////////////////////////////
  // Test whether we already loaded TeX font before
  /////////////////////////////////////////////////////////////////////////////

  string name_tfm = family * as_string (orig_size);
  string name_pk  = family * as_string (size) * "." * as_string (dpi) * "pk";
  if (tex_font_metric::instances -> contains (name_tfm) &&
      bitmap_font::instances -> contains (name_pk)) {
    tfm= tex_font_metric (name_tfm);
    pk = bitmap_font (name_pk);
    return;
  }

  /////////////////////////////////////////////////////////////////////////////
  // Try to open .tfm and .pk file
  /////////////////////////////////////////////////////////////////////////////

  url tfm_name= url_none (), pk_name= url_none ();
  if (status_tfm >= 6) {
    status_pk= max (status_pk, 6);
    tfm_name= tfm_find ("cmr", 10, 10, status_tfm);
    pk_name = pk_find ("cmr", 10, 600, 10, status_pk);
  }
  else if (status_tfm >= 4) {
    status_pk= max (status_pk, 4);
    tfm_name= tfm_find ("cmr", size, 10, status_tfm);
    pk_name = pk_find ("cmr", size, dpi, 10, status_pk);
  }
  else if (status_tfm >= 2) {
    status_pk= max (status_pk, 2);
    tfm_name= tfm_find (family, dsize, dsize, status_tfm);
    pk_name = pk_find (family, dsize, mag (dpi, size, dsize), dsize, status_pk);
  }
  else {
    tfm_name= tfm_find (family, size, dsize, status_tfm);
    pk_name = pk_find (family, size, dpi, dsize, status_pk);
  }

  if ((status_pk >= 6) || (status_tfm >= 6)) {
    cerr << "Warning: I could not find the TeX font "
	 << family << size << " at " << dpi << " dpi\n";
    cerr << "         I will use the standard font cmr10 at 600 dpi instead\n";
    if (status_tfm < 6) {
      status_tfm= 6;
      tfm_name= tfm_find ("cmr", 10, 10, status_tfm);
    }
  }
  else if ((status_pk >= 4) || (status_tfm >= 4)) {
    cerr << "Warning: I could not find the TeX font "
	 << family << size << " at " << dpi << " dpi\n";
    cerr << "         I will use the standard font cmr10 instead\n";
    if (status_tfm < 4) {
      status_tfm= 4;
      tfm_name= tfm_find ("cmr", size, 10, status_tfm);
    }
  }
  if ((err_s == "") && ((status_tfm >= 2) || (status_pk >= 2)))
    save_string (url ("$TEXMACS_HOME_PATH/fonts/error", name_err),
		 as_string (path (status_tfm&(-2), status_pk&(-2))));

  /////////////////////////////////////////////////////////////////////////////
  // Load the files
  /////////////////////////////////////////////////////////////////////////////

  if (tex_font_metric::instances -> contains (name_tfm))
    tfm= tex_font_metric (name_tfm);
  else {
    // if (DEBUG_AUTO) cout << "TeXmacs] Loading " << name_tfm << ".tfm\n";
    tfm= load_tfm (tfm_name, family, orig_size);
    if (status_tfm >= 6);
    else if (status_tfm >= 4)
      tfm->header[1]= mag (tfm->header[1], orig_size, 10);
    else if (status_tfm >= 2)
      tfm->header[1]= mag (tfm->header[1], orig_size, dsize);
  }
  if (bitmap_font::instances -> contains (name_pk))
    pk= bitmap_font (name_pk);
  else {
    // if (DEBUG_AUTO) cout << "TeXmacs] Loading " << name_pk << "\n";
    pk_loader pkl (pk_name, tfm, dpi);
    pk= new bitmap_font_rep (name_pk, pkl.load_pk (), tfm->bc, tfm->ec);
  }
}
