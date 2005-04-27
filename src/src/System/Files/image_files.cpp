
/******************************************************************************
* MODULE     : image_files.cpp
* DESCRIPTION: image file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "file.hpp"
#include "image_files.hpp"
#include "web_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "scheme.hpp"

hashmap<tree,string> ps_bbox ("");

/******************************************************************************
* Loading xpm pixmaps
******************************************************************************/

tree
xpm_load (url u) {
  string s;
  load_string ("$TEXMACS_PIXMAPS_PATH" * u, s);
  if (s == "") load_string ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm", s);

  int i, j;
  tree t (TUPLE);
  for (i=0; i<N(s); i++)
    if (s[i]=='\x22') {
      i++;
      j=i;
      while ((i<N(s)) && (s[i]!='\x22')) i++;
      t << s (j, i);
    }
  if (N(t)==0) return xpm_load ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm");
  return t;
}

void
xpm_size (url u, int& w, int& h) {
  static hashmap<string,string> xpm_size_table ("");
  string file_name= as_string (u);
  if (!xpm_size_table->contains (file_name)) {
    tree t= xpm_load (u);
    xpm_size_table (file_name)= t[0]->label;
  }

  int i= 0;
  bool ok;
  string s= xpm_size_table[file_name];
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  if (!ok) fatal_error ("Invalid xpm (" * file_name * ")", "xpm_size");
}

/******************************************************************************
* Loading postscript files or conversion to postscript
******************************************************************************/

string
ps_load (url image) {
  url name= resolve (image);
  if (is_none (name))
    name= "$TEXMACS_PATH/misc/pixmaps/unknown.ps";
  string s= as_string (call ("image->postscript", object (name)));
  if (s == "") load_string ("$TEXMACS_PATH/misc/pixmaps/unknown.ps", s);
  return s;
}

void
ps_bounding_box (url image, int& x1, int& y1, int& x2, int& y2) {
  tree lookup= image->t;
  if (!ps_bbox->contains (image->t)) {
    int i;
    string s= ps_load (image);
    string r= "0 0 32 32";
    for (i=0; i<N(s); i++)
      if (read (s, i, "%%BoundingBox: ")) {
	double tmp;
	int j = i;
	read_line (s, i, r);
	// Check whether we really have a bounding box line with numbers
	skip_spaces (s, j);
	if(read_double (s, j, tmp))
	  break;
      }
    ps_bbox (image->t)= r;
  }

  int i= 0;
  bool ok;
  string s= ps_bbox [image->t];
  double X1=0.0, Y1=0.0, X2=0.0, Y2=0.0;
  skip_spaces (s, i);
  ok= read_double (s, i, X1);
  skip_spaces (s, i);
  ok= read_double (s, i, Y1) && ok;
  skip_spaces (s, i);
  ok= read_double (s, i, X2) && ok;
  skip_spaces (s, i);
  ok= read_double (s, i, Y2) && ok;
  x1= (int) X1; y1= (int) Y1;
  x2= (int) X2; y2= (int) Y2;
  if (ok) return;
  x1= y1= 0; x2= 596; y2= 842;
}
