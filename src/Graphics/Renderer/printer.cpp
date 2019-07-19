
/******************************************************************************
* MODULE     : printer.cpp
* DESCRIPTION: Renderer for printing post-script graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "printer.hpp"
#include "Metafont/tex_files.hpp"
#include "Freetype/tt_file.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "analyze.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#include "scheme.hpp"
#include "image_files.hpp"
#include "link.hpp"
#include "frame.hpp"
#include "converter.hpp"

#ifdef PDF_RENDERER
#include "Pdf/pdf_hummus_renderer.hpp"
#endif

string PS_CLIP_PUSH ("gsave");
string PS_CLIP_POP ("grestore");
string PS_CLIP ("cl");
string PS_LINE ("ln");
string PS_FILL ("fl");
string PS_ARC ("ac");
string PS_FILL_ARC ("fac");
string PS_STROKE ("st");
string PS_POL_START ("sp");
string PS_POL_NEXT ("np");
string PS_POL_END ("ep");
string PS1 ("u");
string PS2 ("z");

/******************************************************************************
* constructors and destructors
******************************************************************************/

printer_rep::printer_rep (
  url ps_file_name2, int dpi2, int nr_pages2,
  string page_type2, bool landscape2, double paper_w2, double paper_h2):
    renderer_rep (false),
    ps_file_name (ps_file_name2), dpi (dpi2),
    nr_pages (nr_pages2), page_type (page_type2),
    landscape (landscape2), paper_w (paper_w2), paper_h (paper_h2),
    use_alpha (get_preference ("experimental alpha") == "on"),
    linelen (0), fg ((color) (-1)), bg ((color) (-1)), opacity (255),
    pen (black), bgb (white),
    ncols (0), lw (-1), nwidths (0), cfn (""), nfonts (0),
    xpos (0), ypos (0), tex_flag (false), toc (TUPLE),
    defs ("?"), tex_chars ("?"), tex_width ("?"),
    tex_fonts ("?"), tex_font_chars (array<int>(0)), metadata ("")
{
  string tex_pro, special_pro, color_pro, texps_pro;
  load_string ("$TEXMACS_PATH/misc/convert/tex.pro", tex_pro, true);
  load_string ("$TEXMACS_PATH/misc/convert/special.pro", special_pro, true);
  load_string ("$TEXMACS_PATH/misc/convert/color.pro", color_pro, true);
  load_string ("$TEXMACS_PATH/misc/convert/texps.pro", texps_pro, true);
  
  prologue   << "%!PS-Adobe-2.0";
  if (suffix (ps_file_name) == "eps")
    prologue << " EPSF-2.0";
  prologue   << "\n"
	     << "%%Creator: TeXmacs-" TEXMACS_VERSION "\n"
	     << "%%Title: " << as_string (tail (ps_file_name)) << "\n"
	     << "%%Pages: " << as_string (nr_pages) << "\n"
	     << "%%PageOrder: Ascend\n";
  if (page_type != "user")
    prologue << "%%DocumentPaperSizes: " << page_type << "\n";
  if (landscape) {
    psw= (int) (28.36*paper_h+ 0.5);
    psh= (int) (28.36*paper_w+ 0.5);
  }
  else {
    psw= (int) (28.36*paper_w+ 0.5);
    psh= (int) (28.36*paper_h+ 0.5);
  }
  prologue << "%%BoundingBox: 0 0 "
           << as_string (psw) << " "
           << as_string (psh) << "\n";
  if (landscape)
    prologue << "%%Orientation: Landscape\n";
  prologue   << "%%EndComments\n\n"
	     << tex_pro << "\n"
	     << special_pro << "\n"
	     << texps_pro << "\n"
	     << "TeXDict begin\n"
	     << as_string ((int) (1864680.0*paper_w+ 0.5)) << " "
	     << as_string ((int) (1864680.0*paper_h+ 0.5)) << " 1000 "
	     << as_string (dpi) << " " << as_string (dpi)
	     << " (TeXmacs) @start\n";

  define (PS_CLIP, string ("/pt4 X /pt3 X /pt2 X /pt1 X\n") *
	  string ("newpath pt1 pt2 moveto pt3 pt2 lineto ") *
	  string ("pt3 pt4 lineto pt1 pt4 lineto pt1 pt2 lineto clip"));
  define (PS_LINE, string ("/pt4 X /pt3 X /pt2 X /pt1 X\n") *
	  string ("newpath pt1 pt2 moveto pt3 pt4 lineto stroke"));
  define (PS_FILL, string ("/pt4 X /pt3 X /pt2 X /pt1 X\n") *
	  string ("newpath pt1 pt2 moveto pt3 pt2 lineto ") *
	  string ("pt3 pt4 lineto pt1 pt4 lineto pt1 pt2 eofill stroke"));
  define (PS_ARC, string ("/a2 X /a1 X /r2 X /r1 X /pt2 X /pt1 X\n") *
	  string ("newpath pt1 pt2 r1 r2 a1 a2 ellipse stroke"));
  define (PS_FILL_ARC, string ("/a2 X /a1 X /r2 X /r1 X /pt2 X /pt1 X\n") *
	  string ("newpath pt1 pt2 r1 r2 a1 a2 ellipse eofill stroke"));
  define (PS_STROKE, string ("stroke"));
  define (PS_POL_START, string ("/pt2 X /pt1 X\n") *
	  string ("newpath pt1 pt2 moveto"));
  define (PS_POL_NEXT, string ("/pt2 X /pt1 X\n") *
	  string ("pt1 pt2 lineto"));
  define (PS_POL_END, string ("closepath eofill"));
  define (PS1, string ("gsave"));
  define (PS2, string ("1 -1 scale show grestore"));

  cur_page= 0;
  next_page ();
}

printer_rep::~printer_rep () {
  next_page ();
  generate_toc ();
  generate_metadata ();
  body << "\n%%Trailer\n"
       << "end\n"
       << "userdict /end-hook known{end-hook} if\n"
       << "%%EOF\n";

  generate_tex_fonts ();
  prologue << "end\n"

           << "systemdict /pdfmark known{userdict /?pdfmark systemdict /exec get put}{userdict /?pdfmark systemdict /pop get put userdict /pdfmark systemdict /cleartomark get put}ifelse\n"

           << "%%EndProlog\n\n"
	   << "%%BeginSetup\n"
	   << "%%Feature: *Resolution " << as_string (dpi) << "dpi\n"
	   << "TeXDict begin\n";
  prologue << "%%BeginPaperSize: " << page_type << "\n";
  if (page_type != "user")
    prologue << page_type << "\n";
  else {
    prologue << "/setpagedevice where\n";
    prologue << "{ pop << /PageSize ["
             << as_string (psw) << " " << as_string (psh)
             << "] >> setpagedevice }\n";
    prologue << "if\n";
  }
  prologue << "%%EndPaperSize\n";
  if (landscape)
    prologue << "@landscape\n";
  prologue << "%%EndSetup\n";

  string ps_text= prologue * "\n" * body;
  save_string (ps_file_name, ps_text);
}

bool
printer_rep::is_printer () {
  return true;
}

void
printer_rep::next_page () {
  if (cur_page > 0) print ("eop\n");
  if (cur_page >= nr_pages) return;
  cur_page++;
  body << "\n%%Page: " << as_string (cur_page) << " "
       << as_string (cur_page) << "\n"
       << as_string (cur_page) << " "
       << as_string (cur_page-1) << " bop\n";

  set_clipping (0, (int) (-(dpi*PIXEL*paper_h)/2.54),
		(int) ((dpi*PIXEL*paper_w)/2.54), 0);

  fg  = (color) (-1);
  bg  = (color) (-1);
  lw  = -1;
  cfn = "";
  xpos= 0;
  ypos= 0;
}

/******************************************************************************
* subroutines for printing
******************************************************************************/

void
printer_rep::define (string s, string defn) {
  if (defs->contains (s)) return;
  defs (defn)= s;
  prologue << "/" << s << " {" << defn << "} N\n";
}

void
printer_rep::sep () {
  if ((N(body) > 0) &&
      (body [N(body)-1] != ')') &&
      (body [N(body)-1] != '\n')) {
    body << " ";
    linelen++;
    tex_flag= false;
  }
}

void
printer_rep::cr () {
  body << "\n";
  linelen= 0;
  tex_flag= false;
}

void
printer_rep::print (string s) {
  if (N(s)==0) return;
  if ((linelen>0) && (linelen+N(s)>79)) {
    body << "\n";
    linelen= 0;
    tex_flag= false;
  }
  else if (s[0]!='(') sep ();
  if (tex_flag && (s[0]=='(')) {
    body->resize (N(body)-2);
    linelen -= 2;
    s= s (1,N(s));
  }
  body << s;
  linelen += N(s);
  tex_flag= false;
}

void
printer_rep::print (SI x, SI y) {
  decode (x, y);
  print (as_string (x-dpi));
  print (as_string (y-dpi));
}

void
printer_rep::move_to (SI x, SI y) {
  x += ox; y += oy;
  if (x>=0) x= x/PIXEL; else x= (x-PIXEL+1)/PIXEL;
  if (y>=0) y= y/PIXEL; else y= (y-PIXEL+1)/PIXEL;
  if (tex_flag && (xpos==x) && (ypos==y)) return;
  if (tex_flag && (ypos==y)) {
    body->resize (N(body)-1);
    linelen -= 1;
    tex_flag= false;

    int diff= x-xpos;
    if ((diff>=-4) && (diff<=4)) print (string ((char) ('p'+diff)));
    else {
      print (as_string (diff));
      print ("b");
    }
    xpos= x;
    return;
  }
  xpos= x; ypos= y;
  print (as_string (x-dpi));
  print (as_string (-y-dpi));
  print ("a");
}

string
printer_rep::define_alpha (int a) {
  string aa= as_string (((double) a) / 255.0);
  string s= "[ /ca " * aa * " /CA " * aa * " /SetTransparency pdfmark";
  if (!defs->contains (s)) define ("A" * as_string (a), s);
  return defs[s];
}

void
printer_rep::select_color (color c) {
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  r= 10000+ ((r*1000)/255);
  g= 10000+ ((g*1000)/255);
  b= 10000+ ((b*1000)/255);
  string rr= as_string (r); rr= rr(1,2) * "." * rr(2,5);
  string gg= as_string (g); gg= gg(1,2) * "." * gg(2,5);
  string bb= as_string (b); bb= bb(1,2) * "." * bb(2,5);
  string s = rr * " " * gg * " " * bb * " setrgbcolor";
  if (use_alpha) s= s * " " * define_alpha (a);
  if (!defs->contains (s)) {
    define ("C" * as_string (ncols), s);
    ncols++;
  }
  print (defs[s]);
}

void
printer_rep::select_line_width (SI w) {
  w= w/PIXEL; if (w<1) w=1;
  string s = as_string (w) * " setlinewidth";
  if (!defs->contains (s)) {
    define ("W" * as_string (nwidths), s);
    nwidths++;
  }
  print (defs[s]);
}

/******************************************************************************
* subroutines for fonts
******************************************************************************/

static string
prepare_text (string s) {
  int i;
  string r;
  for (i=0; i<N(s); i++) {
    int c= ((unsigned char) s[i]);
    if ((s[i]=='(') || (s[i]==')') || (s[i]=='\\'))
      r << '\\' << s[i];
    else if ((c <= 32) || (c >= 128)) {
      r << '\\';
      r << ('0' + (c >> 6));
      r << ('0' + ((c >> 3) & 7));
      r << ('0' + (c & 7));
    }
    else r << s[i];
  }
  return r;
}

void
printer_rep::select_tex_font (string name) {
  if (cfn==name) return;
  cfn= name;
  print (tex_fonts [name]);
}

/******************************************************************************
* make tex characters and fonts
******************************************************************************/

static const char* hex_string= "0123456789ABCDEF";

void
printer_rep::make_tex_char (string name, unsigned char c, glyph gl) {
  // cout << "Make char " << (int) c << " of " << name << "\n";
  string char_name (name * "-" * as_string ((int) c));
  if (tex_chars->contains (char_name)) return;
  if (!tex_fonts->contains (name)) {
    tex_fonts (name)= "F" * as_string (nfonts);
    tex_font_chars (name)= array<int> (0);
    nfonts++;
  }
  tex_font_chars (name) << ((int) c);

  string hex_code;
  int i, j, count=0, cur= 0;
  for (j=0; j < gl->height; j++)
    for (i=0; i < ((gl->width+7) & (-8)); i++) {
      cur= cur << 1;
      if ((i<gl->width) && (gl->get_x(i,j)>0)) cur++;
      count++;
      if (count==4) {
        hex_code << hex_string[cur];
        cur  = 0;
        count= 0;
      }
    }

  int d1= gl->width;
  int d2= gl->height;
  int d3= 130+ gl->xoff;
  int d4= 126+ gl->yoff;
  int d5= gl->lwidth;
  if ((d1<256) && (d2<256) && (d3<256) && (d4<256) && (d5<256)) {
    hex_code << as_hexadecimal (d1, 2) << as_hexadecimal (d2, 2)
	     << as_hexadecimal (d3, 2) << as_hexadecimal (d4, 2)
	     << as_hexadecimal (d5, 2);
    hex_code= "<" * hex_code * ">";
  }
  else {
    hex_code= "[<" * hex_code * ">";
    hex_code << as_string (d1) << " " << as_string (d2) << " "
	     << as_string (d3) << " " << as_string (d4) << " "
	     << as_string (d5) << " ";
  }

  tex_chars (char_name)= hex_code;
  tex_width (char_name)= as_string (d5);
}

static string
find_ps_font_name (string name, string s) {
  int i, n= N(s);
  for (i=0; i<n; i++) {
    if (test (s, i, "/FontName /")) {
      i += 11;
      int start= i;
      while (i<n && s[i] != ' ') i++;
      return s (start, i);
    }
    while (i<n && s[i] != '\12' && s[i] != '\15') i++;
  }
  return name;
}


#define HEX_PER_LINE 30

static SI parse_length (string pfb, int& pos) {
  QN c4= (QN) pfb[pos++];
  QN c3= (QN) pfb[pos++];
  QN c2= (QN) pfb[pos++];
  QI c1= (QI) pfb[pos++];
  return (((((((SI) c1)<<8)+ ((SI) c2))<<8)+ ((SI) c3))<<8)+ c4;
}

static string pfb_to_pfa (url file) {
  //cout << "pfb_to_pfa :" << file << LF;
  string pfb, pfa;
  QN magic, type = 0;
  SI length;
  
  (void) load_string (file, pfb, true);
  int pos = 0, size = N(pfb);
  while ((pos < size) && (type != 3)) {
    parse (pfb, pos, magic);
    //cout << "magic:" << as_hexadecimal(magic,2) << LF ;
    if (magic != 128) {
      FAILED ("Not a pfb file");
    }
    parse (pfb, pos, type);
    //cout << "type:" << as_hexadecimal(type,2) << LF;
    switch (type) {
        
      case 1 :
        // plain text
        length = parse_length (pfb, pos);
        // parse (pfb, pos, length);
        //cout << "plain text of size " << length << LF;
        for (int i=0; i <length; i++) {
          QI ch;
          parse(pfb, pos, ch);
          if (ch == '\r') pfa << "\n";
          else pfa << ch;
        }
        break;
        
      case 2 :
        // binary data
        length = parse_length (pfb, pos);
        //        parse (pfb, pos, length);
        //cout << "binary data of size " << length << LF;
        for (int i=0; i <length; i++) {
          QI ch;
          parse(pfb, pos, ch);
          pfa << as_hexadecimal (ch, 2);
          if ((i+1) % HEX_PER_LINE == 0) pfa << "\n"; 
        }
        break;
        
      case 3 :
        //cout << "end of file"  << LF;
        // end of file
        break;
        
      default : 
        FAILED ("Unknown field type while reading PFB file");
        break;
        
    }
  }
  return pfa;
}

#undef HEX_PER_LINE

void
printer_rep::generate_tex_fonts () {
  hashset<string> done;
  iterator<string> it= iterate (tex_fonts);
  while (it->busy ()) {
    string fn_name= it->next ();
    array<int> a= tex_font_chars [fn_name];
    merge_sort (a);

    int i, d, l;
    string name = tex_fonts [fn_name], ttf;
    int    pos  = search_forwards (".", fn_name);
    string root = (pos==-1? fn_name: fn_name (0, pos));
#ifndef OS_WIN32 // we need pfbtopfa
    if ((pos!=-1) && ends (fn_name, "tt")) {
      int pos2= search_backwards (":", fn_name);
      root= fn_name (0, pos2);
      url u= tt_font_find (root);
      if (suffix (u) == "pfb")
        ttf = pfb_to_pfa (u);
    }
#endif

    if (ttf != "") {
      string ttf_name= find_ps_font_name (root, ttf);
      if (!done->contains (root)) {
	prologue << "%%BeginFont: " << root << "\n";
	prologue << ttf;
	prologue << "\n%%EndFont\n";
	done->insert (root);
      }

      array<string> cum;
      cum << "{}" * as_string (N(a));
      for (i=0; i<N(a); i++) {
        string w= tex_width [fn_name * "-" * as_string (a[i])];
        d= (i==0? a[0]: (a[i]-a[i-1]-1));
        if (d>0) cum << as_string (d) * "[";
        cum << w * " ";
      }
      d= 255-a[i-1];
      if (d>0) cum << as_string (d) * "[";

      int szpos = pos-1;
      while ((szpos>0) && is_numeric (fn_name[szpos-1])) szpos--;
      double sz = as_double (fn_name (szpos, pos));
      double dpi= as_double (fn_name (pos+1, N(fn_name)-2));
      string mag= as_string (83.022 * (sz/10.0) * (dpi/600.0));

      string fdef;
      for (i=N(cum)-1; i>=0; i--) fdef << cum[i];
      fdef= "/" * name * " " * fdef * " " * mag * " /" * ttf_name * " rf";
      for (i=0, l=0; i<N(fdef); i++, l++)
        if ((l<70) || (fdef[i]!=' ')) prologue << fdef[i];
        else { prologue << '\n'; l=-1; }
      prologue << "\n";
    }
    else {
      prologue << "/" << tex_fonts [fn_name]
	       << " " << as_string (N(a))
	       << " " << as_string (a[N(a)-1]+1) << " df\n";
      for (i=0; i<N(a); i++) {
        int end;
        string hex_code= tex_chars [fn_name * "-" * as_string (a[i])];
        for (end=1; end < N(hex_code); end++)
          if (hex_code[end-1]=='>') break;
        string after= hex_code (end, N(hex_code));
        if ((i>0) && (a[i]==(a[i-1]+1))) after << "I";
        else after << as_string (a[i]) << " D";
        if (i==(N(a)-1)) after << " E";
        hex_code= hex_code (0, end);
      
        int j, l, n= N(hex_code);
        for (j=0; j<n; j+=79) {
          if (n < (j+79)) prologue << hex_code (j, n);
          else prologue << hex_code (j, j+79) << "\n";
        }
        l= 79-(n%79);
        if (l<N(after)) prologue << "\n";
        prologue << after << "\n";
      }
    }
  }
}

/******************************************************************************
* Transformed rendering
******************************************************************************/

void
printer_rep::set_transformation (frame fr) {
  ASSERT (fr->linear, "only linear transformations have been implemented");

  SI cx1, cy1, cx2, cy2;
  get_clipping (cx1, cy1, cx2, cy2);
  rectangle oclip (cx1, cy1, cx2, cy2);

  frame cv= scaling (point (pixel, -pixel),
                     point (-ox+dpi*pixel, -oy-dpi*pixel));
  frame tr= invert (cv) * fr * cv;
  point o = tr (point (0.0, 0.0));
  point ux= tr (point (1.0, 0.0)) - o;
  point uy= tr (point (0.0, 1.0)) - o;
  //cout << "Set transformation " << o << ", " << ux << ", " << uy << "\n";
  double tx= o[0];
  double ty= o[1];
  print ("gsave");
  print ("[");
  print (as_string (ux[0]));
  print (as_string (ux[1]));
  print (as_string (uy[0]));
  print (as_string (uy[1]));
  print (as_string (tx));
  print (as_string (ty));
  print ("]");
  print ("concat");

  rectangle nclip= fr [oclip];
  renderer_rep::clip (nclip->x1, nclip->y1, nclip->x2, nclip->y2);
}

void
printer_rep::reset_transformation () {
  //cout << "Reset transformation\n";
  renderer_rep::unclip ();
  print ("grestore");
}

/******************************************************************************
* Clipping
******************************************************************************/

void
printer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  outer_round (x1, y1, x2, y2);
  renderer_rep::set_clipping (x1, y1, x2, y2);
  if (restore) {
    print (PS_CLIP_POP);
    cfn= "";
  }
  else {
    print (PS_CLIP_PUSH);
    print (x1, y1);
    print (x2, y2);
    print (PS_CLIP);
  }
}
  
/******************************************************************************
* graphical routines
******************************************************************************/

pencil
printer_rep::get_pencil () {
  return pen;
}

brush
printer_rep::get_background () {
  return bgb;
}

void
printer_rep::set_pencil (pencil pen2) {
  pen= pen2;
  color c= pen->get_color ();
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  opacity= a;
  if (a != 255) {
    int r2, g2, b2, a2;
    get_rgb_color (bg, r2, g2, b2, a2);
    r= (r * a + r2 * (255 - a)) / 255;
    g= (g * a + g2 * (255 - a)) / 255;
    b= (b * a + b2 * (255 - a)) / 255;
    c= rgb_color (r, g, b, 255);
  }
  if (c != fg) {
    fg= c;
    select_color (fg);
  }
  //if (pen->w != lw) {
  // FIXME: apparently, the line width can be overidden by some of
  // the graphical constructs (example file: newimpl.tm, in which
  // the second dag was not printed using the right width)
  lw= pen->get_width ();
  select_line_width (lw);
  //}
}

void
printer_rep::set_background (brush b) {
  //if (bgb==b) return;
  bgb= b;
  bg= b->get_color ();
}

void
printer_rep::draw (int ch, font_glyphs fn, SI x, SI y) {
  //cout << "Draw " << ch << " at " << (x/PIXEL) << ", " << (y/PIXEL) << "\n";
  if (opacity == 0) return;
  glyph gl= fn->get(ch);
  if (is_nil (gl)) return;
  string name= fn->res_name;
  unsigned char c= ch;
  if (ch >= 256) {
    name= name * "-" * as_string (ch / 256);
    c= (unsigned char) (ch & 255);
  }
  make_tex_char (name, c, gl);
  select_tex_font (name);
  move_to (x, y);
  print ("(" * prepare_text (string ((char) c)) * ")p");
  tex_flag= true;
  xpos += gl->lwidth;
}

void
printer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  if (opacity == 0) return;
  print (x1, y1);
  print (x2, y2);
  print (PS_LINE);
}

void
printer_rep::lines (array<SI> x, array<SI> y) {
  if (opacity == 0) return;
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  print (x[0], y[0]);
  print (PS_POL_START);
  for (i=1; i<n; i++) {
    print (x[i], y[i]);
    print (PS_POL_NEXT);
  }
  print (PS_STROKE);
}

void
printer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  select_color (bg);
  print (x1, y1);
  print (x2, y2);
  print (PS_FILL);
  select_color (fg);
}

void
printer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if (opacity == 0) return;
  if ((x1<x2) && (y1<y2)) {
    print (x1, y1);
    print (x2, y2);
    print (PS_FILL);
  }
}

void
printer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (opacity == 0) return;
  print ((x1+x2)/2, (y1+y2)/2);
  print (as_string ((x2-x1)/(2*PIXEL)));
  print (as_string ((y1-y2)/(2*PIXEL)));
  print (as_string (((double) alpha)/64));
  print (as_string (((double) (alpha+delta))/64));
  print (PS_ARC);
}

void
printer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (opacity == 0) return;
  print ((x1+x2)/2, (y1+y2)/2);
  print (as_string ((x2-x1)/(2*PIXEL)));
  print (as_string ((y1-y2)/(2*PIXEL)));
  print (as_string (((double) alpha)/64));
  print (as_string (((double) (alpha+delta))/64));
  print (PS_FILL_ARC);
}

void
printer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  if (opacity == 0) return;
  (void) convex;
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  print (x[0], y[0]);
  print (PS_POL_START);
  for (i=1; i<n; i++) {
    print (x[i], y[i]);
    print (PS_POL_NEXT);
  }
  print (PS_POL_END);
}

/*
string
incorporate_postscript (string s) {
  int i;
  string r;
  for (i=0; i<N(s); )
    if (s[i] == '%') {
      for (; (i<N(s)) && (s[i]!='\n'); i++);
      if (i<N(s)) i++;
    }
    else {
      for (; (i<N(s)) && (s[i]!='\n'); ) r << s[i++];
      if (i<N(s)) { r << s[i++]; }
    }
  return r;
}
*/

void
printer_rep::image (
  string name, string eps, SI x1, SI y1, SI x2, SI y2,
  SI w, SI h, SI x, SI y, int alpha)
{
  if (opacity == 0 || alpha == 0) return;

  double sc_x= (72.0/dpi) * ((double) (w/PIXEL)) / ((double) (x2-x1));
  double sc_y= (72.0/dpi) * ((double) (h/PIXEL)) / ((double) (y2-y1));
  cr ();
  cr ();

  print (x, y);
  print ("a");
  print ("currentpoint");
  print ("currentpoint");
  print ("translate");
  print (as_string (sc_x));
  print (as_string (sc_y));
  print ("scale");
  print ("neg");
  print ("exch");
  print ("neg");
  print ("exch");
  print ("translate");
  print (x, y);
  print ("a");
  cr ();
  /* Black Black 248 3155 a currentpoint currentpoint translate
     0.37114 0.37114 scale neg exch neg exch translate 248 3155 a */

  print ("@beginspecial");
  print (as_string (x1));
  print ("@llx");
  print (as_string (y1));
  print ("@lly");
  print (as_string (x2));
  print ("@urx");
  print (as_string (y2));
  print ("@ury");
  print (as_string (10*(x2-x1)));
  print ("@rwi");
  print ("@clip");
  print ("@setspecial");
  cr ();
  /* @beginspecial 0 @llx 0 @lly 613.291260 @urx 613.291260 @ury 6110 @rwi
     @clip @setspecial */
  
  body << "%%BeginDocument: " << name  << "\n";
  body << eps; // incorporate_postscript (eps);
  body << "%%EndDocument";
  cr ();

  print ("@endspecial");
  print (x, y);
  print ("a");
  print ("currentpoint");
  print ("currentpoint");
  print ("translate");
  print (as_string (1/sc_x));
  print (as_string (1/sc_y));
  print ("scale");
  print ("neg");
  print ("exch");
  print ("neg");
  print ("exch");
  print ("translate");
  print (x, y);
  print ("a");
  cr ();
  cr ();
  
  /* @endspecial 248 3155 a currentpoint currentpoint translate
     1 0.37114 div 1 0.37114 div scale neg exch neg exch translate
     248 3155 a 660 3073 a ... */

  (void) w; (void) h;
}

void
printer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
  (void) x1; (void) y1; (void) x2; (void) y2;
  (void) ren; (void) x; (void) y;
}

void
printer_rep::new_shadow (renderer& ren) {
  (void) ren;
}

void
printer_rep::delete_shadow (renderer& ren) {
  (void) ren;
}

void
printer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
printer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
printer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
}

renderer
printer_rep::shadow (picture& pic, SI x1, SI y1, SI x2, SI y2) {
  // NOTE: picture shadows are rasterized at PICTURE_ZOOM times the dpi
  double old_zoomf= this->zoomf;
  set_zoom_factor (5.0 * PICTURE_ZOOM);
  renderer ren= renderer_rep::shadow (pic, x1, y1, x2, y2);
  set_zoom_factor (old_zoomf);
  return ren;
}

void
printer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  (void) alpha; // FIXME
  int w= p->get_width (), h= p->get_height ();
  int ox= p->get_origin_x (), oy= p->get_origin_y ();
  int pixel= (int) (PIXEL / PICTURE_ZOOM);
  string name= "picture";
  string eps= picture_as_eps (p, 600);
  int x1= 0;
  int y1= 0;
  int x2= w;
  int y2= h;
  x -= (int) (ox * pixel);
  y -= (int) (oy * pixel);
  image (name, eps, x1, y1, x2, y2, w * pixel, h * pixel, x, y, 255);
  //save_string ("~/Temp/hummus_aux.eps", eps);
}

void
printer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  if (im->get_type () != scalable_image)
    renderer_rep::draw_scalable (im, x, y, alpha);
  else {
    url u= im->get_name ();
    rectangle r= im->get_logical_extents ();
    SI w= r->x2, h= r->y2;
    string ps_image= ps_load (u);
    string imtext= is_ramdisc (u)? "inline image": as_string (u);
    int x1, y1, x2, y2;
    if (suffix(u)=="eps") ps_bounding_box (u, x1, y1, x2, y2); //get cached value
    //if original image is not eps at this point it was already converted through a temp file
    // and it is the bbox of the temp file was cached...
    // do not call ps_bounding_box otherwise the image will get converted AGAIN
    // extract bbox from ps code that was generated
    else ps_read_bbox (ps_image, x1, y1, x2, y2); 
    image (imtext, ps_image, x1, y1, x2, y2, w, h, x, y, alpha);
  }
}

void
printer_rep::anchor (string label, SI x1, SI y1, SI x2, SI y2) {
  (void) x2; (void) y2;
  string s = "(";
  s = s << prepare_text (label) << ") cvn";
  if (linelen>0) cr ();
  print ("[ /Dest");
  print (s);
  print ("/View [/XYZ");
  print (x1, y1);
  print ("null] /DEST pdfmark");
  cr ();
}

void
printer_rep::href (string label, SI x1, SI y1, SI x2, SI y2) {
  bool preserve= (get_locus_rendering ("locus-on-paper") == "preserve");
  if (linelen>0) cr ();
  print ("[");
  if (starts (label, "#")) {
    print ("/Dest");
    print ("(" * prepare_text (label) * ") cvn");
  }
  else {
    print ("/A");
    print ("<< /S /URI /URI (" * prepare_text (label) * ") >>");
  }
  print ("/Rect [");
  print (x1 - 5*PIXEL, y1 - 10*PIXEL);
  print (x2 + 5*PIXEL, y2 + 10*PIXEL);
  print ("]");
  if (preserve)
    print ("/Border [16 16 1 [3 10]] /Color [0.75 0.5 1.0]");
  else
    print ("/Border [16 16 0 [3 10]] /Color [0.75 0.5 1.0]");
  print ("/Subtype /Link");
  print ("/ANN pdfmark");
  cr ();
}

void
printer_rep::toc_entry (string kind, string title, SI x, SI y) {
  decode (x, y);
  string ls= "1";
  if (kind == "toc-strong-1") ls= "1";
  if (kind == "toc-strong-2") ls= "2";
  if (kind == "toc-1") ls= "3";
  if (kind == "toc-2") ls= "4";
  if (kind == "toc-3") ls= "5";
  if (kind == "toc-4") ls= "6";
  if (kind == "toc-5") ls= "7";
  string ps= as_string (cur_page);
  string xs= as_string (x-dpi);
  string ys= as_string (y-dpi);
  toc << tuple (title, ls, ps, xs, ys);
}

void
structure_toc (tree t, int& i, tree& out, string level) {
  //cout << "Structure " << t[i] << " at " << level << "\n";
  if (i >= N(t)) return;
  string nlevel= t[i][1]->label;
  if (nlevel <= level) return;
  tree next= tuple (t[i]);
  i++;
  while (i < N(t)) {
    string slevel= t[i][1]->label;
    if (slevel <= nlevel) break;
    structure_toc (t, i, next, nlevel);
  }
  out << next;
}

void
printer_rep::generate_toc_item (tree t) {
  string title= t[0][0]->label;
  string level= t[0][1]->label;
  string page = t[0][2]->label;
  string x    = t[0][3]->label;
  string y    = t[0][4]->label;
  string htit = utf8_to_pdf_hex_string (title);
  print ("[");
  if (N(t) > 1) {
    print ("/Count");
    print (as_string (1 - N(t)));
  }
  print ("/Page");
  print (page);
  print ("/View [ /XYZ");
  print (x);
  print (y);
  print ("0");
  print ("]");
  print ("/Title " * htit);
  print ("/OUT pdfmark");
  cr ();
  for (int i=1; i<N(t); i++)
    generate_toc_item (t[i]);
}

void
printer_rep::generate_toc () {
  tree rew (TUPLE);
  int i=0;
  while (i < N(toc))
    structure_toc (toc, i, rew, "0");
  for (i=0; i<N(rew); i++)
    generate_toc_item (rew[i]);
}

void
printer_rep::set_metadata (string kind, string val) {
  metadata (kind)= val;
}

void
printer_rep::generate_metadata () {
  if (N(metadata) == 0) return;
  print ("[");
  if (metadata->contains ("title"))
    print ("/Title " * utf8_to_pdf_hex_string (metadata ["title"]));
  if (metadata->contains ("author"))
    print ("/Author " * utf8_to_pdf_hex_string (metadata ["author"]));
  if (metadata->contains ("subject"))
    print ("/Subject " * utf8_to_pdf_hex_string (metadata ["subject"]));
  print ("/DOCINFO pdfmark");
}

/******************************************************************************
* user interface
******************************************************************************/

bool use_pdf ();
bool use_ps ();

renderer
printer (url ps_file_name, int dpi, int nr_pages,
	 string page_type, bool landscape, double paper_w, double paper_h) {
#ifdef PDF_RENDERER
  if (use_pdf () && (suffix (ps_file_name) == "pdf" || !use_ps ()))
    return pdf_hummus_renderer (ps_file_name, dpi, nr_pages,
                                page_type, landscape, paper_w, paper_h);
#endif
  //cout << "Postscript print to " << ps_file_name << " at " << dpi << " dpi\n";
  page_type= as_string (call ("standard-paper-size", object (page_type)));
  return tm_new<printer_rep> (ps_file_name, dpi, nr_pages,
                              page_type, landscape, paper_w, paper_h);
}
