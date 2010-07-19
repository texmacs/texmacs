
/******************************************************************************
* MODULE     : pdf_renderer.cpp
* DESCRIPTION: Renderer for printing pdf graphics
* COPYRIGHT  : (C) 2010 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "pdf_renderer.hpp"
#include "Metafont/tex_files.hpp"
#include "Freetype/tt_file.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "analyze.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#include "scheme.hpp"
#include "sys_utils.hpp"
#include "convert.hpp"


extern "C" {
  
#include "dvipdfmx/pdfdoc.h"
#include "dvipdfmx/pdfdev.h"
#include "dvipdfmx/pdfdraw.h"  
#include "dvipdfmx/pdffont.h"
#include "dvipdfmx/pdfximage.h"
  
void error_cleanup (void) ;
  
};





  
void error_cleanup (void) 
  {
    pdf_close_images();  /* delete temporary files */
    pdf_error_cleanup();
#if 0
    if (pdf_filename) {
      remove(pdf_filename);
      fprintf(stderr, "\nOutput file removed.\n");
    }
#endif
  }
  



//static double paper_width  = 595.0;
//static double paper_height = 842.0;
static double annot_grow    = 0.0;
static int    bookmark_open = 0;
static int    font_dpi      = 600;

static long opt_flags = 0;



#define OPT_TPIC_TRANSPARENT_FILL (1 << 1)
#define OPT_CIDFONT_FIXEDPITCH    (1 << 2)
#define OPT_FONTMAP_FIRST_MATCH   (1 << 3)
#define OPT_PDFDOC_NO_DEST_REMOVE (1 << 4)

static int do_encryption    = 0;


static double dvi2pts = 1.0;

/*
 * Precision is essentially limited to 0.01pt.
 * See, dev_set_string() in pdfdev.c.
 */
static int pdfdecimaldigits = 2;
static char   ignore_colors = 0;




double
pdf_renderer_rep::to_x (SI x) {
  return p * (ox + x);  
}

double
pdf_renderer_rep::to_y (SI y) {
  return  p * ( oy + y);  
}

void
pdf_renderer_rep::select_color (color c) {;
  int r, g, b;
  get_rgb_color (c, r, g, b);
  double dr= (double)r / 255;
  double dg= (double)g / 255;
  double db= (double)b / 255;
  
  
  pdf_color color;
  pdf_color_rgbcolor(&color, dr, dg, db);
  pdf_dev_set_color(&color,0,0); // stroke color;
  pdf_dev_set_color(&color,0x20,0); // non-stroking color;
  
#if 0
  HPDF_Page_SetRGBFill (page, dr, dg, db);
  HPDF_Page_SetRGBStroke (page, dr, dg, db);
#endif
}

void
pdf_renderer_rep::select_line_width (SI w) {
  double pw= p * w;
  //if (pw < 1) pw= 1;
//  HPDF_Page_SetLineWidth (page, pw);
}

/******************************************************************************
* constructors and destructors
******************************************************************************/

pdf_renderer_rep::pdf_renderer_rep (
  url pdf_file_name2, int dpi2, int nr_pages2,
  string page_type2, bool landscape2, double paper_w2, double paper_h2):
    pdf_file_name (pdf_file_name2), dpi (dpi2),
    nr_pages (nr_pages2), page_type (page_type2),
    landscape (landscape2), paper_w (paper_w2), paper_h (paper_h2),
    fg (-1), bg (-1),
    lw (-1),  cfn (""), cfid(0),
    tex_fonts (0),
    image_resources (0)
{
//  dvi2pts = 1.0;
  width= default_dpi * paper_w / 2.54;
  height= default_dpi * paper_h / 2.54;
  dvi2pts = (double)default_dpi / dpi / PIXEL;
  p= (double)default_dpi / dpi / PIXEL;
//  p = 1.0;
  if (landscape) {
    width= (width > height)? width : height;
    height= (width > height)? height : width;
  } else {
    width= (width > height)? height : width;
    height= (width > height)? width : height;
  }
  
  set_origin(0, paper_h*dpi*PIXEL/2.54);
  
 // mem_debug_init();
  pdf_init_fontmaps(); /* This must come before parsing options... */
  pdf_font_set_dpi(dpi);
  pdf_doc_set_creator("TeXmacs");
  pdf_set_version(3);
  pdf_files_init();
  /* Set default paper size here so that all page's can inherite it.
   * annot_grow:    Margin of annotation.
   * bookmark_open: Miximal depth of open bookmarks.
   */
  
  {
    char* _pdf_file_name;
    _pdf_file_name= as_charp (concretize (pdf_file_name));

    pdf_open_document(_pdf_file_name, do_encryption,
                    width, height, annot_grow, bookmark_open,
                    !(opt_flags & OPT_PDFDOC_NO_DEST_REMOVE));
  
    tm_delete_array (_pdf_file_name);
  }
  
  /* Ignore_colors placed here since
   * they are considered as device's capacity.
   */
  pdf_init_device(dvi2pts, pdfdecimaldigits, ignore_colors);
  
  
  pdf_rect mediabox;
  mediabox.llx = 0.0;
  mediabox.lly = 0.0;
  mediabox.urx = width;
  mediabox.ury = height;
  
  pdf_doc_set_mediabox(0, &mediabox); /* Root node */
  
  pdf_doc_begin_page(1.0,0.0,0.0);
  

  set_clipping (0, (int) ((-dpi*PIXEL*paper_h)/2.54), (int) ((dpi*PIXEL*paper_w)/2.54), 0);
  fg  = -1;
  bg  = -1;
  lw  = -1;
  cfn= "Helvetica";
  cfid = pdf_dev_locate_font("Helvetica",10);
  tex_fonts (cfn)= cfid+1;
}

pdf_renderer_rep::~pdf_renderer_rep () {
  pdf_doc_end_page();
  
  pdf_files_close();
  
  /* Order of close... */
  pdf_close_device  ();
  /* pdf_close_document flushes XObject (image) and other resources. */
  pdf_close_document();
  
  pdf_close_fontmaps(); /* pdf_font may depend on fontmap. */
}

bool
pdf_renderer_rep::is_printer () {
//  cerr << "is_printer\n";
  return true;
}

void
pdf_renderer_rep::next_page () {
//  cerr << "next_page\n";
  pdf_doc_end_page();
  pdf_doc_begin_page(1.0,0.0,0.0);

  set_clipping (0, (int) ((-dpi*PIXEL*paper_h)/2.54), (int) ((dpi*PIXEL*paper_w)/2.54), 0);
  fg  = -1;
  bg  = -1;
  lw  = -1;
  cfn= "Helvetica";
  cfid = tex_fonts(cfn)-1;
}

/******************************************************************************
* Clipping
******************************************************************************/

void
pdf_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
//  cerr << "set_clipping\n";
  outer_round (x1, y1, x2, y2);
  renderer_rep::set_clipping (x1, y1, x2, y2);
/*  if (restore) {
    print (PDF_CLIP_POP);
    cfn= "";
  }
  else {
    print (PDF_CLIP_PUSH);
    print (x1, y1);
    print (x2, y2);
    print (PDF_CLIP);
  }*/
}
  
/******************************************************************************
* graphical routines
******************************************************************************/

color
pdf_renderer_rep::get_color () {
//  cerr << "get_color\n";
  return fg;
}

color
pdf_renderer_rep::get_background () {
//  cerr << "get_background\n";
  return bg;
}

void
pdf_renderer_rep::set_color (color c) {
//  cerr << "set_color\n";
  if (fg==c) return;
  fg= c;
  select_color (c);
}

void
pdf_renderer_rep::set_background (color c) {
//  cerr << "set_background\n";
  if (bg==c) return;
  bg= c;
}

static double font_size (string name) {
  int pos= search_forwards (".", name);
  int szpos= pos-1;
  while ((szpos>0) && is_numeric (name[szpos-1])) szpos--;
  double size= as_double (name (szpos, pos));
  if (size == 0) size= 10;
  return size;
}

void
pdf_renderer_rep::draw (int ch, font_glyphs fn, SI x, SI y, SI w) {
  //cerr << "draw \"" << (char)ch << "\" " << ch << " " << fn->res_name << "\n";
  glyph gl= fn->get(ch);
  if (is_nil (gl)) return;
  if (cfn != fn->res_name) {
    
  //  cerr << "CHANGE FONT" << LF;
    
    if (!tex_fonts [fn->res_name]) {
      int pos= search_forwards (":", fn->res_name);
      string fname= (pos==-1? fn->res_name: fn->res_name (0, pos));
      url u = url_none();
      {        
        //cerr << " try freetype " << LF;
        u = tt_font_find(fname);
        //cerr << fname << " " << u << LF;
      }
      if (is_none (u)) {
        //cerr << " try pk " << LF;
        u= resolve_tex (fn->res_name);
        //cerr << fname << " " << u << LF;
      }
      if (!is_none (u)) {
        int pos= search_forwards (".", fn->res_name);
        string rname= (pos==-1? fn->res_name: fn->res_name (0, pos));
        double fsize= font_size (fn->res_name);

        url utfm =  resolve_tex(fname * ".tfm") ;
   
        char *_rname = as_charp(fname);
        char* _u= as_charp (concretize (u));
        char* _utfm= NULL;
        if (!is_none(utfm)) _utfm = as_charp (concretize (utfm));
        //cout << "DEVFONT " << _rname << " " << fsize << " " << u << " " << utfm << LF;
        int font_id = pdf_dev_physical_font(_rname,fsize*dpi*PIXEL/default_dpi,_u,_utfm); 
        tm_delete_array(_rname);
        tm_delete_array(_u);
        if (_utfm) tm_delete_array(_utfm);
        if (font_id >= 0) {
          tex_fonts(fn->res_name)= font_id+1;
        }  else {
          cout << "(pdf_renderer) Problems with font: " << fname << " file " << u << LF;
        }
      }
    }
    cfn= fn->res_name;
    if (tex_fonts [cfn]) {
      cfid = tex_fonts(cfn) -1;
    } else cfid = 0;
  }

  y += oy;  x += ox;
#if 1
  pdf_dev_set_raw_glyph(x, y, ch, cfid);
#else
  unsigned char buf[2] = { ch, 0 };
  pdf_dev_set_string(x, y, buf, 1, w, cfid, 1);
#endif
//  cerr << "char " << ch << " " << x << " " << y << " font " << cfid << " width " << w << LF;
  
}

void
pdf_renderer_rep::set_line_style (SI w, int type, bool round) {
//  cerr << "set_line_style\n";
  if (lw == w) return;
  lw= w;
  select_line_width (w);
}

void
pdf_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
//  cerr << "line\n";
  pdf_dev_moveto(to_x (x1), to_y (y1));
  pdf_dev_lineto(to_x (x2), to_y (y2));
  pdf_dev_stroke();
}

void
pdf_renderer_rep::lines (array<SI> x, array<SI> y) {
//  cerr << "lines\n";
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  pdf_dev_moveto(to_x (x[0]), to_y (y[0]));
  for (i=1; i<n; i++) {
    pdf_dev_lineto(to_x (x[i]), to_y (y[i]));
  }
  pdf_dev_stroke();
}

void
pdf_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
//  cerr << "clear\n";
  select_color (bg);
  double xx1= to_x (min (x1, x2));
  double yy1= to_y (min (y1, y2));
  double xx2= to_x (max (x1, x2));
  double yy2= to_y (max (y1, y2));
  pdf_dev_moveto(xx1, yy1);
  pdf_dev_lineto(xx2, yy1);
  pdf_dev_lineto(xx2, yy2);
  pdf_dev_lineto(xx1, yy2);
  pdf_dev_closepath();
  pdf_dev_fill();  
  select_color (fg);
}

void
pdf_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
//  cerr << "fill\n";
  double xx1= to_x (min (x1, x2));
  double yy1= to_y (min (y1, y2));
  double xx2= to_x (max (x1, x2));
  double yy2= to_y (max (y1, y2));
  pdf_dev_moveto(xx1, yy1);
  pdf_dev_lineto(xx2, yy1);
  pdf_dev_lineto(xx2, yy2);
  pdf_dev_lineto(xx1, yy2);
  pdf_dev_closepath();
  pdf_dev_fill();  
}

void
pdf_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  cerr << "arc\n";
#if 0
  double cx= to_x ((x1+x2)/2);
  double cy= to_y ((y1+y2)/2);
  double rx= (x2-x1);
  double ry= (y2-y1);
  double a= -(double)alpha / 64 + 90;
  double d= -(double)(alpha + delta) / 64 + 90;
  cerr << "arc GSave\n", HPDF_Page_GSave (page);
  HPDF_Page_SetRGBFill (page, 1, 0, 0);
  cerr << "arc Concat\n", HPDF_Page_Concat (page, p * rx, 0, 0, p * ry, cx, cy);
  cerr << "arc Arc\n", HPDF_Page_Arc (page, 0, 0, 1, min (a, d), max (a, d));
  cerr << "arc LineTo\n", HPDF_Page_LineTo (page, 0, 0);
  cerr << "arc ClosePath\n", HPDF_Page_ClosePath (page);
  cerr << "arc Clip\n", HPDF_Page_Clip (page);
  cerr << "arc EndPath\n", HPDF_Page_EndPath (page);
  cerr << "arc Concat\n", HPDF_Page_Concat (page, 1 / p / rx, 0, 0, 1 / p / ry, 0, 0);
  cerr << "arc SetLineWidth\n", HPDF_Page_SetLineWidth (page, p * lw);  
  cerr << "arc Ellipse\n", HPDF_Page_Ellipse (page, 0, 0, p * rx / 2, p * ry / 2);
  cerr << "arc Stroke\n", HPDF_Page_Stroke (page);
  cerr << "arc GRestore\n", HPDF_Page_GRestore (page);
#endif
}

void
pdf_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  cerr << "fill_arc\n";
#if 0
  double cx= to_x ((x1+x2)/2);
  double cy= to_y ((y1+y2)/2);
  double rx= (x2-x1);
  double ry= (y2-y1);
  double a= -(double)alpha / 64 + 90;
  double d= -(double)(alpha + delta) / 64 + 90;
  cerr << "arc GSave\n", HPDF_Page_GSave (page);
  HPDF_Page_SetRGBFill (page, 1, 0, 0);
  cerr << "arc Concat\n", HPDF_Page_Concat (page, p * (rx / 2), 0, 0, p * (ry / 2), cx, cy);
  cerr << "arc Arc\n", HPDF_Page_Arc (page, 0, 0, 1, min (a, d), max (a, d));
  cerr << "arc EndPath\n", HPDF_Page_ClosePath (page);
  cerr << "arc Stroke\n", HPDF_Page_Fill (page);
  cerr << "arc GRestore\n", HPDF_Page_GRestore (page);
#endif
}

void
pdf_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
//  cerr << "polygon\n";
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  pdf_dev_moveto(to_x (x[0]), to_y (y[0]));
  for (i=1; i<n; i++) {
     pdf_dev_lineto (to_x (x[i]), to_y (y[i]));
  }
  pdf_dev_closepath ();
  pdf_dev_fill (); //  HPDF_Page_Eofill (page);
}

void
pdf_renderer_rep::xpm (url file_name, SI x, SI y) {
//  cerr << "xpm\n";
/*  (void) file_name; (void) x; (void) y;
  FAILED ("not yet implemented");*/
}


/*
 * Compute a transformation matrix
 * transformations are applied in the following
 * order: scaling, rotate, displacement.
 */
static void
make_transmatrix (pdf_tmatrix *M,
                  double xoffset, double yoffset,
                  double xscale,  double yscale,
                  double rotate)
{
  double c, s;
  
  c = cos(rotate);
  s = sin(rotate);
  
  M->a =  xscale * c; M->b = xscale * s;
  M->c = -yscale * s; M->d = yscale * c;
  M->e = xoffset;     M->f = yoffset;
}

void
pdf_renderer_rep::image (
  url u, SI w, SI h, SI x, SI y,
  double cx1, double cy1, double cx2, double cy2)
{
  //cerr << "image " << u << LF;
  
  
  int bx1, by1, bx2, by2;
  ps_bounding_box (u, bx1, by1, bx2, by2);
  int x1= bx1 + (int) (cx1 * (bx2 - bx1) + 0.5);
  int y1= by1 + (int) (cy1 * (by2 - by1) + 0.5);
  int x2= bx1 + (int) (cx2 * (bx2 - bx1) + 0.5);
  int y2= by1 + (int) (cy2 * (by2 - by1) + 0.5);
  
  double sc_x= (72.0/dpi) * ((double) (w/PIXEL)) / ((double) (x2-x1));
  double sc_y= (72.0/dpi) * ((double) (h/PIXEL)) / ((double) (y2-y1));
  
  transform_info ti;
  
  transform_info_clear(&ti);
  
  make_transmatrix(&(ti.matrix), 0, 0, sc_x, sc_y, 0.0);
  ti.bbox.llx = x1;
  ti.bbox.lly = y1;
  ti.bbox.urx = x2;
  ti.bbox.ury = y2;
  ti.flags = INFO_DO_CLIP  | INFO_HAS_USER_BBOX;
  
  string filename = concretize(u);

  if (!image_resources->contains(filename)) {
    url temp= url_temp (".pdf");
    string cmd = "ps2pdf13";
    system(cmd, u, temp);
    cout << temp << LF;
    char *_u = as_charp(concretize(temp));
    
    int form_id = pdf_ximage_findresource(_u, 1, NULL);
    if (form_id < 0) {
      cerr <<  "Failed to read image file:" << _u << LF;
    } else {
      image_resources (filename) = form_id;
    }
    tm_delete_array(_u);
    remove (temp);    
  }
  
  if (image_resources->contains(filename)) {
    pdf_dev_put_image(image_resources (filename), &ti, to_x(x), to_y(y));
  }
  
}

void
pdf_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
//  cerr << "fetch\n";
/*  (void) x1; (void) y1; (void) x2; (void) y2;
  (void) ren; (void) x; (void) y;*/
}

void
pdf_renderer_rep::new_shadow (renderer& ren) {
//  cerr << "new_shadow\n";
//  (void) ren;
}

void
pdf_renderer_rep::delete_shadow (renderer& ren) {
//  cerr << "delete_shadow\n";
//  (void) ren;
}

void
pdf_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
//  cerr << "get_shadow\n";
//  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
pdf_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
// cerr << "put_shadow\n";
//  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
pdf_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
//  cerr << "apply_shadow\n";
//  (void) x1; (void) y1; (void) x2; (void) y2;
}

/******************************************************************************
* user interface
******************************************************************************/

renderer
pdf_renderer (url pdf_file_name, int dpi, int nr_pages,
	 string page_type, 
              bool landscape, 
              double paper_w, 
              double paper_h)
{
  return tm_new<pdf_renderer_rep> (pdf_file_name, dpi, nr_pages,
			  page_type, landscape, paper_w, paper_h);
}


