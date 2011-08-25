
/******************************************************************************
* MODULE     : cairo_renderer.cpp
* DESCRIPTION: Cairo drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "cairo_renderer.hpp"

#ifdef USE_CAIRO

#include "analyze.hpp"
#include "image_files.hpp"
#include "file.hpp"
#include "iterator.hpp"
#include "gui.hpp" // for INTERRUPT_EVENT, INTERRUPTED_EVENT
#include "font.hpp" // for the definition of font

#include "Freetype/tt_file.hpp" // tt_font_find
#include "Freetype/free_type.hpp"
#include "tm_cairo.hpp"

/******************************************************************************
* Cairo images
******************************************************************************/

struct cairo_image_rep: concrete_struct {
  cairo_surface_t* img;
  SI xo,yo;
  int w,h;
  cairo_image_rep (cairo_surface_t* img2, SI xo2, SI yo2, int w2, int h2) :
    img (img2), xo (xo2), yo (yo2), w (w2), h (h2) {
      tm_cairo_surface_reference(img); }
  ~cairo_image_rep() { tm_cairo_surface_destroy(img); }
};

class cairo_image {
  CONCRETE_NULL(cairo_image);
  cairo_image (cairo_surface_t* img2, SI xo2, SI yo2, int w2, int h2) :
    rep (tm_new<cairo_image_rep> (img2, xo2, yo2, w2, h2)) {};
};

CONCRETE_NULL_CODE(cairo_image);

/******************************************************************************
* Global support variables for all cairo_renderers
******************************************************************************/

static hashmap<basic_character,cairo_image> character_image;  // bitmaps of all characters
static hashmap<string,cairo_image> images;

/******************************************************************************
* cairo_renderer
******************************************************************************/

class cairo_renderer_rep:  public basic_renderer_rep {
public:
  cairo_t* context;
  
public:
  cairo_renderer_rep (int w = 0, int h = 0);
  virtual ~cairo_renderer_rep ();
  
  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  void  set_color (color c);
  void  set_line_style (SI w, int type=0, bool round=true);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  lines (array<SI> x, array<SI> y);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y, bool convex=true);
  void  xpm (url file_name, SI x, SI y);
  void  image (url u, SI w, SI h, SI x, SI y,
               double cx1, double cy1, double cx2, double cy2, int alpha);
  
  void next_page ();
  bool is_printer();
  bool interrupted (bool check);
  
  /***** private section *****************************************************/
  
  void draw_clipped (cairo_surface_t* im, int w, int h, SI x, SI y);
	
  bool native_draw (int ch, font_glyphs fn, SI x, SI y);
  
  void begin (void* c); // c must be a cairo context of type (cairo_t*)
  void end ();
    
};


cairo_renderer_rep::cairo_renderer_rep (int w2, int h2):
  basic_renderer_rep(w2,h2), context(NULL)
{
  //cout << "Init cairo renderer" << LF;
}

cairo_renderer_rep::~cairo_renderer_rep () {
  if (context) end ();
}

void
cairo_renderer_rep::begin (void* c) { 
  context = (cairo_t*)c; tm_cairo_reference(context);
  set_clipping (0, -h, w, 0);
}

void
cairo_renderer_rep::end () {
  next_page();
  tm_cairo_destroy(context);
  context = NULL;
}

void
cairo_renderer_rep::next_page () { 
  //cout << "NEXT PAGE" << LF; tm_cairo_show_page (context); 
  set_clipping (0, -h, w, 0);
  // tm_cairo_translate (context, 0, (h*72.0)/pixel);
  // tm_cairo_scale(context, 1.0, -1.0);
}

void
tm_cairo_set_source_color(cairo_t *context, color c) {
  int r, g, b, a;
  get_rgb_color(c, r, g, b, a);
  tm_cairo_set_source_rgba(context, r/255.0, g/255.0, b/255.0, a/255.0);
}

void
cairo_renderer_rep::set_color (color c) {
  //cout << "set_color" << LF;
  basic_renderer_rep::set_color(c);
  tm_cairo_set_source_color(context, cur_fg);
}

void
cairo_renderer_rep::set_line_style (SI lw, int type, bool round) {
  (void) type;
  tm_cairo_set_line_cap (context,
		      round? CAIRO_LINE_CAP_ROUND : CAIRO_LINE_CAP_SQUARE);
  tm_cairo_set_line_join (context, CAIRO_LINE_JOIN_ROUND);
  tm_cairo_set_line_width (context,
			lw <= pixel ? 1 : ((lw+thicken) / (1.0*pixel)));
}

void
cairo_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  decode (x1, y1);
  decode (x2, y2);
  // y1--; y2--; // top-left origin to bottom-left origin conversion
  tm_cairo_set_antialias(context, CAIRO_ANTIALIAS_DEFAULT);
  tm_cairo_move_to(context, x1, y1);
  tm_cairo_line_to(context, x2, y2);
  tm_cairo_stroke(context);
}

void
cairo_renderer_rep::lines (array<SI> x, array<SI> y) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  tm_cairo_set_antialias(context, CAIRO_ANTIALIAS_DEFAULT);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    tm_cairo_line_to (context, xx, yy);
  }
  tm_cairo_stroke (context);
}

void
cairo_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  // outer_round (x1, y1, x2, y2); might still be needed somewhere
  decode (x1, y1);
  decode (x2, y2);
  if ((x1>=x2) || (y1<=y2)) return;
  tm_cairo_set_antialias(context, CAIRO_ANTIALIAS_NONE);
  tm_cairo_set_source_color(context, cur_bg);
  tm_cairo_rectangle(context, x1, y2, x2-x1, y1-y2);
  tm_cairo_fill(context);
  tm_cairo_set_source_color(context, cur_fg);
}

void
cairo_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if ((x2>x1) && ((x2-x1)<pixel)) {
    SI d= pixel-(x2-x1);
    x1 -= (d>>1);
    x2 += ((d+1)>>1);
  }
  if ((y2>y1) && ((y2-y1)<pixel)) {
    SI d= pixel-(y2-y1);
    y1 -= (d>>1);
    y2 += ((d+1)>>1);
  }
  
  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  // outer_round (x1, y1, x2, y2); might still be needed somewhere
  if ((x1>=x2) || (y1>=y2)) return;
  
  decode (x1, y1);
  decode (x2, y2);

  tm_cairo_set_antialias(context, CAIRO_ANTIALIAS_NONE);
  // tm_cairo_set_source_color(context, cur_fg);
  tm_cairo_rectangle(context, x1, y2, x2-x1, y1-y2);
 // cout << "fill " << x1 << "," << y2 << "," << x2-x1 << "," << y1-y2 << LF;
  tm_cairo_fill(context);
}

void
cairo_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  (void) alpha; (void) delta;
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  //FIXME: XDrawArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
cairo_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  (void) alpha; (void) delta;
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  //FIXME: XFillArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
cairo_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {  
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;

  tm_cairo_new_path(context);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    tm_cairo_line_to(context,xx,yy);
  }
  tm_cairo_close_path(context);
  tm_cairo_set_antialias(context, CAIRO_ANTIALIAS_DEFAULT);
  // tm_cairo_set_source_color(context, cur_fg);
  tm_cairo_set_fill_rule(context, convex ? CAIRO_FILL_RULE_EVEN_ODD : CAIRO_FILL_RULE_WINDING);
  tm_cairo_fill(context);
}

/******************************************************************************
* Image rendering
******************************************************************************/

struct cairo_cache_image_rep: cache_image_element_rep {
  cairo_cache_image_rep (int w2, int h2, time_t time2, cairo_surface_t *ptr2) :
    cache_image_element_rep(w2,h2,time2,ptr2) {
      tm_cairo_surface_reference ((cairo_surface_t *) ptr); }
  virtual ~cairo_cache_image_rep() {
    tm_cairo_surface_destroy ((cairo_surface_t *) ptr); }
};

void
cairo_renderer_rep::image (url u, SI w, SI h, SI x, SI y,
			   double cx1, double cy1, double cx2, double cy2,
                           int alpha)
{
  // Given an image of original size (W, H),
  // we display the part (cx1 * W, xy1 * H, cx2 * W, cy2 * H)
  // at position (x, y) in a rectangle of size (w, h)

  // if (DEBUG_EVENTS) cout << "cairo_renderer_rep::image " << as_string(u) << LF;
  (void) alpha; // FIXME

  w= w/pixel; h= h/pixel;
  decode (x, y);
  
  //painter.setRenderHints (0);
  //painter.drawRect (QRect (x, y-h, w, h));
  
  cairo_surface_t* pm = NULL;
  tree lookup= tuple (u->t);
  lookup << as_string (w ) << as_string (h )
	 << as_string (cx1) << as_string (cy1)
	 << as_string (cx2) << as_string (cy2) << "cairo-image" ;
  
  cache_image_element ci = get_image_cache(lookup);
  if (!is_nil(ci)) {
    pm = static_cast<cairo_surface_t*> (ci->ptr);
  }
  else {
    if (suffix (u) == "png") {
      // rendering
      string suu = as_string (u);
      char * buf = as_charp(suu);
      //cout << suu << LF;
      pm = tm_cairo_image_surface_create_from_png(buf);
      tm_delete (buf);
    }
    else if (suffix (u) == "ps" ||
	     suffix (u) == "eps" ||
	     suffix (u) == "pdf") {
      url temp= url_temp (".png");
      system ("convert", u, temp);
      string suu = as_string (temp);
      char * buf = as_charp(suu); 
      //cout << suu << LF;
      pm = tm_cairo_image_surface_create_from_png(buf);
      tm_delete (buf);
      remove (temp);
    }

    if (pm == NULL ) {
      cout << "TeXmacs] warning: cannot render " << as_string (u) << "\n";
      return;
    }
    // caching
    ci = tm_new<cairo_cache_image_rep> (w,h, texmacs_time(), pm);
    set_image_cache(lookup, ci);
    (ci->nr)++;
  }
  
  int iw= tm_cairo_image_surface_get_width(pm);
  int ih= tm_cairo_image_surface_get_height(pm);

  tm_cairo_set_antialias(context, CAIRO_ANTIALIAS_NONE);
  tm_cairo_save(context);
  tm_cairo_translate(context,x,y-h);
  tm_cairo_scale(context, (1.0*w)/iw, (1.0*h)/ih);
  tm_cairo_set_source_surface (context, pm, 0, 0);
  tm_cairo_paint (context);
  tm_cairo_restore(context);
};

void
cairo_renderer_rep::draw_clipped (cairo_surface_t* im, int w, int h, SI x, SI y) {
  decode (x , y );
  y--; // top-left origin to bottom-left origin conversion
       // clear(x1,y1,x2,y2);
  tm_cairo_set_antialias(context, CAIRO_ANTIALIAS_DEFAULT);
  tm_cairo_set_operator(context, CAIRO_OPERATOR_SOURCE);

  // tm_cairo_mask_surface(context, im, x, y);
  tm_cairo_set_source_surface (context, im, x, y);
  
  tm_cairo_paint (context);
}  

static hashmap<string,pointer> native_fonts;
static hashset<string> native_loaded;

static const cairo_user_data_key_t font_face_key = { 0 };

static cairo_font_face_t *
create_font_face_from_file (FT_Library library, const char *file) {
  cairo_font_face_t *font_face;
  FT_Error error;
  FT_Face face;
  cairo_status_t status;

  error = FT_New_Face (library, file, 0, &face);
  if (error) return NULL;
  font_face = tm_cairo_ft_font_face_create_for_ft_face (face, 0);
  status = tm_cairo_font_face_set_user_data (font_face, &font_face_key,
                                          face, (cairo_destroy_func_t) FT_Done_Face);
  if (status) {
    tm_cairo_font_face_destroy (font_face);
    FT_Done_Face (face);
    return NULL;
  }
  return font_face;
}

// WARNING:
// freetype fonts are not supported on quartz surface so for the moment native redering is 
// disabled.

bool 
cairo_renderer_rep::native_draw (int ch, font_glyphs fn, SI x, SI y) {
  string name= fn->res_name;
  unsigned char c= ch;
  if (ch >= 256) {
    name= name * "-" * as_string (ch / 256);
    c= (unsigned char) (ch & 255);
  }
  
  // cout << name << LF;
  int size;
	
  {
    // find size (weird)
    int    pos1  = search_forwards (".", name);
    int pos2= search_backwards (":", name);
    string sz = name(pos2+1,pos1);
    size = as_int(sz);
  }
  cairo_font_face_t* f = (cairo_font_face_t*)native_fonts(name);
	
  if ((f == NULL)&&(! native_loaded->contains(name))) {
    native_loaded->insert(name);
    string ttf;
    int    pos  = search_forwards (".", name);
    string root = (pos==-1? name: name (0, pos));
    if ((pos!=-1) && ends (name, "tt")) {
      int pos2= search_backwards (":", name);
      root= name (0, pos2);
      url u= tt_font_find (root);
      if (suffix (u) == "pfb") {
	//	cout << u << LF;
	char* _name= as_charp (concretize (u));
	f = create_font_face_from_file(ft_library,  _name);
	tm_delete_array (_name);
	if (tm_cairo_font_face_status(f) == CAIRO_STATUS_SUCCESS) {
	  // cout << "Font" << u << " loaded " << LF;
	  native_fonts(name) = f;
	}        
	else {
	  f = NULL;
	  //          cout << "Problems with font" << u << LF;
	}
      }
    }
  } // end caching
	
  if (f) {
    decode (x , y );
    y--; // top-left origin to bottom-left origin conversion
    tm_cairo_set_font_face(context, f);
    //cout << "status " << tm_cairo_status_to_string(tm_cairo_status(context)) << LF;
    tm_cairo_set_font_size(context, size*(PIXEL*600.0/(pixel*72.0)));
    //			CGAffineTransform	kHorizontalMatrix = { PIXEL*600.0/(pixel*72.0),  0.0,  0.0,  -PIXEL*600.0/(pixel*72.0),  0.0,  0.0 };
    tm_cairo_set_antialias(context, CAIRO_ANTIALIAS_DEFAULT);
    // tm_cairo_set_source_color(context, cur_fg);
    cairo_glyph_t gl = { c, x, y };
    tm_cairo_show_glyphs(context, &gl, 1 );
  }

  return true;
}

void
cairo_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  //cout << "draw" << LF;
  // get the pixmap
  basic_character xc (c, fng, sfactor, 0, 0);
  cairo_image mi = character_image [xc];
  if (is_nil(mi)) {
    int r, g, b, a;
    get_rgb (cur_fg, r, g, b, a);
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, sfactor, sfactor, xo, yo);
    int i, j, w= gl->width, h= gl->height;
    cairo_surface_t *im = tm_cairo_image_surface_create(CAIRO_FORMAT_A8,w,h);
    //FIXME: release the surface when done
    {
      cairo_t* ic = tm_cairo_create(im);
      int nr_cols= sfactor*sfactor;
      if (nr_cols >= 64) nr_cols= 64;
      tm_cairo_set_operator(ic, CAIRO_OPERATOR_SOURCE);
      for (j=0; j<h; j++)
	for (i=0; i<w; i++) {
	  int col = gl->get_x (i, j);
	  tm_cairo_set_source_rgba(ic, 0.0, 0.0, 0.0, ((255*col)/(nr_cols+1))/255.0);
	  tm_cairo_rectangle(ic,i,j,1,1);
          tm_cairo_fill(ic);
	}
      tm_cairo_destroy (ic);
    }
    cairo_image mi2 (im, xo, yo, w, h);
    mi = mi2;
    tm_cairo_surface_destroy (im); // cairo_image retains im
    character_image (xc)= mi;
  }
  
  // draw the character
  {
    int x1 = x- mi->xo*sfactor;
    int y1 =  y+ mi->yo*sfactor;
    decode (x1, y1);
    y1--; // top-left origin to bottom-left origin conversion
    tm_cairo_set_antialias(context, CAIRO_ANTIALIAS_DEFAULT);
    tm_cairo_set_operator(context, CAIRO_OPERATOR_SOURCE);
    // tm_cairo_set_source_color (context, cur_fg);
    tm_cairo_mask_surface(context, mi->img, x1, y1);
  }  
}

/******************************************************************************
* Setting up and displaying xpm pixmaps
******************************************************************************/

static cairo_surface_t*
xpm_init (url file_name) {
  tree t= xpm_load (file_name);
  
  // get main info
  int ok, i=0, j, k, w, h, c, b, x, y;
  string s= as_string (t[0]);
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  if ((!ok) || (N(t)<(c+1)) || (c<=0)) {
    cerr << "File name= " << file_name << "\n";
    FAILED ("invalid xpm");
  }
  
  // setup colors
  string first_name;
  hashmap<string,color> pmcs;
  for (k=0; k<c; k++) {
    string s   = as_string (t[k+1]);
    string name= "";
    string def = "none";
    if (N(s)<b) i=N(s);
    else { name= s(0,b); i=b; }
    if (k==0) first_name= name;
    
    skip_spaces (s, i);
    if ((i<N(s)) && (s[i]=='s')) {
      i++;
      skip_spaces (s, i);
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      skip_spaces (s, i);
    }
    if ((i<N(s)) && (s[i]=='c')) {
      i++;
      skip_spaces (s, i);
      j=i;
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      def= locase_all (s (j, i));
    }  
    pmcs(name)= xpm_to_color(def);
  }
  cairo_surface_t *im = tm_cairo_image_surface_create (CAIRO_FORMAT_ARGB32,w,h);
  cairo_t* ic = tm_cairo_create (im);
  tm_cairo_set_operator (ic, CAIRO_OPERATOR_SOURCE);
  // setup pixmap
  for (y=0; y<h; y++) {
    if (N(t)< (y+c+1)) s= "";
    else s= as_string (t[y+c+1]);
    for (x=0; x<w; x++) {
      string name;
      if (N(s)<(b*(x+1))) name= first_name;
      else name= s (b*x, b*(x+1));
      color col = pmcs[(pmcs->contains (name) ? name : first_name)];
      tm_cairo_set_source_color (ic, col);
      tm_cairo_rectangle (ic,x,y,1,1);
      tm_cairo_fill (ic);
    }
  }
  tm_cairo_destroy (ic);
  return im;
}

extern int char_clip;

static cairo_surface_t* 
xpm_image (url file_name) { 
  cairo_surface_t *pxm= NULL;
  cairo_image mi= images [as_string (file_name)];
  if (is_nil (mi)) {    
    pxm = xpm_init(file_name);
    cairo_image mi2 (pxm, 0, 0, tm_cairo_image_surface_get_width(pxm),
		     tm_cairo_image_surface_get_height(pxm));
    mi= mi2;
    images (as_string (file_name))= mi2;
    tm_cairo_surface_destroy (pxm);
  }  
  else pxm= mi->img;
  return pxm;
}

void
cairo_renderer_rep::xpm (url file_name, SI x, SI y) {
  y -= pixel; // counter balance shift in draw_clipped
  cairo_surface_t *image = xpm_image (file_name);
  ASSERT (sfactor == 1, "shrinking factor should be 1");
  int w, h;
  w = tm_cairo_image_surface_get_width(image);
  h = tm_cairo_image_surface_get_height(image);
  int old_clip= char_clip;
  char_clip= true;
  draw_clipped (image, w, h, x, y);
  char_clip=old_clip;
}

bool
cairo_renderer_rep::is_printer () {
  return true;
}

bool
cairo_renderer_rep::interrupted (bool check) {
  return false;
}

/******************************************************************************
* main cairo renderer
******************************************************************************/

static cairo_renderer_rep* the_renderer= NULL;

basic_renderer_rep*
the_cairo_renderer () {
  if (tm_cairo_present()) {
    if (!the_renderer) the_renderer= tm_new<cairo_renderer_rep> ();
    return the_renderer;
  } else {
    return NULL;
  }
}

#if 0
renderer
printer (url ps_file_name, int dpi, int nr_pages,
         string page_type, bool landscape, double paper_w, double paper_h)
{
  cout << "PS output to file : " << as_string(ps_file_name) << LF;
  int h = (dpi*PIXEL*paper_h)/2.54;
  int w = (dpi*PIXEL*paper_w)/2.54;
  cairo_renderer_rep *ren = tm_new<cairo_renderer_rep> (w,h);
  char *buf = as_charp(as_string(ps_file_name));
  cairo_surface_t* surface =
    tm_cairo_ps_surface_create(buf, paper_w/2.54*72.0, paper_h/2.54*72.0);
  tm_delete_array (buf);
  cairo_t *context = tm_cairo_create (surface);
  // tm_cairo_translate (context, 0,  paper_h/2.54*72.0);
  // tm_cairo_scale(context, 1.0, -1.0);

  ren->begin (context);
  tm_cairo_destroy (context);
  tm_cairo_surface_destroy (surface);
  renderer r = ren;
  r->set_color(black);
  r->fill(0,-10000,10000,0);
  r->next_page();
  return r;
}
#endif

#else // USE_CAIRO
basic_renderer_rep*
the_cairo_renderer () {
    return NULL;
}
#endif // USE_CAIRO
