
/******************************************************************************
* MODULE     : pdf_hummus_renderer.cpp
* DESCRIPTION: Renderer for printing pdf graphics using the PDFHummus library
* COPYRIGHT  : (C) 2012 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "pdf_hummus_renderer.hpp"
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
#include "converter.hpp"
#include "ntuple.hpp"
#include "link.hpp"
#include "frame.hpp"
#include "Ghostscript/gs_utilities.hpp" // for gs_prefix
#include "wencoding.hpp"

#ifdef QT_CORE_LIB
#include <QtCore>
#endif
#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#include "Qt/qt_picture.hpp"
#include <QImage>
#endif

#include "PDFWriter/PDFWriter.h"
#include "PDFWriter/PDFPage.h"
#include "PDFWriter/PageContentContext.h"
#include "PDFWriter/DictionaryContext.h"
#include "PDFWriter/PDFImageXObject.h"
#include "PDFWriter/PDFStream.h"
#include "PDFWriter/PDFDocumentCopyingContext.h"
#include "PDFWriter/InputByteArrayStream.h"
#include "PDFWriter/ProcsetResourcesConstants.h"
#include "PDFWriter/OutputStreamTraits.h"
#include "PDFWriter/XObjectContentContext.h"
#include "PDFWriter/PDFFormXObject.h"
#include "PDFWriter/InfoDictionary.h"
#include "PDFWriter/PDFPageInput.h"
#include "PDFWriter/PDFTiledPattern.h"
#include "PDFWriter/TiledPatternContentContext.h"
#include "PDFWriter/PDFUsedFont.h"
 
/******************************************************************************
 * pdf_hummus_renderer
 ******************************************************************************/

static EPDFVersion ePDFVersion= ePDFVersion14;

typedef triple<int,int,int> rgb;
typedef quartet<string,int,SI,SI> dest_data;
typedef quintuple<string,int,SI,SI,int> outline_data;

class pdf_image;
class pdf_raw_image;
class t3font;
class pdf_pattern;

class pdf_hummus_renderer_rep : public renderer_rep {
  
  static const int default_dpi= 72; // PDF initial coordinate system corresponds to 72 dpi
  bool		started;  // initialisation is OK
  url       pdf_file_name;
  int       dpi;
  int       nr_pages;
  string    page_type;
  bool      landscape;
  double    paper_w;
  double    paper_h;

  ObjectIDType initial_GState_id; // GSstate with default values

  int       page_num;
  bool      inText;
  color     fg, bg;
  SI        lw;
  double    current_width;
  int       clip_level;
  
  pencil   pen;
  brush    bgb, fgb;

  string    cfn;
  PDFUsedFont* cfid;
  double fsize;
  double prev_text_x, prev_text_y;
  
  double width, height;
  
  
  hashmap<string,PDFUsedFont*> native_fonts;
  hashset<string> not_native_fonts;
  hashset<string> EuropeanComputerModern_fonts;
  hashmap<string,pdf_raw_image> pdf_glyphs;
  hashmap<tree,pdf_image> image_pool;
  hashmap<tree,pdf_image> pattern_image_pool;
  hashmap<tree,pdf_pattern> pattern_pool;
  hashmap<unsigned long long int,url> picture_cache;
  array<url> temp_images;
  
  hashmap<int,ObjectIDType> alpha_id;
  hashmap<int,ObjectIDType> page_id;
  int t3font_registry_id;
  hashmap<string,t3font> t3font_list;
  
  // link annotation support
  hashmap<ObjectIDType,string> annot_list;
  list<dest_data> dests;
  ObjectIDType destId;
  hashmap<string,int> label_id;
  int label_count;
  hashmap<string,string> metadata;

  // outline support
  ObjectIDType outlineId;
  list<outline_data> outlines;
  
  
  PDFWriter pdfWriter;
  PDFPage* page;
  PageContentContext* contentContext;
  
  // geometry
  
  double to_x (SI x) {
    x += ox;
    if (x>=0) x= x/pixel; else x= (x-pixel+1)/pixel;
    return x;
  };

  double to_y (SI y) {
    y += oy;
    if (y>=0) y= y/pixel; else y= (y-pixel+1)/pixel;
    return y;
  };
  
  // various internal routines

  void init_page_size ();
  void select_stroke_color (color c);
  void select_fill_color (color c);
  void select_alpha (int a);
  
  void register_pattern_image (brush br, SI pixel);
  void select_stroke_pattern (brush br);
  void select_fill_pattern (brush br);

  void select_line_width (SI w);
  void compile_glyph (scheme_tree t);
  void begin_text ();
  void end_text ();
  
  void begin_page();
  void end_page();
  
  int get_label_id(string label);

  // various internal routines
  void flush_images();
  void flush_patterns();
  void flush_glyphs();
  void flush_dests();
  void flush_outlines();
  void flush_fonts();
  PDFImageXObject *create_pdf_image_raw (string raw_data, SI width, SI height, ObjectIDType imageXObjectID);
  void make_pdf_font (string fontname);
  void draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y);
  void  image (url u, double w, double h, SI x, SI y, int alpha);
  
  void bezier_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta, bool filled);

  // hooks for the Hummus library

  class DestinationsWriter : public DocumentContextExtenderAdapter
  {
    pdf_hummus_renderer_rep *ren;
  public:
    DestinationsWriter(pdf_hummus_renderer_rep *_ren) : ren(_ren) {}
    virtual ~DestinationsWriter(){}
    
    // IDocumentContextExtender implementation
    virtual PDFHummus::EStatusCode OnCatalogWrite(CatalogInformation* inCatalogInformation,
                                                  DictionaryContext* inCatalogDictionaryContext,
                                                  ObjectsContext* inPDFWriterObjectContext,
                                                  PDFHummus::DocumentContext* inDocumentContext)
    {
      ren->on_catalog_write (inCatalogInformation, inCatalogDictionaryContext, inPDFWriterObjectContext, inDocumentContext);
      return eSuccess;
    }
  };
  
  PDFHummus::EStatusCode on_catalog_write (CatalogInformation* inCatalogInformation,
                                           DictionaryContext* inCatalogDictionaryContext,
                                           ObjectsContext* inPDFWriterObjectContext,
                                           PDFHummus::DocumentContext* inDocumentContext);
  
  
  void recurse (ObjectsContext& objectsContext, int parent_ls,
		list<outline_data>& it, ObjectIDType parentId,
                ObjectIDType& firstId, ObjectIDType& lastId, int &count);

  
public:
  pdf_hummus_renderer_rep (url pdf_file_name, int dpi, int nr_pages,
                           string ptype, bool landsc, double paper_w, double paper_h);
  ~pdf_hummus_renderer_rep ();
  bool is_printer ();
  bool is_started ();
  void next_page ();
  
  void set_transformation (frame fr);
  void reset_transformation ();
  
  void  set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  
  pencil get_pencil ();
  brush get_background ();
  void  set_pencil (pencil pen2);
  void  set_brush (brush b2);
  void  set_background (brush b2);

  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  lines (array<SI> x, array<SI> y);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y, bool convex=true);
  
  void draw_picture (picture p, SI x, SI y, int alpha);
  void draw_scalable (scalable im, SI x, SI y, int alpha);
  renderer shadow (picture& pic, SI x1, SI y1, SI x2, SI y2);
  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  /************************ subroutines hyperlinks ***************************/
  
  void anchor (string label, SI x1, SI y1, SI x2, SI y2);
  void href (string label, SI x1, SI y1, SI x2, SI y2);
  void toc_entry (string kind, string title, SI x, SI y);
  void set_metadata (string kind, string val);
  void flush_metadata ();
};

/******************************************************************************
 * local utilities
 ******************************************************************************/


static void
write_indirect_obj(ObjectsContext&  objectsContext, ObjectIDType destId, string payload) {
  objectsContext.StartNewIndirectObject(destId);
  c_string buf (payload);
  objectsContext.StartFreeContext()->Write((unsigned char *)(char*)buf, N(payload));
  objectsContext.EndFreeContext();
  objectsContext.EndIndirectObject();
}

void pdf_image_info (url image, int& w, int& h, PDFRectangle& cropBox, double (&tMat)[6], PDFPageInput& pageInput);
  

/******************************************************************************
* constructors and destructors
******************************************************************************/

pdf_hummus_renderer_rep::pdf_hummus_renderer_rep (
  url pdf_file_name2, int dpi2, int nr_pages2,
  string page_type2, bool landscape2, double paper_w2, double paper_h2):
    renderer_rep (false),
    pdf_file_name (pdf_file_name2), dpi (dpi2),
    nr_pages (nr_pages2), page_type (page_type2),
    landscape (landscape2), paper_w (paper_w2), paper_h (paper_h2),
    page_num(0),
    inText (false),
    fg (-1), bg (-1),
    lw (-1),
    pen (black), bgb (white), fgb (black),
    cfn (""), cfid (NULL),
    native_fonts (NULL),
    t3font_registry_id(-1),
    destId(0),
    label_count(0),
    outlineId(0)
{
  width = default_dpi * paper_w / 2.54;
  height= default_dpi * paper_h / 2.54;

  // setup library

  EStatusCode status;
  ePDFVersion= ePDFVersion14; // PDF 1.4 for alpha
  string version= pdf_version ();
  if (version == "1.5") ePDFVersion= ePDFVersion15;
  if (version == "1.6") ePDFVersion= ePDFVersion16;
  if (version == "1.7") ePDFVersion= ePDFVersion17;
  // LogConfiguration log (true, true, "PDFWriterLog.txt");
  LogConfiguration log= LogConfiguration::DefaultLogConfiguration();
  bool compress= true;
  PDFCreationSettings settings (compress, true); //, EncryptionOptions("user", 4, "owner"));
    status = pdfWriter.StartPDF(as_charp(concretize (pdf_file_name)), ePDFVersion, log, settings);
	if (status != PDFHummus::eSuccess) {
		convert_error << "failed to start PDF\n";
		started=false;
	} else {
		started=true;
		pdfWriter.GetDocumentContext().AddDocumentContextExtender (new DestinationsWriter(this));

		// create a graphic state with default values
		initial_GState_id= pdfWriter.GetObjectsContext()
		  .GetInDirectObjectsRegistry().AllocateNewObjectID();
		ObjectsContext& objectsContext = pdfWriter.GetObjectsContext();
		objectsContext.StartNewIndirectObject(initial_GState_id);
		std::stringstream buf;
		buf << "<< /Type /ExtGState\r\n";
		buf << "/LW 1.0\r\n";
      		buf << "/LC 0\r\n";
      		buf << "/LJ 0\r\n";
      		buf << "/ML 10.0\r\n";
      		//buf << "/D [[] 0]\r\n"; // useless
      		buf << "/RI /RelativeColorimetric\r\n";
      		buf << "/OP false\r\n";
      		buf << "/op false\r\n";
		buf << "/FL 1.0\r\n";
		buf << "/SA false\r\n";
		buf << "/BM /Normal\r\n";
		buf << "/SMask /None\r\n";
		buf << "/CA 1.0\r\n";
		buf << "/ca 1.0\r\n";
		buf << "/AIS false\r\n";
		buf << "/TK true\r\n";
		buf << ">>\r\n";
		objectsContext.StartFreeContext()
		  ->Write((const IOBasicTypes::Byte* )(buf.str().c_str()),buf.str().size());
		objectsContext.EndFreeContext();
		objectsContext.EndIndirectObject();

		// start real work
		begin_page();
	}
}

pdf_hummus_renderer_rep::~pdf_hummus_renderer_rep () {
  if (!started) return; // no cleanup to do
  end_page();
  
  flush_images();
  flush_patterns();
  flush_glyphs();
  flush_dests();
  flush_outlines();
  flush_fonts();
  flush_metadata();
 
  {
    // flush alphas
    iterator<int> it = iterate(alpha_id);
    ObjectsContext& objectsContext = pdfWriter.GetObjectsContext();
    while (it->busy()) {
      int a = it->next();
      double da = ((double) a)/1000.0;
      objectsContext.StartNewIndirectObject(alpha_id(a));
      {
        std::stringstream buf;
        buf << "<< /Type /ExtGState /CA "<< da << "  /ca "<< da << " >>\r\n";
        objectsContext.StartFreeContext()->Write((const IOBasicTypes::Byte* )(buf.str().c_str()),buf.str().size());
        objectsContext.EndFreeContext();
      }
      objectsContext.EndIndirectObject();
    }
  }
  
  {
    // flush annotations
    iterator<ObjectIDType> it = iterate(annot_list);
    ObjectsContext& objectsContext = pdfWriter.GetObjectsContext();
    while (it->busy()) {
      ObjectIDType id = it->next();
      write_indirect_obj(objectsContext, id, annot_list(id));
    }
  }
  
  EStatusCode status = pdfWriter.EndPDF();
  if (status != PDFHummus::eSuccess) {
    convert_error << "Failed in end PDF\n";
  }

  // remove temporary pictures
  for (int i=0; i<N(temp_images); i++)
    if (!is_none (temp_images[i]) && exists(temp_images[i]))
      remove (temp_images[i]);
}

bool
pdf_hummus_renderer_rep::is_printer () {
  // debug_convert << "is_printer\n";
  return true;
}

bool
pdf_hummus_renderer_rep::is_started () {
  return started;
}

void
pdf_hummus_renderer_rep::next_page () {
  end_page();
  begin_page();
}

void
pdf_hummus_renderer_rep::begin_page() {
  //EStatusCode status;

  page = new PDFPage();
  page->SetMediaBox(PDFRectangle(0,0,width,height));
  contentContext = pdfWriter.StartPageContentContext(page);
  if (NULL == contentContext) {
    //status = PDFHummus::eFailure;
    convert_error << "Failed to create content context for page\n";
  } else {
    fg  = -1;
    bg  = -1;
    lw  = -1;
    current_width = -1.0;
    cfn= "";
    cfid = NULL;
    inText = false;
    clip_level = 0;
    
      // outmost save of the graphics state
    contentContext->q();
      // set scaling suitable for dpi (pdf default is 72)
    contentContext->cm((double)default_dpi / dpi, 0, 0, (double)default_dpi / dpi, 0, 0);
    
    set_origin (0, paper_h*dpi*pixel/2.54);
    set_clipping (0, (int) ((-dpi*pixel*paper_h)/2.54), (int) ((dpi*pixel*paper_w)/2.54), 0);
  }
}

void
pdf_hummus_renderer_rep::end_page(){
  
  if (!page) return;
  EStatusCode status;

  end_text ();
  

  // reset set_clipping calls in order to have well formed PDF.
  while (clip_level--)
    contentContext->Q();
  
  // outmost restore for the graphics state (see begin_page)
  contentContext->Q();

  status = pdfWriter.EndPageContentContext(contentContext);
  if (status != PDFHummus::eSuccess) {
    convert_error << "Failed to end page content context\n";
  }
  
  EStatusCodeAndObjectIDType res = pdfWriter.GetDocumentContext().WritePageAndRelease(page);
  status = res.first;
  if (status != PDFHummus::eSuccess) {
    convert_error << "Failed to write page and release\n";
  }

  page_id (page_num) = res.second;
  page_num++;
}

void
pdf_hummus_renderer_rep::begin_text () {
  if (!inText) {
    contentContext->BT();
    inText = true;
    prev_text_x = to_x(0);
    prev_text_y = to_y(0);
    contentContext->Tm(1,0,0,1, prev_text_x, prev_text_y);
  }
}


void
pdf_hummus_renderer_rep::end_text () {
  if (inText) {
    contentContext->ET();
    inText = false;
  }
}

/******************************************************************************
 * Transformed rendering
 ******************************************************************************/

void
pdf_hummus_renderer_rep::set_transformation (frame fr) {
  ASSERT (fr->linear, "only linear transformations have been implemented");
  
  end_text ();

  SI cx1, cy1, cx2, cy2;
  get_clipping (cx1, cy1, cx2, cy2);
  rectangle oclip (cx1, cy1, cx2, cy2);
  frame cv= scaling (point (pixel, pixel),
                     point (-ox, -oy));
  frame tr= invert (cv) * fr * cv;
  point o = tr (point (0.0, 0.0));
  point ux= tr (point (1.0, 0.0)) - o;
  point uy= tr (point (0.0, 1.0)) - o;
  // debug_convert << "Set transformation " << o << ", " << ux << ", " << uy << "\n";
  double tx= o[0];
  double ty= o[1];
  contentContext->q();
  contentContext->cm(ux[0],ux[1],uy[0],uy[1],tx,ty);
  
  rectangle nclip= fr [oclip];
  renderer_rep::clip (nclip->x1, nclip->y1, nclip->x2, nclip->y2);
}

void
pdf_hummus_renderer_rep::reset_transformation () {
  end_text ();
  renderer_rep::unclip ();
  contentContext->Q();
}

/******************************************************************************
* Clipping
******************************************************************************/

void
pdf_hummus_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);

  end_text();
  
  outer_round (x1, y1, x2, y2);
  if (restore) {
    // debug_convert << "restore clipping\n";
    if (clip_level > 0) { contentContext->Q(); clip_level--; }
    cfn= "";
  }
  else {
    // debug_convert << "set clipping\n";
    contentContext->q(); clip_level++;
    double xx1= to_x (min (x1, x2));
    double yy1= to_y (min (y1, y2));
    double xx2= to_x (max (x1, x2));
    double yy2= to_y (max (y1, y2));
    contentContext->re(xx1, yy1, xx2-xx1, yy2-yy1);
    contentContext->W();
    contentContext->n();
  }
}

/******************************************************************************
 * Images
 ******************************************************************************/

class pdf_image_rep : public concrete_struct
{
public:
  url u;
  int w,h;
  ObjectIDType id;
  
  pdf_image_rep(url _u, ObjectIDType _id)
    : u(_u), id(_id)
  { image_size (u, w, h); } 
  ~pdf_image_rep() {}

  bool flush_jpg (PDFWriter& pdfw, url image);
#ifndef PDFHUMMUS_NO_PNG
  bool flush_png (PDFWriter& pdfw, url image);
#endif
  void flush (PDFWriter& pdfw);

  bool flush_for_pattern (PDFWriter& pdfw);
}; // class pdf_image_ref

class pdf_image {
  CONCRETE_NULL(pdf_image);
  pdf_image (url _u, ObjectIDType _id):
    rep (tm_new<pdf_image_rep> (_u,_id)) {};
};

CONCRETE_NULL_CODE(pdf_image);

/******************************************************************************
 * Tiled patterns
 ******************************************************************************/

class pdf_pattern_rep : public concrete_struct {
public:
  pdf_image im;
  SI w, h, sx, sy;
  double scale_x, scale_y;
  ObjectIDType id;
  
  pdf_pattern_rep (pdf_image _im, SI _w, SI _h, SI _sx, SI _sy,
		   double _scale_x, double _scale_y, ObjectIDType _id)
    : im (_im), w (_w), h (_h), sx (_sx), sy (_sy),
      scale_x (_scale_x), scale_y (_scale_y), id (_id) {}
  ~pdf_pattern_rep () {}

  void flush (PDFWriter& pdfw) {
    const double matrix[]= { scale_x, 0, 0, scale_y, (double) sx, (double) sy };
    DocumentContext& documentContext= pdfw.GetDocumentContext();
    PDFTiledPattern* tiledPattern= documentContext.StartTiledPattern
      (1, // int inPaintType,
       2, // int inTilingType,
       PDFRectangle(0, 0, w, h),
       w, // double inXStep,
       h, // double inYStep,
       id,
       matrix);
    TiledPatternContentContext* tiledPatternContentContext =
      tiledPattern->GetContentContext();
    std::string imageName=
      tiledPattern->GetResourcesDictionary().AddImageXObjectMapping (im->id);
    tiledPatternContentContext->q();
    tiledPatternContentContext->cm(w, 0, 0, h, 0, 0);
    tiledPatternContentContext->Do(imageName);
    tiledPatternContentContext->Q();
    PDFHummus::EStatusCode st=
      documentContext.EndTiledPatternAndRelease (tiledPattern);
    if (st != PDFHummus::eSuccess)
      convert_error << "Cannot flush tiled pattern "
                    << im->u << "\n"; }
};

class pdf_pattern {
  CONCRETE_NULL(pdf_pattern);
  pdf_pattern (pdf_image _im,  SI _w, SI _h, SI _sx, SI _sy,
	       double _scale_x, double _scale_y, ObjectIDType _id):
    rep (tm_new<pdf_pattern_rep> (_im, _w, _h, _sx, _sy,
				  _scale_x, _scale_y, _id)) {};
};

CONCRETE_NULL_CODE(pdf_pattern);

void
pdf_hummus_renderer_rep::register_pattern_image (brush br, SI pixel) {
  // debug_convert << "register_pattern_image\n";
  if (is_nil (br) || br->get_type () != brush_pattern) {
    convert_warning << "register_pattern_image: "
                    << "brush with pattern expected\n";
    return;
  }
  tree p= br->get_pattern ();
  // debug_convert << p << "\n";
  if (pattern_pool->contains(p)) return;

  url u;
  SI w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, pixel);
  tree key= tuple (u->t, as_string (w), as_string (h), eff);
  
  pdf_image image_pdf;
  if (pattern_image_pool->contains(key))
    image_pdf= pattern_image_pool[key];
  else {
    // debug_convert << "Insert pattern image\n";
    QImage* pim = get_image (u, w, h, eff, pixel);
    if (pim == NULL) {
      convert_error << "Cannot read image file '" << u << "'"
		    << " with get_image" << LF;
      return;
    }
    if (w != pim->width () || h != pim->height ()) {
      convert_error << "Invalid image size '" << u << "'"
		    << " after get_image" << LF;
      return;
    }
    url temp= url_temp (".png");
    pim->save (utf8_to_qstring (concretize (temp)), "PNG");
    temp_images << temp;
    ObjectIDType image_id= pdfWriter.GetObjectsContext()
      .GetInDirectObjectsRegistry().AllocateNewObjectID();
    image_pdf= pdf_image (temp, image_id);
    pattern_image_pool(key) = image_pdf;
  }
  // debug_convert << "  insert pattern\n";
  ObjectIDType id= pdfWriter.GetObjectsContext()
    .GetInDirectObjectsRegistry().AllocateNewObjectID();
  // debug_convert << "pdf_pattern " << ox << ", " << oy
  //  		   << ", " << pixel << ", " << shrinkf
  //		   << ", " << zoomf << LF;
  // debug_convert << "            " << to_x(0) << ", " << to_y(0) << LF;
  // debug_convert << "            " << w << ", " << h << LF;
  pdf_pattern p_pdf (image_pdf, w, h,
		     width + to_x(0), height, // FIXME ???
		     ((double) default_dpi) / dpi,
		     ((double) default_dpi) / dpi, id);
  pattern_pool(p) = p_pdf;
}

/******************************************************************************
 * Graphic state management
 ******************************************************************************/

void
pdf_hummus_renderer_rep::select_alpha (int a) {
  if (!alpha_id->contains(a)) {
    ObjectIDType temp = pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry().AllocateNewObjectID();
    alpha_id(a) = temp;
  }
  std::string name = page->GetResourcesDictionary().AddExtGStateMapping(alpha_id(a));
  contentContext->gs(name);
}

void
pdf_hummus_renderer_rep::select_stroke_color (color c) {;
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  r= ((r*1000)/255);
  g= ((g*1000)/255);
  b= ((b*1000)/255);
  a= ((a*1000)/255);
  double dr= ((double) r) / 1000.0;
  double dg= ((double) g) / 1000.0;
  double db= ((double) b) / 1000.0;
  contentContext->RG(dr, dg, db); // stroke color;
  select_alpha(a);
}

void
pdf_hummus_renderer_rep::select_fill_color (color c) {;
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  r= ((r*1000)/255);
  g= ((g*1000)/255);
  b= ((b*1000)/255);
  a= ((a*1000)/255);
  double dr= ((double) r) / 1000.0;
  double dg= ((double) g) / 1000.0;
  double db= ((double) b) / 1000.0;
  contentContext->rg(dr, dg, db); // non-stroking color
  select_alpha(a);
}

void
pdf_hummus_renderer_rep::select_stroke_pattern (brush br) {
  if (is_nil(br) || br->get_type () != brush_pattern) return;
  tree p_tree= br->get_pattern ();
  register_pattern_image (br, brushpx == -1 ? pixel : brushpx);
  if (!pattern_pool->contains (p_tree)) {
    convert_error << "select_stroke_pattern: "
                  << "cannot find registered pattern\n";
    return;
  }
  pdf_pattern p= pattern_pool[p_tree];
  std::string patternName=
    page->GetResourcesDictionary().AddPatternMapping(p->id);
  contentContext->CS("Pattern");
  contentContext->SCN((double*) NULL, 0, patternName);
}

void
pdf_hummus_renderer_rep::select_fill_pattern (brush br) {
  if (is_nil(br) || br->get_type () != brush_pattern) return;
  tree p_tree= br->get_pattern ();
  register_pattern_image (br, brushpx==-1? pixel: brushpx);
  if (!pattern_pool->contains (p_tree)) {
    convert_error << "select_fill_pattern: "
                  << "cannot find registered pattern\n";
    return;
  }
  pdf_pattern p= pattern_pool[p_tree];
  std::string patternName=
    page->GetResourcesDictionary().AddPatternMapping(p->id);
  contentContext->cs("Pattern");
  contentContext->scn((double*) NULL, 0, patternName);
  select_alpha ((1000*br->get_alpha ())/255);
}

void
pdf_hummus_renderer_rep::select_line_width (SI w) {
  double pw = w /pixel;
  //if (pw < 1) pw= 1;
  if (pw != current_width) {
    contentContext->w(pw);
    current_width = pw;
  }
}

pencil
pdf_hummus_renderer_rep::get_pencil () {
  // debug_convert << "get_color\n";
  return pen;
}

brush
pdf_hummus_renderer_rep::get_background () {
  // debug_convert << "get_background\n";
  return bgb;
}

void
pdf_hummus_renderer_rep::set_pencil (pencil pen2) {
  // debug_convert << "set_pencil\n";
  pen= pen2;
  lw= pen->get_width ();
  select_line_width (lw);
  color c= pen->get_color ();
  fg= c;
  select_fill_color (c);
  select_stroke_color (c);
  if (pen->get_type () == pencil_brush) {
    // debug_convert << "pencil has brush type" << LF;
    brush br= pen->get_brush ();
    fgb= br;
    select_fill_pattern (br);
    select_stroke_pattern (br);
  }
  if (pen->get_cap () == cap_round)
    contentContext->J(1); // round cap
  else
    contentContext->J(2); // square cap
  contentContext->j(1); // round join
}

void
pdf_hummus_renderer_rep::set_brush (brush br) {
  // debug_convert << "set_brush\n";
  fgb= br;
  pen= pencil (br);
  set_pencil (pen);  // FIXME ???
  if (is_nil (br)) return;
  if (br->get_type () == brush_none) {
    pen = pencil ();
    fgb = brush ();
  }
  else {
    select_fill_color (pen->get_color ());
    select_stroke_color (pen->get_color ());
  }
  if (br->get_type () == brush_pattern) {
    tree p_tree= br->get_pattern ();
    register_pattern_image (br, brushpx == -1 ? pixel : brushpx);
    if (!pattern_pool->contains (p_tree)) {
      convert_error << "select_stroke_pattern: "
		    << "cannot find registered pattern\n";
      return;
    }
    select_fill_pattern (br);
    select_stroke_pattern (br);
  }
  //select_alpha (br->get_alpha ());
}
void
pdf_hummus_renderer_rep::set_background (brush b) {
  // debug_convert << "set_background\n";
  //if (bgb==b) return;
  bgb= b;
  bg= b->get_color ();
}

/******************************************************************************
 * Raw images
 ******************************************************************************/

// obsolete support for direct placement of virtual glyphs without Type3 fonts.

static string
load_virtual_glyph (glyph gl) {
  string buf;
  int i, j;
  for (j= 0; j < gl->height; j++)
    for (i= 0; i < gl->width; i++) {
      if (gl->get_x (i, j) > 0) buf << (char)0;
      else buf << (char)255;
    }
 return buf;
}

static const std::string scType = "Type";
static const std::string scXObject = "XObject";
static const std::string scSubType = "Subtype";

static const std::string scImage = "Image";
static const std::string scWidth = "Width";
static const std::string scHeight = "Height";
static const std::string scColorSpace = "ColorSpace";
static const std::string scDeviceGray = "DeviceGray";
static const std::string scDeviceRGB = "DeviceRGB";
static const std::string scDeviceCMYK = "DeviceCMYK";
static const std::string scDecode = "Decode";
static const std::string scBitsPerComponent = "BitsPerComponent";
static const std::string scFilter = "Filter";
static const std::string scDCTDecode = "DCTDecode";
static const std::string scLength = "Length";


static void
create_pdf_image_raw (PDFWriter& pdfw, string raw_data, SI width, SI height, ObjectIDType imageXObjectID)
{
  
	PDFImageXObject* imageXObject = NULL;
	//EStatusCode status = PDFHummus::eSuccess;

  ObjectsContext& objectsContext = pdfw.GetObjectsContext();
  objectsContext.StartNewIndirectObject(imageXObjectID);
  do {
    {
      // write stream dictionary
      DictionaryContext* imageContext = objectsContext.StartDictionary();
      // type
      imageContext->WriteKey(scType);
      imageContext->WriteNameValue(scXObject);
      // subtype
      imageContext->WriteKey(scSubType);
      imageContext->WriteNameValue(scImage);
      // Width
      imageContext->WriteKey(scWidth);
      imageContext->WriteIntegerValue(width);
      // Height
      imageContext->WriteKey(scHeight);
      imageContext->WriteIntegerValue(height);
      // Bits Per Component
      imageContext->WriteKey(scBitsPerComponent);
      imageContext->WriteIntegerValue(8);
      // Color Space and Decode Array if necessary
      imageContext->WriteKey(scColorSpace);
      imageContext->WriteNameValue(scDeviceGray);
      // Length
      imageContext->WriteKey("Length");
      imageContext->WriteIntegerValue(N(raw_data));
      objectsContext.EndDictionary(imageContext);
    }
    {
      // write stream
      objectsContext.WriteKeyword("stream");
      {
        c_string buf (raw_data);
        objectsContext.StartFreeContext()->Write((unsigned char*)(char *)buf, N(raw_data));
        objectsContext.EndFreeContext();
      }
      objectsContext.EndLine();
      objectsContext.WriteKeyword("endstream");
    }
    objectsContext.EndIndirectObject();
    imageXObject = new PDFImageXObject(imageXObjectID, KProcsetImageB);
  } while(false);
 
  if (imageXObject == NULL)
    convert_error <<  "pdf_hummus, failed to include glyph" << LF;
  else delete imageXObject;
}

class pdf_raw_image_rep : public concrete_struct
{
public:
  string data;
  int w,h;
  ObjectIDType id;
  
  pdf_raw_image_rep (string _data, int _w, int _h, ObjectIDType _id)
   : data(_data), w(_w), h(_h), id(_id) {}
  pdf_raw_image_rep () {}
  
  void flush(PDFWriter& pdfw) {
    // debug_convert << "flushing :" << id << LF;
    create_pdf_image_raw (pdfw, data, w, h, id);
  }
}; // pdf_raw_image_rep

class pdf_raw_image {
  CONCRETE_NULL(pdf_raw_image);
  pdf_raw_image (string _data, int _w, int _h, ObjectIDType _id):
  rep (tm_new<pdf_raw_image_rep> (_data,_w,_h,_id)) {};
};

CONCRETE_NULL_CODE(pdf_raw_image);

void
pdf_hummus_renderer_rep::flush_glyphs ()
{
  // flush all images
  iterator<string> it = iterate (pdf_glyphs);
  while (it->busy()) {
    pdf_raw_image im = pdf_glyphs [it->next()];
    im->flush (pdfWriter);
  }
}

void
pdf_hummus_renderer_rep::draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y)
{
  // use bitmap (to be improved)
  string fontname = fn->res_name;
  string char_name (fontname * "-" * as_string ((int) ch));
  if (!pdf_glyphs->contains(char_name)) {
    // debug_convert << "draw bitmap glyph " << (double)gl->width / 8 << " " << (double)gl->height / 8 << "\n";
    glyph gl= fn->get(ch);
    if (is_nil (gl)) return;
    string buf= load_virtual_glyph (gl);
    ObjectIDType imageXObjectID  = pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry().AllocateNewObjectID();
    pdf_glyphs (char_name) = pdf_raw_image (buf, gl->width, gl->height, imageXObjectID);
  }
}


/******************************************************************************
 * Type 3 fonts
 ******************************************************************************/

static int
t3font_font_chunk (int global_glyph) {
  if (global_glyph <= 255) return 0;
  return global_glyph / 255;
}

static int
t3font_get_local_glyph (int global_glyph, int font_chunk, string font_name) {
  int chunk = t3font_font_chunk (global_glyph);
  if (chunk != font_chunk)
    convert_error << "pdf_hummus_renderer, in t3font support, glyph "
		  << as_string (global_glyph) << " out of chunk "
		  << as_string (font_chunk)
		  << " of font " << font_name << LF;
  if (global_glyph <= 255)
    return global_glyph;
  return (global_glyph % 255) + 1;
}

static int
t3font_get_global_glyph (int local_glyph, int font_chunk, string font_name) {
  if (local_glyph > 255)
    convert_error << "pdf_hummus_renderer, in t3font support, local glyph "
		  << as_string (local_glyph)
		  << " is out of the range 0,...,255 "
		  << "in font " << font_name << LF;
  if (font_chunk == 0)
    return local_glyph;
  return 255 * font_chunk + local_glyph - 1;
}

class t3font_rep : public concrete_struct {
public:
  font_glyphs fn;
  int font_chunk;
  ObjectIDType fontId;
  ObjectsContext &objectsContext;
  hashmap<int, int> used_chars;
  int firstchar;
  int lastchar;
  int b0,b1,b2,b3; // glyph bounding box
  bool first_glyph;
  
  t3font_rep (font_glyphs _fn, int _font_chunk,
	      ObjectsContext &_objectsContext)
    : fn (_fn), font_chunk (_font_chunk),
      objectsContext (_objectsContext), first_glyph (true) {
    fontId = objectsContext.GetInDirectObjectsRegistry()
               .AllocateNewObjectID(); }  
  void update_bbox (int llx, int lly, int urx, int ury);
  void add_glyph (int ch) {  used_chars (ch) = 1; }
  void write_char (glyph gl, ObjectIDType inCharID);
  void write_definition (int& registry_id);
};

class t3font {
  CONCRETE_NULL(t3font);
  t3font (font_glyphs _fn, int _font_chunk,
	  ObjectsContext &_objectsContext)
    : rep (tm_new<t3font_rep> (_fn, _font_chunk,_objectsContext)) {};
};

CONCRETE_NULL_CODE(t3font);

void
t3font_rep::update_bbox (int llx, int lly, int urx, int ury) {
  if (first_glyph) {
    b0 = llx; b1 = lly; b2 = urx; b3 = ury;
    first_glyph = false;
  } else {
    if (llx < b0) b0 = llx;
    if (lly < b1) b1 = lly;
    if (urx > b2) b2 = urx;
    if (ury > b3) b3 = ury;
  }
}

void
t3font_rep::write_char (glyph gl, ObjectIDType inCharID) {
  int llx, lly, urx, ury, cwidth, cheight, lwidth;
  llx = -gl->xoff;
  lly = gl->yoff-gl->height+1;
  urx = gl->width-gl->xoff+1;
  ury = gl->yoff+1;
  cwidth = gl->width;
  cheight = gl->height;
  lwidth = gl->lwidth;
  
  objectsContext.StartNewIndirectObject(inCharID);
  // write char stream
  PDFStream *charStream = objectsContext.StartPDFStream(NULL, true);
  string data;
  if (is_nil (gl)) {
    // write d0 command
    data  << "0 0 d0\r\n";
  } else {
    update_bbox (llx, lly, urx, ury);
    data << as_string (lwidth) << " 0 ";
    data << as_string (llx) << " " << as_string (lly) << " "
	 << as_string (urx) << " " << as_string (ury) << " d1\r\n";
    data << "q\r\n";
    data  << as_string ((double)(cwidth)) << " 0 0 "
	  << as_string ((double)(cheight)) << " "
	  << as_string ((double)(llx)) << " "
	  << as_string (lly) << " cm\r\n";
    data << "BI\r\n/W " << as_string (cwidth)
	 << "\r\n/H " << as_string (cheight) << "\r\n";
    data << "/CS /G /BPC 1 /F /AHx /D [0.0 1.0] /IM true\r\nID\r\n";
    static const char* hex_string= "0123456789ABCDEF";
    string hex_code;
    int i, j, count= 0, cur= 0;
    for (j= 0; j < cheight; j++)
      for ( i= 0; i < ((cwidth+7) & (-8)); i++) {
	cur= cur << 1;
	if ((i < cwidth) && (gl->get_x(i,j) == 0)) cur++;
	count++;
	if (count == 4) {
	  hex_code << hex_string[cur];
	  cur  = 0;
	  count= 0;
	}
      }
    data << hex_code;
    data << ">\r\nEI\r\nQ\r\n"; // ">" is the EOD char for ASCIIHex
  }
  c_string buf (data);
  charStream->GetWriteStream()->Write((unsigned char *)(char*)buf, N(data));
  objectsContext.EndPDFStream(charStream); // It does the EndIndirectObject()
  delete charStream;
}

void
t3font_rep::write_definition (int& registry_id) {
  array <int> glyph_list;
  array<ObjectIDType> charIds;
  // order used glyphs
  iterator<int> it = iterate (used_chars);
  while (it->busy())
    glyph_list << t3font_get_local_glyph (it->next(), font_chunk, fn->res_name);
  merge_sort(glyph_list);
  if (N(glyph_list) > 0) {
    firstchar = glyph_list [0];
    lastchar = glyph_list [N(glyph_list)-1];
  }
  else {
    convert_warning << "pdf_hummus_renderer, unexpected empty t3 font "
      << fn->res_name << LF;
    firstchar = 255;
    lastchar = 0;
  }
  // write glyphs definitions
  for (int i = 0; i < N(glyph_list); ++i) {
    int ch = t3font_get_global_glyph (glyph_list[i],
				      font_chunk, fn->res_name);
    glyph gl = fn->get (ch);
    ObjectIDType temp=
      objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
    charIds << temp;
    write_char (gl, temp);
  }
  ObjectIDType tounicodeId;
  // create font dictionary
  string dict;
  dict << "<<\r\n";
  dict << "\t/Type /Font\r\n";
  dict << "\t/Resources << /ProcSet [/PDF /ImageB] >>\r\n";
  dict << "\t/Subtype /Type3\r\n\t/FontBBox [ "
       << as_string (b0) << " " << as_string (b1) << " "
       << as_string (b2) << " " << as_string (b3) << "]\r\n";
  dict << "\t/FontMatrix [" << as_string (1/100.0) << " 0 0 "
       << as_string (1/100.0) << " 0 0 ]\r\n";
  dict << "\t/FirstChar " << as_string (firstchar)
       << "\r\n\t/LastChar " << as_string (lastchar) << "\r\n";
  dict << "\t/Widths [ ";
  if (N(glyph_list) > 0)
    for (int i = firstchar; i <= lastchar ; ++i) {
      int ch = t3font_get_global_glyph (i, font_chunk, fn->res_name);
      if (used_chars->contains (ch))
	dict << as_string ((double)(fn->get (ch)->lwidth))
	     << " ";
      else dict << "0 ";
    }
  // Write CharProcs
  dict << "]\r\n\t/CharProcs <<\r\n";
  for (int i = 0; i < N(glyph_list); ++i) {
    int ch = glyph_list[i];
    ObjectIDType temp = charIds[i];
    dict << "\t\t/ch" << as_string (ch) << " "
	 << as_string (temp) << " 0 R\r\n";
  }
  if (N(glyph_list) > 0)
    dict << "\t\t/.notdef " << as_string (charIds[0]) << " 0 R\r\n";
  dict << "\t>>\r\n";
  // Encodings
  if (N(glyph_list) > 0) {
    tounicodeId =
      objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
    dict << "\t/ToUnicode " << as_string(tounicodeId) << " 0 R\r\n";
  }
  dict << "\t/Encoding <<\r\n";
  dict << "\t\t/Type /Encoding\r\n";
  dict << "\t\t/Differences [";
  if (N(glyph_list) > 0)
    for (int i = firstchar, previousEncoding = firstchar; i <= lastchar; ++i) {
      int ch = t3font_get_global_glyph (i, font_chunk, fn->res_name);
      if (used_chars->contains (ch)) {
        if (previousEncoding + 1 != i) {
          dict << "\r\n\t\t\t" << as_string(i) << " ";
        }
        dict << "/ch" << as_string(i) << " ";
        previousEncoding = i;
      }
    }
  dict << " ]\r\n\t>>\r\n>>\r\n";
  write_indirect_obj (objectsContext, fontId, dict);
  if (N(glyph_list) == 0)
    return;
  // Cmap
  registry_id++;
  string registry = "TeXmacs-type3-ToUnicode-CMap-" * as_string (registry_id);
  string cmap;
  cmap << "/CIDInit /ProcSet findresource begin 12 dict\r\n"
    << "begin begincmap /CIDSystemInfo <<\r\n"
    << "  /Registry (" << registry
    << ") /Ordering (Custom) /Supplement 0 >>\r\n"
    << "def /CMapName /" << registry << " def /CMapType 2 def\r\n";
  cmap << "1 begincodespacerange <" << as_hexadecimal (firstchar, 2)
       << "> <" << as_hexadecimal (lastchar, 2)
       << "> endcodespacerange\r\n";
#if 1
  cmap << "1 beginbfrange\r\n";
  int ch= t3font_get_global_glyph (firstchar, font_chunk, fn->res_name);
  cmap << "\t<" << as_hexadecimal (firstchar, 2) << "> "
       << "<" << as_hexadecimal (lastchar, 2) << "> "
       << "<" << as_hexadecimal (ch, 4) << ">\r\n";
  cmap << "endbfrange\r\n";
#else
  // Left for testing purpose
  cmap << as_string (N(glyph_list))
       << " beginbfchar\r\n";
  for (int i= 0; i < N(glyph_list); i++) {
    int lch= glyph_list[i];
    int ch= t3font_get_global_glyph (lch, font_chunk, fn->res_name);
    cmap << "\t<" << as_hexadecimal (lch, 2)
	 << "> <" << as_hexadecimal (ch, 4) << ">\r\n";
  }
  cmap << "endbfchar\r\n";
#endif
  cmap << "endcmap CMapName currentdict /CMap defineresource\r\n"
       << "pop end end\r\n";
  objectsContext.StartNewIndirectObject(tounicodeId);
  PDFStream* cmapStream = objectsContext.StartPDFStream (NULL,true);
  OutputStreamTraits outputTraits(cmapStream->GetWriteStream ());
  c_string buf (cmap);
  InputByteArrayStream reader ((IOBasicTypes::Byte*)(char*)buf, N(cmap));
  EStatusCode status = outputTraits.CopyToOutputStream (&reader);
  if (status != PDFHummus::eSuccess) {
    delete cmapStream;
    convert_error << "pdf_hummus_renderer, internal error at line "
		  << as_string (__LINE__) << LF;
  }
  objectsContext.EndPDFStream (cmapStream); // It does EndIndirectObject();
  delete cmapStream;
}

void
pdf_hummus_renderer_rep::flush_fonts()
{
  // flush t3 fonts
  iterator<string> it = iterate(t3font_list);
  while (it->busy()) {
    string name = it->next();
    t3font f = t3font_list[name];
    f->write_definition(t3font_registry_id);
  }
}

/******************************************************************************
 * Text handling
 ******************************************************************************/

void
pdf_hummus_renderer_rep::make_pdf_font (string fontname)
{
  int pos= search_forwards (":", fontname);
  string fname= (pos==-1? fontname: fontname (0, pos));
  url u = url_none ();
  {
    //debug_convert << " try freetype " << LF;
    u = tt_font_find (fname);
    //debug_convert << fname << " " << u << LF;
  }
  if (!is_none (u)) {
    int pos= search_forwards (".", fontname);
    string rname= (pos==-1? fontname: fontname (0, pos));
    //double fsize= font_size (fn->res_name);
    
    //char *_rname = as_charp(fname);
    PDFUsedFont* font;
    {
      //debug_convert << "GetFontForFile "  << u  << LF;
      c_string _u (concretize (u));
      font = pdfWriter.GetFontForFile((char*)_u);
    }
    
    if (font != NULL) {
      native_fonts (fontname)= font;
      std::string _ps_name= font->GetFreeTypeFont()->GetPostscriptName();
      string ps_name (_ps_name.c_str ());
      if (starts (ps_name, "EuropeanComputerModern"))
	EuropeanComputerModern_fonts->insert (fontname);
      //cout << EuropeanComputerModern_fonts << LF;
      return;
    }
    else {
      convert_warning << "pdf_hummus_renderer, font: " << fname
		      << " in file " << u << " cannot be loaded. "
		      << "It is converted to bitmap type 3 font." << LF;
    }
  }
  not_native_fonts->insert (fontname);
}

static double
font_size (string name) {
  int pos= search_backwards (".", name);
  int szpos= pos-1;
  while ((szpos>0) && is_numeric (name[szpos-1])) szpos--;
  double size= as_double (name (szpos, pos));
  if (size == 0) size= 10;
  int end= pos+1;
  while (end < N(name) && is_numeric (name[end])) end++;
  double dpi= as_double (name (pos+1, end));
  double mag= (size) * (dpi/72.0);
  return mag;
}

static bool
match_font_base_name (string fontname, string basename) {
  if (!starts (fontname, basename))
    return false;
  int i= N(basename);
  while (i < N(fontname)) {
    if (fontname[i] == ':') break;
    if (fontname[i] < '0' || fontname[i] > '9')
      return false;
    i++;
  }
  return true;
}

static bool
requires_hack_notdef_for_tex_font (string fontname) {
  // This fix is necessary for avoiding bugs in certain Pdf viewers,
  // such as old versions of Preview under MacOS (<= 10.6.*).
  // It is expected to be removed in near future.
  static hashmap<string,bool> remember;
  if (remember->contains(fontname))
    return remember[fontname];
  bool r=
    match_font_base_name (fontname, "cmbsy") ||
    match_font_base_name (fontname, "cmmib") ||
    match_font_base_name (fontname, "cmb") ||
    match_font_base_name (fontname, "cmbx") ||
    match_font_base_name (fontname, "cmbxsl") ||
    match_font_base_name (fontname, "cmbxti") ||
    match_font_base_name (fontname, "cmbcsc") ||
    match_font_base_name (fontname, "cmdunh") ||
    match_font_base_name (fontname, "cmex") ||
    //match_font_base_name (fontname, "cmexb") ||	       
    match_font_base_name (fontname, "cmff") ||
    match_font_base_name (fontname, "cmfi") ||
    match_font_base_name (fontname, "cmfib") ||
    match_font_base_name (fontname, "cminch") ||
    match_font_base_name (fontname, "cmitt") ||
    match_font_base_name (fontname, "cmmi") ||
    match_font_base_name (fontname, "cmmib") ||
    match_font_base_name (fontname, "cmr") ||
    match_font_base_name (fontname, "cmsl") ||
    match_font_base_name (fontname, "cmsltt") ||
    match_font_base_name (fontname, "cmss") ||
    match_font_base_name (fontname, "cmssbx") ||
    match_font_base_name (fontname, "cmssdc") ||
    match_font_base_name (fontname, "cmssi") ||
    match_font_base_name (fontname, "cmssq") ||
    match_font_base_name (fontname, "cmssqi") ||
    match_font_base_name (fontname, "cmsy") ||
    match_font_base_name (fontname, "cmtcsc") ||
    match_font_base_name (fontname, "cmtex") ||
    match_font_base_name (fontname, "cmti") ||
    match_font_base_name (fontname, "cmtt") ||
    match_font_base_name (fontname, "cmu") ||
    match_font_base_name (fontname, "cmvtt") ||
    match_font_base_name (fontname, "euex") ||
    match_font_base_name (fontname, "eufb") ||
    match_font_base_name (fontname, "eufm") ||
    match_font_base_name (fontname, "eurb") ||
    match_font_base_name (fontname, "eurm") ||
    match_font_base_name (fontname, "eusb") ||
    match_font_base_name (fontname, "eusm") ||
    match_font_base_name (fontname, "msam") ||
    match_font_base_name (fontname, "msbm");
  remember(fontname)= r;
  return r;
}
     
void
pdf_hummus_renderer_rep::draw (int ch, font_glyphs fn, SI x, SI y) {
  //debug_convert << "draw \"" << (char)ch << "\" " << ch << " "
  //		<< fn->res_name << "\n";
  glyph gl= fn->get(ch);
  if (is_nil (gl)) return;
  string fontname = fn->res_name;
  int fontchunk= t3font_font_chunk (ch);
  string fontchunkname= fontname * string ("-chunk") * as_string (fontchunk);

  if (ch == 0 && requires_hack_notdef_for_tex_font (fontname)) {
    draw (161, fn, x, y);
    return;
  }
  string char_name (fontname * "-" * as_string (ch));
  pdf_raw_image glyph;
  
  if (cfn != fontname && cfn != fontchunkname) {
    if (!native_fonts->contains (fontname) &&
	!not_native_fonts->contains (fontname))
      make_pdf_font (fontname);
    if (not_native_fonts->contains (fontname)) {
      fontname= fontchunkname;
      if (!t3font_list->contains (fontname)) {
	//cout << "create t3font for chunk " << fontchunk
	//     << " of " << fn->res_name << LF;
	t3font f (fn, fontchunk, pdfWriter.GetObjectsContext());
	t3font_list (fontchunkname) = f;
      }
    }
    //debug_convert << "CHANGE FONT" << LF;
    begin_text ();
    fsize = font_size (fontname);
    if (native_fonts->contains (fontname)) {
      cfid = native_fonts (fontname);
      contentContext->Tf (cfid, fsize);
    } else {
      cfid = NULL;
      std::string name = page->GetResourcesDictionary()
	.AddFontMapping (t3font_list (fontname)->fontId);
      // pk fonts are encoded in t3 fonts as bitmaps.
      // they cannot be scaled and are encoded in such a way that
      // they should be rendered at size 100 (conventional value)
      // to give the correct result (see the Font Matrix defined
      // in t3font_rep::write_definition).
      contentContext->TfLow (name, 100);
    }
    cfn = fontname;
  }
  else
    cfid= native_fonts (fontname);
  begin_text ();
  contentContext->Td (to_x(x) - prev_text_x, to_y(y) - prev_text_y);
  prev_text_x = to_x(x);
  prev_text_y = to_y(y);
  //debug_convert << "char " << ch << "index " << gl->index
  //              << " " << x << " " << y << " font " << cfn  << LF;
  if (cfid == NULL)
    t3font_list(cfn)->add_glyph (ch);
  GlyphUnicodeMappingList glyphs;
  if (cfid != NULL &&
      EuropeanComputerModern_fonts->contains (cfn) &&
      gl->index >= 27 && gl->index <= 31) {
    ch += 0xfb00 - 27;
  }
  int gl_index;
  if (cfid != NULL)
    gl_index= gl->index;
  else
    gl_index= t3font_get_local_glyph
      (ch, t3font_list(cfn)->font_chunk,
       t3font_list(cfn)->fn->res_name);
  static const std::string ligature_ff= "/Span << /ActualText (ff) >> BDC ";
  static const std::string ligature_fi= "/Span << /ActualText (fi) >> BDC ";
  static const std::string ligature_fl= "/Span << /ActualText (fl) >> BDC ";
  static const std::string ligature_ffi= "/Span << /ActualText (ffi) >> BDC ";
  static const std::string ligature_ffl= "/Span << /ActualText (ffl) >> BDC ";
  if (ch >= 0xfb00 && ch <= 0xfb04 && ePDFVersion >= ePDFVersion15) {
    if (ch == 0xfb00) contentContext->WriteFreeCode (ligature_ff);
    if (ch == 0xfb01) contentContext->WriteFreeCode (ligature_fi);
    if (ch == 0xfb02) contentContext->WriteFreeCode (ligature_fl);
    if (ch == 0xfb03) contentContext->WriteFreeCode (ligature_ffi);
    if (ch == 0xfb04) contentContext->WriteFreeCode (ligature_ffl);
    if (cfid != NULL) {
      glyphs.push_back(GlyphUnicodeMapping(gl_index, ch));
      contentContext->Tj (glyphs);
    }
    else {
      std::string buf; buf.push_back (gl_index);
      contentContext->TjLow (buf);
    }
    contentContext->WriteFreeCode (" EMC\r\n");
  }
  else {
    if (cfid != NULL) {
      glyphs.push_back (GlyphUnicodeMapping (gl_index, ch));
      contentContext->Tj(glyphs);
    }
    else {
      std::string buf; buf.push_back (gl_index);
      contentContext->TjLow (buf);
    }
  }    
}

/******************************************************************************
 * Graphics primitives
 ******************************************************************************/

void
pdf_hummus_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  // debug_convert << "line\n";
  end_text ();
  contentContext->m(to_x (x1), to_y (y1));
  contentContext->l(to_x (x2), to_y (y2));
  contentContext->S();
}

void
pdf_hummus_renderer_rep::lines (array<SI> x, array<SI> y) {
  // debug_convert << "lines\n";
  end_text ();
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  end_text ();
  contentContext->q();
  if (pen->get_cap () == cap_round ||
      (x[N(x)-1] == x[0] && y[N(y)-1] == y[0]))
    contentContext->J(1); // round cap
  else
    contentContext->J(2); // square cap
  contentContext->j(1); // round join
  contentContext->m(to_x (x[0]), to_y (y[0]));
  for (i=1; i<n; i++) {
    contentContext->l(to_x (x[i]), to_y (y[i]));
  }
  contentContext->S();
  contentContext->Q();
}

void
pdf_hummus_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  end_text ();
  double xx1= to_x (min (x1, x2));
  double yy1= to_y (min (y1, y2));
  double xx2= to_x (max (x1, x2));
  double yy2= to_y (max (y1, y2));
  // debug_convert << "clear" << xx1 << " " << yy1 << " " << xx2 << " " << yy2 << LF;
  contentContext->q();
  select_fill_color (bg);
  select_fill_pattern (bgb);
  contentContext->re(xx1, yy1, xx2-xx1, yy2-yy1);
  contentContext->h();
  contentContext->f();
  select_fill_color (fg);
  select_fill_pattern (fgb);
  contentContext->Q();
}

void
pdf_hummus_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if ((x1<x2) && (y1<y2)) {
    end_text ();
    double xx1= to_x (min (x1, x2));
    double yy1= to_y (min (y1, y2));
    double xx2= to_x (max (x1, x2));
    double yy2= to_y (max (y1, y2));
    contentContext->re(xx1, yy1, xx2-xx1, yy2-yy1);
    contentContext->h();
    contentContext->f(); // FIXME Winding
  }
}

void
pdf_hummus_renderer_rep::bezier_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta, bool filled)
{
  // PDF can describe only cubic bezier paths, so we have to make up the arc with them
  // Since this is not mathematically exact, we minimize errors by drawing beziers sub-arcs of at most 90??
  end_text ();
  contentContext->q(); // save graphics state

    double xx1 = to_x(x1), yy1 = to_y(y1), xx2 = to_x(x2), yy2 = to_y(y2);
    double cx = (xx1+xx2)/2, cy = (yy1+yy2)/2;
    double rx = (xx2-xx1)/2, ry = (yy2-yy1)/2;
    contentContext->cm(1, 0, 0, 1, cx, cy); // centering
	//we can't apply scale here because in pdf the pen is scaled too

	int i=1+abs(delta)/(90*64); //number of sub-arcs needed 
	if ((abs(delta)%(90*64))==0) i-- ; //correction needed if exact multiple of 90??
	double phi= 2.0*M_PI*(delta)/(i*360.0*64.0); //angular span of each sub-arc
	double a = 2.0*M_PI*(alpha)/(360.0*64.0); //start angle in radians

  // Control points for an arc of radius 1, centered on the x-axis and spanning phi degrees
  // from: http://www.tinaja.com/glib/bezcirc2.pdf
    double sphi = sin(phi/2),	cphi = cos(phi/2);
    double bx0 = cphi,			by0 = -sphi;
    double bx1 = (4.0-bx0)/3.0,	by1 = (1.0-bx0)*(3.0-bx0)/(3.0*by0);
    double bx2 = bx1,			by2 = -by1;
    double bx3 = bx0,			by3 = -by0;
	
	// repeatedly draw rotated and scaled sub-arc
	// cannot use user-space transformations with cm util path is painted (otherwise path is lost)
	// so we perform explicit rotation+scaling calculations
	int k;
	for (k=0; k<i;k++) {
	sphi = sin(phi*(k+0.5)+a); 
    cphi = cos(phi*(k+0.5)+a);
	if (k==0) contentContext->m(rx*(bx0*cphi-by0*sphi),ry* (+bx0*sphi+by0*cphi)); //start point
	contentContext->c(
		rx*(bx1*cphi-by1*sphi), ry*(+bx1*sphi+by1*cphi),
		rx*(bx2*cphi-by2*sphi), ry*(+bx2*sphi+by2*cphi),
		rx*(bx3*cphi-by3*sphi), ry*(+bx3*sphi+by3*cphi));
	}
	
	//paint
    if (filled) {
		// contentContext->l(0.0, 0.0); // for a filled "pie" with vertex at the center
		contentContext->f();
	}	
	else  ( (abs(delta) == 360*64) ? contentContext->s() : contentContext->S()); 
	// here we close the path if it's a full circle 
  contentContext->Q(); // restore the graphics state (undoes centering only)
}

void
pdf_hummus_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  // debug_convert << "arc\n";
  end_text ();
  bezier_arc(x1, y1, x2, y2, alpha, delta, false);
}

void
pdf_hummus_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  // debug_convert << "fill_arc\n";
  end_text ();
  bezier_arc(x1, y1, x2, y2, alpha, delta, true);
}

void
pdf_hummus_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  // debug_convert << "polygon\n";
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  end_text ();
  contentContext->m(to_x (x[0]), to_y (y[0]));
  for (i=1; i<n; i++)
    contentContext->l(to_x (x[i]), to_y (y[i]));
  contentContext->h();
  if (convex)
    contentContext->f(); // odd-even
  else
    contentContext->fStar(); // nonzero winding
}

void
pdf_image_rep::flush (PDFWriter& pdfw)
{
  url name= resolve (u);
  if (is_none (name))
    name= "$TEXMACS_PATH/misc/pixmaps/unknown.ps";
  
  url temp;
  string s= suffix (name);
  // debug_convert << "flushing :" << fname << LF;
  if (s == "pdf") {
    // double v= as_double (pdf_version (name));
    // if (10 * v > ((double) ePDFVersion))
    //   convert_warning << "\"" << concretize (name) << "\" has version "
    // 		      << v << "." << LF
    // 		      << "But current PDF version has been set to " << ((double) ePDFVersion)/10
    // 		      << " (see the preference menu)." << LF;
    if (get_preference ("texmacs->pdf:distill inclusion") == "on") {
      temp= url_temp (".pdf");
      if (!gs_PDF_EmbedAllFonts (name, temp)) {
	temp= name;
	name= url_none ();
      }
    }
    else {
      temp= name;
      name= url_none ();
    }
  } 
  else {
    temp= url_temp (".pdf");
    // first try to work out inclusion using our own tools
    // note that we have to return since flush_raster and flush_jpg
    // already build the appopriate Form XObject into the PDF
  
    if ((s == "jpg") || (s == "jpeg")) 
      if (flush_jpg(pdfw, name)) return;
#ifndef PDFHUMMUS_NO_PNG
    if (s == "png")
      if (flush_png(pdfw, name)) return;
#endif
    // other formats we generate a pdf (with available converters) that we'll embbed
    image_to_pdf (name, temp, w, h, 300);
    // the 300 dpi setting is the maximum dpi of raster images that will be generated:
    // images that are to dense will de downsampled to keep file small
    // (other are not up-sampled) 
    // dpi DOES NOT apply for vector images that we know how to handle : eps, svg(if inkscape present)
    // 
    // TODO: make the max dpi setting smarter (printer resolution, preference ...)
  }
  EStatusCode status = PDFHummus::eFailure;
  DocumentContext& dc = pdfw.GetDocumentContext();

  char* _temp= as_charp(concretize(temp));
  PDFDocumentCopyingContext *copyingContext = pdfw.CreatePDFCopyingContext(_temp);
  if(copyingContext) {
    PDFPageInput pageInput(copyingContext->GetSourceDocumentParser(),
			   copyingContext->GetSourceDocumentParser()->ParsePage(0));
    EPDFVersion version= (EPDFVersion)(int)(copyingContext->GetSourceDocumentParser()->GetPDFLevel() * 10);
    if (version > ePDFVersion)
      convert_warning << "\"" << _temp << "\" has version " << ((double) version)/10 << "." << LF
		      << "But current PDF version has been set to " << ((double) ePDFVersion)/10
		      << " (see the preference menu)." << LF;
    double tMat[6] ={ 1,0, 0, 1, 0, 0} ;
    PDFRectangle cropBox (0,0,0,0);
    pdf_image_info (temp, w, h, cropBox, tMat, pageInput);
    PDFFormXObject *form = dc.StartFormXObject(cropBox, id, tMat, true);
    status = copyingContext->MergePDFPageToFormXObject(form,0);
    if(status == eSuccess) pdfw.EndFormXObjectAndRelease(form);
    delete copyingContext;
  }
  if(!is_none(name)) remove (temp);
  
  if (status == PDFHummus::eFailure) {
    convert_error << "pdf_hummus, failed to include image file: "
                  << temp << LF;
  }
}

void
hummus_pdf_image_size (url image, int& w, int& h) {
  InputFile pdfFile;
  PDFParser* parser= new PDFParser();
  pdfFile.OpenFile(as_charp(concretize(image)));
  EStatusCode status = parser->StartPDFParsing(pdfFile.GetInputStream());
  if (status != PDFHummus::eFailure) {
    PDFPageInput pageInput(parser, parser->ParsePage(0));
    double tMat[6] ={ 1,0, 0, 1, 0, 0};
    PDFRectangle cropBox (0,0,0,0);
    pdf_image_info (image, w, h, cropBox, tMat, pageInput);
    delete(parser);
  }
  else {
    convert_error << "pdf_hummus, failed to get image size for: "
                  <<image << LF;
    w=h=0;              
  }
}

void
pdf_image_info (url image, int& w, int& h, PDFRectangle& cropBox, double (&tMat)[6], PDFPageInput& pageInput) {
  int rot= (pageInput.GetRotate())%360;
  if (rot < 0) rot +=360;
  cropBox=pageInput.GetCropBox();
  PDFRectangle mediaBox=pageInput.GetMediaBox();
  if (!(cropBox.LowerLeftX >= mediaBox.LowerLeftX &&
	cropBox.LowerLeftY >= mediaBox.LowerLeftY &&
	cropBox.UpperRightX <= mediaBox.UpperRightX &&
	cropBox.UpperRightY <= mediaBox.UpperRightY))
    convert_warning << "pdf_image_info, " << image << ": "
                    << "cropbox not included in mediabox\n";
  w= cropBox.UpperRightX-cropBox.LowerLeftX;
  h= cropBox.UpperRightY-cropBox.LowerLeftY;
  if (DEBUG_CONVERT) {
    debug_convert << "hummus_pdf_image_info:"<< LF
      << "image ="<<image<<LF
      << "crop box={"<<cropBox.LowerLeftX<< ", "<<cropBox.UpperRightX 
      << ", "<<cropBox.LowerLeftY<< ", "<<cropBox.UpperRightY<<"}"<< LF
      << "w,h={"<<w<< ", "<<h <<"}"<< LF;
  }
  int z;
  switch  (rot) {
    case 0 :
      tMat[0] = 1; 
      tMat[1] = 0;
      tMat[2] = 0;
      tMat[3] = 1;
      tMat[4] = -cropBox.LowerLeftX;
      tMat[5] = -cropBox.LowerLeftY;
      break;
    case 90 :
      tMat[0] = 0;
      tMat[1] = -1 ;
      tMat[2] = 1;
      tMat[3] = 0;
      tMat[4] = -cropBox.LowerLeftY;
      tMat[5] = cropBox.UpperRightX ;
      z = w;
      w = h;
      h = z;
      break;
    case 180 :
      tMat[0] = -1;
      tMat[1] = 0;
      tMat[2] = 0;
      tMat[3] = -1;
      tMat[4] = cropBox.UpperRightX ;
      tMat[5] = cropBox.UpperRightY ;
      break;
    case 270 :
      tMat[0] = 0;
      tMat[1] = 1 ;
      tMat[2] = -1;
      tMat[3] = 0;
      tMat[4] = cropBox.UpperRightY ;
      tMat[5] = -cropBox.LowerLeftX;
      z = w;
      w = h;
      h = z;
      break;
    default :
      convert_error << "unexpected rotate()="<< rot<<" in image "<<image << LF;
    }
  if (DEBUG_CONVERT) debug_convert << "degrees image rotated :"<< rot << LF
    << "dx,dy={"<<tMat[4]<< ", "<<tMat[5] <<"}"<< LF;
}

#ifdef QTTEXMACS
void
qt_image_data (url image, int& w, int&h, string& data, string& mask) {
  // debug_convert << "in qt_image_data"<<LF; 
  QImage im (utf8_to_qstring (concretize (image)));
  if (im.isNull ()) {
    convert_error << "Cannot read image file '" << image << "'"
    << " in qt_image_data" << LF;
    return;
  }
  w=  im.width ();
  h=  im.height ();    
  data = string ((w*h)*3);
  mask = string (w*h); 
  int i= 0, j= 0, k= 0, l= 0;
  for (j= 0; j < im.height (); j++) {
    for (i=0; i < im.width (); i++) {
      QRgb p= im.pixel (i, j);
      data[l++] = qRed (p);
      data[l++] = qGreen (p);
      data[l++] = qBlue (p);
      mask[k++] = qAlpha (p);
    }
  }
}
#endif

bool
pdf_image_rep::flush_for_pattern (PDFWriter& pdfw) {
  string data, smask;
  int iw = 0, ih =0;
#ifdef QTTEXMACS
  qt_image_data (u, iw, ih, data, smask);
#else
  convert_error << "pdf_image_rep::flush_for_pattern: cannot export pattern "
		<< u << "  to PDF" << LF;
#endif
  if ((iw==0)||(ih==0)) return false;
  
  ObjectsContext& objectsContext = pdfw.GetObjectsContext();
  objectsContext.StartNewIndirectObject(id);

  DictionaryContext* imageContext = objectsContext.StartDictionary();
  imageContext->WriteKey(scType);
  imageContext->WriteNameValue(scXObject);
  imageContext->WriteKey(scSubType);
  imageContext->WriteNameValue(scImage);
  imageContext->WriteKey(scWidth);
  imageContext->WriteIntegerValue(iw);
  imageContext->WriteKey(scHeight);
  imageContext->WriteIntegerValue(ih);
  imageContext->WriteKey(scBitsPerComponent);
  imageContext->WriteIntegerValue(8);
  imageContext->WriteKey(scColorSpace);
  imageContext->WriteNameValue(scDeviceRGB);
  ObjectIDType smaskId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
  imageContext->WriteKey("SMask");
  imageContext->WriteNewObjectReferenceValue(smaskId);
  PDFStream* imageStream = objectsContext.StartPDFStream(imageContext, true);
  OutputStreamTraits outputTraits(imageStream->GetWriteStream());
  c_string buf (data);
  InputByteArrayStream reader((IOBasicTypes::Byte*)(char *)buf, N(data));
  EStatusCode status = outputTraits.CopyToOutputStream(&reader);
  if(status != PDFHummus::eSuccess) {
    delete imageStream;
    return false;
  }
  objectsContext.EndPDFStream(imageStream); // It does EndIndirectObject();
  delete imageStream;

  objectsContext.StartNewIndirectObject(smaskId);
  DictionaryContext* smaskContext = objectsContext.StartDictionary();
  smaskContext->WriteKey(scType);
  smaskContext->WriteNameValue(scXObject);
  smaskContext->WriteKey(scSubType);
  smaskContext->WriteNameValue(scImage);
  smaskContext->WriteKey(scWidth);
  smaskContext->WriteIntegerValue(iw);
  smaskContext->WriteKey(scHeight);
  smaskContext->WriteIntegerValue(ih);
  smaskContext->WriteKey(scBitsPerComponent);
  smaskContext->WriteIntegerValue(8);
  smaskContext->WriteKey(scColorSpace);
  smaskContext->WriteNameValue(scDeviceGray);
  PDFStream* smaskStream = objectsContext.StartPDFStream(smaskContext, true);
  OutputStreamTraits smaskOutputTraits (smaskStream->GetWriteStream());
  c_string buf_smask (smask);
  InputByteArrayStream smaskReader ((IOBasicTypes::Byte*)(char *)buf_smask, N(smask));
  status = smaskOutputTraits.CopyToOutputStream(&smaskReader);
  if(status != PDFHummus::eSuccess) {
    delete smaskStream;
    return false;
  }
  objectsContext.EndPDFStream(smaskStream); // It does EndIndirectObject();
  delete smaskStream;
  return true;
}

bool
pdf_image_rep::flush_jpg (PDFWriter& pdfw, url image) {
  c_string f (concretize (image));
  DocumentContext& documentContext= pdfw.GetDocumentContext();
  PDFImageXObject* imageXObject= documentContext.CreateImageXObjectFromJPGFile ((char*) f);
  if ((void*) imageXObject == NULL) { 
    convert_error << "pdf_hummus, failed to include JPG file " << image << LF;
    return false;
  }
  PDFFormXObject* xobjectForm = pdfw.StartFormXObject(PDFRectangle(0, 0, w, h), id); 
  XObjectContentContext* xobjectContentContext = xobjectForm->GetContentContext();
  xobjectContentContext->q();
  xobjectContentContext->cm(w, 0, 0, h, 0, 0);
  std::string pdfImageName = xobjectForm->GetResourcesDictionary().AddImageXObjectMapping(imageXObject);
  xobjectContentContext->Do(pdfImageName);
  xobjectContentContext->Q();
  EStatusCode status = pdfw.EndFormXObjectAndRelease(xobjectForm);
  delete imageXObject;
  return status == eSuccess;
}

#ifndef PDFHUMMUS_NO_PNG
bool
pdf_image_rep::flush_png (PDFWriter& pdfw, url image) {
  c_string f (concretize (image));
  PNGImageHandler pngHandler;
  InputFile file;
  if (file.OpenFile (std::string ((char*) f)) != PDFHummus::eSuccess) {
    convert_error << "pdf_hummus, failed to include PNG file " << image << LF;
    return false;
  }
  IByteReaderWithPosition* stream= file.GetInputStream ();
  DoubleAndDoublePair dim= pngHandler.ReadImageDimensions (stream);
  double iw= dim.first, ih= dim.second;

  stream->SetPosition (0);
  PDFFormXObject* formXObject= pdfw.CreateFormXObjectFromPNGStream (stream);
  if ((void*) formXObject == NULL) { 
    convert_error << "pdf_hummus, failed to create Form from PNG stream" << LF;
    return false;
  }
  PDFFormXObject* xobjectForm= pdfw.StartFormXObject (PDFRectangle (0, 0, w, h), id); 
  XObjectContentContext* xobjectContentContext = xobjectForm->GetContentContext ();
  xobjectContentContext->q ();
  xobjectContentContext->cm (w / iw, 0, 0, h / ih, 0, 0);
  std::string pdfImageName = xobjectForm->GetResourcesDictionary ()
    .AddFormXObjectMapping (formXObject->GetObjectID ());
  xobjectContentContext->Do (pdfImageName);
  xobjectContentContext->Q ();
  EStatusCode status = pdfw.EndFormXObjectAndRelease (xobjectForm);
  delete formXObject;
  return status == eSuccess;
}
#endif

void
pdf_hummus_renderer_rep::flush_images ()
{
  // flush all images
  iterator<tree> it = iterate (image_pool);
  while (it->busy()) {
    pdf_image im = image_pool[it->next()];
    im->flush(pdfWriter);
  }
}

void
pdf_hummus_renderer_rep::flush_patterns ()
{
  // flush all pattern images
  iterator<tree> it = iterate (pattern_image_pool);
  while (it->busy()) {
    pdf_image im = pattern_image_pool[it->next()];
    im->flush_for_pattern(pdfWriter);
  }
  // flush all patterns
  it = iterate (pattern_pool);
  while (it->busy()) {
    pdf_pattern pa = pattern_pool[it->next()];
    pa->flush(pdfWriter);
  }
}

void
pdf_hummus_renderer_rep::image (
  url u, double w, double h, SI x, SI y, int alpha)
{
  // debug_convert << "pdf renderer, image " << u << ", " << w << " x " << h
  //		<< " + (" << x << ", " << y << ")" << LF;
  tree lookup= tuple (u->t);
  pdf_image im = ( image_pool->contains(lookup) ? image_pool[lookup] : pdf_image() );
  
  if (is_nil(im)) {
    im = pdf_image(u, pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry().AllocateNewObjectID());
    image_pool(lookup) = im;
  }

  if (is_nil(im)) return;
  
  end_text();
  
  contentContext->q();
  std::string initial_GState_name = page->GetResourcesDictionary()
    .AddExtGStateMapping(initial_GState_id);
  contentContext->gs(initial_GState_name);
  contentContext->cm (((double)w) / ((double)im->w), 0,
		      0, ((double)h) / ((double)im->h),
		      to_x (x), to_y (y));
  std::string pdfFormName = page->GetResourcesDictionary().AddFormXObjectMapping(im->id);
  select_alpha((1000 * alpha) / 255);
  contentContext->Do(pdfFormName);
  //contentContext->re(0,0,im->w,im->h);
  //contentContext->S();
  contentContext->Q();
}

/******************************************************************************
* Applying effects to existing pictures
******************************************************************************/

#ifdef QTTEXMACS
static picture
pdf_raster_picture (picture pic) {
  picture ret= qt_picture (QImage (pic->get_width (), pic->get_height (),
                                   QImage::Format_ARGB32),
                           pic->get_origin_x (), pic->get_origin_y ());
  ret->copy_from (pic);
  return ret;
}
#endif

void
pdf_hummus_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  // debug_convert << "pdf renderer, draw_picture " << x << ", " << y
  //		<< " (" << alpha << ")" << LF;
  url temp;
  unsigned long long int key= p->get_unique_id ();
  if (picture_cache->contains (key)) temp= picture_cache[key];
  else {
    // As an improvement one could handle native pictures without conversions
#ifdef QTTEXMACS
    picture q= pdf_raster_picture (p);
    qt_picture_rep* pict= (qt_picture_rep*) q->get_handle ();
    temp= url_temp (".png");
    pict->pict.save (utf8_to_qstring (concretize (temp)), "PNG");
    temp_images << temp;	
#else
    convert_error << "pdf renderer, draw_picture: "
      << "cannot export picture " << p->get_name() << LF;
#endif
    picture_cache (key)= temp;
  }
  int _pixel= (int) (PIXEL / PICTURE_ZOOM);
  int w= p->get_width (), h= p->get_height ();
  int ox= p->get_origin_x (), oy= p->get_origin_y ();
  image (temp, w, h, x - ox * _pixel, y - oy * _pixel, alpha);
}

void
pdf_hummus_renderer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  // debug_convert << "pdf renderer, draw_scalable "
  //   << im->get_name () << " at " << x << ", " << y
  //   << " (" << alpha << ")" << LF;
  if (im->get_type () != scalable_image ||
      (im->get_type () == scalable_image && im->get_effect () != tree ("")))
    renderer_rep::draw_scalable (im, x, y, alpha);
  else {
    url u= im->get_name ();
    rectangle r= im->get_logical_extents ();
    SI w= r->x2 - r->x1, h= r->y2 - r->y1;
    int ox= r->x1, oy= r->y1;
    image (u, ((double) w) / pixel, ((double) h) / pixel,
	   x - ox, y - oy, alpha);
  }
}

/******************************************************************************
* Conversions between strings and std::string
******************************************************************************/

string
std_string_to_string (std::string str) {
  string r;
  for (std::string::iterator it=str.begin(); it!=str.end(); ++it) {
    r << *it;
  }
  return r;
}

// At least in outlines a single paren will break the PDF and no more
// outlines will show up after it. Does it need this for other PDF
// strings?
//
// Jim King, Document Management — Portable Document Format — Part 1:
// PDF 1.7 (Adobe Sys. Inc., First ed. 2008),
// §7.3.4.2, Literal Strings, Table 3, Escape sequences in literal strings
//
// Fixme? This does not handle octal character sequences \nnn. Does it
// ever need to?
//
string
escape_string (string s) {
    int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '\n':
      r << '\\' << 'n';
      break;
    case '\r':
      r << '\\' << 'r';
      break;
    case '\t':
      r << '\\' << 't';
      break;
    case '\b':
      r << '\\' << 'b';
      break;
    case '\f':
      r << '\\' << 'f';
      break;
    case '(':
    case ')':
    case '\\':
      r << '\\' << s[i];
      break;
    default:
      r << s[i];
    }
  return r;
}

static PDFTextString
utf8_as_hummus_string (string s) {
  c_string u (s);
  PDFTextString r;
  std::string stds ((char*) u);
  return r.FromUTF8 (stds);
}

static PDFTextString
as_hummus_string (string s) {
  return utf8_as_hummus_string(cork_to_utf8 (s));
}

static string
prepare_text (string s) {
  std::string r= as_hummus_string(s).ToString();
  return std_string_to_string (r);
}

/******************************************************************************
 * hyperlinks
 ******************************************************************************/

PDFHummus::EStatusCode
pdf_hummus_renderer_rep::on_catalog_write (CatalogInformation* inCatalogInformation,
                                         DictionaryContext* inCatalogDictionaryContext,
                                         ObjectsContext* inPDFWriterObjectContext,
                                         PDFHummus::DocumentContext* inDocumentContext)
{
  (void) inCatalogInformation;
  (void) inPDFWriterObjectContext;
  (void) inDocumentContext;
  if (destId) {
    inCatalogDictionaryContext->WriteKey("Dests");
    inCatalogDictionaryContext->WriteNewObjectReferenceValue(destId);
  }
  if (outlineId) {
    inCatalogDictionaryContext->WriteKey("Outlines");
    inCatalogDictionaryContext->WriteNewObjectReferenceValue(outlineId);
  }
  return PDFHummus::eSuccess;
}

int pdf_hummus_renderer_rep::get_label_id(string label)
{
  if (!(label_id->contains(label))) {
    label_id(label) = label_count;
    label_count++;
  }
  return label_id(label);
}

void
pdf_hummus_renderer_rep::anchor (string label, SI x1, SI y1, SI x2, SI y2)
{
  (void) x2; (void) y1;
  string l = prepare_text (label);
  dests << dest_data(l, page_num, to_x(x1), to_y(y2+20*pixel));
}

void
pdf_hummus_renderer_rep::href (string label, SI x1, SI y1, SI x2, SI y2)
{
  bool preserve= (get_locus_rendering ("locus-on-paper") == "preserve");
  ObjectIDType annotId = pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry().AllocateNewObjectID();
  pdfWriter.GetDocumentContext().RegisterAnnotationReferenceForNextPageWrite(annotId);
  string dict;
  dict << "<<\r\n\t/Type /Annot\r\n\t/Subtype /Link\r\n";
//  dict << "\t/Border [1.92 1.92 0.12[]]\r\n\t/Color [0.75 0.5 1.0]\r\n";
  if (preserve)
    dict << "\t/Border [16 16 1 [3 10]] /Color [0.75 0.5 1.0]\r\n";
  else
    dict << "\t/Border [16 16 0 [3 10]] /Color [0.75 0.5 1.0]\r\n";
  dict << "\t/Rect [" << as_string(((double)default_dpi / dpi)*to_x(x1 - 5*pixel)) << " ";
  dict << as_string(((double)default_dpi / dpi)*to_y(y1 - 10*pixel)) << " ";
  dict << as_string(((double)default_dpi / dpi)*to_x(x2 + 5*pixel)) << " ";
  dict << as_string(((double)default_dpi / dpi)*to_y(y2 + 10*pixel)) << "]\r\n";
  if (starts (label, "#")) {
    dict << "\t/Dest /label" << as_string(get_label_id(prepare_text (label))) << "\r\n";
  }
  else {
    dict << "/A << /S /URI /URI (" << prepare_text (label) << ") >>\r\n";
  }
  dict << ">>\r\n";
  annot_list (annotId) = dict;
}


void
pdf_hummus_renderer_rep::flush_dests()
{
  if (is_nil(dests)) return;

  // flush destinations
  
  string dict;
  dict << "<<\r\n";
  list<dest_data> it = dests;
  while (!is_nil(it)) {
    string label = it->item.x1;
    int dest_page = it->item.x2;
    int dest_x = it->item.x3;
    int dest_y = it->item.x4;
    {
      dict << "\t\t/label" << as_string(get_label_id(label)) << " [ " << as_string(page_id(dest_page)) << " 0 R /XYZ "
           << as_string(((double)default_dpi / dpi)*dest_x) << " " << as_string(((double)default_dpi / dpi)*dest_y) << " null ]\r\n";
    }
    it = it->next;
  }
  dict << ">>\r\n";
  {
    // flush the buffer
    ObjectsContext& objectsContext = pdfWriter.GetObjectsContext();
    destId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
    write_indirect_obj(objectsContext, destId, dict);
  }
}

void
pdf_hummus_renderer_rep::toc_entry (string kind, string title, SI x, SI y) {
  (void) kind; (void) title; (void) x; (void) y;
  // debug_convert << kind << ", " << title << "\n";
  int ls= 1;
  if (kind == "toc-strong-1") ls= 1;
  if (kind == "toc-strong-2") ls= 2;
  if (kind == "toc-1") ls= 3;
  if (kind == "toc-2") ls= 4;
  if (kind == "toc-3") ls= 5;
  if (kind == "toc-4") ls= 6;
  if (kind == "toc-5") ls= 7;

  // Use escape_string here. Should it be part of prepare_text instead?
  outlines << outline_data(escape_string (title), page_num, to_x(x), to_y(y+20*pixel), ls);
}

void pdf_hummus_renderer_rep::recurse (ObjectsContext& objectsContext, int parent_ls,
				       list<outline_data>& it, ObjectIDType parentId,
				       ObjectIDType& firstId, ObjectIDType& lastId, int &count)
{
  // weave the tangle of forward/backward references
  // are recurse over substructures
  
  ObjectIDType prevId = 0, curId = 0, nextId = 0;
  
  if (!is_nil(it))
    curId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();

  firstId = curId;
  while (curId) {
    ObjectIDType subFirstId = 0, subLastId = 0;
    int subCount = 0;

    outline_data oitem = it->item;
    count++;
    it = it->next;
    
    if (!is_nil(it) && ((it->item).x5 > oitem.x5)) {
      // go down in level
      recurse(objectsContext, oitem.x5, it, curId, subFirstId, subLastId, subCount);
    }
    
    // continue at this level or above
    if (!is_nil(it) && ((it->item).x5 <= oitem.x5) && ((it->item).x5 > parent_ls)) {
      nextId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
    } else {
      // finished this level, go up.
      nextId = 0;
    }
    
    {
      // write current outline item dictionary
      string dict;
      dict << "<<\r\n"
           << "\t/Title " << utf8_to_pdf_hex_string((oitem).x1) << "\r\n"
           << "\t/Parent " << as_string(parentId) << " 0 R \r\n";
      if (prevId != 0) dict << "\t/Prev " << as_string(prevId) << " 0 R \r\n";
      if (nextId  != 0)  dict << "\t/Next " << as_string(nextId) << " 0 R \r\n";
      if (subCount > 0) {
        dict << "\t/First " << as_string(subFirstId) << " 0 R \r\n"
             << "\t/Last " << as_string(subLastId) << " 0 R \r\n"
             << "\t/Count " << as_string(-subCount) << "\r\n";
      }
      dict << "\t/Dest [ " << as_string(page_id((oitem).x2)) << " 0 R /XYZ "
           << as_string(((double)default_dpi / dpi)*((oitem).x3))
           << " " << as_string(((double)default_dpi / dpi)*((oitem).x4)) << " null ]\r\n"
           << ">>\r\n";
      write_indirect_obj(objectsContext, curId, dict);
    }
    prevId = curId; curId = nextId;
  }
  lastId = prevId;
}

void
pdf_hummus_renderer_rep::flush_outlines()
{
  if (is_nil(outlines)) return;
 
  ObjectsContext& objectsContext= pdfWriter.GetObjectsContext();
  ObjectIDType firstId= 0 , lastId= 0;
  int count = 0;
  list<outline_data> it = outlines;
  
  outlineId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
  recurse(objectsContext, 0, it, outlineId, firstId, lastId, count);
  {
    // create top-level outline dictionary
    string dict;
    dict << "<<\r\n";
    dict << "\t/Type /Outlines\r\n";
    dict << "\t/First " << as_string(firstId) << " 0 R \r\n";
    dict << "\t/Last " << as_string(lastId) << " 0 R \r\n";
    dict << "\t/Count " << as_string(count) << "\r\n";
    dict << ">>\r\n";
    write_indirect_obj(objectsContext, outlineId, dict);
  }
}

/******************************************************************************
* shadow rendering is trivial on pdf
******************************************************************************/

void
pdf_hummus_renderer_rep::set_metadata (string kind, string val) {
  metadata (kind)= val;
}

void
pdf_hummus_renderer_rep::flush_metadata () {
  if (N(metadata) == 0) return;
  DocumentContext& documentContext= pdfWriter.GetDocumentContext();
  TrailerInformation& trailerInfo= documentContext.GetTrailerInformation();
  InfoDictionary& info= trailerInfo.GetInfo();
  if (metadata->contains ("title"))
    info.Title= as_hummus_string (metadata ["title"]);
  if (metadata->contains ("author"))
    info.Author= as_hummus_string (metadata ["author"]);
  if (metadata->contains ("subject"))
    info.Subject= as_hummus_string (metadata ["subject"]);
  string creator= "TeXmacs " * string (TEXMACS_VERSION);
  string producer= creator * " + Hummus 4.0";
  info.Creator= utf8_as_hummus_string (creator);
  info.Producer= utf8_as_hummus_string (producer);
  PDFDate date; date.SetToCurrentTime ();
  info.CreationDate= date;
}

/******************************************************************************
* shadow rendering is trivial on pdf
******************************************************************************/

void
pdf_hummus_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
  // debug_convert << "fetch\n";
  (void) x1; (void) y1; (void) x2; (void) y2;
  (void) ren; (void) x; (void) y;
}

void
pdf_hummus_renderer_rep::new_shadow (renderer& ren) {
  // debug_convert << "new_shadow\n";
  (void) ren;
}

void
pdf_hummus_renderer_rep::delete_shadow (renderer& ren) {
  // debug_convert << "delete_shadow\n";
  (void) ren;
}

void
pdf_hummus_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // debug_convert << "get_shadow\n";
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
pdf_hummus_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // debug_convert << "put_shadow\n";
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
pdf_hummus_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  // debug_convert << "apply_shadow\n";
   (void) x1; (void) y1; (void) x2; (void) y2;
}

renderer
pdf_hummus_renderer_rep::shadow (picture& pic, SI x1, SI y1, SI x2, SI y2) {
  double old_zoomf= this->zoomf;
  set_zoom_factor (5.0 * PICTURE_ZOOM);
  renderer ren= renderer_rep::shadow (pic, x1, y1, x2, y2);
  set_zoom_factor (old_zoomf);
  return ren;
}


/******************************************************************************
* user interface
******************************************************************************/

renderer
pdf_hummus_renderer (url pdf_file_name, int dpi, int nr_pages,
                     string page_type, bool landscape, double paper_w, double paper_h)
{
  //cout << "Hummus print to " << pdf_file_name << " at " << dpi << " dpi\n";
  page_type= as_string (call ("standard-paper-size", object (page_type)));
  return tm_new<pdf_hummus_renderer_rep> (pdf_file_name, dpi, nr_pages,
			  page_type, landscape, paper_w, paper_h);
}
