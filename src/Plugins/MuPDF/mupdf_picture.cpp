
/******************************************************************************
* MODULE     : mupdf_picture.cpp
* DESCRIPTION: Picture objects for MuPDF
* COPYRIGHT  : (C) 2022 Massimiliano Gubinelli, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mupdf_picture.hpp"

#include "file.hpp"
#include "image_files.hpp"
#include "effect.hpp"


/******************************************************************************
* Abstract mupdf pictures
******************************************************************************/

mupdf_picture_rep::mupdf_picture_rep (fz_pixmap *_pix, int ox2, int oy2):
  pix (_pix), im (NULL),
  w (fz_pixmap_width (mupdf_context (), pix)),
  h (fz_pixmap_height (mupdf_context (), pix)),
  ox (ox2), oy (oy2) {
  fz_keep_pixmap (mupdf_context (), pix);
}

mupdf_picture_rep::~mupdf_picture_rep () {
  fz_drop_pixmap (mupdf_context (), pix);
  fz_drop_image (mupdf_context (), im);
}

picture_kind mupdf_picture_rep::get_type () { return picture_native; }
void* mupdf_picture_rep::get_handle () { return (void*) this; }

int mupdf_picture_rep::get_width () { return w; }
int mupdf_picture_rep::get_height () { return h; }
int mupdf_picture_rep::get_origin_x () { return ox; }
int mupdf_picture_rep::get_origin_y () { return oy; }
void mupdf_picture_rep::set_origin (int ox2, int oy2) { ox= ox2; oy= oy2; }

color
mupdf_picture_rep::internal_get_pixel (int x, int y) {
  unsigned char *samples= fz_pixmap_samples (mupdf_context (), pix);
  return  rgbap_to_argb (((color*)samples)[x+w*(h-1-y)]);
}

void
mupdf_picture_rep::internal_set_pixel (int x, int y, color c) {
  unsigned char *samples= fz_pixmap_samples (mupdf_context (), pix);
  ((color*)samples)[x+w*(h-1-y)]= argb_to_rgbap (c);
}

picture
mupdf_picture (fz_pixmap *_pix, int ox, int oy) {
  return (picture)tm_new<mupdf_picture_rep, fz_pixmap*,int,int> (_pix, ox, oy);
}

picture
as_mupdf_picture (picture pic) {
  if (pic->get_type () == picture_native) return pic;
  fz_pixmap *pix= fz_new_pixmap (mupdf_context (),
                                 fz_device_rgb (mupdf_context ()),
                                 pic->get_width (), pic->get_height (),
                                 NULL, 1);
  picture ret= mupdf_picture (pix, pic->get_origin_x (), pic->get_origin_y ());
  fz_drop_pixmap (mupdf_context (), pix);
  ret->copy_from (pic); // FIXME: is this inefficient???
  return ret;
}

#ifdef MUPDF_RENDERER
picture
as_native_picture (picture pict) {
  return as_mupdf_picture (pict);
}

picture
native_picture (int w, int h, int ox, int oy) {
  fz_pixmap *pix= fz_new_pixmap (mupdf_context (),
                                 fz_device_rgb (mupdf_context ()),
                                 w, h, NULL, 1);
  fz_clear_pixmap (mupdf_context (), pix);
  picture p= mupdf_picture (pix, ox, oy);
  fz_drop_pixmap (mupdf_context (), pix);
  return p;
}
#endif

/******************************************************************************
* Rendering on images
******************************************************************************/

class mupdf_image_renderer_rep: public mupdf_renderer_rep {
public:
  picture pict;
  
public:
  mupdf_image_renderer_rep (picture pict, double zoom);
  void* get_data_handle ();
};

mupdf_image_renderer_rep::mupdf_image_renderer_rep (picture p, double zoom)
  : mupdf_renderer_rep (), pict (p)
{
  zoomf  = zoom;
  shrinkf= (int) tm_round (std_shrinkf / zoomf);
  pixel  = (SI)  tm_round ((std_shrinkf * PIXEL) / zoomf);
  thicken= (shrinkf >> 1) * PIXEL;

  int pw = p->get_width ();
  int ph = p->get_height ();
  int pox= p->get_origin_x ();
  int poy= p->get_origin_y ();

  ox = pox * pixel;
  oy = poy * pixel;
  /*
  cx1= 0;
  cy1= 0;
  cx2= pw * pixel;
  cy2= ph * pixel;
  */
  cx1= 0;
  cy1= -ph * pixel;
  cx2= pw * pixel;
  cy2= 0;

  mupdf_picture_rep* handle= (mupdf_picture_rep*) pict->get_handle ();
  begin (handle->pix);
}

void*
mupdf_image_renderer_rep::get_data_handle () {
  return (void*) this;
}

#ifdef MUPDF_RENDERER
renderer
picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<mupdf_image_renderer_rep> (p, zoomf);
}
#endif

/******************************************************************************
* Loading pictures
******************************************************************************/

fz_image *
mupdf_load_image (url u) {
  fz_image *im = NULL;
  string suf= suffix (u);
  if (suf == "svg") {
      // FIXME: implement!
  #if 0
      QSvgRenderer renderer (utf8_to_qstring (concretize (u)));
      pm= new QImage (w, h, QImage::Format_ARGB32);
      pm->fill (Qt::transparent);
      QPainter painter (pm);
      renderer.render (&painter);
  #endif
    } else if ((suf == "jpg") || (suf == "png")) {
      // FIXME: add more supported formats
      c_string path (concretize (u));
      im= fz_new_image_from_file (mupdf_context (), path);
    } else if (suf == "xpm") {
      // try to load higher definition png equivalent if available
      url png_equiv= glue (unglue (u, 4), "_x4.png");
      if (exists(png_equiv)) {
        return mupdf_load_image (png_equiv);
      }
      png_equiv= glue (unglue (u, 4), "_x2.png");
      if (exists(png_equiv)) {
        return mupdf_load_image (png_equiv);
      }
      png_equiv= glue (unglue (u, 4), ".png");
      if (exists(png_equiv)) {
        return mupdf_load_image (png_equiv);
      }
      // ok, try to load the xpm finally
      picture xp= as_mupdf_picture (load_xpm (u));
      fz_pixmap *pix= ((mupdf_picture_rep*)xp->get_handle())->pix;
      im= fz_new_image_from_pixmap (mupdf_context(), pix, NULL);
    }
  return im;
}

fz_pixmap*
mupdf_load_pixmap (url u, int w, int h, tree eff, SI pixel) {
  fz_image *im = mupdf_load_image (u);

  if (im == NULL) {
    // attempt to convert to png
    url temp= url_temp (".png");
    image_to_png (u, temp, w, h);
    c_string path (as_string (temp));
    im= fz_new_image_from_file (mupdf_context (), path);
    remove (temp);
  }
  
  // Error Handling
  if (im == NULL) {
      cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
      return NULL;
  }

  // Scaling
  if (im->w != w || im->h != h) {
    // FIXME: implement?
    // we opt to draw natively scalables, so here we do not support rescaling
    // (*pm)= pm->scaled (w, h, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
    cout <<  "TeXmacs] warning: image rescaling not supported "
         << concretize (u) << "\n";
  }

  fz_pixmap *pix= fz_get_pixmap_from_image (mupdf_context (), im,
                                            NULL, NULL, NULL, NULL);
  fz_drop_image (mupdf_context (), im); // we do not need it anymore

  // Build effect
  if (eff != "") {
    effect e= build_effect (eff);
    picture src= mupdf_picture (pix, 0, 0);
    array<picture> a;
    a << src;
    picture pic= e->apply (a, pixel);
    picture dest= as_mupdf_picture (pic);
    mupdf_picture_rep* rep= (mupdf_picture_rep*) dest->get_handle ();
    fz_pixmap* tpix= rep->pix;
    fz_drop_pixmap (mupdf_context (), pix);
    pix= tpix;
  }
  return pix;
}

#ifdef MUPDF_RENDERER
picture
load_picture (url u, int w, int h, tree eff, int pixel) {
  fz_pixmap* pix= mupdf_load_pixmap (u, w, h, eff, pixel);
  if (pix == NULL) return error_picture (w, h);
  picture p= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context(), pix);
  return p;
}

void
save_picture (url dest, picture p) {
  if (suffix(dest) != "png") {
    cout << "TeXmacs] warning: cannot save " << concretize (dest)
         << ", format not supported\n";
    return;
  }
  picture q= as_mupdf_picture (p);
  mupdf_picture_rep* pict= (mupdf_picture_rep*) q->get_handle ();
  if (exists (dest)) remove (dest);
  c_string path= concretize (dest);
  fz_output *out= fz_new_output_with_path (mupdf_context (), path, 0);
  fz_write_pixmap_as_png (mupdf_context (), out, pict->pix);
  fz_close_output (mupdf_context (), out);
  fz_drop_output (mupdf_context (), out);
}
#endif
