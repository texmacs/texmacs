/******************************************************************************
* MODULE     : resvg.cpp
* DESCRIPTION: Interface with resvg
* COPYRIGHT  : (C) 2026  Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_configure.hpp"
#ifdef USE_RESVG

#include "Resvg/resvg.hpp"
#include "file.hpp"
#include <resvg.h>
#include <cmath>
#include <cstring>
#include <algorithm>

static const char*
resvg_error_string (int err) {
  switch (err) {
    case RESVG_OK: return "OK";
    case RESVG_ERROR_NOT_AN_UTF8_STR: return "Not an UTF-8 string";
    case RESVG_ERROR_SVGZ_UNSUPPORTED: return "SVGZ unsupported";
    case RESVG_ERROR_FILE_OPEN_FAILED: return "File open failed";
    case RESVG_ERROR_MALFORMED_GZIP: return "Malformed GZip";
    case RESVG_ERROR_ELEMENTS_LIMIT_REACHED: return "Elements limit reached";
    case RESVG_ERROR_INVALID_SIZE: return "Invalid SVG size";
    case RESVG_ERROR_PARSING_FAILED: return "SVG parsing failed";
    default: return "Unknown error";
  }
}

resvg_options*
resvg_get_options () {
  static resvg_options* opt = NULL;
  if (opt == NULL) {
    resvg_init_log ();
    opt = resvg_options_create ();
    resvg_options_load_system_fonts (opt);
  }
  return opt;
}

int
resvg_parse_tree (url u, const resvg_options* opt, resvg_render_tree** tree) {
  if (opt == NULL) {
    opt = resvg_get_options ();
  }
  string data;
  bool err_load = load_string (u, data, false);
  if (err_load || N (data) == 0) {
    string path = concretize (u);
    if (path == "") path = materialize (u);
    if (path != "") {
      FILE* f = texmacs_fopen (path, "rb");
      if (f != NULL) {
        ssize_t sz = texmacs_fsize (f);
        if (sz > 0) {
          data = string ((int) sz);
          size_t rd = fread (&data[0], 1, sz, f);
          if (rd != (size_t) sz) data = "";
        }
        fclose (f);
      }
    }
  }
  if (N (data) == 0) {
    std_warning << "resvg warning: failed to open/read SVG file '" << u << "'" << LF;
    return RESVG_ERROR_FILE_OPEN_FAILED;
  }
  resvg_render_tree* local_tree = NULL;
  resvg_render_tree** out_tree = tree ? tree : &local_tree;
  int err = resvg_parse_tree_from_data (&data[0],
                                        (uintptr_t) N (data),
                                        opt, out_tree);
  if (tree == NULL && local_tree != NULL) {
    resvg_tree_destroy (local_tree);
  }
  if (err != RESVG_OK) {
    std_warning << "resvg warning: failed to parse SVG '" << u
                << "' (" << resvg_error_string (err) << ", code " << err << ")" << LF;
  }
  return err;
}

bool
resvg_supports (url image) {
  string suf = suffix (image);
  return suf == "svg" || suf == "svgz";
}

void
resvg_image_size (url image, int& w_pt, int& h_pt) {
  resvg_render_tree *tree = NULL;
  int err = resvg_parse_tree (image, NULL, &tree);
  if (err == RESVG_OK && tree != NULL) {
    resvg_size sz = resvg_get_image_size (tree);
    w_pt = (int) rint (sz.width * 72.0 / 96.0);
    h_pt = (int) rint (sz.height * 72.0 / 96.0);
    resvg_tree_destroy (tree);
  } else {
    w_pt = 0;
    h_pt = 0;
  }
}

bool
resvg_native_image_size (url image, int& w, int& h) {
  resvg_render_tree *tree = NULL;
  int err = resvg_parse_tree (image, NULL, &tree);
  if (err == RESVG_OK && tree != NULL) {
    resvg_size sz = resvg_get_image_size (tree);
    w = (int) ceil (sz.width);
    h = (int) ceil (sz.height);
    resvg_tree_destroy (tree);
    return true;
  }
  return false;
}

bool
resvg_do_render_tree (resvg_render_tree* tree, int w, int h, char* rgba_pixels, bool fit_aspect) {
  if (tree == NULL || w <= 0 || h <= 0 || rgba_pixels == NULL) return false;
  resvg_size sz = resvg_get_image_size (tree);
  resvg_transform tr = resvg_transform_identity ();
  if (sz.width > 0 && sz.height > 0) {
    if (fit_aspect) {
      double scale = std::min ((double) w / sz.width, (double) h / sz.height);
      tr.a = scale;
      tr.d = scale;
      tr.e = (w - sz.width * scale) / 2.0;
      tr.f = (h - sz.height * scale) / 2.0;
    } else {
      tr.a = (float) w / sz.width;
      tr.d = (float) h / sz.height;
    }
  }
  resvg_render (tree, tr, w, h, rgba_pixels);
  return true;
}

bool
resvg_render_image (url image, int w, int h, char* rgba_pixels, bool fit_aspect) {
  resvg_render_tree *tree = NULL;
  int err = resvg_parse_tree (image, NULL, &tree);
  if (err != RESVG_OK || tree == NULL) return false;
  bool ok = resvg_do_render_tree (tree, w, h, rgba_pixels, fit_aspect);
  resvg_tree_destroy (tree);
  return ok;
}

void
resvg_destroy_tree (resvg_render_tree* tree) {
  if (tree != NULL) {
    resvg_tree_destroy (tree);
  }
}

#endif // USE_RESVG
