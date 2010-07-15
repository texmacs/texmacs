/*  $Header: /home/cvsroot/dvipdfmx/src/epdf.c,v 1.30 2009/09/19 18:48:27 matthias Exp $

    This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2007 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team <dvipdfmx@project.ktug.or.kr>
    
    Copyright (C) 1998, 1999 by Mark A. Wicks <mwicks@kettering.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/

/*
 * Concatinating content streams are only supported for streams that only uses
 * single FlateDecode filter, i.e.,
 *
 *   /Filter /FlateDecode or /Filter [/FlateDecode]
 *
 * TrimBox, BleedBox, ArtBox, Rotate ...
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "system.h"
#include "mem.h"
#include "mfileio.h"
#include "error.h"

#include "dvipdfmx.h"

#include "pdfobj.h"
#include "pdfdev.h"
#include "pdfdoc.h"

#include "pdfximage.h"

#include "epdf.h"

int
pdf_include_page (pdf_ximage *ximage, FILE *image_file, const char *filename)
{
  pdf_file *pf;
  xform_info info;
  pdf_obj *contents = NULL, *catalog;
  pdf_obj *page = NULL, *resources = NULL, *markinfo = NULL;

  pf = pdf_open(filename, image_file);
  if (!pf)
    return -1;

  if (pdf_file_get_version(pf) > pdf_get_version())
    goto too_recent;

  pdf_ximage_init_form_info(&info);  

  page = pdf_doc_get_page(pf, pdf_ximage_get_page(ximage), NULL,
			  &info.bbox, &resources);

  if(!page)
    goto error_silent;

  catalog = pdf_file_get_catalog(pf);
  markinfo = pdf_deref_obj(pdf_lookup_dict(catalog, "MarkInfo"));
  if (markinfo) {
    pdf_obj *tmp = pdf_deref_obj(pdf_lookup_dict(markinfo, "Marked"));
    pdf_release_obj(markinfo);
    if (!PDF_OBJ_BOOLEANTYPE(tmp)) {
      if (tmp)
	pdf_release_obj(tmp);
      goto error;
    } else if (pdf_boolean_value(tmp))
      WARN("File contains tagged PDF. Ignoring tags.");
    pdf_release_obj(tmp);
  }

  contents = pdf_deref_obj(pdf_lookup_dict(page, "Contents"));
  pdf_release_obj(page);

  /*
   * Handle page content stream.
   */
  {
    pdf_obj *content_new;

    if (!contents) {
      /*
       * Empty page
       */
      content_new = pdf_new_stream(0);
      /* TODO: better don't include anything if the page is empty */
    } else if (PDF_OBJ_STREAMTYPE(contents)) {
      /* 
       * We must import the stream because its dictionary
       * may contain indirect references.
       */
      content_new = pdf_import_object(contents);
    } else if (PDF_OBJ_ARRAYTYPE(contents)) {
      /*
       * Concatenate all content streams.
       */
      int idx, len = pdf_array_length(contents);
      content_new = pdf_new_stream(STREAM_COMPRESS);
      for (idx = 0; idx < len; idx++) {
	pdf_obj *content_seg = pdf_deref_obj(pdf_get_array(contents, idx));
	if (!PDF_OBJ_STREAMTYPE(content_seg) ||
	    pdf_concat_stream(content_new, content_seg) < 0) {
	  pdf_release_obj(content_seg);
	  pdf_release_obj(content_new);
	  goto error;
	}
	pdf_release_obj(content_seg);
      }
    } else
      goto error;

    if (contents)
      pdf_release_obj(contents);
    contents = content_new;
  }

  /*
   * Add entries to contents stream dictionary.
   */
  {
    pdf_obj *contents_dict, *bbox, *matrix;

    contents_dict = pdf_stream_dict(contents);
    pdf_add_dict(contents_dict,
		 pdf_new_name("Type"), 
		 pdf_new_name("XObject"));
    pdf_add_dict(contents_dict,
		 pdf_new_name("Subtype"),
		 pdf_new_name("Form"));
    pdf_add_dict(contents_dict,
		 pdf_new_name("FormType"),
		 pdf_new_number(1.0));

    bbox = pdf_new_array();
    pdf_add_array(bbox, pdf_new_number(info.bbox.llx));
    pdf_add_array(bbox, pdf_new_number(info.bbox.lly));
    pdf_add_array(bbox, pdf_new_number(info.bbox.urx));
    pdf_add_array(bbox, pdf_new_number(info.bbox.ury));

    pdf_add_dict(contents_dict, pdf_new_name("BBox"), bbox);

    matrix = pdf_new_array();
    pdf_add_array(matrix, pdf_new_number(1.0));
    pdf_add_array(matrix, pdf_new_number(0.0));
    pdf_add_array(matrix, pdf_new_number(0.0));
    pdf_add_array(matrix, pdf_new_number(1.0));
    pdf_add_array(matrix, pdf_new_number(0.0));
    pdf_add_array(matrix, pdf_new_number(0.0));

    pdf_add_dict(contents_dict, pdf_new_name("Matrix"), matrix);

    pdf_add_dict(contents_dict,
		 pdf_new_name("Resources"),
		 pdf_import_object(resources));
    pdf_release_obj(resources);
  }

  pdf_close(pf);

  pdf_ximage_set_form(ximage, &info, contents);

  return 0;

 error:
  WARN("Cannot parse document. Broken PDF file?");
 error_silent:
  if (resources)
    pdf_release_obj(resources);
  if (markinfo)
    pdf_release_obj(markinfo);
  if (page)
    pdf_release_obj(page);
  if (contents)
    pdf_release_obj(contents);

  pdf_close(pf);

  return -1;

 too_recent:
  pdf_close(pf);
  WARN("PDF version of input file more recent than in output file.");
  if (compat_mode) {
    WARN("Converting. Use \"-V\" switch to change output PDF version.");
    return 1;
  } else {
    WARN("Use \"-V\" switch to change output PDF version.");
    return -1;
  }
}
