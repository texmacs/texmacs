/*  $Header: /home/cvsroot/dvipdfmx/src/jpegimage.c,v 1.11 2009/05/10 17:04:54 matthias Exp $

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


#if HAVE_CONFIG_H
#include "config.h"
#endif

/*
 * JPEG SUPPORT
 *
 * Accroding to Libjpeg document:
 *
 *  CAUTION: it appears that Adobe Photoshop writes inverted data in CMYK
 *  JPEG files: 0 represents 100% ink coverage, rather than 0% ink as you'd
 *  expect....
 *
 * To wrok with this problem, we must detect whether CMYK JPEG file is
 * created by Photoshop. But there are no reliable way to determine this.
 *
 * According to Adobe Technical Note #5516,
 * "Supporting the DCT Filters in PostScript Level 2", Section 18, p.27.
 *
 *  DCTDecode ignores and skips any APPE marker segment does not begin with
 *  the `Adobe' 5-character string.
 *
 * PDF Reference Manual 4th ed., p.61-62.
 *
 *  The JPEG filter implementation in Adobe Acrobat products does not
 *  support features of the JPEG standard that are irrelevant to images.
 *  In addition, certain choices have been made regarding reserved marker
 *  codes and other optional features of the standard. For details, see
 *  Adobe Technical Note #5116, Supporting the DCT Filters in PostScript
 *  Level 2.
 */

#include "system.h"
#include "error.h"
#include "mem.h"

#include "mfileio.h"
#include "numbers.h"

#include "dvipdfmx.h"

#include "pdfobj.h"

#include "jpegimage.h"
#include "pdfcolor.h"

#include "pdfximage.h"

#define JPEG_DEBUG_STR "JPEG"
#define JPEG_DEBUG     3

#ifdef    HAVE_LIBJPEG
#include <jpeglib.h>
#endif /* HAVE_LIBJPEG */

/* JPEG Markers */
typedef enum {
  JM_SOF0  = 0xc0,
  JM_SOF1  = 0xc1,
  JM_SOF2  = 0xc2,
  JM_SOF3  = 0xc3,
  JM_SOF5  = 0xc5,
  JM_DHT   = 0xc4,
  JM_SOF6  = 0xc6,
  JM_SOF7  = 0xc7,
  JM_SOF9  = 0xc9,
  JM_SOF10 = 0xca,
  JM_SOF11 = 0xcb,
  JM_DAC   = 0xcc,
  JM_SOF13 = 0xcd,
  JM_SOF14 = 0xce,
  JM_SOF15 = 0xcf,

  JM_RST0  = 0xd0,
  JM_RST1  = 0xd1,
  JM_RST2  = 0xd2,
  JM_RST3  = 0xd3,
  JM_RST4  = 0xd4,
  JM_RST5  = 0xd5,
  JM_RST6  = 0xd6,
  JM_RST7  = 0xd7,

  JM_SOI   = 0xd8,
  JM_EOI   = 0xd9,
  JM_SOS   = 0xda,
  JM_DQT   = 0xdb,
  JM_DNL   = 0xdc,
  JM_DRI   = 0xdd,
  JM_DHP   = 0xde,
  JM_EXP   = 0xdf,

  JM_APP0  = 0xe0,
  JM_APP2  = 0xe2,
  JM_APP14 = 0xee,
  JM_APP15 = 0xef,

  JM_COM   = 0xfe
} JPEG_marker;

typedef enum {
  JS_APPn_JFIF,
  JS_APPn_ADOBE,
  JS_APPn_ICC
} JPEG_APPn_sig;

struct JPEG_APPn_JFIF  /* APP0 */
{
  unsigned short version;
  unsigned char  units;      /* 0: only aspect ratio
			      * 1: dots per inch
			      * 2: dots per cm
			      */
  unsigned short Xdensity;
  unsigned short Ydensity;
  unsigned char  Xthumbnail;
  unsigned char  Ythumbnail;
  unsigned char *thumbnail;  /* Thumbnail data. */
};

struct JPEG_APPn_ICC   /* APP2 */
{
  unsigned char  seq_id;
  unsigned char  num_chunks;
  unsigned char *chunk;

  /* Length of ICC profile data in this chunk. */
  unsigned short length;
};

struct JPEG_APPn_Adobe /* APP14 */
{
  unsigned short version;
  unsigned short flag0;
  unsigned short flag1;
  unsigned char  transform; /* color transform code */
};

struct JPEG_ext
{
  JPEG_marker   marker;
  JPEG_APPn_sig app_sig;
  void         *app_data;
};

#define MAX_COUNT 1024
struct  JPEG_info
{
  unsigned short height;
  unsigned short width;

  unsigned char  bits_per_component;
  unsigned char  num_components;

  /* Application specific extensions */
  int flags;
  int num_appn, max_appn;
  struct JPEG_ext *appn;

  /* Skip chunks not necessary. */
  char skipbits[MAX_COUNT / 8 + 1];
};

#define HAVE_APPn_JFIF  (1 << 0)
#define HAVE_APPn_ADOBE (1 << 1)
#define HAVE_APPn_ICC   (1 << 2)

static int      JPEG_scan_file   (struct JPEG_info *j_info, FILE *fp);
static int      JPEG_copy_stream (struct JPEG_info *j_info,
				  pdf_obj *stream, FILE *fp, int flags); /* flags unused yet */

static void     JPEG_info_init   (struct JPEG_info *j_info);
static void     JPEG_info_clear  (struct JPEG_info *j_info);
static pdf_obj *JPEG_get_iccp    (struct JPEG_info *j_info);
static void     jpeg_get_density (struct JPEG_info *j_info,
				  double *xdensity, double *ydensity);

int
check_for_jpeg (FILE *fp)
{
  unsigned char jpeg_sig[2];

  rewind(fp);
  if (fread(jpeg_sig, sizeof(unsigned char), 2, fp) != 2)
    return 0;
  else if (jpeg_sig[0] != 0xff || jpeg_sig[1] != JM_SOI)
    return 0;

  return 1;
}

int
jpeg_include_image (pdf_ximage *ximage, FILE *fp)
{
  pdf_obj    *stream;
  pdf_obj    *stream_dict;
  pdf_obj    *colorspace;
  int         colortype;
  ximage_info info;
  struct JPEG_info j_info;

  if (!check_for_jpeg(fp)) {
    WARN("%s: Not a JPEG file?", JPEG_DEBUG_STR);
    rewind(fp);
    return -1;
  }
  /* File position is 2 here... */

  pdf_ximage_init_image_info(&info);

  JPEG_info_init(&j_info);

  if (JPEG_scan_file(&j_info, fp) < 0) {
    WARN("%s: Not a JPEG file?", JPEG_DEBUG_STR);
    JPEG_info_clear(&j_info);
    return -1;
  }

  switch (j_info.num_components) {
  case 1:
    colortype = PDF_COLORSPACE_TYPE_GRAY;
    break;
  case 3:
    colortype = PDF_COLORSPACE_TYPE_RGB;
    break;
  case 4:
    colortype = PDF_COLORSPACE_TYPE_CMYK;
    break;
  default:
    WARN("%s: Unknown color space (num components: %d)",
	 JPEG_DEBUG_STR, info.num_components);
    JPEG_info_clear(&j_info);
    return -1;
  }

  /* JPEG image use DCTDecode. */
  stream      = pdf_new_stream (0);
  stream_dict = pdf_stream_dict(stream);
  pdf_add_dict(stream_dict,
	       pdf_new_name("Filter"), pdf_new_name("DCTDecode"));

  colorspace  = NULL;
  if (j_info.flags & HAVE_APPn_ICC) {
    pdf_obj *icc_stream, *intent;

    icc_stream = JPEG_get_iccp(&j_info);

    if (!icc_stream)
      colorspace = NULL;
    else {
      int   cspc_id;

      if (iccp_check_colorspace(colortype,
				pdf_stream_dataptr(icc_stream),
				pdf_stream_length (icc_stream)) < 0)
	colorspace = NULL;
      else {
	cspc_id = iccp_load_profile(NULL, /* noname */
				    pdf_stream_dataptr(icc_stream),
				    pdf_stream_length (icc_stream));
	if (cspc_id < 0)
	  colorspace = NULL;
	else {
	  colorspace = pdf_get_colorspace_reference(cspc_id);
	  intent     = iccp_get_rendering_intent(pdf_stream_dataptr(icc_stream),
						 pdf_stream_length (icc_stream));
	  if (intent)
	    pdf_add_dict(stream_dict, pdf_new_name("Intent"), intent);
	}
      }
      pdf_release_obj(icc_stream);
    }
  }

  /* No ICC or invalid ICC profile. */
  if (!colorspace) {
    switch (colortype) {
    case PDF_COLORSPACE_TYPE_GRAY:
      colorspace = pdf_new_name("DeviceGray");
      break;
    case PDF_COLORSPACE_TYPE_RGB:
      colorspace = pdf_new_name("DeviceRGB");
      break;
    case PDF_COLORSPACE_TYPE_CMYK:
      colorspace = pdf_new_name("DeviceCMYK");
      break;
    }
  }
  pdf_add_dict(stream_dict, pdf_new_name("ColorSpace"), colorspace);

#define IS_ADOBE_CMYK(j) (((j).flags & HAVE_APPn_ADOBE) && (j).num_components == 4)

  if (IS_ADOBE_CMYK(j_info)) {
    pdf_obj *decode;
    int      i;

    WARN("Adobe CMYK JPEG: Inverted color assumed.");
    decode = pdf_new_array();
    for (i = 0; i < j_info.num_components; i++) {
      pdf_add_array(decode, pdf_new_number(1.0));
      pdf_add_array(decode, pdf_new_number(0.0));
    }
    pdf_add_dict(stream_dict, pdf_new_name("Decode"), decode);
  }

  /* Copy file */
  JPEG_copy_stream(&j_info, stream, fp, 0);

  info.width              = j_info.width;
  info.height             = j_info.height;
  info.bits_per_component = j_info.bits_per_component;
  info.num_components     = j_info.num_components;

  jpeg_get_density(&j_info, &info.xdensity, &info.ydensity);

  pdf_ximage_set_image(ximage, &info, stream);
  JPEG_info_clear(&j_info);

  return 0;
}

#define IS_JFIF(j) ((j)->flags & HAVE_APPn_JFIF)

static void
jpeg_get_density (struct JPEG_info *j_info,
		  double *xdensity, double *ydensity)
{
  if (compat_mode) {
    *xdensity = *ydensity = 72.0 / 100.0;
    return;
  }

  *xdensity = *ydensity = 1.0;

  if (IS_JFIF(j_info)) {
    struct JPEG_APPn_JFIF *app_data;
    int i;
    for (i = 0; i < j_info->num_appn; i++) {
      if (j_info->appn[i].marker  == JM_APP0 ||
	  j_info->appn[i].app_sig == JS_APPn_JFIF)
        break;
    }
    if (i < j_info->num_appn) {
      app_data = (struct JPEG_APPn_JFIF *)j_info->appn[i].app_data;
      switch (app_data->units) {
      case 1: /* pixels per inch */
        *xdensity = 72.0 / app_data->Xdensity;
        *ydensity = 72.0 / app_data->Ydensity;
        break;
      case 2: /* pixels per centimeter */
        *xdensity = 72.0 / 2.54 / app_data->Xdensity;
        *ydensity = 72.0 / 2.54 / app_data->Ydensity;
        break;
      default:
        break;
      }
    }
  }
}

static void
JPEG_info_init (struct JPEG_info *j_info)
{
  j_info->width  = 0;
  j_info->height = 0;
  j_info->bits_per_component = 0;
  j_info->num_components = 0;

  j_info->flags    = 0;
  j_info->num_appn = 0;
  j_info->max_appn = 0;
  j_info->appn     = NULL;

  memset(j_info->skipbits, 0, MAX_COUNT / 8 + 1);
}

static void
JPEG_release_APPn_data (JPEG_marker marker, JPEG_APPn_sig app_sig, void *app_data)
{
  if (marker  == JM_APP0 &&
      app_sig == JS_APPn_JFIF) {
    struct JPEG_APPn_JFIF *data;

    data = (struct JPEG_APPn_JFIF *) app_data;
    if (data->thumbnail)
      RELEASE(data->thumbnail);
    data->thumbnail = NULL;

    RELEASE(data);
  } else if (marker  == JM_APP2 &&
	     app_sig == JS_APPn_ICC) {
    struct JPEG_APPn_ICC *data;

    data = (struct JPEG_APPn_ICC *) app_data;
    if (data->chunk)
      RELEASE(data->chunk);
    data->chunk = NULL;

    RELEASE(data);
  } else if (marker  == JM_APP14 &&
	     app_sig == JS_APPn_ADOBE) {
    struct JPEG_APPn_Adobe *data;

    data = (struct JPEG_APPn_Adobe *) app_data;

    RELEASE(data);
  }
}

static void
JPEG_info_clear (struct JPEG_info *j_info)
{
  if (j_info->num_appn > 0 &&
      j_info->appn    != NULL) {
    int i;

    for (i = 0; i < j_info->num_appn; i++)
      JPEG_release_APPn_data(j_info->appn[i].marker,
			     j_info->appn[i].app_sig, j_info->appn[i].app_data);
    RELEASE(j_info->appn);
  }
  j_info->appn     = NULL;
  j_info->num_appn = 0;
  j_info->max_appn = 0;
  j_info->flags    = 0;
}

static pdf_obj *
JPEG_get_iccp (struct JPEG_info *j_info)
{
  pdf_obj *icc_stream;
  struct JPEG_APPn_ICC *icc;
  int i, prev_id = 0, num_icc_seg = -1;

  icc_stream = pdf_new_stream(STREAM_COMPRESS);
  for (i = 0; i < j_info->num_appn; i++) {
    if (j_info->appn[i].marker  != JM_APP2 ||
	j_info->appn[i].app_sig != JS_APPn_ICC)
      continue;
    icc = (struct JPEG_APPn_ICC *) j_info->appn[i].app_data;
    if (num_icc_seg < 0 && prev_id == 0) {
      num_icc_seg = icc->num_chunks;
      /* ICC chunks are sorted? */
    } else if (icc->seq_id != prev_id + 1 ||
	       num_icc_seg != icc->num_chunks ||
	       icc->seq_id  > icc->num_chunks) {
      WARN("Invalid JPEG ICC chunk: %d (p:%d, n:%d)",
	   icc->seq_id, prev_id, icc->num_chunks);
      pdf_release_obj(icc_stream);
      icc_stream = NULL;
      break;
    }
    pdf_add_stream(icc_stream, icc->chunk, icc->length);
    prev_id = icc->seq_id;
    num_icc_seg = icc->num_chunks;
  }

  return icc_stream;
}

static JPEG_marker
JPEG_get_marker (FILE *fp)
{
  int c;

  c = fgetc(fp);
  if (c != 255)
    return -1;

  for (;;) {
    c = fgetc(fp);
    if (c < 0)
      return -1;
    else if (c > 0 && c < 255) {
      return c;
    }
  }

  return -1;
}

static int
add_APPn_marker (struct JPEG_info *j_info,
		 JPEG_marker marker, int app_sig, void *app_data)
{
  int n;

  if (j_info->num_appn >= j_info->max_appn) {
    j_info->max_appn += 16;
    j_info->appn = RENEW(j_info->appn, j_info->max_appn, struct JPEG_ext);
  }
  n = j_info->num_appn;

  j_info->appn[n].marker   = marker;
  j_info->appn[n].app_sig  = app_sig;
  j_info->appn[n].app_data = app_data;

  j_info->num_appn += 1;

  return n;
}

static unsigned short
read_APP14_Adobe (struct JPEG_info *j_info, FILE *fp, unsigned short length)
{
  struct JPEG_APPn_Adobe *app_data;

  app_data = NEW(1, struct JPEG_APPn_Adobe);
  app_data->version   = get_unsigned_pair(fp);
  app_data->flag0     = get_unsigned_pair(fp);
  app_data->flag1     = get_unsigned_pair(fp);
  app_data->transform = get_unsigned_byte(fp);

  add_APPn_marker(j_info, JM_APP14, JS_APPn_ADOBE, app_data);

  return 7;
}

static unsigned short
read_APP0_JFIF (struct JPEG_info *j_info, FILE *fp, unsigned short length)
{
  struct JPEG_APPn_JFIF *app_data;
  unsigned short thumb_data_len;

  app_data = NEW(1, struct JPEG_APPn_JFIF);
  app_data->version  = get_unsigned_pair(fp);
  app_data->units    = get_unsigned_byte(fp);
  app_data->Xdensity = get_unsigned_pair(fp);
  app_data->Ydensity = get_unsigned_pair(fp);
  app_data->Xthumbnail = get_unsigned_byte(fp);
  app_data->Ythumbnail = get_unsigned_byte(fp);
  thumb_data_len = 3 * app_data->Xthumbnail * app_data->Ythumbnail;
  if (thumb_data_len > 0) {
    app_data->thumbnail = NEW(thumb_data_len, unsigned char);
    fread(app_data->thumbnail, 1, thumb_data_len, fp);
  } else {
    app_data->thumbnail = NULL;
  }

  add_APPn_marker(j_info, JM_APP0, JS_APPn_JFIF, app_data);

  return (9 + thumb_data_len);
}

static unsigned short
read_APP0_JFXX (struct JPEG_info *j_info, FILE *fp, unsigned short length)
{
  unsigned char extension_code;

  extension_code = get_unsigned_byte(fp);
  /* Extension Code:
   *
   * 0x10: Thumbnail coded using JPEG
   * 0x11: Thumbnail stored using 1 byte/pixel
   * 0x13: Thumbnail stored using 3 bytes/pixel
   */
  seek_relative(fp, length-1); /* Thunbnail image */

  /* Ignore */

  return length;
}

static unsigned short
read_APP2_ICC (struct JPEG_info *j_info, FILE *fp, unsigned short length)
{
  struct JPEG_APPn_ICC *app_data;

  app_data = NEW(1, struct JPEG_APPn_ICC);
  app_data->seq_id      = get_unsigned_byte(fp); /* Starting at 1 */
  app_data->num_chunks  = get_unsigned_byte(fp);
  app_data->length = length - 2;
  app_data->chunk  = NEW(app_data->length, unsigned char);
  fread(app_data->chunk, 1, app_data->length, fp);

  add_APPn_marker(j_info, JM_APP2, JS_APPn_ICC, app_data);

  return length;
}

static int
JPEG_copy_stream (struct JPEG_info *j_info,
		  pdf_obj *stream, FILE *fp, int flags) /* flags unused yet */
{
  JPEG_marker marker;
  long length, nb_read;
  int  found_SOFn, count;

  rewind(fp);
  count      = 0;
  found_SOFn = 0;
  while (!found_SOFn &&
	 count < MAX_COUNT &&
	 (marker = JPEG_get_marker(fp)) >= 0) {
    if (marker == JM_SOI  ||
	(marker >= JM_RST0 && marker <= JM_RST7)) {
      work_buffer[0] = (char) 0xff;
      work_buffer[1] = (char) marker;
      pdf_add_stream(stream, work_buffer, 2);
      count++;
      continue;
    }
    length = get_unsigned_pair(fp) - 2;
    switch (marker) {
    case JM_SOF0:  case JM_SOF1:  case JM_SOF2:  case JM_SOF3:
    case JM_SOF5:  case JM_SOF6:  case JM_SOF7:  case JM_SOF9:
    case JM_SOF10: case JM_SOF11: case JM_SOF13: case JM_SOF14:
    case JM_SOF15:
      work_buffer[0] = (char) 0xff;
      work_buffer[1] = (char) marker;
      work_buffer[2] = ((length + 2) >> 8) & 0xff;
      work_buffer[3] =  (length + 2) & 0xff;
      pdf_add_stream(stream, work_buffer, 4);
      while (length > 0) {
	nb_read = fread(work_buffer, sizeof(char),
			MIN(length, WORK_BUFFER_SIZE), fp);
	if (nb_read > 0)
	  pdf_add_stream(stream, work_buffer, nb_read);
	length -= nb_read;
      }
      found_SOFn = 1;
      break;
    default:
      if (j_info->skipbits[count / 8] & (1 << (7 - (count % 8)))) {
	/* skip */
	while (length > 0) {
	  nb_read = fread(work_buffer, sizeof(char),
			  MIN(length, WORK_BUFFER_SIZE), fp);
	  length -= nb_read;
	}
      } else {
	work_buffer[0] = (char) 0xff;
	work_buffer[1] = (char) marker;
	work_buffer[2] = ((length + 2) >> 8) & 0xff;
	work_buffer[3] =  (length + 2) & 0xff;
	pdf_add_stream(stream, work_buffer, 4);
	while (length > 0) {
	  nb_read = fread(work_buffer, sizeof(char),
			  MIN(length, WORK_BUFFER_SIZE), fp);
	  if (nb_read > 0)
	    pdf_add_stream(stream, work_buffer, nb_read);
	  length -= nb_read;
	}
      }
    }
    count++;
  }

  while ((length = fread(work_buffer,
			 sizeof(char), WORK_BUFFER_SIZE, fp)) > 0) {
    pdf_add_stream(stream, work_buffer, length);
  }

  return (found_SOFn ? 0 : -1);
}

static int
JPEG_scan_file (struct JPEG_info *j_info, FILE *fp)
{
  JPEG_marker marker;
  unsigned short length;
  int  found_SOFn, count;
  char app_sig[128];

  rewind(fp);
  count      = 0;
  found_SOFn = 0;
  while (!found_SOFn &&
	 (marker = JPEG_get_marker(fp)) >= 0) {
    if (marker == JM_SOI  ||
	(marker >= JM_RST0 && marker <= JM_RST7)) {
      count++;
      continue;
    }
    length = get_unsigned_pair(fp) - 2;
    switch (marker) {
    case JM_SOF0:  case JM_SOF1:  case JM_SOF2:  case JM_SOF3:
    case JM_SOF5:  case JM_SOF6:  case JM_SOF7:  case JM_SOF9:
    case JM_SOF10: case JM_SOF11: case JM_SOF13: case JM_SOF14:
    case JM_SOF15:
      j_info->bits_per_component = get_unsigned_byte(fp);
      j_info->height = get_unsigned_pair(fp);
      j_info->width  = get_unsigned_pair(fp);
      j_info->num_components = get_unsigned_byte(fp);
      found_SOFn = 1;
      break;
    case JM_APP0:
      if (length > 5) {
	if (fread(app_sig, sizeof(char), 5, fp) != 5)
	  return -1;
	length -= 5;
	if (!memcmp(app_sig, "JFIF\000", 5)) {
	  j_info->flags |= HAVE_APPn_JFIF;
	  length -= read_APP0_JFIF(j_info, fp, length);
	} else if (!memcmp(app_sig, "JFXX", 5)) {
	  length -= read_APP0_JFXX(j_info, fp, length);
	}
      }
      seek_relative(fp, length);
      break;
    case JM_APP2:
      if (length >= 14) {
	if (fread(app_sig, sizeof(char), 12, fp) != 12)
	  return -1;
	length -= 12;
	if (!memcmp(app_sig, "ICC_PROFILE\000", 12)) {
	  j_info->flags |= HAVE_APPn_ICC;
	  length -= read_APP2_ICC(j_info, fp, length);
	  if (count < MAX_COUNT) {
	    j_info->skipbits[count / 8] |= (1 << (7 - (count % 8)));
	  }
	}
      }
      seek_relative(fp, length);
      break;
    case JM_APP14:
      if (length > 5) {
	if (fread(app_sig, sizeof(char), 5, fp) != 5)
	  return -1;
	length -= 5;
	if (!memcmp(app_sig, "Adobe", 5)) {
	  j_info->flags |= HAVE_APPn_ADOBE;
	  length -= read_APP14_Adobe(j_info, fp, length);
	} else {
	  if (count < MAX_COUNT) {
	    j_info->skipbits[count/8] |= (1 << (7 - (count % 8)));
	  }
	}
      }
      seek_relative(fp, length);
      break;
    default:
      seek_relative(fp, length);
      if (marker >= JM_APP0 &&
	  marker <= JM_APP15) {
	if (count < MAX_COUNT) {
	  j_info->skipbits[count / 8] |= (1 << (7 - (count % 8)));
	}
      }
      break;
    }
    count++;
  }

  return (found_SOFn ? 0 : -1);
}

int
jpeg_get_bbox (FILE *fp, long *width, long *height,
	       double *xdensity, double *ydensity)
{
  struct JPEG_info j_info;

  JPEG_info_init(&j_info);

  if (JPEG_scan_file(&j_info, fp) < 0) {
    WARN("%s: Not a JPEG file?", JPEG_DEBUG_STR);
    JPEG_info_clear(&j_info);
    return -1;
  }

  *width  = j_info.width;
  *height = j_info.height;

  jpeg_get_density(&j_info, xdensity, ydensity);

  JPEG_info_clear(&j_info);

  return 0;
}
