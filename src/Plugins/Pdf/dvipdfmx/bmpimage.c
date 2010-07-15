/*  $Header: /home/cvsroot/dvipdfmx/src/bmpimage.c,v 1.2 2004/07/27 12:08:46 hirata Exp $

    This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002 by Jin-Hwan Cho and Shunsaku Hirata,
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
 * BMP SUPPORT:
 */

#include "system.h"
#include "error.h"
#include "mem.h"

#include "pdfobj.h"

#include "bmpimage.h"

#define DIB_FILE_HEADER_SIZE 14
#define DIB_CORE_HEADER_SIZE 14
#define DIB_INFO_HEADER_SIZE 40

#define DIB_COMPRESS_NONE 0
#define DIB_COMPRESS_RLE8 1
#define DIB_COMPRESS_RLE4 2

#define DIB_HEADER_SIZE_MAX (DIB_FILE_HEADER_SIZE+DIB_INFO_HEADER_SIZE)

static long read_raster_rle8 (unsigned char *data_ptr,
			      long width, long height, FILE *fp);
static long read_raster_rle4 (unsigned char *data_ptr,
			      long width, long height, FILE *fp);

int
check_for_bmp (FILE *fp)
{
 unsigned char sigbytes[2];

  if (!fp)
    return 0;

  rewind(fp);
  if (fread(sigbytes, 1, sizeof(sigbytes), fp) != sizeof(sigbytes) ||
      sigbytes[0] != 'B' || sigbytes[1] != 'M')
    return 0;
  else
    return 1;
  
  return 0;
}

int
bmp_include_image (pdf_ximage *ximage, FILE *fp)
{
  pdf_obj *stream, *stream_dict, *colorspace;
  ximage_info info;
  unsigned char  buf[DIB_HEADER_SIZE_MAX+4];
  unsigned char *p;
  long offset, fsize, hsize, compression;
  long psize; /* Bytes per palette color: 3 for OS2, 4 for Win */
  unsigned short bit_count; /* Bits per pix */
  int  num_palette, flip;
  int  i;

  pdf_ximage_init_image_info(&info);

  stream = stream_dict = colorspace = NULL;
  p = buf;

  rewind(fp);
  if (fread(buf, 1, DIB_FILE_HEADER_SIZE + 4, fp)
      != DIB_FILE_HEADER_SIZE + 4) {
    WARN("Could not read BMP file header...");
  }

  if (p[0] != 'B' || p[1] != 'M') {
    WARN("File not starting with \'B\' \'M\'... Not a BMP file?");
    return -1;
  }
  p += 2;

#define ULONG_LE(b)  ((b)[0] + ((b)[1] << 8) +\
		      ((b)[2] << 16) + ((b)[3] << 24))
#define USHORT_LE(b) ((b)[0] + ((b)[1] << 8))

  fsize  = ULONG_LE(p); p += 4;
  if (ULONG_LE(p) != 0) {
    WARN("Not a BMP file???");
    return -1;
  }
  p += 4;
  offset = ULONG_LE(p); p += 4;

  /* info header */
  hsize  = ULONG_LE(p); p += 4;
  if (fread(p, sizeof(char), hsize - 4, fp) != hsize - 4) {
    WARN("Could not read BMP file header...");
    return -1;
  }
  flip = 1;
  if (hsize == DIB_CORE_HEADER_SIZE) {
    info.width  = USHORT_LE(p); p += 2;
    info.height = USHORT_LE(p); p += 2;
    if (USHORT_LE(p) != 1) {
      WARN("Unknown bcPlanes value in BMP COREHEADER.");
      return -1;
    }
    p += 2;
    bit_count   = USHORT_LE(p); p += 2;
    compression = DIB_COMPRESS_NONE;
    psize = 3;
  } else if (hsize == DIB_INFO_HEADER_SIZE) {
    info.width  = ULONG_LE(p);  p += 4;
    info.height = ULONG_LE(p);  p += 4;
    if (USHORT_LE(p) != 1) {
      WARN("Unknown biPlanes value in BMP INFOHEADER.");
      return -1;
    }
    p += 2;
    bit_count   = USHORT_LE(p); p += 2;
    compression = ULONG_LE(p);  p += 4;
    if (info.height < 0) {
      info.height = -info.height;
      flip = 0;
    }
    psize = 4;
  } else {
    WARN("Unknown BMP header type.");
    return -1;
  }

  if (bit_count < 24) {
    if (bit_count != 1 &&
	bit_count != 4 && bit_count != 8) {
      WARN("Unsupported palette size: %ld", bit_count);
      return -1;
    }
    num_palette = (offset - hsize - DIB_FILE_HEADER_SIZE) / psize;
    info.bits_per_component = bit_count;
    info.num_components = 1;
  } else if (bit_count == 24) { /* full color */
    num_palette = 1; /* dummy */
    info.bits_per_component = 8;
    info.num_components = 3;
  } else {
    WARN("Unkown BMP bitCount: %ld", bit_count);
    return -1;
  }

  if (info.width == 0 || info.height == 0 || num_palette < 1) {
    WARN("Invalid BMP file: width=%ld, height=%ld, #palette=%d",
	 info.width, info.height, num_palette);
    return -1;
  }

  stream      = pdf_new_stream(STREAM_COMPRESS);
  stream_dict = pdf_stream_dict(stream);

  if (bit_count < 24) {
    pdf_obj *lookup;
    unsigned char *palette, bgrq[4];

    palette = NEW(num_palette*3+1, unsigned char);
    for (i = 0; i < num_palette; i++) {
      if (fread(bgrq, 1,  psize, fp) != psize) {
	WARN("Reading file failed...");
	RELEASE(palette);
	return -1;
      }
      /* BGR data */
      palette[3*i  ] = bgrq[2];
      palette[3*i+1] = bgrq[1];
      palette[3*i+2] = bgrq[0];
    }
    lookup = pdf_new_string(palette, num_palette*3);
    RELEASE(palette);

    colorspace = pdf_new_array();
    pdf_add_array(colorspace, pdf_new_name("Indexed"));
    pdf_add_array(colorspace, pdf_new_name("DeviceRGB"));
    pdf_add_array(colorspace, pdf_new_number(num_palette-1));
    pdf_add_array(colorspace, lookup);
  } else {
    colorspace = pdf_new_name("DeviceRGB");
  }
  pdf_add_dict(stream_dict, pdf_new_name("ColorSpace"), colorspace);

  /* Raster data of BMP is four-byte aligned. */
  {
    long rowbytes, n;
    unsigned char *stream_data_ptr = NULL;

    rowbytes = (info.width * bit_count + 7) / 8;

    seek_absolute(fp, offset);
    if (compression == DIB_COMPRESS_NONE) {
      long dib_rowbytes;
      int  padding;

      padding = (rowbytes % 4) ? 4 - (rowbytes % 4) : 0;
      dib_rowbytes = rowbytes + padding;
      stream_data_ptr = NEW(rowbytes*info.height + padding,
			    unsigned char);
      for (n = 0; n < info.height; n++) {
	p = stream_data_ptr + n * rowbytes;
	if (fread(p, 1, dib_rowbytes, fp) != dib_rowbytes) {
	  WARN("Reading BMP raster data failed...");
	  pdf_release_obj(stream);
	  RELEASE(stream_data_ptr);
	  return -1;
	}
      }
    } else if (compression == DIB_COMPRESS_RLE8) {
      stream_data_ptr = NEW(rowbytes*info.height, unsigned char);
      if (read_raster_rle8(stream_data_ptr,
			   info.width, info.height, fp) < 0) {
	WARN("Reading BMP raster data failed...");
	pdf_release_obj(stream);
	RELEASE(stream_data_ptr);
	return -1;
      }
    } else if (compression == DIB_COMPRESS_RLE4) {
      stream_data_ptr = NEW(rowbytes*info.height, unsigned char);
      if (read_raster_rle4(stream_data_ptr,
			   info.width, info.height, fp) < 0) {
	WARN("Reading BMP raster data failed...");
	pdf_release_obj(stream);
	RELEASE(stream_data_ptr);
	return -1;
      }
    } else {
      pdf_release_obj(stream);
      return -1;
    }

    /* gbr --> rgb */
    if (bit_count == 24) {
      for (n = 0; n < info.width * info.height * 3; n += 3) {
	unsigned char g;
	g = stream_data_ptr[n];
	stream_data_ptr[n  ] = stream_data_ptr[n+2];
	stream_data_ptr[n+2] = g;
      }
    }

    if (flip) {
      for (n = info.height - 1; n >= 0; n--) {
	p = stream_data_ptr + n * rowbytes;
	pdf_add_stream(stream, p, rowbytes);
      }
    } else {
      pdf_add_stream(stream, stream_data_ptr, rowbytes*info.height);
    }
    RELEASE(stream_data_ptr);
  }

  pdf_ximage_set_image(ximage, &info, stream);

  return 0;
}

static long
read_raster_rle8 (unsigned char *data_ptr,
		  long width, long height, FILE *fp)
{
  long count = 0;
  unsigned char *p, b0, b1;
  long h, v, rowbytes;
  int  eol, eoi;

  p = data_ptr;
  rowbytes = width;
  memset(data_ptr, 0, rowbytes*height);
  for (v = 0, eoi = 0; v < height && !eoi; v++) {
    for (h = 0, eol = 0; h < width && !eol; ) {

      b0 = get_unsigned_byte(fp);
      b1 = get_unsigned_byte(fp);
      count += 2;

      p = data_ptr + v * rowbytes + h;

      if (b0 == 0x00) {
	switch (b1) {
	case 0x00: /* EOL */
	  eol = 1;
	  break;
	case 0x01: /* EOI */
	  eoi = 1;
	  break;
	case 0x02:
	  h += get_unsigned_byte(fp);
	  v += get_unsigned_byte(fp);
	  count += 2;
	  break;
	default:
	  h += b1;
	  if (h > width) {
	    WARN("RLE decode failed...");
	    return -1;
	  }
	  if (fread(p, 1, b1, fp) != b1)
	    return -1;
	  count += b1;
	  if (b1 % 2) {
	    get_unsigned_byte(fp);
	    count++;
	  }
	  break;
	}
      } else {
	h += b0;
	if (h > width) {
	  WARN("RLE decode failed...");
	  return -1;
	}
	memset(p, b1, b0);
      }
    }

    /* Check for EOL and EOI marker */
    if (!eol && !eoi) {
      b0 = get_unsigned_byte(fp);
      b1 = get_unsigned_byte(fp);
      if (b0 != 0x00) {
	WARN("RLE decode failed...");
	return -1;
      } else if (b1 == 0x01) {
	eoi = 1;
      } else if (b1 != 0x00) {
	WARN("RLE decode failed...");
	return -1;
      }
    }

    /* next row ... */
  }

  return count;
}

static long
read_raster_rle4 (unsigned char *data_ptr,
		  long width, long height, FILE *fp)
{
  long count = 0;
  unsigned char *p, b0, b1, b;
  long h, v, rowbytes;
  int  eol, eoi, i, nbytes;

  p = data_ptr;
  rowbytes = (width + 1) / 2;
  memset(data_ptr, 0, rowbytes*height);
  for (v = 0, eoi = 0; v < height && !eoi; v++) {
    for (h = 0, eol = 0; h < width && !eol; ) {

      b0 = get_unsigned_byte(fp);
      b1 = get_unsigned_byte(fp);
      count += 2;

      p  = data_ptr + v * rowbytes + (h / 2);
      if (b0 == 0x00) {
	switch (b1) {
	case 0x00: /* EOL */
	  eol = 1;
	  break;
	case 0x01: /* EOI */
	  eoi = 1;
	  break;
	case 0x02:
	  h += get_unsigned_byte(fp);
	  v += get_unsigned_byte(fp);
	  count += 2;
	  break;
	default:
	  if (h + b1 > width) {
	    WARN("RLE decode failed...");
	    return -1;
	  }
	  nbytes = (b1 + 1)/2;
	  if (h % 2) { /* starting at hi-nib */
	    for (i = 0; i < nbytes; i++) {
	      b = get_unsigned_byte(fp);
	      *p++ |= (b >> 4) & 0x0f;
	      *p    = (b << 4) & 0xf0;
	    }
	  } else {
	    if (fread(p, 1, nbytes, fp) != nbytes) {
	      return -1;
	    }
	  }
	  h     += b1;
	  count += nbytes;
	  if (nbytes % 2) {
	    get_unsigned_byte(fp);
	    count++;
	  }
	  break;
	}
      } else {
	if (h + b0 > width) {
	  WARN("RLE decode failed...");
	  return -1;
	}
	if (h % 2) {
	  *p++ = (b1 >> 4) & 0x0f;
	  b1   = ((b1 << 4) & 0xf0)|((b1 >> 4) & 0x0f);
	  b0--;
	  h++;
	}
	nbytes = (b0 + 1)/2;
	memset(p, b1, nbytes);
	h += b0;
	if (h % 2)
	  p[nbytes-1] &= 0xf0;
      }
    }

    /* Check for EOL and EOI marker */
    if (!eol && !eoi) {
      b0 = get_unsigned_byte(fp);
      b1 = get_unsigned_byte(fp);
      if (b0 != 0x00) {
	WARN("No EOL/EOI marker. RLE decode failed...");
	return -1;
      } else if (b1 == 0x01) {
	eoi = 1;
      } else if (b1 != 0x00) {
	WARN("No EOL/EOI marker. RLE decode failed...");
	return -1;
      }
    }

    /* next row ... */
  }

  return count;
}
