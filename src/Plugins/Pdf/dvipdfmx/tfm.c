/*  $Header: /home/cvsroot/dvipdfmx/src/tfm.c,v 1.23 2008/11/30 21:12:27 matthias Exp $
    
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

#include <string.h>

#include "system.h"
#include "mem.h"
#include "mfileio.h"
#include "error.h"

#include "numbers.h"
#include "dpxutil.h"

#include "tfm.h"

#define TFM_FORMAT 1
#define OFM_FORMAT 2

#define FWBASE ((double) (1<<20))

static int verbose = 0;


#ifndef WITHOUT_ASCII_PTEX
/*
 * ID is 9 for vertical JFM file.
 */
#define JFM_ID  11
#define JFMV_ID  9
#define IS_JFM(i) ((i) == JFM_ID || (i) == JFMV_ID)
#endif /* !WITHOUT_ASCII_PTEX */

/*
 * TFM Record structure:
 * Multiple TFM's may be read in at once.
 */

struct tfm_font
{
#ifndef WITHOUT_ASCII_PTEX
  UNSIGNED_BYTE id;
  UNSIGNED_BYTE nt;
#endif /* !WITHOUT_ASCII_PTEX */
#ifndef WITHOUT_OMEGA
  SIGNED_QUAD   level;
#endif /* !WITHOUT_OMEGA */
  UNSIGNED_QUAD wlenfile;
  UNSIGNED_QUAD wlenheader;
  UNSIGNED_QUAD bc, ec;
  UNSIGNED_QUAD nwidths, nheights, ndepths;
  UNSIGNED_QUAD nitcor, nlig, nkern, nextens;
  UNSIGNED_QUAD nfonparm;
#ifndef WITHOUT_OMEGA
  UNSIGNED_QUAD fontdir;
  UNSIGNED_QUAD nco, ncw, npc;
#endif /* !WITHOUT_OMEGA */
  SIGNED_QUAD   *header;
#ifndef WITHOUT_ASCII_PTEX
  UNSIGNED_PAIR *chartypes;
#endif /* !WITHOUT_ASCII_PTEX */
  UNSIGNED_QUAD *char_info;
  UNSIGNED_PAIR *width_index;
  UNSIGNED_BYTE *height_index;
  UNSIGNED_BYTE *depth_index;
  SIGNED_QUAD   *width;
  SIGNED_QUAD   *height;
  SIGNED_QUAD   *depth;
};

static void
tfm_font_init (struct tfm_font *tfm) 
{
  tfm->header = NULL;
#ifndef WITHOUT_ASCII_PTEX
  tfm->id = 0;
  tfm->nt = 0;
  tfm->chartypes = NULL;
#endif /* !WITHOUT_ASCII_PTEX */
#ifndef WITHOUT_OMEGA
  tfm->level   = 0;
  tfm->fontdir = 0;
  tfm->nco = tfm->ncw = tfm->npc = 0;
#endif
  tfm->char_info    = NULL;
  tfm->width_index  = NULL;
  tfm->height_index = NULL;
  tfm->depth_index  = NULL;
  tfm->width = tfm->height = tfm->depth = NULL;
}

static void
tfm_font_clear (struct tfm_font *tfm)
{
  if (tfm) {
    if (tfm->header) {
      RELEASE(tfm->header);
      tfm->header = NULL;
    }
    if (tfm->char_info) {
      RELEASE(tfm->char_info);
      tfm->char_info = NULL;
    }
    if (tfm->width) {
      RELEASE(tfm->width);
      tfm->width = NULL;
    }
    if (tfm->height) {
      RELEASE(tfm->height);
      tfm->height = NULL;
    }
    if (tfm->depth) {
      RELEASE(tfm->depth);
      tfm->depth = NULL;
    }
#ifndef WITHOUT_ASCII_PTEX
    if (tfm->chartypes) {
      RELEASE(tfm->chartypes);
      tfm->chartypes = NULL;
    }
#endif /* !WITHOUT_ASCII_PTEX */
    if (tfm->width_index) {
      RELEASE(tfm->width_index);
      tfm->width_index = NULL;
    }
    if (tfm->height_index) {
      RELEASE(tfm->height_index);
      tfm->height_index = NULL;
    }
    if (tfm->depth_index) {
      RELEASE(tfm->depth_index);
      tfm->depth_index = NULL;
    }
  }
}


struct coverage
{
  long           first_char;
  unsigned short num_chars;
};

/*
 * All characters in the same range have same metrics.
 */

struct range_map {
  unsigned short   num_coverages;
  struct coverage *coverages;
  unsigned short  *indices;
};

/* Special case of num_coverages = 1 */
struct char_map
{
  struct coverage coverage;
  unsigned short *indices;
};

static void
release_char_map (struct char_map *map)
{
  if (map->indices)
    RELEASE(map->indices);
  map->indices = NULL;
  RELEASE(map);
}

static void
release_range_map (struct range_map *map)
{
  if (map->coverages)
    RELEASE(map->coverages);
  if (map->indices)
    RELEASE(map->indices);
  map->coverages = NULL;
  map->indices   = NULL;
  RELEASE(map);
}

static long
lookup_char (const struct char_map *map, long charcode)
{
  if (charcode >= map->coverage.first_char &&
      charcode <= map->coverage.first_char + map->coverage.num_chars)
    return map->indices[charcode - map->coverage.first_char];
  else
    return -1;

  return -1;
}

static long
lookup_range (const struct range_map *map, long charcode)
{
  long  idx;

  for (idx = map->num_coverages - 1; idx >= 0 &&
	 charcode >= map->coverages[idx].first_char; idx--) {
    if (charcode <=
	map->coverages[idx].first_char + map->coverages[idx].num_chars)
      return map->indices[idx];
  }

  return -1;
}

#define SOURCE_TYPE_TFM 0
#define SOURCE_TYPE_JFM 1
#define SOURCE_TYPE_OFM 2

#define MAPTYPE_NONE  0
#define MAPTYPE_CHAR  1
#define MAPTYPE_RANGE 2

#define FONT_DIR_HORIZ 0
#define FONT_DIR_VERT  1

struct font_metric
{
  char    *tex_name;
  fixword  designsize;
  char    *codingscheme;

  int  fontdir;
  long firstchar, lastchar;
  
  fixword *widths;
  fixword *heights;
  fixword *depths;

  struct {
    int   type;
    void *data;
  } charmap;

  int source;
};

static void
fm_init (struct font_metric *fm) 
{
  fm->tex_name = NULL;
  fm->firstchar = 0;
  fm->lastchar  = 0;
  fm->fontdir   = FONT_DIR_HORIZ;
  fm->codingscheme = NULL;
  fm->designsize   = 0;

  fm->widths  = NULL;
  fm->heights = NULL;
  fm->depths  = NULL;

  fm->charmap.type = MAPTYPE_NONE;
  fm->charmap.data = NULL;

  fm->source = SOURCE_TYPE_TFM;
}

static void
fm_clear (struct font_metric *fm)
{
  if (fm) {
    if (fm->tex_name)
      RELEASE(fm->tex_name);
    if (fm->widths)
      RELEASE(fm->widths);
    if (fm->heights)
      RELEASE(fm->heights);
    if (fm->depths)
      RELEASE(fm->depths);
    if (fm->codingscheme)
      RELEASE(fm->codingscheme);

    switch (fm->charmap.type) {
    case MAPTYPE_CHAR:
      release_char_map(fm->charmap.data);
      break;
    case MAPTYPE_RANGE:
      release_range_map(fm->charmap.data);
      break;
    }
  }
}

#ifndef MAX_FONTS
#define MAX_FONTS 16
#endif

struct font_metric *fms = NULL;
static unsigned numfms = 0, max_fms = 0;

static void
fms_need (unsigned n)
{
  if (n > max_fms) {
    max_fms = MAX(max_fms + MAX_FONTS, n);
    fms = RENEW(fms, max_fms, struct font_metric);
  }
}

void
tfm_set_verbose (void)
{
  verbose++;
}


static long
fread_fwords (SIGNED_QUAD *words, SIGNED_QUAD nmemb, FILE *fp)
{
  long i;

  for (i = 0; i < nmemb; i++)
    words[i] = get_signed_quad(fp);

  return nmemb*4;
}

static long
fread_uquads (UNSIGNED_QUAD *quads, SIGNED_QUAD nmemb, FILE *fp)
{
  long i;

  for (i = 0; i < nmemb; i++) {
    quads[i] = get_unsigned_quad(fp);
  }

  return nmemb*4;
}

/*
 * TFM and JFM
 */
static void
tfm_check_size (struct tfm_font *tfm, SIGNED_QUAD tfm_file_size)
{
  UNSIGNED_QUAD expected_size = 6;

  /* Removed the warning message caused by EC TFM metric files.
   *
  if (tfm->wlenfile != tfm_file_size / 4) {
    WARN("TFM file size is %ld bytes but it says it is %ld bytes!",
	 tfm_file_size, tfm->wlenfile * 4);
    if (tfm_file_size > tfm->wlenfile * 4) {
      WARN("Proceeding nervously...");
    } else {
      ERROR("Can't proceed...");
    }
  }
   */
  if (tfm_file_size < tfm->wlenfile * 4) {
    ERROR("Can't proceed...");
  }

  expected_size += (tfm->ec - tfm->bc + 1);
  expected_size += tfm->wlenheader;
  expected_size += tfm->nwidths;
  expected_size += tfm->nheights;
  expected_size += tfm->ndepths;
  expected_size += tfm->nitcor;
  expected_size += tfm->nlig;
  expected_size += tfm->nkern;
  expected_size += tfm->nextens;
  expected_size += tfm->nfonparm;
#ifndef WITHOUT_ASCII_PTEX
  if (IS_JFM(tfm->id)) {
    expected_size += tfm->nt + 1;
  }
#endif /* !WITHOUT_ASCII_PTEX */
  if (expected_size != tfm->wlenfile) {
    WARN("TFM file size is expected to be %ld bytes but it says it is %ld bytes!",
	 expected_size * 4, tfm->wlenfile * 4);
    if (tfm_file_size > expected_size *4) {
      WARN("Proceeding nervously...");
    } else {
      ERROR("Can't proceed...");
    }
  }
}

static void
tfm_get_sizes (FILE *tfm_file, SIGNED_QUAD tfm_file_size, struct tfm_font *tfm)
{
#ifndef WITHOUT_ASCII_PTEX
  {
    UNSIGNED_PAIR first_hword;

    /*
     * The first half word of TFM/JFM is TFM ID for JFM or size of
     * TFM file in word for TFM. TFM with 9*4 or 11*4 bytes is not
     * expected to be a valid TFM. So, we always assume that TFMs
     * starting with 00 09 or 00 0B is JFM.
     */
    first_hword = get_unsigned_pair(tfm_file);
    if (IS_JFM(first_hword)) {
      tfm->id = first_hword;
      tfm->nt = get_unsigned_pair(tfm_file);
      tfm->wlenfile = get_unsigned_pair(tfm_file);
    } else {
      tfm->wlenfile = first_hword;
    }
  }
#else /* WITHOUT_ASCII_PTEX */
  tfm->wlenfile = get_unsigned_pair(tfm_file);
#endif /* !WITHOUT_ASCII_PTEX */

  tfm->wlenheader = get_unsigned_pair(tfm_file);
  tfm->bc = get_unsigned_pair(tfm_file);
  tfm->ec = get_unsigned_pair(tfm_file);
  if (tfm->ec < tfm->bc) {
    ERROR("TFM file error: ec(%u) < bc(%u) ???", tfm->ec, tfm->bc);
  }
  tfm->nwidths  = get_unsigned_pair(tfm_file);
  tfm->nheights = get_unsigned_pair(tfm_file);
  tfm->ndepths  = get_unsigned_pair(tfm_file);
  tfm->nitcor   = get_unsigned_pair(tfm_file);
  tfm->nlig     = get_unsigned_pair(tfm_file);
  tfm->nkern    = get_unsigned_pair(tfm_file);
  tfm->nextens  = get_unsigned_pair(tfm_file);
  tfm->nfonparm = get_unsigned_pair(tfm_file);

  tfm_check_size(tfm, tfm_file_size);

  return;
}

#ifndef WITHOUT_ASCII_PTEX
static void
jfm_do_char_type_array (FILE *tfm_file, struct tfm_font *tfm)
{
  UNSIGNED_PAIR charcode;
  UNSIGNED_PAIR chartype;
  long i;

  tfm->chartypes = NEW(65536, UNSIGNED_PAIR);
  for (i = 0; i < 65536; i++) {
    tfm->chartypes[i] = 0;
  }
  for (i = 0; i < tfm->nt; i++) {
    charcode = get_unsigned_pair(tfm_file);
    chartype = get_unsigned_pair(tfm_file);
    tfm->chartypes[charcode] = chartype;
  }
}

static void
jfm_make_charmap (struct font_metric *fm, struct tfm_font *tfm)
{
  if (tfm->nt > 1) {
    struct char_map *map;
    long   code;

    fm->charmap.type = MAPTYPE_CHAR;
    fm->charmap.data = map = NEW(1, struct char_map);
    map->coverage.first_char = 0;
    map->coverage.num_chars  = 0xFFFFu;
    map->indices    = NEW(0x10000L, unsigned short);

    for (code = 0; code <= 0xFFFFu; code++) {
      map->indices[code] = tfm->chartypes[code];
    }
  } else {
    struct range_map *map;

    fm->charmap.type = MAPTYPE_RANGE;
    fm->charmap.data = map = NEW(1, struct range_map);
    map->num_coverages = 1;
    map->coverages     = NEW(map->num_coverages, struct coverage);
    map->coverages[0].first_char = 0;
    map->coverages[0].num_chars  = 0xFFFFu;
    map->indices = NEW(1, unsigned short);
    map->indices[0] = 0; /* Only default type used. */
  }
}
#endif /* !WITHOUT_ASCII_PTEX */

static void
tfm_unpack_arrays (struct font_metric *fm, struct tfm_font *tfm)
{
  UNSIGNED_QUAD charinfo;
  UNSIGNED_PAIR width_index, height_index, depth_index;
  int i;

  fm->widths  = NEW(256, fixword);
  fm->heights = NEW(256, fixword);
  fm->depths  = NEW(256, fixword);
  for (i = 0; i < 256; i++) {
    fm->widths [i] = 0;
    fm->heights[i] = 0;
    fm->depths [i] = 0;
  }

  for (i = tfm->bc; i <= tfm->ec; i++ ) {
    charinfo     = tfm->char_info[i - tfm->bc];
    width_index  = (charinfo / 16777216ul);
    height_index = (charinfo / 0x100000ul) & 0xf;
    depth_index  = (charinfo / 0x10000ul)  & 0xf;
    fm->widths [i] = tfm->width [width_index];
    fm->heights[i] = tfm->height[height_index];
    fm->depths [i] = tfm->depth [depth_index];
  }

  return;
}

static int
sput_bigendian (char *s, SIGNED_QUAD v, int n)
{
  int i;

  for (i = n-1; i >= 0; i--) {
    s[i] = (char) (v & 0xff);
    v >>= 8;
  }

  return n;
}

static void
tfm_unpack_header (struct font_metric *fm, struct tfm_font *tfm)
{
  if (tfm->wlenheader < 12) {
    fm->codingscheme = NULL;
  } else {
    int   i, len;
    char *p;

    len = (tfm->header[2] >> 24);
    if (len < 0 || len > 39)
      ERROR("Invalid TFM header.");
    if (len > 0) {
      fm->codingscheme = NEW(40, char);
      p = fm->codingscheme;
      p += sput_bigendian(p, tfm->header[2], 3);
      for (i = 1; i <= len / 4; i++) {
	p += sput_bigendian(p, tfm->header[2+i], 4);
      }
      fm->codingscheme[len] = '\0';
    } else {
      fm->codingscheme = NULL;
    }
  }

  fm->designsize = tfm->header[1];
}

#ifndef WITHOUT_OMEGA

static void
ofm_check_size_one (struct tfm_font *tfm, SIGNED_QUAD ofm_file_size)
{
  UNSIGNED_QUAD ofm_size = 14;

  ofm_size += 2*(tfm->ec - tfm->bc + 1);
  ofm_size += tfm->wlenheader;
  ofm_size += tfm->nwidths;
  ofm_size += tfm->nheights;
  ofm_size += tfm->ndepths;
  ofm_size += tfm->nitcor;
  ofm_size += 2*(tfm->nlig);
  ofm_size += tfm->nkern;
  ofm_size += 2*(tfm->nextens);
  ofm_size += tfm->nfonparm;
  if (tfm->wlenfile != ofm_file_size / 4 ||
      tfm->wlenfile != ofm_size) {
    ERROR("OFM file problem.  Table sizes don't agree.");
  }
}

static void
ofm_get_sizes (FILE *ofm_file, UNSIGNED_QUAD ofm_file_size, struct tfm_font *tfm)
{
  tfm->level = get_signed_quad(ofm_file);

  tfm->wlenfile   = get_signed_quad(ofm_file);
  tfm->wlenheader = get_signed_quad(ofm_file);
  tfm->bc = get_signed_quad(ofm_file);
  tfm->ec = get_signed_quad(ofm_file);
  if (tfm->ec < tfm->bc) {
    ERROR("OFM file error: ec(%u) < bc(%u) ???", tfm->ec, tfm->bc);
  }
  tfm->nwidths  = get_signed_quad(ofm_file);
  tfm->nheights = get_signed_quad(ofm_file);
  tfm->ndepths  = get_signed_quad(ofm_file);
  tfm->nitcor   = get_signed_quad(ofm_file);
  tfm->nlig     = get_signed_quad(ofm_file);
  tfm->nkern    = get_signed_quad(ofm_file);
  tfm->nextens  = get_signed_quad(ofm_file);
  tfm->nfonparm = get_signed_quad(ofm_file);
  tfm->fontdir  = get_signed_quad(ofm_file);
  if (tfm->fontdir) {
    WARN("I may be interpreting a font direction incorrectly.");
  }
  if (tfm->level == 0) {
    ofm_check_size_one(tfm, ofm_file_size);
  } else if (tfm->level == 1) {
    tfm->nco = get_signed_quad(ofm_file);
    tfm->ncw = get_signed_quad(ofm_file);
    tfm->npc = get_signed_quad(ofm_file);
    seek_absolute(ofm_file, 4*(tfm->nco - tfm->wlenheader));
  } else {
    ERROR("Can't handle OFM files with level > 1");
  }

  return;
}

static void
ofm_do_char_info_zero (FILE *tfm_file, struct tfm_font *tfm)
{
  UNSIGNED_QUAD num_chars;

  num_chars = tfm->ec - tfm->bc + 1;
  if (num_chars != 0) {
    UNSIGNED_QUAD i;

    tfm->width_index  = NEW(num_chars, UNSIGNED_PAIR);
    tfm->height_index = NEW(num_chars, UNSIGNED_BYTE);
    tfm->depth_index  = NEW(num_chars, UNSIGNED_BYTE);
    for (i = 0; i < num_chars; i++) {
      tfm->width_index [i] = get_unsigned_pair(tfm_file);
      tfm->height_index[i] = get_unsigned_byte(tfm_file);
      tfm->depth_index [i] = get_unsigned_byte(tfm_file);
      /* Ignore remaining quad */
      get_unsigned_quad(tfm_file);
    }
  }
}

static void
ofm_do_char_info_one (FILE *tfm_file, struct tfm_font *tfm)
{
  UNSIGNED_QUAD num_char_infos;
  UNSIGNED_QUAD num_chars;

  num_char_infos = tfm->ncw / (3 + (tfm->npc / 2));
  num_chars      = tfm->ec - tfm ->bc + 1;

  if (num_chars != 0) {
    UNSIGNED_QUAD i;
    UNSIGNED_QUAD char_infos_read;

    tfm->width_index  = NEW(num_chars, UNSIGNED_PAIR);
    tfm->height_index = NEW(num_chars, UNSIGNED_BYTE);
    tfm->depth_index  = NEW(num_chars, UNSIGNED_BYTE);
    char_infos_read   = 0;
    for (i = 0; i < num_chars &&
	   char_infos_read < num_char_infos; i++) {
      int repeats, j;

      tfm->width_index [i] = get_unsigned_pair(tfm_file);
      tfm->height_index[i] = get_unsigned_byte(tfm_file);
      tfm->depth_index [i] = get_unsigned_byte(tfm_file);
      /* Ignore next quad */
      get_unsigned_quad(tfm_file);
      repeats = get_unsigned_pair(tfm_file);
      /* Skip params */
      for (j = 0; j < tfm->npc; j++) {
	get_unsigned_pair(tfm_file);
      }
      /* Remove word padding if necessary */
      if (ISEVEN(tfm->npc)){
	get_unsigned_pair(tfm_file);
      }
      char_infos_read++;
      if (i + repeats > num_chars) {
	ERROR("Repeats causes number of characters to be exceeded.");
      }
      for (j = 0; j < repeats; j++) {
	tfm->width_index [i+j+1] = tfm->width_index [i];
	tfm->height_index[i+j+1] = tfm->height_index[i];
	tfm->depth_index [i+j+1] = tfm->depth_index [i];
      }
      /* Skip ahead because we have already handled repeats */
      i += repeats;
    }
  }
}

static void
ofm_unpack_arrays (struct font_metric *fm,
		   struct tfm_font *tfm, UNSIGNED_QUAD num_chars)
{
  long i;

  fm->widths  = NEW(tfm->bc + num_chars, fixword);
  fm->heights = NEW(tfm->bc + num_chars, fixword);
  fm->depths  = NEW(tfm->bc + num_chars, fixword);
  for (i = 0; i < num_chars; i++) {
    fm->widths [tfm->bc + i] = tfm->width [ tfm->width_index [i] ];
    fm->heights[tfm->bc + i] = tfm->height[ tfm->height_index[i] ];
    fm->depths [tfm->bc + i] = tfm->depth [ tfm->depth_index [i] ];
  }
}

static void
read_ofm (struct font_metric *fm, FILE *ofm_file, UNSIGNED_QUAD ofm_file_size)
{
  struct tfm_font tfm;

  tfm_font_init(&tfm);

  ofm_get_sizes(ofm_file, ofm_file_size, &tfm);

  if (tfm.level < 0 || tfm.level > 1)
    ERROR ("OFM level %d not supported.", tfm.level);

  if (tfm.wlenheader > 0) {
    tfm.header = NEW(tfm.wlenheader, fixword);
    fread_fwords(tfm.header, tfm.wlenheader, ofm_file);
  }
  if (tfm.level == 0) {
    ofm_do_char_info_zero(ofm_file, &tfm);
  } else if (tfm.level == 1) {
    ofm_do_char_info_one(ofm_file, &tfm);
  }
  if (tfm.nwidths > 0) {
    tfm.width = NEW(tfm.nwidths, fixword);
    fread_fwords(tfm.width, tfm.nwidths, ofm_file);
  }
  if (tfm.nheights > 0) {
    tfm.height = NEW(tfm.nheights, fixword);
    fread_fwords(tfm.height, tfm.nheights, ofm_file);
  }
  if (tfm.ndepths > 0) {
    tfm.depth = NEW(tfm.ndepths, fixword);
    fread_fwords(tfm.depth, tfm.ndepths, ofm_file);
  }

  ofm_unpack_arrays(fm, &tfm, tfm.ec - tfm.bc + 1);
  tfm_unpack_header(fm, &tfm);
  fm->firstchar = tfm.bc;
  fm->lastchar  = tfm.ec;
  fm->source    = SOURCE_TYPE_OFM;

  tfm_font_clear(&tfm);

  return;
}
#endif /* !WITHOUT_OMEGA */

static void
read_tfm (struct font_metric *fm, FILE *tfm_file, UNSIGNED_QUAD tfm_file_size)
{
  struct tfm_font tfm;

  tfm_font_init(&tfm);

  tfm_get_sizes(tfm_file, tfm_file_size, &tfm);
  fm->firstchar = tfm.bc;
  fm->lastchar  = tfm.ec;
  if (tfm.wlenheader > 0) {
    tfm.header = NEW(tfm.wlenheader, fixword);
    fread_fwords(tfm.header, tfm.wlenheader, tfm_file);
  }
#ifndef WITHOUT_ASCII_PTEX
  if (IS_JFM(tfm.id)) {
    jfm_do_char_type_array(tfm_file, &tfm);
    jfm_make_charmap(fm, &tfm);
    fm->firstchar = 0;
    fm->lastchar  = 0xFFFFl;
    fm->fontdir   = (tfm.id == JFMV_ID) ? FONT_DIR_VERT : FONT_DIR_HORIZ;
    fm->source    = SOURCE_TYPE_JFM;
  }
#endif /* !WITHOUT_ASCII_PTEX */
  if (tfm.ec - tfm.bc + 1 > 0) {
    tfm.char_info = NEW(tfm.ec - tfm.bc + 1, UNSIGNED_QUAD);
    fread_uquads(tfm.char_info, tfm.ec - tfm.bc + 1, tfm_file);
  }
  if (tfm.nwidths > 0) {
    tfm.width = NEW(tfm.nwidths, fixword);
    fread_fwords(tfm.width, tfm.nwidths, tfm_file);
  }
  if (tfm.nheights > 0) {
    tfm.height = NEW(tfm.nheights, fixword);
    fread_fwords(tfm.height, tfm.nheights, tfm_file);
  }
  if (tfm.ndepths > 0) {
    tfm.depth = NEW(tfm.ndepths, fixword);
    fread_fwords(tfm.depth, tfm.ndepths, tfm_file);
  }
  tfm_unpack_arrays(fm, &tfm);
  tfm_unpack_header(fm, &tfm);

  tfm_font_clear(&tfm);

  return;
}

int
tfm_open (const char *tfm_name, int must_exist)
{
#ifdef NOKPSE
  FILE *tfm_file;
  int i, format = TFM_FORMAT;
  UNSIGNED_QUAD tfm_file_size;
  char *file_name = NULL;

  for (i = 0; i < numfms; i++) {
    if (!strcmp(tfm_name, fms[i].tex_name))
      return i;
  }

  /*
   * The procedure to search tfm or ofm files:
   * 1. Search tfm file with the given name with the must_exist flag unset.
   * 2. Search ofm file with the given name with the must_exist flag unset.
   * 3. If not found and must_exist flag is set, try again to search
   *    tfm file with the must_exist flag set.
   * 4. If not found and must_exist flag is not set, return -1.
   */


  /*
   * We first look for OFM and then TFM.
   * The reason for this change is incompatibility introduced when dvipdfmx
   * started to write correct glyph metrics to output PDF for CID fonts.
   * I'll not explain this in detail... This change is mostly specific to
   * Japanese support.
   */
#if 0
  if ((file_name = kpse_find_file(tfm_name, kpse_tfm_format, 0))) {
    format = TFM_FORMAT;
  } else if ((file_name = kpse_find_file(tfm_name, kpse_ofm_format, 0))) {
    format = OFM_FORMAT;
  }
#endif
 {
   char *ofm_name, *suffix;

   suffix = strrchr(tfm_name, '.');
   if (!suffix || (strcmp(suffix, ".tfm") != 0 &&
		   strcmp(suffix, ".ofm") != 0)) {
     ofm_name = NEW(strlen(tfm_name) + strlen(".ofm") + 1, char);
     strcpy(ofm_name, tfm_name);
     strcat(ofm_name, ".ofm");
   } else {
     ofm_name = NULL;
   }
   if (ofm_name &&
       (file_name = kpse_find_file(ofm_name, kpse_ofm_format, 0)) != NULL) {
     format = OFM_FORMAT;
   } else if ((file_name =
	       kpse_find_file(tfm_name, kpse_tfm_format, 0)) != NULL) {
     format = TFM_FORMAT;
   } else if ((file_name =
	       kpse_find_file(tfm_name, kpse_ofm_format, 0)) != NULL) {
     format = OFM_FORMAT;
   }
   if (ofm_name)
     RELEASE(ofm_name);
 }

  /*
   * In case that must_exist is set, MiKTeX returns always non-NULL value
   * even if the tfm file is not found.
   */
  if (file_name == NULL) {
    if (must_exist) {
      if ((file_name = kpse_find_file(tfm_name, kpse_tfm_format, 1)) != NULL)
	format = TFM_FORMAT;
      else {
	ERROR("Unable to find TFM file \"%s\".", tfm_name);
      }
    } else {
      return -1;
    }
  }

  tfm_file = MFOPEN(file_name, FOPEN_RBIN_MODE);
  if (!tfm_file) {
    ERROR("Could not open specified TFM/OFM file \"%s\".", tfm_name);
  }

  if (verbose) {
    if (format == TFM_FORMAT)
      MESG("(TFM:%s", tfm_name);
    else if (format == OFM_FORMAT)
      MESG("(OFM:%s", tfm_name);
    if (verbose > 1)
      MESG("[%s]", file_name);
  }

  RELEASE(file_name);

  tfm_file_size = file_size(tfm_file);
  if (tfm_file_size < 24) {
    ERROR("TFM/OFM file too small to be a valid file.");
  }

  fms_need(numfms + 1);
  fm_init(fms + numfms);

#ifndef WITHOUT_OMEGA
  if (format == OFM_FORMAT)
    read_ofm(&fms[numfms], tfm_file, tfm_file_size);
  else
#endif /* !WITHOUT_OMEGA */
    {
      read_tfm(&fms[numfms], tfm_file, tfm_file_size);
    }

  MFCLOSE(tfm_file);

  fms[numfms].tex_name = NEW(strlen(tfm_name)+1, char);
  strcpy(fms[numfms].tex_name, tfm_name);

  if (verbose) 
    MESG(")");

  return numfms++;
#else
  return -1;
#endif
}

void
tfm_close_all (void)
{
  int  i;

  if (fms) {
    for (i = 0; i < numfms; i++) {
      fm_clear(&(fms[i]));
    }
    RELEASE(fms);
  }
}

#define CHECK_ID(n) do {\
  if ((n) < 0 || (n) >= numfms)\
    ERROR("TFM: Invalid TFM ID: %d", (n));\
} while (0)

fixword
tfm_get_fw_width (int font_id, SIGNED_QUAD ch)
{
  struct font_metric *fm;
  long idx = 0;

  CHECK_ID(font_id);

  fm = &(fms[font_id]);
  if (ch >= fm->firstchar && ch <= fm->lastchar) {
    switch (fm->charmap.type) {
    case MAPTYPE_CHAR:
      idx = lookup_char(fm->charmap.data, ch);
      if (idx < 0)
	ERROR("Invalid char: %ld\n", ch);
      break;
    case MAPTYPE_RANGE:
      idx = lookup_range(fm->charmap.data, ch);
      if (idx < 0)
	ERROR("Invalid char: %ld\n", ch);
      break;
    default:
      idx = ch;
    }
  } else {
    ERROR("Invalid char: %ld\n", ch);
  }

  return fm->widths[idx];
}

fixword
tfm_get_fw_height (int font_id, SIGNED_QUAD ch)
{
  struct font_metric *fm;
  long idx = 0;

  CHECK_ID(font_id);

  fm = &(fms[font_id]);
  if (ch >= fm->firstchar && ch <= fm->lastchar) {
    switch (fm->charmap.type) {
    case MAPTYPE_CHAR:
      idx = lookup_char(fm->charmap.data, ch);
      if (idx < 0)
	ERROR("Invalid char: %ld\n", ch);
      break;
    case MAPTYPE_RANGE:
      idx = lookup_range(fm->charmap.data, ch);
      if (idx < 0)
	ERROR("Invalid char: %ld\n", ch);
      break;
    default:
      idx = ch;
    }
  } else {
    ERROR("Invalid char: %ld\n", ch);
  }

  return fm->heights[idx];
}

fixword
tfm_get_fw_depth (int font_id, SIGNED_QUAD ch)
{
  struct font_metric *fm;
  long idx = 0;

  CHECK_ID(font_id);

  fm = &(fms[font_id]);
  if (ch >= fm->firstchar && ch <= fm->lastchar) {
    switch (fm->charmap.type) {
    case MAPTYPE_CHAR:
      idx = lookup_char(fm->charmap.data, ch);
      if (idx < 0)
	ERROR("Invalid char: %ld\n", ch);
      break;
    case MAPTYPE_RANGE:
      idx = lookup_range(fm->charmap.data, ch);
      if (idx < 0)
	ERROR("Invalid char: %ld\n", ch);
      break;
    default:
      idx = ch;
    }
  } else {
    ERROR("Invalid char: %ld\n", ch);
  }

  return fm->depths[idx];
}


/*
 * tfm_get_width returns the width of the font
 * as a (double) fraction of the design size.
 */
double
tfm_get_width (int font_id, SIGNED_QUAD ch)
{
  return ((double) tfm_get_fw_width(font_id, ch)/FWBASE);
}

#if 0
double
tfm_get_height (int font_id, SIGNED_QUAD ch)
{
  return ((double) tfm_get_fw_height(font_id, ch)/FWBASE);
}

double
tfm_get_depth (int font_id, SIGNED_QUAD ch)
{
  return ((double) tfm_get_fw_depth(font_id, ch)/FWBASE);
}
#endif

/* tfm_string_xxx() do not work for OFM... */
fixword
tfm_string_width (int font_id, const unsigned char *s, unsigned len)
{
  fixword result = 0;
  struct font_metric *fm;
  unsigned i;

  CHECK_ID(font_id);

  fm = &(fms[font_id]);
#ifndef WITHOUT_ASCII_PTEX
  if (fm->source == SOURCE_TYPE_JFM) {
    for (i = 0; i < len/2; i++) {
      SIGNED_QUAD ch;

      ch = (s[2*i] << 8)|s[2*i+1];
      result += tfm_get_fw_width(font_id, ch);
    }
  } else
#endif
    for (i = 0; i < len; i++) {
      result += tfm_get_fw_width(font_id, s[i]);
    }

  return result;
}

fixword
tfm_string_depth (int font_id, const unsigned char *s, unsigned len)
{
  fixword result = 0;
  struct font_metric *fm;
  unsigned i;

  CHECK_ID(font_id);

  fm = &(fms[font_id]);
#ifndef WITHOUT_ASCII_PTEX
  if (fm->source == SOURCE_TYPE_JFM) {
    for (i = 0; i < len/2; i++) {
      SIGNED_QUAD ch;

      ch = (s[2*i] << 8)|s[2*i+1];
      result += tfm_get_fw_depth(font_id, ch);
    }
  } else
#endif
    for (i = 0; i < len; i++) {
      result = MAX(result, tfm_get_fw_depth(font_id, s[i]));
    }

  return result;
}

fixword
tfm_string_height (int font_id, const unsigned char *s, unsigned len)
{
  fixword result = 0;
  struct font_metric *fm;
  unsigned i;

  CHECK_ID(font_id);

  fm = &(fms[font_id]);
#ifndef WITHOUT_ASCII_PTEX
  if (fm->source == SOURCE_TYPE_JFM) {
    for (i = 0; i < len/2; i++) {
      SIGNED_QUAD ch;

      ch = (s[2*i] << 8)|s[2*i+1];
      result += tfm_get_fw_height(font_id, ch);
    }
  } else
#endif
    for (i = 0; i < len; i++) {
      result = MAX(result, tfm_get_fw_height(font_id, s[i]));
    }

  return result;
}

double
tfm_get_design_size (int font_id)
{
  CHECK_ID(font_id);

  return (double) (fms[font_id].designsize)/FWBASE*(72.0/72.27);
}

#if 0
char *
tfm_get_codingscheme (int font_id)
{
  CHECK_ID(font_id);

  return fms[font_id].codingscheme;
}

#ifndef WITHOUT_ASCII_PTEX
/* Vertical version of JFM */
int
tfm_is_vert (int font_id)
{
  CHECK_ID(font_id);

  return (fms[font_id].fontdir == FONT_DIR_VERT) ? 1 : 0;
}
#else /* WITHOUT_ASCII_PTEX */
int
tfm_is_vert (int font_id)
{
  return 0;
}
#endif /* !WITHOUT_ASCII_PTEX */
#endif

int
tfm_exists (const char *tfm_name)
{
#ifdef NOKPSE
  char *fullname;

  fullname = kpse_find_file(tfm_name, kpse_ofm_format, 0);
  if (fullname) {
    RELEASE(fullname);
    return 1;
  }
  fullname = kpse_find_file(tfm_name, kpse_tfm_format, 0);
  if (fullname) {
    RELEASE(fullname);
    return 1;
  }
#else
#endif
  return 0;
}
