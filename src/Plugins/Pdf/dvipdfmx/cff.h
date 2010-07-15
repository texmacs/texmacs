/*  $Header: /home/cvsroot/dvipdfmx/src/cff.h,v 1.10 2008/10/13 19:42:48 matthias Exp $
    
    This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team <dvipdfmx@project.ktug.or.kr>
    
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

#ifndef _CFF_H_
#define _CFF_H_

#include "mfileio.h"
#include "cff_types.h"

/* Flag */
#define FONTTYPE_CIDFONT  (1 << 0)
#define FONTTYPE_FONT     (1 << 1)
#define FONTTYPE_MMASTER  (1 << 2)

#define ENCODING_STANDARD (1 << 3)
#define ENCODING_EXPERT   (1 << 4)

#define CHARSETS_ISOADOBE (1 << 5)
#define CHARSETS_EXPERT   (1 << 6)
#define CHARSETS_EXPSUB   (1 << 7)

#define HAVE_STANDARD_ENCODING (ENCODING_STANDARD|ENCODING_EXPERT)
#define HAVE_STANDARD_CHARSETS \
  (CHARSETS_ISOADOBE|CHARSETS_EXPERT|CHARSETS_EXPSUB)

#define CFF_STRING_NOTDEF 65535

typedef struct
{
  char         *fontname; /* FontName */

  /* - CFF structure - */
  cff_header    header;   /* CFF Header */
  cff_index    *name;     /* Name INDEX */
  cff_dict     *topdict;  /* Top DICT (single) */
  cff_index    *string;   /* String INDEX */
  cff_index    *gsubr;    /* Global Subr INDEX */
  cff_encoding *encoding; /* Encodings */
  cff_charsets *charsets; /* Charsets  */
  cff_fdselect *fdselect; /* FDSelect, CIDFont only */
  cff_index    *cstrings; /* CharStrings */
  cff_dict    **fdarray;  /* CIDFont only */
  cff_dict    **private;  /* per-Font DICT */
  cff_index   **subrs;    /* Local Subr INDEX, per-Private DICT */

  /* -- extra data -- */
  l_offset    offset;     /* non-zero for OpenType or PostScript wrapped */
  l_offset    gsubr_offset;
  card16      num_glyphs; /* number of glyphs (CharString INDEX count) */
  card8       num_fds;    /* number of Font DICT */

  /* Updated String INDEX.
   * Please fix this. We should separate input and output.
   */
  cff_index  *_string;

  FILE         *stream;
  int           filter;   /* not used, ASCII Hex filter if needed */

  int           index;    /* CFF fontset index */
  int           flag;     /* Flag: see above */
} cff_font;

extern cff_font *cff_open  (FILE *file, long offset, int idx);
extern void      cff_close (cff_font *cff);

#define cff_seek_set(c, p) seek_absolute (((c)->stream), ((c)->offset) + (p));

/* CFF Header */
extern long cff_put_header (cff_font *cff, card8 *dest, long destlen);

/* CFF INDEX */
extern cff_index *cff_get_index        (cff_font *cff);
extern cff_index *cff_get_index_header (cff_font *cff);
extern void       cff_release_index    (cff_index *idx);
extern cff_index *cff_new_index        (card16 count);
extern long       cff_index_size       (cff_index *idx);
extern long       cff_pack_index       (cff_index *idx, card8 *dest, long destlen);

/* Name INDEX */
extern char *cff_get_name (cff_font *cff);
extern long  cff_set_name (cff_font *cff, char *name);

/* Global and Local Subrs INDEX */
extern long  cff_read_subrs (cff_font *cff);

/* Encoding */
extern long   cff_read_encoding    (cff_font *cff);
extern long   cff_pack_encoding    (cff_font *cff, card8 *dest, long destlen);
extern card16 cff_encoding_lookup  (cff_font *cff, card8 code);
extern void   cff_release_encoding (cff_encoding *encoding);

/* Charsets */
extern long   cff_read_charsets    (cff_font *cff);
extern long   cff_pack_charsets    (cff_font *cff, card8 *dest, long destlen);

/* Returns GID of PS name "glyph" */
extern card16 cff_glyph_lookup     (cff_font *cff, const char *glyph);
/* Returns GID of glyph with SID/CID "cid" */
extern card16 cff_charsets_lookup  (cff_font *cff, card16 cid);
extern void   cff_release_charsets (cff_charsets *charset);
/* Returns SID or CID */
extern card16 cff_charsets_lookup_inverse (cff_font *cff, card16 gid);

/* FDSelect */
extern long  cff_read_fdselect    (cff_font *cff);
extern long  cff_pack_fdselect    (cff_font *cff, card8 *dest, long destlen);
extern card8 cff_fdselect_lookup  (cff_font *cff, card16 gid);
extern void  cff_release_fdselect (cff_fdselect *fdselect);

/* Font DICT(s) */
extern long  cff_read_fdarray (cff_font *cff);

/* Private DICT(s) */
extern long  cff_read_private (cff_font *cff);

/* String */
extern int   cff_match_string  (cff_font *cff, const char *str, s_SID sid);
extern char *cff_get_string    (cff_font *cff, s_SID id);
extern long  cff_get_sid       (cff_font *cff, char *str);
extern s_SID cff_add_string    (cff_font *cff, const char *str, int unique);
extern void  cff_update_string (cff_font *cff);

#define cff_is_stdstr(s) (cff_get_sid(NULL, (s)) >= 0)

#endif /* _CFF_H_ */
