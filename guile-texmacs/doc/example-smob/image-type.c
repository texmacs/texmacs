/* image-type.c
 * 
 *	Copyright (C) 1998, 2000, 2004, 2006 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 */

#include <stdlib.h>
#include <libguile.h>

static scm_t_bits image_tag;

struct image {
  int width, height;
  char *pixels;

  /* The name of this image */
  SCM name;

  /* A function to call when this image is
     modified, e.g., to update the screen,
     or SCM_BOOL_F if no action necessary */
  SCM update_func;
};

static SCM
make_image (SCM name, SCM s_width, SCM s_height)
{
  SCM smob;
  struct image *image;
  int width = scm_to_int (s_width);
  int height = scm_to_int (s_height);

  /* Step 1: Allocate the memory block.
   */
  image = (struct image *) scm_gc_malloc (sizeof (struct image), "image");

  /* Step 2: Initialize it with straight code.
   */
  image->width = width;
  image->height = height;
  image->pixels = NULL;
  image->name = SCM_BOOL_F;
  image->update_func = SCM_BOOL_F;

  /* Step 3: Create the smob.
   */
  SCM_NEWSMOB (smob, image_tag, image);

  /* Step 4: Finish the initialization.
   */
  image->name = name;
  image->pixels = scm_gc_malloc (width * height, "image pixels");

  return smob;
}

SCM
clear_image (SCM image_smob)
{
  int area;
  struct image *image;

  scm_assert_smob_type (image_tag, image_smob);

  image = (struct image *) SCM_SMOB_DATA (image_smob);
  area = image->width * image->height;
  memset (image->pixels, 0, area);

  /* Invoke the image's update function.
   */
  if (scm_is_true (image->update_func))
    scm_call_0 (image->update_func);

  scm_remember_upto_here_1 (image_smob);

  return SCM_UNSPECIFIED;
}

static SCM
mark_image (SCM image_smob)
{
  /* Mark the image's name and update function.  */
  struct image *image = (struct image *) SCM_SMOB_DATA (image_smob);

  scm_gc_mark (image->name);
  return image->update_func;
}

static size_t
free_image (SCM image_smob)
{
  struct image *image = (struct image *) SCM_SMOB_DATA (image_smob);

  scm_gc_free (image->pixels, image->width * image->height, "image pixels");
  scm_gc_free (image, sizeof (struct image), "image");

  return 0;
}

static int
print_image (SCM image_smob, SCM port, scm_print_state *pstate)
{
  struct image *image = (struct image *) SCM_SMOB_DATA (image_smob);

  scm_puts ("#<image ", port);
  scm_display (image->name, port);
  scm_puts (">", port);

  /* non-zero means success */
  return 1;
}

void
init_image_type (void)
{
  image_tag = scm_make_smob_type ("image", sizeof (struct image));
  scm_set_smob_mark (image_tag, mark_image);
  scm_set_smob_free (image_tag, free_image);
  scm_set_smob_print (image_tag, print_image);

  scm_c_define_gsubr ("clear-image", 1, 0, 0, clear_image);
  scm_c_define_gsubr ("make-image", 3, 0, 0, make_image);
}
