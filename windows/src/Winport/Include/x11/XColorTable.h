/******************************************************************************
* MODULE     : XColorTable.h
* DESCRIPTION: Windows version of X11 Color Mapping
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef XCOLORTABLE_H
#define XCOLORTABLE_H

#include <sys/_types.h>

#define INVALID_COLOR_VALUE	0x11000000

unsigned int XGetColorValue(const char *name);
const char* XGetColorName(unsigned int value);

#endif