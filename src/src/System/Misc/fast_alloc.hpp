
/******************************************************************************
* MODULE     : fast_alloc.hpp
* DESCRIPTION: see fast_alloc.cpp
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef FAST_ALLOC_H
#define FAST_ALLOC_H
#include "tm_configure.hpp"
#include <stdlib.h>

// g++ >= 3.2 requires
#include <iostream>
using std::ostream;
using std::cout;
using std::cerr;
// instead of include <iostream.h>

#define BLOCK_SIZE 65536 // should be >>> MAX_FAST

/******************************************************************************
* Globals
******************************************************************************/

extern char   alloc_table[MAX_FAST];
extern char*  alloc_mem;
extern size_t alloc_remains;
extern int    allocated;
extern int    large_uses;

#define alloc_ptr(i) (*((void **) (alloc_table+i)))
#define ind(ptr) (*((void **) ptr))

/******************************************************************************
* General purpose fast allocation routines
******************************************************************************/

extern void* safe_malloc (register size_t s);
extern void* enlarge_malloc (register size_t s);
extern void* fast_alloc (register size_t s);
extern void  fast_free (register void* ptr, register size_t s);

extern int   mem_used ();
extern void  mem_info ();

#endif // defined FAST_ALLOC_H
