
/******************************************************************************
* MODULE     : basic.cpp
* DESCRIPTION: fast global new and delete
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "fast_alloc.hpp"
#include "basic.hpp"

#ifndef NO_FAST_ALLOC

void*
operator new (register size_t s)
{
  register void* ptr;
  s= (s+ WORD_LENGTH+ WORD_LENGTH_INC)&WORD_MASK;
  if (s<MAX_FAST) {
    ptr= alloc_ptr(s);
    if (ptr==NULL) ptr= enlarge_malloc (s);
    else alloc_ptr(s)= ind(ptr);
  }
  else {
    ptr= safe_malloc (s);
    large_uses += s;
  }
  *((size_t *) ptr)=s;
  return (void*) (((char*) ptr)+ WORD_LENGTH);
}

void
operator delete (register void* ptr)
{
  ptr= (void*) (((char*) ptr)- WORD_LENGTH);
  register size_t s= *((size_t *) ptr);
  if (s<MAX_FAST) {
    ind(ptr)    = alloc_ptr(s);
    alloc_ptr(s)= ptr;
  }
  else {
    free (ptr);
    large_uses -= s;
  }
}

void*
operator new[] (register size_t s)
{
  register void* ptr;
  s= (s+ WORD_LENGTH+ WORD_LENGTH_INC)&WORD_MASK;
  if (s<MAX_FAST) {
    ptr= alloc_ptr(s);
    if (ptr==NULL) ptr= enlarge_malloc (s);
    else alloc_ptr(s)= ind(ptr);
  }
  else {
    ptr= safe_malloc (s);
    large_uses += s;
  }
  *((size_t *) ptr)=s;
  return (void*) (((char*) ptr)+ WORD_LENGTH);
}

void
operator delete[] (register void* ptr)
{
  ptr= (void*) (((char*) ptr)- WORD_LENGTH);
  register size_t s= *((size_t *) ptr);
  if (s<MAX_FAST) {
    ind(ptr)    = alloc_ptr(s);
    alloc_ptr(s)= ptr;
  }
  else {
    free (ptr);
    large_uses -= s;
  }
}

#endif // not defined NO_FAST_ALLOC

static int debug_status= 0;

bool
debug (int which, bool write_flag) {
  if (write_flag) {
    debug_status= debug_status | (1 << which);
    return 0;
  }
  else return (debug_status & (1 << which)) > 0;
}

int
debug_off () {
  int status= debug_status;
  debug_status= 0;
  return status;
}

void
debug_on (int status) {
  debug_status= status;
}

static int current_indent= 0;

ostream&
operator << (ostream& out, display_control ctrl) {
  int i;
  switch (ctrl) {
  case INDENT:
    out << "  ";
    current_indent += 2;
    break;
  case UNINDENT:
    out << "\b\b";
    current_indent -= 2;
    break;
  case HRULE:
    for (i=current_indent; i<78; i++) out << "-";
  case LF:
    out << "\n";
    for (i=0; i<current_indent; i++) out << " ";
    break;    
  }
  return out;
}
