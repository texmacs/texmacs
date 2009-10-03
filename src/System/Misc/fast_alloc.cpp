
/******************************************************************************
* MODULE     : Fast memory allocation
* DESCRIPTION: Fast allocations is realized by using a linked list
*              of allocations for each fixed size divisible by
*              a word legth up to MAX_FAST. Otherwise,
*              usual memory allocation is used.
* ASSUMPTIONS: The word size of the computer is 4.
*              Otherwise, change WORD_LENGTH.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "fast_alloc.hpp"

char   alloc_table[MAX_FAST]; // Static declaration initializes with NULL's
char*  alloc_mem=NULL;
size_t alloc_remains=0;
int    allocated=0;
int    fast_chunks=0;
int    large_uses=0;
int    MEM_DEBUG=0;
int    mem_used ();

/*****************************************************************************/
// General purpose fast allocation routines
/*****************************************************************************/

void*
safe_malloc (register size_t sz) {
  void* ptr= malloc (sz);
  if (ptr==NULL) {
    cerr << "Fatal error: out of memory\n";
    abort ();
  }
  return ptr;
}

void*
enlarge_malloc (register size_t sz) {
  if (alloc_remains<sz) {
    alloc_mem    = (char *) safe_malloc (BLOCK_SIZE);
    alloc_remains= BLOCK_SIZE;
    fast_chunks++;
  }
  register void* ptr= alloc_mem;
  alloc_mem    += sz;
  alloc_remains-= sz;
  return ptr;
}

void*
fast_alloc (register size_t sz) {
  sz= (sz+WORD_LENGTH_INC)&WORD_MASK;
  if (sz<MAX_FAST) {
    register void *ptr= alloc_ptr (sz);
    if (ptr==NULL) return enlarge_malloc (sz);
    alloc_ptr (sz)= ind (ptr);
    return ptr;
  }
  else {
    if (MEM_DEBUG>=3) cout << "Big alloc of " << sz << " bytes\n";
    if (MEM_DEBUG>=3) cout << "Memory used: " << mem_used () << " bytes\n";
    large_uses += sz;
    return safe_malloc (sz);
  }
}

void
fast_free (register void* ptr, register size_t sz) {
  sz=(sz+WORD_LENGTH_INC)&WORD_MASK;
  if (sz<MAX_FAST) {
    ind (ptr)     = alloc_ptr (sz);
    alloc_ptr (sz)= ptr;
  }
  else {
    if (MEM_DEBUG>=3) cout << "Big free of " << sz << " bytes\n";
    large_uses -= sz;    
    free (ptr);
    if (MEM_DEBUG>=3) cout << "Memory used: " << mem_used () << " bytes\n";
  }
}

/*
void*
fast_alloc (register size_t s) {
  register void* ptr;
  s= (s+ WORD_LENGTH+ WORD_LENGTH_INC)&WORD_MASK;
  if (s<MAX_FAST) {
    ptr= alloc_ptr(s);
    if (ptr==NULL) ptr= enlarge_malloc (s);
    else alloc_ptr(s)= ind(ptr);
  }
  else {
    if (MEM_DEBUG>=3) cout << "Big alloc of " << s << " bytes\n";
    if (MEM_DEBUG>=3) cout << "Memory used: " << mem_used () << " bytes\n";
    ptr= safe_malloc (s);
    large_uses += s;
  }
  *((size_t *) ptr)=s;
  return (void*) (((char*) ptr)+ WORD_LENGTH);
}

void
fast_free (register void* ptr, register size_t sz) {
  ptr= (void*) (((char*) ptr)- WORD_LENGTH);
  register size_t s= *((size_t *) ptr);
  sz= (sz+ WORD_LENGTH+ WORD_LENGTH_INC)&WORD_MASK;
  if (s != sz) {
    cerr << "At " << ptr << ": " << sz << " -> " << s << "\n";
    assert (s == sz);
  }
  if (s<MAX_FAST) {
    ind(ptr)    = alloc_ptr(s);
    alloc_ptr(s)= ptr;
  }
  else {
    if (MEM_DEBUG>=3) cout << "Big free of " << s << " bytes\n";
    free (ptr);
    large_uses -= s;
    if (MEM_DEBUG>=3) cout << "Memory used: " << mem_used () << " bytes\n";
  }
}
*/

void*
fast_new (register size_t s) {
  register void* ptr;
  s= (s+ WORD_LENGTH+ WORD_LENGTH_INC)&WORD_MASK;
  if (s<MAX_FAST) {
    ptr= alloc_ptr(s);
    if (ptr==NULL) ptr= enlarge_malloc (s);
    else alloc_ptr(s)= ind(ptr);
  }
  else {
    if (MEM_DEBUG>=3) cout << "Big alloc of " << s << " bytes\n";
    if (MEM_DEBUG>=3) cout << "Memory used: " << mem_used () << " bytes\n";
    ptr= safe_malloc (s);
    //if ((((int) ptr) & 15) != 0) cout << "Unaligned new " << ptr << "\n";
    large_uses += s;
  }
  *((size_t *) ptr)=s;
  return (void*) (((char*) ptr)+ WORD_LENGTH);
}

void
fast_delete (register void* ptr) {
  ptr= (void*) (((char*) ptr)- WORD_LENGTH);
  register size_t s= *((size_t *) ptr);
  if (s<MAX_FAST) {
    ind(ptr)    = alloc_ptr(s);
    alloc_ptr(s)= ptr;
  }
  else {
    if (MEM_DEBUG>=3) cout << "Big free of " << s << " bytes\n";
    //if ((((int) ptr) & 15) != 0) cout << "Unaligned delete " << ptr << "\n";
    free (ptr);
    large_uses -= s;
    if (MEM_DEBUG>=3) cout << "Memory used: " << mem_used () << " bytes\n";
  }
}

/******************************************************************************
* Fast allocation of blocks whose sizes are small multiples of WORD_LENGTH
******************************************************************************/

void*
fast_alloc_mw (register size_t s)
{
  if (s<MAX_FAST) {
    register void *ptr= alloc_ptr(s);
    if (ptr==NULL) return enlarge_malloc (s);
    alloc_ptr(s)= ind(ptr);
    return ptr;
  }
  else return safe_malloc (s);
}

void
fast_free_mw (register void* ptr, register size_t s)
{
  if (s<MAX_FAST) {
    ind(ptr)    = alloc_ptr(s);
    alloc_ptr(s)= ptr;
  }
  else free (ptr);
}

/******************************************************************************
* Statistics
******************************************************************************/

int
compute_free (void* ptr) {
  int i=-1;
  while (ptr!=NULL) {
    i++;
    ptr= ind (ptr);
  }
  return i;
}

int
mem_used () {
  int free_bytes= alloc_remains;
  int chunks_use= BLOCK_SIZE*fast_chunks;
  int i;
  for (i=WORD_LENGTH; i<MAX_FAST; i+=WORD_LENGTH)
    free_bytes += i*compute_free (alloc_table+i);
  int small_uses= chunks_use- free_bytes;
  return small_uses+ large_uses;
}

void
mem_info () {
  cout << "\n---------------- memory statistics ----------------\n";
  int free_bytes= alloc_remains;
  int chunks_use= BLOCK_SIZE*fast_chunks;
  int i;
  for (i=WORD_LENGTH; i<MAX_FAST; i+=WORD_LENGTH)
    free_bytes += i*compute_free (alloc_table+i);
  int small_uses= chunks_use- free_bytes;
  int total_uses= small_uses+ large_uses;
  // cout << "Fast chunks   : " << chunks_use << " bytes\n";
  // cout << "Free on chunks: " << alloc_remains << " bytes\n";
  cout << "User          : " << total_uses << " bytes\n";
  cout << "Allocator     : " << chunks_use+ large_uses << " bytes\n";
  cout << "Small mallocs : "
       << ((100*((float) small_uses))/((float) total_uses)) << "%\n";
}

/******************************************************************************
* Redefine standard new and delete
******************************************************************************/

#if defined(X11TEXMACS) && (!defined(NO_FAST_ALLOC))

void*
operator new (register size_t s) {
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
operator delete (register void* ptr) {
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
operator new[] (register size_t s) {
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
operator delete[] (register void* ptr) {
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

#endif // defined(X11TEXMACS) && (!defined(NO_FAST_ALLOC))
