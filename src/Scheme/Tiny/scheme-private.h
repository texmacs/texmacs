/* scheme-private.h */

#ifndef _SCHEME_PRIVATE_H
#define _SCHEME_PRIVATE_H

#include "scheme.h"
/*------------------ Ugly internals -----------------------------------*/
/*------------------ Of interest only to FFI users --------------------*/

#ifdef __cplusplus
extern "C" {
#endif

enum scheme_port_kind {
  port_free=0,
  port_file=1,
  port_string=2,
  port_srfi6=4,
  port_input=16,
  port_output=32,
  port_saw_EOF=64,
};

typedef struct port {
  unsigned char kind;
  union {
    struct {
      FILE *file;
      int closeit;
#if SHOW_ERROR_LINE
      int curr_line;
      char *filename;
#endif
    } stdio;
    struct {
      char *start;
      char *past_the_end;
      char *curr;
    } string;
  } rep;
} port;

/* cell structure */
struct cell {
  unsigned int _flag;
  union {
    struct {
      char   *_svalue;
      int   _length;
    } _string;
    num _number;
    port *_port;
    foreign_func _ff;
    struct {
      struct cell *_car;
      struct cell *_cdr;
    } _cons;
  } _object;
};

struct scheme {
/* arrays for segments */
func_alloc malloc;
func_dealloc free;

/* return code */
int retcode;
int tracing;


#define CELL_SEGSIZE    5000  /* # of cells in one segment */
#define CELL_NSEGMENT   10    /* # of segments for cells */
char *alloc_seg[CELL_NSEGMENT];
cell_ptr cell_seg[CELL_NSEGMENT];
int     last_cell_seg;

/* We use 4 registers. */
cell_ptr args;            /* register for arguments of function */
cell_ptr envir;           /* stack register for current environment */
cell_ptr code;            /* register for current code */
cell_ptr dump;            /* stack register for next evaluation */

int interactive_repl;    /* are we in an interactive REPL? */

struct cell _sink;
cell_ptr sink;            /* when mem. alloc. fails */
struct cell _NIL;
cell_ptr NIL;             /* special cell representing empty cell */
struct cell _HASHT;
cell_ptr T;               /* special cell representing #t */
struct cell _HASHF;
cell_ptr F;               /* special cell representing #f */
struct cell _EOF_OBJ;
cell_ptr EOF_OBJ;         /* special cell representing end-of-file object */
cell_ptr oblist;          /* pointer to symbol table */
cell_ptr global_env;      /* pointer to global environment */
cell_ptr c_nest;          /* stack for nested calls from C */

/* global pointers to special symbols */
cell_ptr LAMBDA;               /* pointer to syntax lambda */
cell_ptr QUOTE;           /* pointer to syntax quote */

cell_ptr QQUOTE;               /* pointer to symbol quasiquote */
cell_ptr UNQUOTE;         /* pointer to symbol unquote */
cell_ptr UNQUOTESP;       /* pointer to symbol unquote-splicing */
cell_ptr FEED_TO;         /* => */
cell_ptr COLON_HOOK;      /* *colon-hook* */
cell_ptr ERROR_HOOK;      /* *error-hook* */
cell_ptr SHARP_HOOK;  /* *sharp-hook* */
cell_ptr COMPILE_HOOK;  /* *compile-hook* */

cell_ptr free_cell;       /* pointer to top of free cells */
long    fcells;          /* # of free cells */

cell_ptr inport;
cell_ptr outport;
cell_ptr save_inport;
cell_ptr loadport;

#define MAXFIL 64
port load_stack[MAXFIL];     /* Stack of open files for port -1 (LOADing) */
int nesting_stack[MAXFIL];
int file_i;
int nesting;

char    gc_verbose;      /* if gc_verbose is not zero, print gc status */
char    no_memory;       /* Whether mem. alloc. has failed */

#define LINESIZE 1024
char    linebuff[LINESIZE];
#define STRBUFFSIZE 256
char    strbuff[STRBUFFSIZE];

FILE *tmpfp;
int tok;
int print_flag;
cell_ptr value;
int op;

void *ext_data;     /* For the benefit of foreign functions */
long gensym_cnt;

struct scheme_interface *vptr;
void *dump_base;	 /* pointer to base of allocated dump stack */
int dump_size;		 /* number of frames allocated for dump stack */
};

/* operator code */
enum scheme_opcodes {
#define _OP_DEF(A,B,C,D,E,OP) OP,
#include "opdefines.h"
  OP_MAXDEFINED
};


#define cons(sc,a,b) _cons(sc,a,b,0)
#define immutable_cons(sc,a,b) _cons(sc,a,b,1)

int is_string(cell_ptr p);
char *string_value(cell_ptr p);
int string_length(cell_ptr p);
int is_number(cell_ptr p);
num nvalue(cell_ptr p);
long ivalue(cell_ptr p);
double rvalue(cell_ptr p);
int is_integer(cell_ptr p);
int is_real(cell_ptr p);
int is_character(cell_ptr p);
long charvalue(cell_ptr p);
int is_vector(cell_ptr p);
int is_blackbox(cell_ptr p);
void *blackboxvalue(cell_ptr p);
	
int is_port(cell_ptr p);

int is_pair(cell_ptr p);
cell_ptr pair_car(cell_ptr p);
cell_ptr pair_cdr(cell_ptr p);
cell_ptr set_car(cell_ptr p, cell_ptr q);
cell_ptr set_cdr(cell_ptr p, cell_ptr q);

int is_symbol(cell_ptr p);
char *symname(cell_ptr p);
int symlen(cell_ptr p);
int hasprop(cell_ptr p);

int is_syntax(cell_ptr p);
int is_proc(cell_ptr p);
int is_foreign(cell_ptr p);
char *syntaxname(cell_ptr p);
int is_closure(cell_ptr p);
#ifdef USE_MACRO
int is_macro(cell_ptr p);
#endif
cell_ptr closure_code(cell_ptr p);
cell_ptr closure_env(cell_ptr p);

int is_continuation(cell_ptr p);
int is_promise(cell_ptr p);
int is_environment(cell_ptr p);
int is_immutable(cell_ptr p);
void setimmutable(cell_ptr p);

#ifdef __cplusplus
}
#endif

#endif

/*
Local variables:
c-file-style: "k&r"
End:
*/
