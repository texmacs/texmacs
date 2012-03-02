/* SCHEME.H */

#ifndef _SCHEME_H
#define _SCHEME_H

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Default values for #define'd symbols
 */
#ifndef STANDALONE       /* If used as standalone interpreter */
# define STANDALONE 1
#endif

#ifndef _MSC_VER
# define USE_STRCASECMP 1
# ifndef USE_STRLWR
#   define USE_STRLWR 1
# endif
# define SCHEME_EXPORT
#else
# define USE_STRCASECMP 0
# define USE_STRLWR 0
# ifdef _SCHEME_SOURCE
#  define SCHEME_EXPORT __declspec(dllexport)
# else
#  define SCHEME_EXPORT __declspec(dllimport)
# endif
#endif

#if USE_NO_FEATURES
# define USE_MATH 0
# define USE_CHAR_CLASSIFIERS 0
# define USE_ASCII_NAMES 0
# define USE_STRING_PORTS 0
# define USE_ERROR_HOOK 0
# define USE_TRACING 0
# define USE_COLON_HOOK 0
# define USE_DL 0
# define USE_PLIST 0
#endif

/*
 * Leave it defined if you want continuations, and also for the Sharp Zaurus.
 * Undefine it if you only care about faster speed and not strict Scheme compatibility.
 */
#define USE_SCHEME_STACK

#if USE_DL
# define USE_INTERFACE 1
#endif


#ifndef USE_MATH         /* If math support is needed */
# define USE_MATH 1
#endif

#ifndef USE_CHAR_CLASSIFIERS  /* If char classifiers are needed */
# define USE_CHAR_CLASSIFIERS 1
#endif

#ifndef USE_ASCII_NAMES  /* If extended escaped characters are needed */
# define USE_ASCII_NAMES 1
#endif

#ifndef USE_STRING_PORTS      /* Enable string ports */
# define USE_STRING_PORTS 1
#endif

#ifndef USE_TRACING
# define USE_TRACING 1
#endif

#ifndef USE_PLIST
# define USE_PLIST 0
#endif

/* To force system errors through user-defined error handling (see *error-hook*) */
#ifndef USE_ERROR_HOOK
# define USE_ERROR_HOOK 1
#endif

#ifndef USE_COLON_HOOK   /* Enable qualified qualifier */
# define USE_COLON_HOOK 1
#endif

#ifndef USE_STRCASECMP   /* stricmp for Unix */
# define USE_STRCASECMP 0
#endif

#ifndef USE_STRLWR
# define USE_STRLWR 1
#endif

#ifndef STDIO_ADDS_CR    /* Define if DOS/Windows */
# define STDIO_ADDS_CR 0
#endif

#ifndef INLINE
# define INLINE
#endif

#ifndef USE_INTERFACE
# define USE_INTERFACE 0
#endif

#ifndef SHOW_ERROR_LINE   /* Show error line in file */
# define SHOW_ERROR_LINE 1
#endif

typedef struct scheme scheme;
typedef struct cell *cell_ptr;

typedef void * (*func_alloc)(size_t);
typedef void (*func_dealloc)(void *);

/* num, for generic arithmetic */
typedef struct num {
     char is_fixnum;
     union {
          long ivalue;
          double rvalue;
     } value;
} num;

SCHEME_EXPORT scheme *scheme_init_new();
SCHEME_EXPORT scheme *scheme_init_new_custom_alloc(func_alloc malloc, func_dealloc free);
SCHEME_EXPORT int scheme_init(scheme *sc);
SCHEME_EXPORT int scheme_init_custom_alloc(scheme *sc, func_alloc, func_dealloc);
SCHEME_EXPORT void scheme_deinit(scheme *sc);
void scheme_set_input_port_file(scheme *sc, FILE *fin);
void scheme_set_input_port_string(scheme *sc, char *start, char *past_the_end);
SCHEME_EXPORT void scheme_set_output_port_file(scheme *sc, FILE *fin);
void scheme_set_output_port_string(scheme *sc, char *start, char *past_the_end);
SCHEME_EXPORT void scheme_load_file(scheme *sc, FILE *fin);
SCHEME_EXPORT void scheme_load_named_file(scheme *sc, FILE *fin, const char *filename);
SCHEME_EXPORT void scheme_load_string(scheme *sc, const char *cmd);
SCHEME_EXPORT cell_ptr scheme_apply0(scheme *sc, const char *procname);
SCHEME_EXPORT cell_ptr scheme_call(scheme *sc, cell_ptr func, cell_ptr args);
SCHEME_EXPORT cell_ptr scheme_eval(scheme *sc, cell_ptr obj);
void scheme_set_external_data(scheme *sc, void *p);
SCHEME_EXPORT void scheme_define(scheme *sc, cell_ptr env, cell_ptr symbol, cell_ptr value);

typedef cell_ptr (*foreign_func)(scheme *, cell_ptr);

cell_ptr _cons(scheme *sc, cell_ptr a, cell_ptr b, int immutable);
cell_ptr mk_integer(scheme *sc, long num);
cell_ptr mk_real(scheme *sc, double num);
cell_ptr mk_symbol(scheme *sc, const char *name);
cell_ptr gensym(scheme *sc);
cell_ptr mk_string(scheme *sc, const char *str);
cell_ptr mk_counted_string(scheme *sc, const char *str, int len);
cell_ptr mk_empty_string(scheme *sc, int len, char fill);
cell_ptr mk_character(scheme *sc, int c);
cell_ptr mk_foreign_func(scheme *sc, foreign_func f);
cell_ptr mk_blackbox(scheme *sc, void* b);
void putstr(scheme *sc, const char *s);
int list_length(scheme *sc, cell_ptr a);
int eqv(cell_ptr a, cell_ptr b);

void finalize_blackbox(void *p);
	
#if USE_INTERFACE
struct scheme_interface {
  void (*scheme_define)(scheme *sc, cell_ptr env, cell_ptr symbol, cell_ptr value);
  cell_ptr (*cons)(scheme *sc, cell_ptr a, cell_ptr b);
  cell_ptr (*immutable_cons)(scheme *sc, cell_ptr a, cell_ptr b);
  cell_ptr (*reserve_cells)(scheme *sc, int n);
  cell_ptr (*mk_integer)(scheme *sc, long num);
  cell_ptr (*mk_real)(scheme *sc, double num);
  cell_ptr (*mk_symbol)(scheme *sc, const char *name);
  cell_ptr (*gensym)(scheme *sc);
  cell_ptr (*mk_string)(scheme *sc, const char *str);
  cell_ptr (*mk_counted_string)(scheme *sc, const char *str, int len);
  cell_ptr (*mk_character)(scheme *sc, int c);
  cell_ptr (*mk_vector)(scheme *sc, int len);
  cell_ptr (*mk_foreign_func)(scheme *sc, foreign_func f);
  void (*putstr)(scheme *sc, const char *s);
  void (*putcharacter)(scheme *sc, int c);

  int (*is_string)(cell_ptr p);
  char *(*string_value)(cell_ptr p);
  int (*is_number)(cell_ptr p);
  num (*nvalue)(cell_ptr p);
  long (*ivalue)(cell_ptr p);
  double (*rvalue)(cell_ptr p);
  int (*is_integer)(cell_ptr p);
  int (*is_real)(cell_ptr p);
  int (*is_character)(cell_ptr p);
  long (*charvalue)(cell_ptr p);
  int (*is_list)(scheme *sc, cell_ptr p);
  int (*is_vector)(cell_ptr p);
  int (*list_length)(scheme *sc, cell_ptr vec);
  long (*vector_length)(cell_ptr vec);
  void (*fill_vector)(cell_ptr vec, cell_ptr elem);
  cell_ptr (*vector_elem)(cell_ptr vec, int ielem);
  cell_ptr (*set_vector_elem)(cell_ptr vec, int ielem, cell_ptr newel);
  int (*is_port)(cell_ptr p);

  int (*is_pair)(cell_ptr p);
  cell_ptr (*pair_car)(cell_ptr p);
  cell_ptr (*pair_cdr)(cell_ptr p);
  cell_ptr (*set_car)(cell_ptr p, cell_ptr q);
  cell_ptr (*set_cdr)(cell_ptr p, cell_ptr q);

  int (*is_symbol)(cell_ptr p);
  char *(*symname)(cell_ptr p);

  int (*is_syntax)(cell_ptr p);
  int (*is_proc)(cell_ptr p);
  int (*is_foreign)(cell_ptr p);
  char *(*syntaxname)(cell_ptr p);
  int (*is_closure)(cell_ptr p);
  int (*is_macro)(cell_ptr p);
  cell_ptr (*closure_code)(cell_ptr p);
  cell_ptr (*closure_env)(cell_ptr p);

  int (*is_continuation)(cell_ptr p);
  int (*is_promise)(cell_ptr p);
  int (*is_environment)(cell_ptr p);
  int (*is_immutable)(cell_ptr p);
  void (*setimmutable)(cell_ptr p);
  void (*load_file)(scheme *sc, FILE *fin);
  void (*load_string)(scheme *sc, const char *input);
};
#endif

#if !STANDALONE
typedef struct scheme_registerable
{
  foreign_func  f;
  char *        name;
}
scheme_registerable;

void scheme_register_foreign_func_list(scheme * sc,
                                       scheme_registerable * list,
                                       int n);

#endif /* !STANDALONE */

#ifdef __cplusplus
}
#endif

#endif


/*
Local variables:
c-file-style: "k&r"
End:
*/
