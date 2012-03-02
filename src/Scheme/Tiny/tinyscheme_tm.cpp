
/******************************************************************************
 * MODULE     : tinyscheme_tm.cpp
 * DESCRIPTION: TinyScheme interface
 * COPYRIGHT  : (C) 2011 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tinyscheme_tm.hpp"
#include "object.hpp"
#include "glue.hpp"




#define TST_BLACKBOX   17


scheme* the_scheme = scheme_init_new();


void finalize_blackbox(void *p) {
	tm_delete((blackbox*)p);
}



/******************************************************************************
 * Entry points to Scheme
 ******************************************************************************/


scm scm_eval_string (const char *str) 
{
	scheme_load_string(the_scheme, (char*)str);
	return the_scheme->value;
}

scm scm_eval_file (FILE *f) 
{
	scheme_load_file(the_scheme,f);
	return the_scheme->value;
}

scm scm_apply (scm func, scm args) 
{
	scheme_call(the_scheme, func, args);
	return the_scheme->value;
}


scm scm_lookup_string(const char *name)
{
	return scheme_eval(the_scheme, mk_symbol(the_scheme, name));
}

void scm_define(scm symbol, scm value)
{
	scheme_define(the_scheme, the_scheme->global_env, symbol, value);
}

scm object_stack;

/******************************************************************************
 * Installation of guile and initialization of guile
 ******************************************************************************/

void
start_scheme (int argc, char** argv, void (*call_back) (int, char**)) {
	// gh_enter (argc, argv, call_back);
	call_back(argc, argv);
	
}

void
initialize_scheme () {
	if(!scheme_init(the_scheme)) {
		cout << "Could not initialize TinyScheme" << LF;
	}
	scheme_set_output_port_file(the_scheme, stdout);
	scheme_set_input_port_file(the_scheme, stdin);

	const char* init_prg =
#if 0
	"(read-set! keywords 'prefix)\n"
	"(read-enable 'positions)\n"
	"(debug-enable 'debug)\n"
	";(debug-enable 'backtrace)\n"
	"\n"
#endif
	"(define (display-to-string obj)\n"
	"  (call-with-output-string\n"
	"    (lambda (port) (display obj port))))\n"
	"(define (object->string obj)\n"
	"  (call-with-output-string\n"
	"    (lambda (port) (write obj port))))\n"
	"\n"
	"(define (texmacs-version) \"" TEXMACS_VERSION "\")\n"
	"(define object-stack '(()))";
	
	scm_eval_string (init_prg);
	initialize_glue ();
	object_stack= scm_lookup_string ("object-stack");
	
	
	scm_eval_string("(load (url-concretize \"$TEXMACS_PATH/progs/init-tinyscheme.scm\"))");
	scm_eval_string("(load (url-concretize \"$TEXMACS_PATH/progs/init-scheme-tm.scm\"))");
	
	//REPL
	//scm_eval_file (stdin);
	scheme_load_named_file(the_scheme,stdin,0);

}

#if 0
/******************************************************************************
 * Catching errors (with thanks to Dale P. Smith)
 ******************************************************************************/

scm
TeXmacs_lazy_catcher (void *data, scm tag, scm throw_args) {
	scm eport= scm_current_error_port();
	scm_handle_by_message_noexit (data, tag, throw_args);
	scm_force_output (eport);
	scm_ithrow (tag, throw_args, 1);
	return scm_UNSPECIFIED; /* never returns */
}

scm
TeXmacs_catcher (void *data, scm tag, scm args) {
	(void) data;
	return scm_cons (tag, args);
}
#endif

/******************************************************************************
 * Evaluation of files
 ******************************************************************************/
#if 0
static scm
TeXmacs_lazy_eval_file (char *file) {
	
	return scm_internal_lazy_catch (scm_BOOL_T,
									(scm_t_catch_body) scm_c_primitive_load, file,
									(scm_t_catch_handler) TeXmacs_lazy_catcher, file);
}

static scm
TeXmacs_eval_file (char *file) {
	return scm_internal_catch (scm_BOOL_T,
							   (scm_t_catch_body) TeXmacs_lazy_eval_file, file,
							   (scm_t_catch_handler) TeXmacs_catcher, file);
}
#endif 
scm
eval_scheme_file (string file) {
	//static int cumul= 0;
	//timer tm;
	if (DEBUG_STD) cout << "TeXmacs] Evaluating " << file << "...\n";
	char* _file= as_charp (file);
	FILE *f = fopen(_file, "r");
	scm result= scm_eval_file (f);
	fclose(f);
	tm_delete_array (_file);
	//int extra= tm->watch (); cumul += extra;
	//cout << extra << "\t" << cumul << "\t" << file << "\n";
	return result;
}

/******************************************************************************
 * Evaluation of strings
 ******************************************************************************/
#if 0
static scm
TeXmacs_lazy_eval_string (char *s) {
	return scm_internal_lazy_catch (scm_BOOL_T,
									(scm_t_catch_body) scm_c_eval_string, s,
									(scm_t_catch_handler) TeXmacs_lazy_catcher, s);
}

static scm
TeXmacs_eval_string (char *s) {
	return scm_internal_catch (scm_BOOL_T,
							   (scm_t_catch_body) TeXmacs_lazy_eval_string, s,
							   (scm_t_catch_handler) TeXmacs_catcher, s);
}
#endif
scm
eval_scheme (string s) {
	// cout << "Eval] " << s << "\n";
	char* _s= as_charp (s);
	scm result= scm_eval_string (_s);
	tm_delete_array (_s);
	return result;
}

/******************************************************************************
 * Using scheme objects as functions
 ******************************************************************************/

struct arg_list { int  n; scm* a; };

scm
TeXmacs_call_scm (arg_list* args) {
	switch (args->n) {
		default:
		{
			int i;
			scm l= scm_null ();
			for (i=args->n; i>=1; i--)
				l= scm_cons (args->a[i], l);
			return scm_apply (args->a[0], l);
		}
	}
}
#if 0
static scm
TeXmacs_lazy_call_scm (arg_list* args) {
	return scm_internal_lazy_catch (scm_BOOL_T,
									(scm_t_catch_body) TeXmacs_call, (void*) args,
									(scm_t_catch_handler) TeXmacs_lazy_catcher, (void*) args);
}

static scm
TeXmacs_call_scm (arg_list *args) {
	return scm_internal_catch (scm_BOOL_T,
							   (scm_t_catch_body) TeXmacs_lazy_call_scm, (void*) args,
							   (scm_t_catch_handler) TeXmacs_catcher, (void*) args);
}
#endif
scm
call_scheme (scm fun) {
	scm a[]= { fun }; arg_list args= { 0, a };
	return TeXmacs_call_scm (&args);
}

scm
call_scheme (scm fun, scm a1) {
	scm a[]= { fun, a1 }; arg_list args= { 1, a };
	return TeXmacs_call_scm (&args);
}

scm
call_scheme (scm fun, scm a1, scm a2) {
	scm a[]= { fun, a1, a2 }; arg_list args= { 2, a };
	return TeXmacs_call_scm (&args);
}

scm
call_scheme (scm fun, scm a1, scm a2, scm a3) {
	scm a[]= { fun, a1, a2, a3 }; arg_list args= { 3, a };
	return TeXmacs_call_scm (&args);
}

scm
call_scheme (scm fun, scm a1, scm a2, scm a3, scm a4) {
	scm a[]= { fun, a1, a2, a3, a4 }; arg_list args= { 4, a };
	return TeXmacs_call_scm (&args);
}

scm
call_scheme (scm fun, array<scm> a) {
	const int n= N(a);
	STACK_NEW_ARRAY(v, scm, n+1);
	int i;
	v[0]= fun;
	for (i=0; i<n; i++) v[i+1]= a[i];
	arg_list args= { n, v };
	scm ret= TeXmacs_call_scm (&args);
	STACK_DELETE_ARRAY(scm);
	return ret;
}


/******************************************************************************
 * Gluing
 ******************************************************************************/


string
scheme_dialect () {
	return "littlescheme";
}

void scm_define_glue(const char *name, scm_foreign_func f)
{
	//  cout << "Define glue: " << name << LF;
	scm_define(symbol_to_scm(name), mk_foreign_func (the_scheme, f));
}




/******************************************************************************
 * Strings
 ******************************************************************************/

scm
string_to_scm (string s) {
	char* _s= as_charp (s);
	scm r= mk_counted_string (the_scheme,_s, N(s));
	tm_delete_array (_s);
	return r;
}

/******************************************************************************
 * Symbols
 ******************************************************************************/

scm
symbol_to_scm (string s) {
	char* _s= as_charp (s);
	scm r= mk_symbol (the_scheme,_s);
	tm_delete_array (_s);
	return r;
}

