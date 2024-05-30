/* readline.c --- line editing support for Guile */

/* Copyright (C) 1997,1999,2000,2001, 2002, 2003, 2006, 2007, 2008 Free Software Foundation, Inc.
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
 *
 */




/* Include private, configure generated header (i.e. config.h). */
#include "guile-readline-config.h"

#ifdef HAVE_RL_GETC_FUNCTION
#include "libguile.h"
#include "libguile/gh.h"
#include "libguile/iselect.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <readline/readline.h>
#include <readline/history.h>
#ifndef __MINGW32__
#include <sys/time.h>
#else
#include <io.h>
#endif
#include <signal.h>

#include "libguile/validate.h"
#include "guile-readline/readline.h"

scm_t_option scm_readline_opts[] = {
  { SCM_OPTION_BOOLEAN, "history-file", 1,
    "Use history file." },
  { SCM_OPTION_INTEGER, "history-length", 200,
    "History length." },
  { SCM_OPTION_INTEGER, "bounce-parens", 500,
    "Time (ms) to show matching opening parenthesis (0 = off)."}
};

extern void stifle_history (int max);

SCM_DEFINE (scm_readline_options, "readline-options-interface", 0, 1, 0, 
            (SCM setting),
"")
#define FUNC_NAME s_scm_readline_options
{
  SCM ans = scm_options (setting,
			 scm_readline_opts,
			 SCM_N_READLINE_OPTIONS,
			 FUNC_NAME);
  stifle_history (SCM_HISTORY_LENGTH);
  return ans;
}
#undef FUNC_NAME

#ifndef HAVE_STRDUP
static char *
strdup (char *s)
{
  size_t len = strlen (s);
  char *new = malloc (len + 1);
  strcpy (new, s);
  return new;
}
#endif /* HAVE_STRDUP */

#ifndef HAVE_RL_CLEANUP_AFTER_SIGNAL

/* These are readline functions added in release 2.3.  They will work
 * together with readline-2.1 and 2.2.  (The readline interface is
 * disabled for earlier releases.)
 * They are declared static; if we want to use them elsewhere, then
 * we need external declarations for them, but at the moment, I don't
 * think anything else in Guile ought to use these.
 */

extern void _rl_clean_up_for_exit ();
extern void _rl_kill_kbd_macro ();
extern int _rl_init_argument ();

void
rl_cleanup_after_signal ()
{
#ifdef HAVE_RL_CLEAR_SIGNALS
  _rl_clean_up_for_exit ();
#endif
  (*rl_deprep_term_function) ();
#ifdef HAVE_RL_CLEAR_SIGNALS
  rl_clear_signals ();
#endif
  rl_pending_input = 0;
}

void
rl_free_line_state ()
{
  register HIST_ENTRY *entry;
   
  free_undo_list ();

  entry = current_history ();
  if (entry)
    entry->data = (char *)NULL; 
  
  _rl_kill_kbd_macro ();
  rl_clear_message ();
  _rl_init_argument ();
}

#endif /* !HAVE_RL_CLEANUP_AFTER_SIGNAL */

static int promptp;
static SCM input_port;
static SCM before_read;

static int
current_input_getc (FILE *in SCM_UNUSED)
{
  if (promptp && scm_is_true (before_read))
    {
      scm_apply (before_read, SCM_EOL, SCM_EOL);
      promptp = 0;
    }
  return scm_getc (input_port);
}

static int in_readline = 0;
static SCM reentry_barrier_mutex;

static SCM internal_readline (SCM text);
static SCM handle_error (void *data, SCM tag, SCM args);
static void reentry_barrier (void);


SCM_DEFINE (scm_readline, "%readline", 0, 4, 0, 
            (SCM text, SCM inp, SCM outp, SCM read_hook),
"")
#define FUNC_NAME s_scm_readline
{
  SCM ans;
  
  reentry_barrier ();
  
  before_read = SCM_BOOL_F;

  if (!SCM_UNBNDP (text))
    {
      if (!scm_is_string (text))
	{
	  --in_readline;
	  scm_wrong_type_arg (s_scm_readline, SCM_ARG1, text);
	}
    }
  
  if (!((SCM_UNBNDP (inp) && SCM_OPINFPORTP (scm_current_input_port ()))
	|| SCM_OPINFPORTP (inp)))
    {
      --in_readline;
      scm_misc_error (s_scm_readline,
		      "Input port is not open or not a file port",
		      SCM_EOL);
    }
  
  if (!((SCM_UNBNDP (outp) && SCM_OPOUTFPORTP (scm_current_output_port ()))
	|| SCM_OPOUTFPORTP (outp)))
    {
      --in_readline;
      scm_misc_error (s_scm_readline,
		      "Output port is not open or not a file port",
		      SCM_EOL);
    }

  if (!(SCM_UNBNDP (read_hook) || scm_is_false (read_hook)))
    {
      if (scm_is_false (scm_thunk_p (read_hook)))
	{
	  --in_readline;
	  scm_wrong_type_arg (s_scm_readline, SCM_ARG4, read_hook);
	}
      before_read = read_hook;
    }

  scm_readline_init_ports (inp, outp);

  ans = scm_internal_catch (SCM_BOOL_T,
			    (scm_t_catch_body) internal_readline,
			    (void *) SCM_UNPACK (text),
			    handle_error, 0);

#ifndef __MINGW32__
  fclose (rl_instream);
  fclose (rl_outstream);
#endif

  --in_readline;
  return ans;
}
#undef FUNC_NAME


static void
reentry_barrier ()
{
  int reentryp = 0;
  /* We should rather use scm_try_mutex when it becomes available */
  scm_lock_mutex (reentry_barrier_mutex);
  if (in_readline)
    reentryp = 1;
  else
    ++in_readline;
  scm_unlock_mutex (reentry_barrier_mutex);
  if (reentryp)
    scm_misc_error (s_scm_readline, "readline is not reentrant", SCM_EOL);
}

static SCM
handle_error (void *data, SCM tag, SCM args)
{
  rl_free_line_state ();
  rl_cleanup_after_signal ();
  fputc ('\n', rl_outstream); /* We don't want next output on this line */
#ifndef __MINGW32__
  fclose (rl_instream);
  fclose (rl_outstream);
#endif
  --in_readline;
  scm_handle_by_throw (data, tag, args);
  return SCM_UNSPECIFIED; /* never reached */
}

static SCM
internal_readline (SCM text)
{
  SCM ret;
  char *s;
  char *prompt = SCM_UNBNDP (text) ? "" : scm_to_locale_string (text);

  promptp = 1;
  s = readline (prompt);
  if (s)
    ret = scm_from_locale_string (s);
  else 
    ret = SCM_EOF_VAL;

  if (!SCM_UNBNDP (text))
    free (prompt);
  free (s);

  return ret;
}

static FILE *
stream_from_fport (SCM port, char *mode, const char *subr)
{
  int fd;
  FILE *f;

  fd = dup (((struct scm_t_fport *) SCM_STREAM (port))->fdes);
  if (fd == -1)
    {
      --in_readline;
      scm_syserror (subr);
    }

  f = fdopen (fd, mode);
  if (f == NULL)
    {
      --in_readline;
      scm_syserror (subr);
    }

  return f;
}

void
scm_readline_init_ports (SCM inp, SCM outp)
{
  if (SCM_UNBNDP (inp))
    inp = scm_current_input_port ();
  
  if (SCM_UNBNDP (outp))
    outp = scm_current_output_port ();
  
  if (!SCM_OPINFPORTP (inp)) {
    scm_misc_error (0,
                    "Input port is not open or not a file port",
                    SCM_EOL);
  }

  if (!SCM_OPOUTFPORTP (outp)) {
    scm_misc_error (0,
                    "Output port is not open or not a file port",
                    SCM_EOL);
  }

  input_port = inp;
#ifndef __MINGW32__
  rl_instream = stream_from_fport (inp, "r", s_scm_readline);
  rl_outstream = stream_from_fport (outp, "w", s_scm_readline);
#endif
}



SCM_DEFINE (scm_add_history, "add-history", 1, 0, 0, 
            (SCM text),
"")
#define FUNC_NAME s_scm_add_history
{
  char* s;

  s = scm_to_locale_string (text);
  add_history (s);
  free (s);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_read_history, "read-history", 1, 0, 0, 
            (SCM file),
"")
#define FUNC_NAME s_scm_read_history
{
  char *filename;
  SCM ret;

  filename = scm_to_locale_string (file);
  ret = scm_from_bool (!read_history (filename));
  free (filename);
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_write_history, "write-history", 1, 0, 0, 
            (SCM file),
"")
#define FUNC_NAME s_scm_write_history
{
  char *filename;
  SCM ret;

  filename = scm_to_locale_string (file);
  ret = scm_from_bool (!write_history (filename));
  free (filename);
  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_clear_history, "clear-history", 0, 0, 0, 
            (),
	    "Clear the history buffer of the readline machinery.")
#define FUNC_NAME s_scm_clear_history
{
  clear_history();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_filename_completion_function, "filename-completion-function", 2, 0, 0, 
            (SCM text, SCM continuep),
"")
#define FUNC_NAME s_scm_filename_completion_function
{
  char *s;
  SCM ans;
  char *c_text = scm_to_locale_string (text);
#ifdef HAVE_RL_FILENAME_COMPLETION_FUNCTION
  s = rl_filename_completion_function (c_text, scm_is_true (continuep));
#else
  s = filename_completion_function (c_text, scm_is_true (continuep));
#endif
  ans = scm_take_locale_string (s);
  free (c_text);
  return ans;
}
#undef FUNC_NAME

/*
 * The following has been modified from code contributed by
 * Andrew Archibald <aarchiba@undergrad.math.uwaterloo.ca>
 */

SCM scm_readline_completion_function_var;

static char *
completion_function (char *text, int continuep)
{
  SCM compfunc = SCM_VARIABLE_REF (scm_readline_completion_function_var);
  SCM res;

  if (scm_is_false (compfunc))
    return NULL; /* #f => completion disabled */
  else
    {
      SCM t = scm_from_locale_string (text);
      SCM c = scm_from_bool (continuep);
      res = scm_apply (compfunc, scm_list_2 (t, c), SCM_EOL);
  
      if (scm_is_false (res))
	return NULL;
  
      return scm_to_locale_string (res);
    }
}

#if HAVE_RL_GET_KEYMAP
/*Bouncing parenthesis (reimplemented by GH, 11/23/98, since readline is strict gpl)*/

static int match_paren (int x, int k);
static int find_matching_paren (int k);
static void init_bouncing_parens ();

static void
init_bouncing_parens ()
{
  if (strncmp (rl_get_keymap_name (rl_get_keymap ()), "vi", 2))
    {
      rl_bind_key (')', match_paren);
      rl_bind_key (']', match_paren);
      rl_bind_key ('}', match_paren);
    }
}

static int
find_matching_paren(int k)
{
  register int i;
  register char c = 0;
  int end_parens_found = 0;

  /* Choose the corresponding opening bracket.  */
  if (k == ')') c = '(';
  else if (k == ']') c = '[';
  else if (k == '}') c = '{';

  for (i=rl_point-2; i>=0; i--)
    {
      /* Is the current character part of a character literal?  */
      if (i - 2 >= 0
	  && rl_line_buffer[i - 1] == '\\'
	  && rl_line_buffer[i - 2] == '#')
	;
      else if (rl_line_buffer[i] == k)
	end_parens_found++;
      else if (rl_line_buffer[i] == '"')
	{
	  /* Skip over a string literal.  */
	  for (i--; i >= 0; i--)
	    if (rl_line_buffer[i] == '"'
		&& ! (i - 1 >= 0
		      && rl_line_buffer[i - 1] == '\\'))
	      break;
	}
      else if (rl_line_buffer[i] == c)
	{
	  if (end_parens_found==0)
	    return i;
	  else --end_parens_found;
	}
    }
  return -1;
}

static int
match_paren (int x, int k)
{
  int tmp;
#ifndef __MINGW32__
  int fno;
  SELECT_TYPE readset;
  struct timeval timeout;
#endif

  rl_insert (x, k);
  if (!SCM_READLINE_BOUNCE_PARENS)
    return 0;

  /* Did we just insert a quoted paren?  If so, then don't bounce.  */
  if (rl_point - 1 >= 1
      && rl_line_buffer[rl_point - 2] == '\\')
    return 0;

#ifndef __MINGW32__
  tmp = 1000 * SCM_READLINE_BOUNCE_PARENS;
  timeout.tv_sec = tmp / 1000000;
  timeout.tv_usec = tmp % 1000000;
  FD_ZERO (&readset);
  fno = fileno (rl_instream);
  FD_SET (fno, &readset);
#endif

  if (rl_point > 1)
    {
      tmp = rl_point;
      rl_point = find_matching_paren (k);
      if (rl_point > -1)
	{
	  rl_redisplay ();
#ifndef __MINGW32__
	  scm_std_select (fno + 1, &readset, NULL, NULL, &timeout);
#else
	  WaitForSingleObject (GetStdHandle(STD_INPUT_HANDLE),
			       SCM_READLINE_BOUNCE_PARENS); 
#endif
	}
      rl_point = tmp;
    }
  return 0;
}
#endif /* HAVE_RL_GET_KEYMAP */

#endif /* HAVE_RL_GETC_FUNCTION */

void
scm_init_readline ()
{
#ifdef HAVE_RL_GETC_FUNCTION
#include "guile-readline/readline.x"
  scm_readline_completion_function_var
    = scm_c_define ("*readline-completion-function*", SCM_BOOL_F);
#ifndef __MINGW32__
  rl_getc_function = current_input_getc;
#endif
#if defined (_RL_FUNCTION_TYPEDEF)
  rl_completion_entry_function = (rl_compentry_func_t*) completion_function;
#else  
  rl_completion_entry_function = (Function*) completion_function;
#endif
  rl_basic_word_break_characters = "\t\n\"'`;()";
  rl_readline_name = "Guile";

  reentry_barrier_mutex = scm_permanent_object (scm_make_mutex ());
  scm_init_opts (scm_readline_options,
		 scm_readline_opts,
		 SCM_N_READLINE_OPTIONS);
#if HAVE_RL_GET_KEYMAP
  init_bouncing_parens();
#endif
  scm_add_feature ("readline");
#endif /* HAVE_RL_GETC_FUNCTION */
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
