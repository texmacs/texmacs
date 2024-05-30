%option noyywrap
%option nounput
%pointer

EOL             \n
SPACE           [ \t\v\f]
WS              [ \t\v\n\f]
DIGIT		[0-9]
LETTER		[a-zA-Z_]
OCTDIGIT	[0-7]
HEXDIGIT	[a-fA-F0-9]
EXPONENT	[Ee][+-]?{DIGIT}+
FLOQUAL		(f|F|l|L)
INTQUAL		(l|L|ll|LL|lL|Ll|u|U)

%{
    
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Prevent compilation of static input() function in generated scanner
   code.  This function is never actually used, and GCC 4.3 will emit
   an error for that. */
#define YY_NO_INPUT

int yylex(void);

int yyget_lineno (void);
FILE *yyget_in (void);
FILE *yyget_out (void);
int yyget_leng (void);
char *yyget_text (void);
void yyset_lineno (int line_number);
void yyset_in (FILE * in_str);
void yyset_out (FILE * out_str);
int yyget_debug (void);
void yyset_debug (int  bdebug);
int yylex_destroy (void);
 
int filter_snarfage = 0;
int print = 1; 

enum t_state {
    SKIP,
    MULTILINE,
    MULTILINE_COOKIE,
    COOKIE
};

enum t_state state = SKIP;
int cookie_was_last = 0; 

#define OUT_RAW(type,text) if (print) printf ("(%s . \"%s\")\n", #type, text)
 
#define OUT_T(type)        OUT_RAW (type, yytext)
#define OUT_S              if (print) printf ("%s\n", yytext)
#define OUT(type)          if (print) printf ("%s\n", #type)

#define IS_COOKIE cookie_was_last = 1
#define IS_NOT_COOKIE cookie_was_last = 0
 
%}

%%

\/\*(\n|[^*]|\*[^/])*\*\/                  { OUT_T (comment); }

({SPACE}*(\\\n)*{SPACE}*)+                 ;

({SPACE}*\n*{SPACE}*)+                     { OUT(eol); }

#.*\n                                      { OUT(hash); IS_NOT_COOKIE; }

{LETTER}({LETTER}|{DIGIT})*                { OUT_T (id); IS_NOT_COOKIE; }

0[xX]{HEXDIGIT}+{INTQUAL}?                 { OUT_RAW (int_hex, yytext + 2); IS_NOT_COOKIE; }
0{OCTDIGIT}+{INTQUAL}?                     { OUT_RAW (int_oct, yytext + 1); IS_NOT_COOKIE; }
{DIGIT}+{INTQUAL}?                         { OUT_T (int_dec); IS_NOT_COOKIE; }

L?\'(\\.|[^\\\'])+\'                       { OUT_T (char); IS_NOT_COOKIE; }

{DIGIT}+{EXPONENT}{FLOQUAL}?               { OUT_T (flo_dec); IS_NOT_COOKIE; }
{DIGIT}*"."{DIGIT}+({EXPONENT})?{FLOQUAL}? { OUT_T (flo_dec); IS_NOT_COOKIE; }
{DIGIT}+"."{DIGIT}*({EXPONENT})?{FLOQUAL}? { OUT_T (flo_dec); IS_NOT_COOKIE; }

L?\"(\\.|[^\\\"])*\"                       { OUT_S; IS_NOT_COOKIE; }

"..."                                      { OUT (ellipsis); IS_NOT_COOKIE; }

">>="                                      { OUT (shift_right_assign); IS_NOT_COOKIE; }
"<<="                                      { OUT (shift_left_assign); IS_NOT_COOKIE; }
"+="                                       { OUT (add_assign); IS_NOT_COOKIE; }
"-="                                       { OUT (sub_assign); IS_NOT_COOKIE; }
"*="                                       { OUT (mul-assign); IS_NOT_COOKIE; }
"/="                                       { OUT (div_assign); IS_NOT_COOKIE; }
"%="                                       { OUT (mod_assign); IS_NOT_COOKIE; }
"&="                                       { OUT (logand_assign); IS_NOT_COOKIE; }
"^="                                       { OUT (logxor_assign); IS_NOT_COOKIE; }
"|="                                       { OUT (logior_assign); IS_NOT_COOKIE; }
">>"                                       { OUT (right_shift); IS_NOT_COOKIE; }
"<<"                                       { OUT (left_shift); IS_NOT_COOKIE; }
"++"                                       { OUT (inc); IS_NOT_COOKIE; }
"--"                                       { OUT (dec); IS_NOT_COOKIE; }
"->"                                       { OUT (ptr); IS_NOT_COOKIE; }
"&&"                                       { OUT (and); IS_NOT_COOKIE; }
"||"                                       { OUT (or); IS_NOT_COOKIE; }
"<="                                       { OUT (le); IS_NOT_COOKIE; }
">="                                       { OUT (ge); IS_NOT_COOKIE; }
"=="                                       { OUT (eq); IS_NOT_COOKIE; }
"!="                                       { OUT (ne); IS_NOT_COOKIE; }
";"                                        { OUT (semicolon); IS_NOT_COOKIE; }

("{"|"<%")                                 {
  OUT (brace_open);
  if (filter_snarfage && cookie_was_last && state == COOKIE)
    state = MULTILINE;
  IS_NOT_COOKIE; }

("}"|"%>")                                 {
  OUT (brace_close);
  if (filter_snarfage && cookie_was_last && state == MULTILINE_COOKIE) {
    state = SKIP;
    print = 0;
  }
  IS_NOT_COOKIE; }

","                                        { OUT (comma); IS_NOT_COOKIE; }
":"                                        { OUT (colon); IS_NOT_COOKIE; }
"="                                        { OUT (assign); IS_NOT_COOKIE; }
"("                                        { OUT (paren_open); IS_NOT_COOKIE; }
")"                                        { OUT (paren_close); IS_NOT_COOKIE; }
("["|"<:")                                 { OUT (bracket_open); IS_NOT_COOKIE; }
("]"|":>")                                 { OUT (bracket_close); IS_NOT_COOKIE; }
"."                                        { OUT (dot); IS_NOT_COOKIE; }
"&"                                        { OUT (amp); IS_NOT_COOKIE; }
"!"                                        { OUT (bang); IS_NOT_COOKIE; }
"~"                                        { OUT (tilde); IS_NOT_COOKIE; }
"-"                                        { OUT (minus); IS_NOT_COOKIE; }
"+"                                        { OUT (plus); IS_NOT_COOKIE; }
"*"                                        { OUT (star); IS_NOT_COOKIE; }
"/"                                        { OUT (slash); IS_NOT_COOKIE; }
"%"                                        { OUT (percent); IS_NOT_COOKIE; }
"<"                                        { OUT (lt); IS_NOT_COOKIE; }
">"                                        { OUT (gt); IS_NOT_COOKIE; }

\^{WS}*\^                                  {
  if (filter_snarfage)
    switch (state) {
    case SKIP:
      state = COOKIE;
      print = 1;
      OUT (snarf_cookie);
      break;
    case MULTILINE:
    case MULTILINE_COOKIE:
      state = MULTILINE_COOKIE;
      OUT (snarf_cookie);
      break;
    case COOKIE:
      state = SKIP;
      OUT (snarf_cookie);
      print = 0;
      break;
    default:
      /* whoops */
      abort ();
      break;
    }
  else
    OUT (snarf_cookie);
  
  IS_COOKIE; }

"^"                                        { OUT (caret); IS_NOT_COOKIE; }
"|"                                        { OUT (pipe); IS_NOT_COOKIE; }
"?"                                        { OUT (question); IS_NOT_COOKIE; }

.                                          { fprintf (stderr, "*%s", yytext);  fflush (stderr); IS_NOT_COOKIE; }

%%

int
main (int argc, char *argv[])
{
    if (argc > 1 && !strcmp (argv[1], "--filter-snarfage")) {
        filter_snarfage = 1;
        print = 0;
    }

    yylex ();

    return EXIT_SUCCESS;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
