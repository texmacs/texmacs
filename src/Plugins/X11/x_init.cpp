
/******************************************************************************
* MODULE     : x_init.cpp
* DESCRIPTION: Initialization of the X11 window manager
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "X11/x_window.hpp"
#include "language.hpp"
#include "font.hpp"
#include "analyze.hpp"
#include "dictionary.hpp"
#include "iterator.hpp"
#include "message.hpp"
#include "sys_utils.hpp"
#include <locale.h>

x_gui_rep* the_gui= NULL;
extern hashmap<Window,pointer> Window_to_window;

/******************************************************************************
* Set up colors
******************************************************************************/

bool true_color= false;
bool reverse_colors= false;

color black, white, red, green, blue;
color yellow, magenta, orange, brown, pink;
color light_grey, grey, dark_grey;

static int CSCALES= 4;
static int CFACTOR= 5;
static int GREYS  = 16;
static int CTOTAL = (CFACTOR*CFACTOR*CFACTOR+GREYS+1);

static void
reverse (int& r, int& g, int& b) {
  int m= min (r, min (g, b));
  int M= max (r, max (g, b));
  int t= (r + g + b) / 3;
  int tt= 255 - t;
  double mu= 1.0;
  // tt= 6 * tt / 7;
  if (M != m) {
    double lambda1= max (((double) (t - m)) / t,
			 ((double) (M - t)) / (255 - t));
    double lambda2= max (((double) (t - m)) / tt,
			 ((double) (M - t)) / (255 - tt));
    mu= lambda1 / lambda2;
  }
  r= (int) (tt + mu * (r - t) + 0.5);
  g= (int) (tt + mu * (g - t) + 0.5);
  b= (int) (tt + mu * (b - t) + 0.5);
}

int
x_alloc_color (int r, int g, int b) {
  if (true_color)
    return ((r >> 8) << 16) + ((g >> 8) << 8) + (b >> 8);

  if (reverse_colors) {
    int m= min (r, min (g, b));
    int M= max (r, max (g, b));
    int t= (r + g + b) / 3;
    int tt= 65535 - t;
    double mu= 1.0;
    tt= 6 * tt / 7;
    if (M != m) {
      double lambda1= max (((double) (t - m)) / t,
			   ((double) (M - t)) / (65535 - t));
      double lambda2= max (((double) (t - m)) / tt,
			   ((double) (M - t)) / (65535 - tt));
      mu= lambda1 / lambda2;
    }
    r= (int) (tt + mu * (r - t) + 0.5);
    g= (int) (tt + mu * (g - t) + 0.5);
    b= (int) (tt + mu * (b - t) + 0.5);
  }

  XColor col;
  col.red  = r;
  col.green= g;
  col.blue = b;
  if (!XAllocColor (the_gui->dpy, the_gui->cols, &col))
    cerr << "Warning: can't allocate color\n";
  return col.pixel;
}

void
x_init_color_map () {
  if (true_color) return;

  int i, r, g, b;
  the_gui->cmap= tm_new_array<color> (CTOTAL);

  for (i=0; i<=GREYS; i++)
    the_gui->cmap[i]=
      x_alloc_color ((i*65535)/GREYS, (i*65535)/GREYS, (i*65535)/GREYS);

  for (r=0; r<=CSCALES; r++)
    for (g=0; g<=CSCALES; g++)
      for (b=0; b<=CSCALES; b++) {
	i= r*CFACTOR*CFACTOR+ g*CFACTOR+ b+ GREYS+ 1;
	the_gui->cmap[i]= x_alloc_color ((r*65535)/CSCALES,
					 (g*65535)/CSCALES,
					 (b*65535)/CSCALES);
      }
}

color
rgb_color (int r, int g, int b) {
  if (true_color) {
    if (reverse_colors) reverse (r, g, b);
    return (r << 16) + (g << 8) + b;
  }
  else if ((r==g) && (g==b)) return (r*GREYS+ 128)/255;
  else {
    r= (r*CSCALES+ 128)/255;
    g= (g*CSCALES+ 128)/255;
    b= (b*CSCALES+ 128)/255;
    return r*CFACTOR*CFACTOR+ g*CFACTOR+ b+ GREYS+ 1;
  }
}

void
get_rgb_color (color col, int& r, int& g, int& b) {
  if (true_color) {
    r= (col >> 16) & 255;
    g= (col >> 8 ) & 255;
    b=  col        & 255;
    if (reverse_colors) reverse (r, g, b);
  }
  else if (col <= GREYS) {
    r= (col*255)/GREYS;
    g= (col*255)/GREYS;
    b= (col*255)/GREYS;
  }
  else {
    int rr, gg, bb;
    col-= (GREYS+1);
    bb  = col % CFACTOR;
    gg  = (col/CFACTOR) % CFACTOR;
    rr  = (col/(CFACTOR*CFACTOR)) % CFACTOR;
    r   = (rr*255)/CSCALES;
    g   = (gg*255)/CSCALES;
    b   = (bb*255)/CSCALES;
  }
}

color
named_color (string s) {
  if ((N(s) == 4) && (s[0]=='#')) {
    int r= 17 * from_hexadecimal (s (1, 2));
    int g= 17 * from_hexadecimal (s (2, 3));
    int b= 17 * from_hexadecimal (s (3, 4));
    return rgb_color (r, g, b);
  }
  if ((N(s) == 7) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 3));
    int g= from_hexadecimal (s (3, 5));
    int b= from_hexadecimal (s (5, 7));
    return rgb_color (r, g, b);
  }
  int pastel= (the_gui->depth>=16? 223: 191);
  if (s == "black")          return black;
  if (s == "white")          return white;
  if (s == "grey")           return grey;
  if (s == "red")            return red;
  if (s == "blue")           return blue;
  if (s == "yellow")         return yellow;
  if (s == "green")          return green;
  if (s == "magenta")        return magenta;
  if (s == "cyan")           return rgb_color (0, 255, 255);
  if (s == "orange")         return orange;
  if (s == "brown")          return brown;
  if (s == "pink")           return pink;
  if (s == "broken white")   return rgb_color (255, 255, pastel);
  if (s == "light grey")     return light_grey;
  if (s == "dark grey")      return dark_grey;
  if (s == "dark red")       return rgb_color (128, 0, 0);
  if (s == "dark blue")      return rgb_color (0, 0, 128);
  if (s == "dark yellow")    return rgb_color (128, 128, 0);
  if (s == "dark green")     return rgb_color (0, 128, 0);
  if (s == "dark magenta")   return rgb_color (128, 0, 128);
  if (s == "dark cyan")      return rgb_color (0, 128, 128);
  if (s == "dark orange")    return rgb_color (128, 64, 0);
  if (s == "dark brown")     return rgb_color (64, 16, 0);
  if (s == "pastel grey")    return rgb_color (pastel, pastel, pastel);
  if (s == "pastel red")     return rgb_color (255, pastel, pastel);
  if (s == "pastel blue")    return rgb_color (pastel, pastel, 255);
  if (s == "pastel yellow")  return rgb_color (255, 255, pastel);
  if (s == "pastel green")   return rgb_color (pastel, 255, pastel);
  if (s == "pastel magenta") return rgb_color (255, pastel, 255);
  if (s == "pastel cyan")    return rgb_color (pastel, 255, 255);
  if (s == "pastel orange")  return rgb_color (255, pastel, 2*pastel-255);
  if (s == "pastel brown")   return rgb_color (pastel, 2*pastel-255, 2*pastel-255);
  return black;
}

string
get_named_color (color c) {
  SI r, g, b;
  get_rgb_color (c, r, g, b);
  return "#" *
    as_hexadecimal (r, 2) *
    as_hexadecimal (g, 2) *
    as_hexadecimal (b, 2);
}

void
x_initialize_colors () {
  if (the_gui->depth >= 16) {
    CSCALES= 8;
    CFACTOR= 9;
    GREYS  = 256;
    CTOTAL = (CFACTOR*CFACTOR*CFACTOR+GREYS+1);
  }

  x_init_color_map ();

  black   = rgb_color (0, 0, 0);
  white   = rgb_color (255, 255, 255);
  red     = rgb_color (255, 0, 0);
  blue    = rgb_color (0, 0, 255);
  yellow  = rgb_color (255, 255, 0);
  green   = rgb_color (0, 255, 0);
  magenta = rgb_color (255, 0, 255);
  orange  = rgb_color (255, 128, 0);
  brown   = rgb_color (128, 32, 0);
  pink    = rgb_color (255, 128, 128);

  light_grey = rgb_color (208, 208, 208);
  grey       = rgb_color (184, 184, 184);
  dark_grey  = rgb_color (112, 112, 112);
}

/******************************************************************************
* Set up input method
******************************************************************************/

void
x_gui_rep::initialize_input_method () {
  im_ok= false;
  if (setlocale (LC_CTYPE, "") == NULL)
    cerr << "TeXmacs] Warning: locale could not be set\n";
  else {
    if (!XSetLocaleModifiers (""))
      cerr << "TeXmacs] Warning: could not set locale modifiers\n";
    if (XSupportsLocale () == False)
      cerr << "TeXmacs] Warning: locale is not supported\n";
    else if ((im = XOpenIM (dpy, NULL, NULL, NULL)) == NULL)
      cout << "TeXmacs] Warning: could not open input method\n";
    else im_ok= true;
  }
}

/******************************************************************************
* Get xmodmap
******************************************************************************/

#ifndef XK_ISO_Left_Tab
#define	XK_ISO_Left_Tab 0xFE20
#endif

static XModifierKeymap* xmodmap;
static int        mod_n;
static KeyCode*   mod_k;
static array<int> mod_shift;
static array<int> mod_ctrl;
static array<int> mod_alt;

void
x_gui_rep::insert_keysym (array<int>& a, int i, int j) {
  int ks= XKeycodeToKeysym (dpy, mod_k[i*mod_n+j], 0);
  if (ks!=0) a << ks;
}

void
x_gui_rep::get_xmodmap () {
  int i;
  xmodmap= XGetModifierMapping (dpy);
  mod_n= xmodmap->max_keypermod;
  mod_k= xmodmap->modifiermap;
  for (i=0; i<mod_n; i++) {
    insert_keysym (mod_shift, 0, i);
    insert_keysym (mod_shift, 1, i);
    insert_keysym (mod_ctrl, 2, i);
    insert_keysym (mod_alt, 3, i);
  }
  XFreeModifiermap (xmodmap);
}

/******************************************************************************
* Set up keyboard
******************************************************************************/

void
x_gui_rep::map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= "S-" * s;
}

void
x_gui_rep::Map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= s;
}

void
x_gui_rep::initialize_keyboard_pointer () {
  // Latin characters
  Map (XK_a, "a");
  Map (XK_b, "b");
  Map (XK_c, "c");
  Map (XK_d, "d");
  Map (XK_e, "e");
  Map (XK_f, "f");
  Map (XK_g, "g");
  Map (XK_h, "h");
  Map (XK_i, "i");
  Map (XK_j, "j");
  Map (XK_k, "k");
  Map (XK_l, "l");
  Map (XK_m, "m");
  Map (XK_n, "n");
  Map (XK_o, "o");
  Map (XK_p, "p");
  Map (XK_q, "q");
  Map (XK_r, "r");
  Map (XK_s, "s");
  Map (XK_t, "t");
  Map (XK_u, "u");
  Map (XK_v, "v");
  Map (XK_w, "w");
  Map (XK_x, "x");
  Map (XK_y, "y");
  Map (XK_z, "z");
  Map (XK_A, "A");
  Map (XK_B, "B");
  Map (XK_C, "C");
  Map (XK_D, "D");
  Map (XK_E, "E");
  Map (XK_F, "F");
  Map (XK_G, "G");
  Map (XK_H, "H");
  Map (XK_I, "I");
  Map (XK_J, "J");
  Map (XK_K, "K");
  Map (XK_L, "L");
  Map (XK_M, "M");
  Map (XK_N, "N");
  Map (XK_O, "O");
  Map (XK_P, "P");
  Map (XK_Q, "Q");
  Map (XK_R, "R");
  Map (XK_S, "S");
  Map (XK_T, "T");
  Map (XK_U, "U");
  Map (XK_V, "V");
  Map (XK_W, "W");
  Map (XK_X, "X");
  Map (XK_Y, "Y");
  Map (XK_Z, "Z");
  Map (XK_0, "0");
  Map (XK_1, "1");
  Map (XK_2, "2");
  Map (XK_3, "3");
  Map (XK_4, "4");
  Map (XK_5, "5");
  Map (XK_6, "6");
  Map (XK_7, "7");
  Map (XK_8, "8");
  Map (XK_9, "9");

  // Cyrillic letters
  Map (XK_Cyrillic_a,   "\xe0");
  Map (XK_Cyrillic_be,  "\xe1");
  Map (XK_Cyrillic_ve,  "\xe2");
  Map (XK_Cyrillic_ghe, "\xe3");
  Map (XK_Cyrillic_de,  "\xe4");
  Map (XK_Cyrillic_ie,  "\xe5");
  Map (XK_Cyrillic_io,  "\xbc");
  Map (XK_Cyrillic_zhe, "\xe6");
  Map (XK_Cyrillic_ze,  "\xe7");
  Map (XK_Cyrillic_i,   "\xe8");
  Map (XK_Cyrillic_shorti,   "\xe9");
  Map (XK_Cyrillic_ka,  "\xea");
  Map (XK_Cyrillic_el,  "\xeb");
  Map (XK_Cyrillic_em,  "\xec");
  Map (XK_Cyrillic_en,  "\xed");
  Map (XK_Cyrillic_o,   "\xee");
  Map (XK_Cyrillic_pe,  "\xef");
  Map (XK_Cyrillic_er,  "\xf0");
  Map (XK_Cyrillic_es,  "\xf1");
  Map (XK_Cyrillic_te,  "\xf2");
  Map (XK_Cyrillic_u,   "\xf3");
  Map (XK_Cyrillic_ef,  "\xf4");
  Map (XK_Cyrillic_ha,  "\xf5");
  Map (XK_Cyrillic_tse, "\xf6");
  Map (XK_Cyrillic_che, "\xf7");
  Map (XK_Cyrillic_sha, "\xf8");
  Map (XK_Cyrillic_shcha,    "\xf9");
  Map (XK_Cyrillic_hardsign, "\xfa");
  Map (XK_Cyrillic_yeru,     "\xfb");
  Map (XK_Cyrillic_softsign, "\xfc");
  Map (XK_Cyrillic_e,   "\xfd");
  Map (XK_Cyrillic_yu,  "\xfe");
  Map (XK_Cyrillic_ya,  "\xff");
  Map (XK_Cyrillic_A,   "\xc0");
  Map (XK_Cyrillic_BE,  "\xc1");
  Map (XK_Cyrillic_VE,  "\xc2");
  Map (XK_Cyrillic_GHE, "\xc3");
  Map (XK_Cyrillic_DE,  "\xc4");
  Map (XK_Cyrillic_IE,  "\xc5");
  Map (XK_Cyrillic_IO,  "\x9c");
  Map (XK_Cyrillic_ZHE, "\xc6");
  Map (XK_Cyrillic_ZE,  "\xc7");
  Map (XK_Cyrillic_I,   "\xc8");
  Map (XK_Cyrillic_SHORTI,   "\xc9");
  Map (XK_Cyrillic_KA,  "\xca");
  Map (XK_Cyrillic_EL,  "\xcb");
  Map (XK_Cyrillic_EM,  "\xcc");
  Map (XK_Cyrillic_EN,  "\xcd");
  Map (XK_Cyrillic_O,   "\xce");
  Map (XK_Cyrillic_PE,  "\xcf");
  Map (XK_Cyrillic_ER,  "\xd0");
  Map (XK_Cyrillic_ES,  "\xd1");
  Map (XK_Cyrillic_TE,  "\xd2");
  Map (XK_Cyrillic_U,   "\xd3");
  Map (XK_Cyrillic_EF,  "\xd4");
  Map (XK_Cyrillic_HA,  "\xd5");
  Map (XK_Cyrillic_TSE, "\xd6");
  Map (XK_Cyrillic_CHE, "\xd7");
  Map (XK_Cyrillic_SHA, "\xd8");
  Map (XK_Cyrillic_SHCHA,    "\xd9");
  Map (XK_Cyrillic_HARDSIGN, "\xda");
  Map (XK_Cyrillic_YERU,     "\xdb");
  Map (XK_Cyrillic_SOFTSIGN, "\xdc");
  Map (XK_Cyrillic_E,   "\xdd");
  Map (XK_Cyrillic_YU,  "\xde");
  Map (XK_Cyrillic_YA,  "\xdf");

  //Ukrainian letters in T2A encoding
  Map (XK_Ukrainian_i,   "i"); // Fall back!
  Map (XK_Ukrainian_I,   "I"); // Fall back!
  Map (XK_Ukrainian_yi,   "\xa8");
  Map (XK_Ukrainian_YI,   "\x88");
  Map (XK_Ukrainian_ie,   "\xb9");
  Map (XK_Ukrainian_IE,   "\x99");
  // Map (XK_Ukrainian_ghe_with_upturn,   "\xa0");
  // Map (XK_Ukrainian_GHE_WITH_UPTURN,   "\x80");
  Map (0x6ad,   "\xa0");
  Map (0x6bd,   "\x80");

  // Standard ASCII Symbols
  Map (XK_exclam, "!");
  Map (XK_quotedbl, "\x22");
  Map (XK_numbersign, "#");
  Map (XK_dollar, "$");
  Map (XK_percent, "%");
  Map (XK_ampersand, "&");
  Map (XK_apostrophe, "'");
  Map (XK_quoteright, "'");
  Map (XK_parenleft, "(");
  Map (XK_parenright, ")");
  Map (XK_asterisk, "*");
  Map (XK_plus, "+");
  Map (XK_comma, ",");
  Map (XK_minus, "-");
  Map (XK_period, ".");
  Map (XK_slash, "/");
  Map (XK_colon, ":");
  Map (XK_semicolon, ";");
  Map (XK_less, "<");
  Map (XK_equal, "=");
  Map (XK_greater, ">");
  Map (XK_question, "?");
  Map (XK_at, "@");
  Map (XK_bracketleft, "[");
  Map (XK_backslash, "\\");
  Map (XK_bracketright, "]");
  Map (XK_asciicircum, "^");
  Map (XK_underscore, "_");
  Map (XK_grave, "`");
  Map (XK_quoteleft, "`");
  Map (XK_braceleft, "{");
  Map (XK_bar, "|");
  Map (XK_braceright, "}");
  Map (XK_asciitilde, "~");

  // dead keys
  Map (0xFE50, "grave");
  Map (0xFE51, "acute");
  Map (0xFE52, "hat");
  Map (0xFE53, "tilde");
  Map (0xFE54, "macron");
  Map (0xFE55, "breve");
  Map (0xFE56, "abovedot");
  Map (0XFE57, "umlaut");
  Map (0xFE58, "abovering");
  Map (0xFE59, "doubleacute");
  Map (0xFE5A, "check");
  Map (0xFE5B, "cedilla");
  Map (0xFE5C, "ogonek");
  Map (0xFE5D, "iota");
  Map (0xFE5E, "voicedsound");
  Map (0xFE5F, "semivoicedsound");
  Map (0xFE60, "belowdot");

  // Extended symbols and accented characters
  Map (XK_nobreakspace, "varspace");
  Map (XK_exclamdown, "exclamdown");
  Map (XK_cent, "cent");
  Map (XK_sterling, "sterling");
  Map (XK_currency, "currency");
  Map (XK_yen, "yen");
  Map (XK_brokenbar, "brokenbar");
  Map (XK_section, "section");
  Map (XK_diaeresis, "umlaut");
  Map (XK_copyright, "copyright");
  Map (XK_ordfeminine, "ordfeminine");
  Map (XK_guillemotleft, "guillemotleft");
  Map (XK_notsign, "notsign");
  Map (XK_hyphen, "hyphen");
  Map (XK_registered, "registered");
  Map (XK_macron, "macron");
  Map (XK_degree, "degree");
  Map (XK_plusminus, "plusminus");
  Map (XK_twosuperior, "twosuperior");
  Map (XK_threesuperior, "threesuperior");
  Map (XK_acute, "acute");
  Map (XK_mu, "mu");
  Map (XK_paragraph, "paragraph");
  Map (XK_periodcentered, "periodcentered");
  Map (XK_cedilla, "cedilla");
  Map (XK_onesuperior, "onesuperior");
  Map (XK_masculine, "masculine");
  Map (XK_guillemotright, "guillemotright");
  Map (XK_onequarter, "onequarter");
  Map (XK_onehalf, "onehalf");
  Map (XK_threequarters, "threequarters");
  Map (XK_questiondown, "questiondown");
  Map (XK_multiply, "times");
  Map (XK_division, "div");

  Map (XK_Agrave, "\xc0");
  Map (XK_Aacute, "\xc1");
  Map (XK_Acircumflex, "\xc2");
  Map (XK_Atilde, "\xc3");
  Map (XK_Adiaeresis, "\xc4");
  Map (XK_Aring, "\xc5");
  Map (XK_AE, "\xc6");
  Map (XK_Ccedilla, "\xc7");
  Map (XK_Egrave, "\xc8");
  Map (XK_Eacute, "\xc9");
  Map (XK_Ecircumflex, "\xca");
  Map (XK_Ediaeresis, "\xcb");
  Map (XK_Igrave, "\xcc");
  Map (XK_Iacute, "\xcd");
  Map (XK_Icircumflex, "\xce");
  Map (XK_Idiaeresis, "\xcf");
  Map (XK_ETH, "\xd0");
  Map (XK_Eth, "\xd0");
  Map (XK_Ntilde, "\xd1");
  Map (XK_Ograve, "\xd2");
  Map (XK_Oacute, "\xd3");
  Map (XK_Ocircumflex, "\xd4");
  Map (XK_Otilde, "\xd5");
  Map (XK_Odiaeresis, "\xd6");
  Map (XK_OE, "\xd7");
  Map (XK_Ooblique, "\xd8");
  Map (XK_Ugrave, "\xd9");
  Map (XK_Uacute, "\xda");
  Map (XK_Ucircumflex, "\xdb");
  Map (XK_Udiaeresis, "\xdc");
  Map (XK_Yacute, "\xdd");
  Map (XK_THORN, "\xde");
  Map (XK_Thorn, "\xde");
  Map (XK_ssharp, "sz");
  Map (XK_agrave, "\xe0");
  Map (XK_aacute, "\xe1");
  Map (XK_acircumflex, "\xe2");
  Map (XK_atilde, "\xe3");
  Map (XK_adiaeresis, "\xe4");
  Map (XK_aring, "\xe5");
  Map (XK_ae, "\xe6");
  Map (XK_ccedilla, "\xe7");
  Map (XK_egrave, "\xe8");
  Map (XK_eacute, "\xe9");
  Map (XK_ecircumflex, "\xea");
  Map (XK_ediaeresis, "\xeb");
  Map (XK_igrave, "\xec");
  Map (XK_iacute, "\xed");
  Map (XK_icircumflex, "\xee");
  Map (XK_idiaeresis, "\xef");
  Map (XK_eth, "\xf0");
  Map (XK_ntilde, "\xf1");
  Map (XK_ograve, "\xf2");
  Map (XK_oacute, "\xf3");
  Map (XK_ocircumflex, "\xf4");
  Map (XK_otilde, "\xf5");
  Map (XK_odiaeresis, "\xf6");
  Map (XK_oe, "\xf7");
  Map (XK_oslash, "\xf8");
  Map (XK_ugrave, "\xf9");
  Map (XK_uacute, "\xfa");
  Map (XK_ucircumflex, "\xfb");
  Map (XK_udiaeresis, "\xfc");
  Map (XK_yacute, "\xfd");
  Map (XK_thorn, "\xfe");
  Map (XK_ydiaeresis, "\xff");

  // Symbols from iso-latin-2
  Map (XK_Aogonek, "\x81");
  Map (XK_breve, "breve");
  Map (XK_Lstroke, "\x8a");
  Map (XK_Lcaron, "\x89");
  Map (XK_Sacute, "\x91");
  Map (XK_Scaron, "\x92");
  Map (XK_Scedilla, "\x93");
  Map (XK_Tcaron, "\x94");
  Map (XK_Zacute, "\x99");
  Map (XK_Zcaron, "\x9a");
  Map (XK_Zabovedot, "\x9b");
  Map (XK_aogonek, "\xa1");
  Map (XK_ogonek, "ogonek");
  Map (XK_lstroke, "\xaa");
  Map (XK_lcaron, "\xa9");
  Map (XK_sacute, "\xb1");
  Map (XK_caron, "caron");
  Map (XK_scaron, "\xb2");
  Map (XK_scedilla, "\xb3");
  Map (XK_tcaron, "\xb4");
  Map (XK_zacute, "\xb9");
  Map (XK_doubleacute, "doubleacute");
  Map (XK_zcaron, "\xba");
  Map (XK_zabovedot, "\xbb");
  Map (XK_Racute, "\x8f");
  Map (XK_Abreve, "\x80");
  Map (XK_Lacute, "\x88");
  Map (XK_Cacute, "\x82");
  Map (XK_Ccaron, "\x83");
  Map (XK_Eogonek, "\x86");
  Map (XK_Ecaron, "\x85");
  Map (XK_Dcaron, "\x84");
  Map (XK_Dstroke, "\xd0");
  Map (XK_Nacute, "\x8b");
  Map (XK_Ncaron, "\x8c");
  Map (XK_Odoubleacute, "\x8e");
  Map (XK_Rcaron, "\x90");
  Map (XK_Uring, "\x97");
  Map (XK_Udoubleacute, "\x96");
  Map (XK_Tcedilla, "\x95");
  Map (XK_racute, "\xaf");
  Map (XK_abreve, "\xa0");
  Map (XK_lacute, "\xa8");
  Map (XK_cacute, "\xa2");
  Map (XK_ccaron, "\xa3");
  Map (XK_eogonek, "\xa6");
  Map (XK_ecaron, "\xa5");
  Map (XK_dcaron, "\xa4");
  Map (XK_dstroke, "\x9e");
  Map (XK_nacute, "\xab");
  Map (XK_ncaron, "\xac");
  Map (XK_odoubleacute, "\xae");
  Map (XK_udoubleacute, "\xb6");
  Map (XK_rcaron, "\xb0");
  Map (XK_uring, "\xb7");
  Map (XK_tcedilla, "\xb5");
  Map (XK_abovedot, "abovedot");

  // Special control keys
  Map (XK_Prior, "pageup");
  Map (XK_Next, "pagedown");
  Map (XK_Undo, "undo");
  Map (XK_Redo, "redo");
  Map (XK_Cancel, "cancel");

  // Control keys
  map (XK_space, "space");
  map (XK_Return, "return");
  map (XK_BackSpace, "backspace");
  map (XK_Delete, "delete");
  map (XK_Insert, "insert");
  map (XK_Tab, "tab");
  map (XK_ISO_Left_Tab, "tab");
  map (XK_Escape, "escape");
  map (XK_Left, "left");
  map (XK_Right, "right");
  map (XK_Up, "up");
  map (XK_Down, "down");
  map (XK_Page_Up, "pageup");
  map (XK_Page_Down, "pagedown");
  map (XK_Home, "home");
  map (XK_End, "end");
  map (XK_F1, "F1");
  map (XK_F2, "F2");
  map (XK_F3, "F3");
  map (XK_F4, "F4");
  map (XK_F5, "F5");
  map (XK_F6, "F6");
  map (XK_F7, "F7");
  map (XK_F8, "F8");
  map (XK_F9, "F9");
  map (XK_F10, "F10");
  map (XK_F11, "F11");
  map (XK_F12, "F12");
  map (XK_F13, "F13");
  map (XK_F14, "F14");
  map (XK_F15, "F15");
  map (XK_F16, "F16");
  map (XK_F17, "F17");
  map (XK_F18, "F18");
  map (XK_F19, "F19");
  map (XK_F20, "F20");
  // map (XK_Mode_switch, "modeswitch");

  // Keypad keys
  Map (XK_KP_Space, "K-space");
  Map (XK_KP_Enter, "K-return");
  Map (XK_KP_Delete, "K-delete");
  Map (XK_KP_Insert, "K-insert");
  Map (XK_KP_Tab, "K-tab");
  Map (XK_KP_Left, "K-left");
  Map (XK_KP_Right, "K-right");
  Map (XK_KP_Up, "K-up");
  Map (XK_KP_Down, "K-down");
  Map (XK_KP_Page_Up, "K-pageup");
  Map (XK_KP_Page_Down, "K-pagedown");
  Map (XK_KP_Home, "K-home");
  Map (XK_KP_Begin, "K-begin");
  Map (XK_KP_End, "K-end");
  Map (XK_KP_F1, "K-F1");
  Map (XK_KP_F2, "K-F2");
  Map (XK_KP_F3, "K-F3");
  Map (XK_KP_F4, "K-F4");
  Map (XK_KP_Equal, "K-=");
  Map (XK_KP_Multiply, "K-*");
  Map (XK_KP_Add, "K-+");
  Map (XK_KP_Subtract, "K--");
  Map (XK_KP_Decimal, "K-.");
  Map (XK_KP_Separator, "K-,");
  Map (XK_KP_Divide, "K-/");
  Map (XK_KP_0, "K-0");
  Map (XK_KP_1, "K-1");
  Map (XK_KP_2, "K-2");
  Map (XK_KP_3, "K-3");
  Map (XK_KP_4, "K-4");
  Map (XK_KP_5, "K-5");
  Map (XK_KP_6, "K-6");
  Map (XK_KP_7, "K-7");
  Map (XK_KP_8, "K-8");
  Map (XK_KP_9, "K-9");

  // For Sun keyboards
  Map (SunXK_FA_Grave, "grave");
  Map (SunXK_FA_Circum, "hat");
  Map (SunXK_FA_Tilde, "tilde");
  Map (SunXK_FA_Acute, "acute");
  Map (SunXK_FA_Diaeresis, "umlaut");
  Map (SunXK_FA_Cedilla, "cedilla");
  Map (SunXK_F36, "F11");
  Map (SunXK_F37, "F12");
  Map (SunXK_Copy, "copy");
  Map (SunXK_Paste, "paste");
  Map (SunXK_Cut, "cut");
  // Map (XK_L1, "stop");   On Sun, but conflicts with F11
  // Map (XK_L2, "again");  On Sun, but conflicts with F12
  Map (XK_L3, "props");
  Map (XK_L4, "undo");
  Map (XK_L5, "front");
  Map (XK_L6, "copy");
  Map (XK_L7, "open");
  Map (XK_L8, "paste");
  Map (XK_L9, "find");
  Map (XK_L10, "cut");

  // Miscellaneous
  Map (0x20ac, "euro");
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void
x_gui_rep::get_extents (SI& width, SI& height) {
  width = screen_width  * PIXEL;
  height= screen_height * PIXEL;
}

void
x_gui_rep::get_max_size (SI& width, SI& height) {
  width = 8000 * PIXEL;
  height= 6000 * PIXEL;
}

/******************************************************************************
* Main initialization
******************************************************************************/

x_gui_rep::x_gui_rep (int& argc2, char** argv2):
  color_scale ((void*) NULL),
  character_bitmap (NULL), character_pixmap ((pointer) 0),
  xpm_bitmap (0), xpm_pixmap (0),
  lower_key (""), upper_key (""),
  selection_t ("none"), selection_s (""), selection_w ((Window) 0)
{
  the_gui= this;
  ASSERT ((dpy= XOpenDisplay (NULL)) != NULL,
	  "failure to connect to Xserver");
  // XSynchronize (dpy, true);

  XGCValues values;
  XVisualInfo visual;

  scr                = DefaultScreen (dpy);
  root               = RootWindow (dpy, scr);
  gc                 = XCreateGC (dpy, root, 0, &values);
  pixmap_gc          = XCreateGC (dpy, root, 0, &values);
  depth              = DefaultDepth (dpy, scr);
  screen_width       = DisplayWidth  (dpy, scr);
  screen_height      = DisplayHeight (dpy, scr);
  cols               = DefaultColormap (dpy, DefaultScreen (dpy));
  state              = 0;
  gswindow           = NULL;
  argc               = argc2;
  argv               = argv2;
  balloon_win        = NULL;
  interrupted        = false;
  interrupt_time     = texmacs_time ();

  XA_CLIPBOARD = XInternAtom (dpy, "CLIPBOARD", false);
  XA_TARGETS = XInternAtom (dpy, "TARGETS", false);

  if (XMatchVisualInfo (dpy, scr, depth, TrueColor, &visual) != 0) {
    if (visual.red_mask   == (255 << 16) &&
	visual.green_mask == (255 << 8) &&
	visual.blue_mask  == 255)
      true_color= true;
  }

  XSetGraphicsExposures (dpy, gc, true);

  int start= 0;
  string xmm= eval_system ("xmodmap");
  for (int i=0; i<=N(xmm); i++)
    if (i == N(xmm) || xmm[i] == '\n') {
      string s= xmm (start, i);
      if (starts (s, "mod") && N(s)>3 && s[3] >= '1' && s[3] <= '5') {
	int nr= ((int) (s[3] - '0'));
	int mask= 4 << nr;
	if (occurs ("Alt_L", s) || occurs ("Alt_R", s)) {
	  //cout << "alt_mask= " << mask << "\n";
	  alt_mask= mask;
	}
	else if (alt_mask == 0 && occurs ("Mode_switch", s)) {
	  //cout << "alt_mask= " << mask << "\n";
	  alt_mask= mask;
	}
	else if (occurs ("Meta_L", s) || occurs ("Meta_R", s)) {
	  //cout << "meta_mask= " << mask << "\n";
	  meta_mask= mask;
	}
	else if (meta_mask == 0 &&
		 (occurs ("Super_L", s) || occurs ("Super_R", s))) {
	  //cout << "meta_mask= " << mask << "\n";
	  meta_mask= mask;
	}
      }
      start= i+1;
    }

  //get_xmodmap ();
  x_initialize_colors ();
  initialize_input_method ();
  initialize_keyboard_pointer ();
  set_output_language (get_locale_language ());
  (void) default_font ();
}

x_gui_rep::~x_gui_rep () {
  if (im_ok) XCloseIM (im);
  clear_selection ("primary");
  XFreeGC (dpy, gc);
  XCloseDisplay (dpy);
  if (!true_color) tm_delete_array (cmap);
}

void
gui_open (int& argc2, char** argv2) {
  ASSERT (the_gui == NULL, "gui already open");
  the_gui= tm_new<x_gui_rep> (argc2, argv2);
}

void
gui_start_loop () {
  the_gui->event_loop ();
}

void
gui_close () {
  ASSERT (the_gui != NULL, "gui not yet open");
  tm_delete (the_gui);
  the_gui= NULL;
}

void
gui_root_extents (SI& width, SI& height) {
  the_gui->get_extents (width, height);
}

void
gui_maximal_extents (SI& width, SI& height) {
  the_gui->get_max_size (width, height);
}

void
gui_refresh () {
  iterator<Window> it= iterate (Window_to_window);
  while (it->busy()) {
    x_window win= (x_window) Window_to_window [it->next()];
    if (get_x_window (win->w) != NULL)
      send_update (win->w);
  }
}
