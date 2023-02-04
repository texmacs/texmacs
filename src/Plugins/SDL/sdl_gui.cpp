
/******************************************************************************
* MODULE     : sdl_gui.cpp
* DESCRIPTION: Graphical user interface for SDL
* COPYRIGHT  : (C) 2022 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "sdl_gui.hpp"

#include "tm_timer.hpp"
#include "dictionary.hpp"
#include "image_files.hpp"
#include "message.hpp"
#include "iterator.hpp"
#include "font.hpp"
#include "tm_link.hpp" // number_of_servers

#include "sdl_window.hpp"

extern hashmap<SDL_Window*,pointer> Window_to_window;

sdl_gui_rep* the_gui= NULL;

bool char_clip= true;


void initialize_keyboard ();

/******************************************************************************
* General stuff
******************************************************************************/

sdl_gui_rep::sdl_gui_rep (int& argc2, char** argv2)
  : selection_t ("none"), selection_s (""), selection_w ((SDL_Window*) 0),
    mouse_state (0)
{
  the_gui= this;
  
  if (SDL_Init(SDL_INIT_VIDEO|SDL_INIT_AUDIO) != 0) {
          SDL_Log("Unable to initialize SDL: %s", SDL_GetError());
    exit(-1);
  }
  
  screen_width= 600;
  screen_height= 600;
  set_retina_factor (2);

  SDL_Rect r;
  if (SDL_GetDisplayBounds(0, &r) != 0) {
      SDL_Log("SDL_GetDisplayBounds failed: %s", SDL_GetError());
  } else {
    screen_width= r.w;
    screen_height= r.h;
  }
  
  initialize_colors ();
  initialize_keyboard ();
}

sdl_gui_rep::~sdl_gui_rep () {
  SDL_Quit();
}

void
sdl_gui_rep::get_extents (SI& width, SI& height) {
  width = screen_width  * PIXEL;
  height= screen_height * PIXEL;
}

void
sdl_gui_rep::get_max_size (SI& width, SI& height) {
  width = 8000 * PIXEL;
  height= 6000 * PIXEL;
}

void sdl_gui_rep::update_mouse_state (Uint32 mask) {
  unsigned int state= 0;

  int x, y;

  Uint32 buttons= SDL_GetGlobalMouseState (&x, &y);
  SDL_Keymod mods= SDL_GetModState();

//  buttons ^= mask;
  
  // compute state
  if ((buttons & SDL_BUTTON_LMASK) != 0)  state += 1;
  if ((buttons & SDL_BUTTON_MMASK) != 0)  state += 2;
  if ((buttons & SDL_BUTTON_RMASK) != 0)  state += 4;
  if ((buttons & SDL_BUTTON_X1MASK) != 0) state += 8;
  if ((buttons & SDL_BUTTON_X2MASK) != 0) state += 16;
  if ((mods & KMOD_SHIFT) != 0) state += 256;
  if ((mods & KMOD_CTRL)  != 0) state += 1024;
  if ((mods & KMOD_ALT)  != 0)  state += 2048;
//  if ((mods & KMOD_CAPS)  != 0) state += 1024;
  mouse_state= state;
}

void
sdl_gui_rep::emulate_leave_enter (widget old_widget, widget new_widget) {
  int x, y, ox, oy, x1, y1;
  
  //update_mouse_state ();
  // cout << "emulate_leave_enter mouse_state " << mouse_state << LF;
  //SDL_PumpEvents();  // make sure we have the latest mouse state.
  Uint32 buttons= SDL_GetGlobalMouseState (&x, &y);
  // cout << "emulate_leave_enter buttons " << buttons << LF;
  //update_mouse_state ();

  SDL_GetWindowPosition (get_Window (old_widget), &ox, &oy);
  x1= x - ox; y1= y - oy;
  x1= (x1 * PIXEL); y1= ((-y1) * PIXEL);
  // cout << "Emulate leave " << old_widget << "\n";
  send_mouse (old_widget, "leave", x1, y1, mouse_state, 0);
  // cout << "Leave OK\n";
  SDL_GetWindowPosition (get_Window (new_widget), &ox, &oy);
  x1= x - ox; y1= y - oy;
  x1= (x1 * PIXEL); y1= ((-y1) * PIXEL);
  // cout << "Emulate enter " << new_widget << "\n";
  send_mouse (new_widget, "enter", x1, y1, mouse_state, 0);
  // cout << "Enter OK\n\n";
}

/******************************************************************************
* Grabbing
******************************************************************************/

void
sdl_gui_rep::obtain_mouse_grab (widget wid) {
  SDL_Window *win= get_Window (wid);
  if ((!is_nil (grab_ptr)) && (wid==grab_ptr->item)) return;
  widget old_widget; if (!is_nil (grab_ptr)) old_widget= grab_ptr->item;
  grab_ptr= list<widget> (wid, grab_ptr);
  widget new_widget= grab_ptr->item;
  notify_mouse_grab (new_widget, true);
  SDL_RaiseWindow (win);
  SDL_CaptureMouse (SDL_TRUE);
  // SDL_SetWindowGrab (win, SDL_TRUE);
  // cout << "---> obtain_mouse_grab: in grab " << wid << "\n";
  if (!is_nil (old_widget)) {
    notify_mouse_grab (old_widget, false);
    emulate_leave_enter (old_widget, new_widget);
  }
}

void
sdl_gui_rep::release_mouse_grab () {
  if (is_nil (grab_ptr)) return;
  widget old_widget= grab_ptr->item;
  grab_ptr= grab_ptr->next;
  widget new_widget; if (!is_nil (grab_ptr)) new_widget= grab_ptr->item;
  if (is_nil (grab_ptr)) {
    SDL_Window *win= SDL_GetGrabbedWindow ();
    // if (win) SDL_SetWindowGrab (win, SDL_FALSE);
    SDL_CaptureMouse (SDL_FALSE);
    // cout << "---> release_mouse_grab: no grab\n";
  }
  else {
    sdl_window grab_win= get_sdl_window (new_widget);
    notify_mouse_grab (new_widget, true);
    SDL_RaiseWindow (grab_win->win);
    SDL_CaptureMouse (SDL_TRUE);
    // SDL_SetWindowGrab (grab_win->win, SDL_TRUE);
    // cout << "---> release_mouse_grab: next grab " <<  new_widget  << "\n";
    notify_mouse_grab (old_widget, false);
    emulate_leave_enter (old_widget, new_widget);
  }
}

bool
sdl_gui_rep::has_mouse_grab (widget w) {
  return (!is_nil (grab_ptr)) && (grab_ptr->item == w);
}

/******************************************************************************
* Hack for getting the remote time
******************************************************************************/

static bool   time_initialized= false;
static time_t time_difference = 0;

static void
synchronize_time (Uint32 t) {
  if (time_initialized && time_difference == 0) return;
  time_t d= texmacs_time () - ((time_t) t);
  if (time_initialized) {
    if (d < time_difference)
      time_difference= d;
  }
  else {
    time_initialized= true;
    time_difference = d;
  }
  if (-1000 <= time_difference && time_difference <= 1000)
    time_difference= 0;
}

static time_t
remote_time (Uint32 t) {
  return ((time_t) t) + time_difference;
}


/******************************************************************************
* Set up keyboard
******************************************************************************/

#ifndef SDLK_ISO_Left_Tab
#define SDLK_ISO_Left_Tab 0xFE20
#endif

hashmap<int,string>          lower_key;
hashmap<int,string>          upper_key;

void
map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= "S-" * s;
}

void
Map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= s;
}

void
MMap (int key, string s1, string s2) {
  lower_key (key)= s1;
  upper_key (key)= s2;
}

void
initialize_keyboard () {
  static bool initialized= false;
  if (initialized) return;
  initialized= true;
  
  // Latin characters
  MMap (SDLK_a, "a", "A");
  MMap (SDLK_b, "b", "B");
  MMap (SDLK_c, "c", "C");
  MMap (SDLK_d, "d", "D");
  MMap (SDLK_e, "e", "E");
  MMap (SDLK_f, "f", "F");
  MMap (SDLK_g, "g", "G");
  MMap (SDLK_h, "h", "H");
  MMap (SDLK_i, "i", "I");
  MMap (SDLK_j, "j", "J");
  MMap (SDLK_k, "k", "K");
  MMap (SDLK_l, "l", "L");
  MMap (SDLK_m, "m", "M");
  MMap (SDLK_n, "n", "N");
  MMap (SDLK_o, "o", "O");
  MMap (SDLK_p, "p", "P");
  MMap (SDLK_q, "q", "Q");
  MMap (SDLK_r, "r", "R");
  MMap (SDLK_s, "s", "S");
  MMap (SDLK_t, "t", "T");
  MMap (SDLK_u, "u", "U");
  MMap (SDLK_v, "v", "V");
  MMap (SDLK_w, "w", "W");
  MMap (SDLK_x, "x", "X");
  MMap (SDLK_y, "y", "Y");
  MMap (SDLK_z, "z", "Z");
#if 0
  Map (SDLK_A, "A");
  Map (SDLK_B, "B");
  Map (SDLK_C, "C");
  Map (SDLK_D, "D");
  Map (SDLK_E, "E");
  Map (SDLK_F, "F");
  Map (SDLK_G, "G");
  Map (SDLK_H, "H");
  Map (SDLK_I, "I");
  Map (SDLK_J, "J");
  Map (SDLK_K, "K");
  Map (SDLK_L, "L");
  Map (SDLK_M, "M");
  Map (SDLK_N, "N");
  Map (SDLK_O, "O");
  Map (SDLK_P, "P");
  Map (SDLK_Q, "Q");
  Map (SDLK_R, "R");
  Map (SDLK_S, "S");
  Map (SDLK_T, "T");
  Map (SDLK_U, "U");
  Map (SDLK_V, "V");
  Map (SDLK_W, "W");
  Map (SDLK_X, "X");
  Map (SDLK_Y, "Y");
  Map (SDLK_Z, "Z");
#endif
  Map (SDLK_0, "0");
  Map (SDLK_1, "1");
  Map (SDLK_2, "2");
  Map (SDLK_3, "3");
  Map (SDLK_4, "4");
  Map (SDLK_5, "5");
  Map (SDLK_6, "6");
  Map (SDLK_7, "7");
  Map (SDLK_8, "8");
  Map (SDLK_9, "9");

#if 0
  // Cyrillic letters
  Map (SDLK_Cyrillic_a,   "\xe0");
  Map (SDLK_Cyrillic_be,  "\xe1");
  Map (SDLK_Cyrillic_ve,  "\xe2");
  Map (SDLK_Cyrillic_ghe, "\xe3");
  Map (SDLK_Cyrillic_de,  "\xe4");
  Map (SDLK_Cyrillic_ie,  "\xe5");
  Map (SDLK_Cyrillic_io,  "\xbc");
  Map (SDLK_Cyrillic_zhe, "\xe6");
  Map (SDLK_Cyrillic_ze,  "\xe7");
  Map (SDLK_Cyrillic_i,   "\xe8");
  Map (SDLK_Cyrillic_shorti,   "\xe9");
  Map (SDLK_Cyrillic_ka,  "\xea");
  Map (SDLK_Cyrillic_el,  "\xeb");
  Map (SDLK_Cyrillic_em,  "\xec");
  Map (SDLK_Cyrillic_en,  "\xed");
  Map (SDLK_Cyrillic_o,   "\xee");
  Map (SDLK_Cyrillic_pe,  "\xef");
  Map (SDLK_Cyrillic_er,  "\xf0");
  Map (SDLK_Cyrillic_es,  "\xf1");
  Map (SDLK_Cyrillic_te,  "\xf2");
  Map (SDLK_Cyrillic_u,   "\xf3");
  Map (SDLK_Cyrillic_ef,  "\xf4");
  Map (SDLK_Cyrillic_ha,  "\xf5");
  Map (SDLK_Cyrillic_tse, "\xf6");
  Map (SDLK_Cyrillic_che, "\xf7");
  Map (SDLK_Cyrillic_sha, "\xf8");
  Map (SDLK_Cyrillic_shcha,    "\xf9");
  Map (SDLK_Cyrillic_hardsign, "\xfa");
  Map (SDLK_Cyrillic_yeru,     "\xfb");
  Map (SDLK_Cyrillic_softsign, "\xfc");
  Map (SDLK_Cyrillic_e,   "\xfd");
  Map (SDLK_Cyrillic_yu,  "\xfe");
  Map (SDLK_Cyrillic_ya,  "\xff");
  Map (SDLK_Cyrillic_A,   "\xc0");
  Map (SDLK_Cyrillic_BE,  "\xc1");
  Map (SDLK_Cyrillic_VE,  "\xc2");
  Map (SDLK_Cyrillic_GHE, "\xc3");
  Map (SDLK_Cyrillic_DE,  "\xc4");
  Map (SDLK_Cyrillic_IE,  "\xc5");
  Map (SDLK_Cyrillic_IO,  "\x9c");
  Map (SDLK_Cyrillic_ZHE, "\xc6");
  Map (SDLK_Cyrillic_ZE,  "\xc7");
  Map (SDLK_Cyrillic_I,   "\xc8");
  Map (SDLK_Cyrillic_SHORTI,   "\xc9");
  Map (SDLK_Cyrillic_KA,  "\xca");
  Map (SDLK_Cyrillic_EL,  "\xcb");
  Map (SDLK_Cyrillic_EM,  "\xcc");
  Map (SDLK_Cyrillic_EN,  "\xcd");
  Map (SDLK_Cyrillic_O,   "\xce");
  Map (SDLK_Cyrillic_PE,  "\xcf");
  Map (SDLK_Cyrillic_ER,  "\xd0");
  Map (SDLK_Cyrillic_ES,  "\xd1");
  Map (SDLK_Cyrillic_TE,  "\xd2");
  Map (SDLK_Cyrillic_U,   "\xd3");
  Map (SDLK_Cyrillic_EF,  "\xd4");
  Map (SDLK_Cyrillic_HA,  "\xd5");
  Map (SDLK_Cyrillic_TSE, "\xd6");
  Map (SDLK_Cyrillic_CHE, "\xd7");
  Map (SDLK_Cyrillic_SHA, "\xd8");
  Map (SDLK_Cyrillic_SHCHA,    "\xd9");
  Map (SDLK_Cyrillic_HARDSIGN, "\xda");
  Map (SDLK_Cyrillic_YERU,     "\xdb");
  Map (SDLK_Cyrillic_SOFTSIGN, "\xdc");
  Map (SDLK_Cyrillic_E,   "\xdd");
  Map (SDLK_Cyrillic_YU,  "\xde");
  Map (SDLK_Cyrillic_YA,  "\xdf");

  //Ukrainian letters in T2A encoding
  Map (SDLK_Ukrainian_i,   "i"); // Fall back!
  Map (SDLK_Ukrainian_I,   "I"); // Fall back!
  Map (SDLK_Ukrainian_yi,   "\xa8");
  Map (SDLK_Ukrainian_YI,   "\x88");
  Map (SDLK_Ukrainian_ie,   "\xb9");
  Map (SDLK_Ukrainian_IE,   "\x99");
  // Map (SDLK_Ukrainian_ghe_with_upturn,   "\xa0");
  // Map (SDLK_Ukrainian_GHE_WITH_UPTURN,   "\x80");
  Map (0x6ad,   "\xa0");
  Map (0x6bd,   "\x80");
#endif
  
  // Standard ASCII Symbols
  Map (SDLK_EXCLAIM, "!");
  Map (SDLK_QUOTEDBL, "\x22");
  Map (SDLK_HASH, "#");
  Map (SDLK_DOLLAR, "$");
  Map (SDLK_PERCENT, "%");
  Map (SDLK_AMPERSAND, "&");
  Map (SDLK_QUOTE, "'");
  Map (SDLK_LEFTPAREN, "(");
  Map (SDLK_RIGHTPAREN, ")");
  Map (SDLK_ASTERISK, "*");
  Map (SDLK_PLUS, "+");
  Map (SDLK_COMMA, ",");
  Map (SDLK_MINUS, "-");
  Map (SDLK_PERIOD, ".");
  Map (SDLK_SLASH, "/");
  Map (SDLK_COLON, ":");
  Map (SDLK_SEMICOLON, ";");
  Map (SDLK_LESS, "<");
  Map (SDLK_EQUALS, "=");
  Map (SDLK_GREATER, ">");
  Map (SDLK_QUESTION, "?");
  Map (SDLK_AT, "@");
  Map (SDLK_LEFTBRACKET, "[");
  Map (SDLK_BACKSLASH, "\\");
  Map (SDLK_RIGHTBRACKET, "]");
  Map (SDLK_CARET, "^");
  Map (SDLK_UNDERSCORE, "_");
  Map (SDLK_BACKQUOTE, "`");
  Map (SDLK_LEFTBRACKET, "{");
  Map (SDLK_KP_VERTICALBAR, "|");
  Map (SDLK_RIGHTBRACKET, "}");
  //Map (SDLK_TILDA, "~");

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

#if 0
  // Extended symbols and accented characters
  Map (SDLK_nobreakspace, "varspace");
  Map (SDLK_exclamdown, "exclamdown");
  Map (SDLK_cent, "cent");
  Map (SDLK_sterling, "sterling");
  Map (SDLK_currency, "currency");
  Map (SDLK_yen, "yen");
  Map (SDLK_brokenbar, "brokenbar");
  Map (SDLK_section, "section");
  Map (SDLK_diaeresis, "umlaut");
  Map (SDLK_copyright, "copyright");
  Map (SDLK_ordfeminine, "ordfeminine");
  Map (SDLK_guillemotleft, "guillemotleft");
  Map (SDLK_notsign, "notsign");
  Map (SDLK_hyphen, "hyphen");
  Map (SDLK_registered, "registered");
  Map (SDLK_macron, "macron");
  Map (SDLK_degree, "degree");
  Map (SDLK_plusminus, "plusminus");
  Map (SDLK_twosuperior, "twosuperior");
  Map (SDLK_threesuperior, "threesuperior");
  Map (SDLK_acute, "acute");
  Map (SDLK_mu, "mu");
  Map (SDLK_paragraph, "paragraph");
  Map (SDLK_periodcentered, "periodcentered");
  Map (SDLK_cedilla, "cedilla");
  Map (SDLK_onesuperior, "onesuperior");
  Map (SDLK_masculine, "masculine");
  Map (SDLK_guillemotright, "guillemotright");
  Map (SDLK_onequarter, "onequarter");
  Map (SDLK_onehalf, "onehalf");
  Map (SDLK_threequarters, "threequarters");
  Map (SDLK_questiondown, "questiondown");
  Map (SDLK_multiply, "times");
  Map (SDLK_division, "div");

  Map (SDLK_Agrave, "\xc0");
  Map (SDLK_Aacute, "\xc1");
  Map (SDLK_Acircumflex, "\xc2");
  Map (SDLK_Atilde, "\xc3");
  Map (SDLK_Adiaeresis, "\xc4");
  Map (SDLK_Aring, "\xc5");
  Map (SDLK_AE, "\xc6");
  Map (SDLK_Ccedilla, "\xc7");
  Map (SDLK_Egrave, "\xc8");
  Map (SDLK_Eacute, "\xc9");
  Map (SDLK_Ecircumflex, "\xca");
  Map (SDLK_Ediaeresis, "\xcb");
  Map (SDLK_Igrave, "\xcc");
  Map (SDLK_Iacute, "\xcd");
  Map (SDLK_Icircumflex, "\xce");
  Map (SDLK_Idiaeresis, "\xcf");
  Map (SDLK_ETH, "\xd0");
  Map (SDLK_Eth, "\xd0");
  Map (SDLK_Ntilde, "\xd1");
  Map (SDLK_Ograve, "\xd2");
  Map (SDLK_Oacute, "\xd3");
  Map (SDLK_Ocircumflex, "\xd4");
  Map (SDLK_Otilde, "\xd5");
  Map (SDLK_Odiaeresis, "\xd6");
  Map (SDLK_OE, "\xd7");
  Map (SDLK_Ooblique, "\xd8");
  Map (SDLK_Ugrave, "\xd9");
  Map (SDLK_Uacute, "\xda");
  Map (SDLK_Ucircumflex, "\xdb");
  Map (SDLK_Udiaeresis, "\xdc");
  Map (SDLK_Yacute, "\xdd");
  Map (SDLK_THORN, "\xde");
  Map (SDLK_Thorn, "\xde");
  Map (SDLK_ssharp, "sz");
  Map (SDLK_agrave, "\xe0");
  Map (SDLK_aacute, "\xe1");
  Map (SDLK_acircumflex, "\xe2");
  Map (SDLK_atilde, "\xe3");
  Map (SDLK_adiaeresis, "\xe4");
  Map (SDLK_aring, "\xe5");
  Map (SDLK_ae, "\xe6");
  Map (SDLK_ccedilla, "\xe7");
  Map (SDLK_egrave, "\xe8");
  Map (SDLK_eacute, "\xe9");
  Map (SDLK_ecircumflex, "\xea");
  Map (SDLK_ediaeresis, "\xeb");
  Map (SDLK_igrave, "\xec");
  Map (SDLK_iacute, "\xed");
  Map (SDLK_icircumflex, "\xee");
  Map (SDLK_idiaeresis, "\xef");
  Map (SDLK_eth, "\xf0");
  Map (SDLK_ntilde, "\xf1");
  Map (SDLK_ograve, "\xf2");
  Map (SDLK_oacute, "\xf3");
  Map (SDLK_ocircumflex, "\xf4");
  Map (SDLK_otilde, "\xf5");
  Map (SDLK_odiaeresis, "\xf6");
  Map (SDLK_oe, "\xf7");
  Map (SDLK_oslash, "\xf8");
  Map (SDLK_ugrave, "\xf9");
  Map (SDLK_uacute, "\xfa");
  Map (SDLK_ucircumflex, "\xfb");
  Map (SDLK_udiaeresis, "\xfc");
  Map (SDLK_yacute, "\xfd");
  Map (SDLK_thorn, "\xfe");
  Map (SDLK_ydiaeresis, "\xff");

  // Symbols from iso-latin-2
  Map (SDLK_Aogonek, "\x81");
  Map (SDLK_breve, "breve");
  Map (SDLK_Lstroke, "\x8a");
  Map (SDLK_Lcaron, "\x89");
  Map (SDLK_Sacute, "\x91");
  Map (SDLK_Scaron, "\x92");
  Map (SDLK_Scedilla, "\x93");
  Map (SDLK_Tcaron, "\x94");
  Map (SDLK_Zacute, "\x99");
  Map (SDLK_Zcaron, "\x9a");
  Map (SDLK_Zabovedot, "\x9b");
  Map (SDLK_aogonek, "\xa1");
  Map (SDLK_ogonek, "ogonek");
  Map (SDLK_lstroke, "\xaa");
  Map (SDLK_lcaron, "\xa9");
  Map (SDLK_sacute, "\xb1");
  Map (SDLK_caron, "caron");
  Map (SDLK_scaron, "\xb2");
  Map (SDLK_scedilla, "\xb3");
  Map (SDLK_tcaron, "\xb4");
  Map (SDLK_zacute, "\xb9");
  Map (SDLK_doubleacute, "doubleacute");
  Map (SDLK_zcaron, "\xba");
  Map (SDLK_zabovedot, "\xbb");
  Map (SDLK_Racute, "\x8f");
  Map (SDLK_Abreve, "\x80");
  Map (SDLK_Lacute, "\x88");
  Map (SDLK_Cacute, "\x82");
  Map (SDLK_Ccaron, "\x83");
  Map (SDLK_Eogonek, "\x86");
  Map (SDLK_Ecaron, "\x85");
  Map (SDLK_Dcaron, "\x84");
  Map (SDLK_Dstroke, "\xd0");
  Map (SDLK_Nacute, "\x8b");
  Map (SDLK_Ncaron, "\x8c");
  Map (SDLK_Odoubleacute, "\x8e");
  Map (SDLK_Rcaron, "\x90");
  Map (SDLK_Uring, "\x97");
  Map (SDLK_Udoubleacute, "\x96");
  Map (SDLK_Tcedilla, "\x95");
  Map (SDLK_racute, "\xaf");
  Map (SDLK_abreve, "\xa0");
  Map (SDLK_lacute, "\xa8");
  Map (SDLK_cacute, "\xa2");
  Map (SDLK_ccaron, "\xa3");
  Map (SDLK_eogonek, "\xa6");
  Map (SDLK_ecaron, "\xa5");
  Map (SDLK_dcaron, "\xa4");
  Map (SDLK_dstroke, "\x9e");
  Map (SDLK_nacute, "\xab");
  Map (SDLK_ncaron, "\xac");
  Map (SDLK_odoubleacute, "\xae");
  Map (SDLK_udoubleacute, "\xb6");
  Map (SDLK_rcaron, "\xb0");
  Map (SDLK_uring, "\xb7");
  Map (SDLK_tcedilla, "\xb5");
  Map (SDLK_abovedot, "abovedot");
#endif
  
  // Special control keys
  Map (SDLK_PAGEUP, "pageup");
  Map (SDLK_PAGEDOWN, "pagedown");
  Map (SDLK_UNDO, "undo");
//  Map (SDLK_REDO, "redo");
  Map (SDLK_CANCEL, "cancel");

  // Control keys
  map (SDLK_SPACE, "space");
  map (SDLK_RETURN, "return");
  map (SDLK_BACKSPACE, "backspace");
  map (SDLK_DELETE, "delete");
  map (SDLK_INSERT, "insert");
  map (SDLK_TAB, "tab");
  map (SDLK_ISO_Left_Tab, "tab");
  map (SDLK_ESCAPE, "escape");
  map (SDLK_LEFT, "left");
  map (SDLK_RIGHT, "right");
  map (SDLK_UP, "up");
  map (SDLK_DOWN, "down");
  map (SDLK_PAGEUP, "pageup");
  map (SDLK_PAGEDOWN, "pagedown");
  map (SDLK_HOME, "home");
  map (SDLK_END, "end");
  map (SDLK_F1, "F1");
  map (SDLK_F2, "F2");
  map (SDLK_F3, "F3");
  map (SDLK_F4, "F4");
  map (SDLK_F5, "F5");
  map (SDLK_F6, "F6");
  map (SDLK_F7, "F7");
  map (SDLK_F8, "F8");
  map (SDLK_F9, "F9");
  map (SDLK_F10, "F10");
  map (SDLK_F11, "F11");
  map (SDLK_F12, "F12");
  map (SDLK_F13, "F13");
  map (SDLK_F14, "F14");
  map (SDLK_F15, "F15");
  map (SDLK_F16, "F16");
  map (SDLK_F17, "F17");
  map (SDLK_F18, "F18");
  map (SDLK_F19, "F19");
  map (SDLK_F20, "F20");
  // map (SDLK_Mode_switch, "modeswitch");

  // Keypad keys
  Map (SDLK_KP_SPACE, "K-space");
  Map (SDLK_KP_ENTER, "K-return");
//  Map (SDLK_KP_DELETE, "K-delete");
//  Map (SDLK_KP_INSERT, "K-insert");
  Map (SDLK_KP_TAB, "K-tab");
//  Map (SDLK_KP_LEFT, "K-left");
//  Map (SDLK_KP_Right, "K-right");
//  Map (SDLK_KP_Up, "K-up");
//  Map (SDLK_KP_Down, "K-down");
//  Map (SDLK_KP_Page_Up, "K-pageup");
//  Map (SDLK_KP_Page_Down, "K-pagedown");
//  Map (SDLK_KP_Home, "K-home");
//  Map (SDLK_KP_Begin, "K-begin");
//  Map (SDLK_KP_End, "K-end");
//  Map (SDLK_KP_F1, "K-F1");
//  Map (SDLK_KP_F2, "K-F2");
//  Map (SDLK_KP_F3, "K-F3");
//  Map (SDLK_KP_F4, "K-F4");
  Map (SDLK_KP_EQUALS, "K-=");
  Map (SDLK_KP_MULTIPLY, "K-*");
  Map (SDLK_KP_PLUS, "K-+");
  Map (SDLK_KP_MINUS, "K--");
  Map (SDLK_KP_PERIOD, "K-.");
  Map (SDLK_KP_COMMA, "K-,");
  Map (SDLK_KP_DIVIDE, "K-/");
  Map (SDLK_KP_0, "K-0");
  Map (SDLK_KP_1, "K-1");
  Map (SDLK_KP_2, "K-2");
  Map (SDLK_KP_3, "K-3");
  Map (SDLK_KP_4, "K-4");
  Map (SDLK_KP_5, "K-5");
  Map (SDLK_KP_6, "K-6");
  Map (SDLK_KP_7, "K-7");
  Map (SDLK_KP_8, "K-8");
  Map (SDLK_KP_9, "K-9");

  // Miscellaneous
  Map (0x20ac, "euro");
}


/******************************************************************************
* Event loop
******************************************************************************/

#define MIN_DELAY   10
#define MAX_DELAY   1000
#define SLEEP_AFTER 120000

extern int nr_windows;
static void (*the_interpose_handler) (void) = NULL;

static int  kbd_count= 0;
static bool request_partial_redraw= false;


void
sdl_gui_rep::event_loop () {
  bool wait = true;
  int  count= 0;
  int  delay= MIN_DELAY;

  while (nr_windows>0 || number_of_servers () != 0) {
    request_partial_redraw= false;

    // Get events
    SDL_Event event;
    if (SDL_PollEvent (&event)) {
      process_event (&event);
      count= 0;
      delay= MIN_DELAY;
      wait = false;
    }
    if (nr_windows == 0) continue;

    // FIXME: Don't typeset when resizing window

    // Wait for events on all channels and interpose
    //time_t t1= texmacs_time ();
    if (wait) {
//      struct timeval tv;
//      tv.tv_sec  = delay/1000;
//      tv.tv_usec = 1000 * (delay%1000);
//      select (0, NULL, NULL, NULL, &tv);
      SDL_Delay (delay);
      count += delay;
      if (count >= SLEEP_AFTER) delay= MAX_DELAY;
    }
    else wait= true;
    if (the_interpose_handler != NULL) the_interpose_handler ();
    if (nr_windows == 0) continue;
    //time_t t2= texmacs_time ();
    //if (t2 - t1 >= 10) cout << "interpose took " << t2-t1 << "ms\n";

    // Popup help balloons
    if (!is_nil (balloon_wid))
      if (texmacs_time () - balloon_time >= 666)
        if (balloon_win == NULL)
          map_balloon ();

    // Redraw invalid windows
    //time_t t3= texmacs_time ();
    if (SDL_PollEvent (NULL) == 0 || request_partial_redraw) {
      interrupted= false;
      interrupt_time= texmacs_time () + (100 / (1 + 1));
//      interrupt_time= texmacs_time () + (100 / (XPending (dpy) + 1));
      iterator<SDL_Window*> it= iterate (Window_to_window);
      while (it->busy()) { // first the window which has the focus
        sdl_window win= (sdl_window) Window_to_window[it->next()];
        if (win->has_focus) win->repaint_invalid_regions();
      }
      it= iterate (Window_to_window);
      while (it->busy()) { // and then the other windows
        sdl_window win= (sdl_window) Window_to_window[it->next()];
        if (!win->has_focus) win->repaint_invalid_regions();
      }
    }
    //time_t t4= texmacs_time ();
    //if (t4 - t3 >= 10) cout << "redraw took " << t4-t3 << "ms\n";

    // Handle alarm messages
    if (!is_nil (messages)) {
      list<message> not_ready;
      while (!is_nil (messages)) {
        time_t ct= texmacs_time ();
        message m= messages->item;
        if ((m->t - ct) <= 0) send_delayed_message (m->wid, m->s, m->t);
        else not_ready= list<message> (m, not_ready);
        messages= messages->next;
      }
      messages= not_ready;
    }
  }
}

static sdl_window
get_window_from_ID (Uint32 ID) {
  SDL_Window *w= SDL_GetWindowFromID (ID);
  if (w == NULL) return NULL;
  sdl_window win= (sdl_window) Window_to_window [w];
  return win;
}

static string
lookup_mouse (Uint8 button) {
  if (button == SDL_BUTTON_LEFT)   return "left";
  if (button == SDL_BUTTON_MIDDLE) return "middle";
  if (button == SDL_BUTTON_RIGHT)  return "right";
  if (button == SDL_BUTTON_X1)     return "extra1";
  if (button == SDL_BUTTON_X2)     return "extra2";
  return "button-error";
}

static string
lookup_key (SDL_Keysym *key) {
  const char* str= SDL_GetKeyName (key->sym);
  string r(str, strlen(str));
  r= utf8_to_cork (r);
  if (contains_unicode_char (r)) return r;
//  string s=r;
  string s= ((key->mod & KMOD_SHIFT) ? upper_key [key->sym] : lower_key [key->sym]);
  if ((N(s)>=2) && (s[0]=='K') && (s[1]=='-')) s= s (2, N(s));

  if (key->mod & KMOD_CTRL) s= "C-" * s;
  if (key->mod & KMOD_ALT)  s= "A-" * s;
  if (key->mod & KMOD_GUI)  s= "M-" * s;
  cout << "key press: " << s << LF;
  return s;
}

void
sdl_gui_rep::process_event (SDL_Event *event) {
  switch (event->type) {
    case SDL_WINDOWEVENT:
    {
      sdl_window win= get_window_from_ID (event->window.windowID);
      if (win == NULL) break;
      switch (event->window.event) {
        case SDL_WINDOWEVENT_SHOWN:
          SDL_Log("Window %d shown", event->window.windowID);
          break;
        case SDL_WINDOWEVENT_HIDDEN:
          SDL_Log("Window %d hidden", event->window.windowID);
          break;
        case SDL_WINDOWEVENT_EXPOSED:
          SDL_Log("Window %d exposed", event->window.windowID);
          win->invalidate_all ();
          break;
        case SDL_WINDOWEVENT_MOVED:
          SDL_Log("Window %d moved to %d,%d",
                  event->window.windowID, event->window.data1,
                  event->window.data2);
          win->move_event (event->window.data1, event->window.data2);
          break;
        case SDL_WINDOWEVENT_RESIZED:
          SDL_Log("Window %d resized to %dx%d",
                  event->window.windowID, event->window.data1,
                  event->window.data2);
          break;
        case SDL_WINDOWEVENT_SIZE_CHANGED:
          SDL_Log("Window %d size changed to %dx%d",
                  event->window.windowID, event->window.data1,
                  event->window.data2);
          win->resize_event (event->window.data1, event->window.data2);
          win->invalidate_all ();
          break;
        case SDL_WINDOWEVENT_MINIMIZED:
          SDL_Log("Window %d minimized", event->window.windowID);
          break;
        case SDL_WINDOWEVENT_MAXIMIZED:
          SDL_Log("Window %d maximized", event->window.windowID);
          break;
        case SDL_WINDOWEVENT_RESTORED:
          SDL_Log("Window %d restored", event->window.windowID);
          break;
        case SDL_WINDOWEVENT_ENTER:
          SDL_Log("Mouse entered window %d",
                  event->window.windowID);
          //unmap_balloon ();
        {
          // FIXME: not quite right
          int x,y, ox,oy;
          update_mouse_state ();
          SDL_GetGlobalMouseState (&x, &y);
          SDL_GetWindowPosition (win->win, &ox, &oy);
          x -= ox; y -= oy;
          win->mouse_event ("enter", x, y, event->window.timestamp);

        }
          break;
        case SDL_WINDOWEVENT_LEAVE:
          SDL_Log("Mouse left window %d", event->window.windowID);
          //unmap_balloon ();
        {
          // FIXME: not quite right
          int x,y, ox,oy;
          update_mouse_state ();
          SDL_GetGlobalMouseState (&x, &y);
          SDL_GetWindowPosition(win->win, &ox, &oy);
          x -= ox; y -= oy;
          win->mouse_event ("leave", x, y, event->window.timestamp);

        }
          break;
        case SDL_WINDOWEVENT_FOCUS_GAINED:
          SDL_Log("Window %d gained keyboard focus",
                  event->window.windowID);
          win->focus_in_event ();
          break;
        case SDL_WINDOWEVENT_FOCUS_LOST:
          SDL_Log("Window %d lost keyboard focus",
                  event->window.windowID);
          win->focus_out_event ();
          break;
        case SDL_WINDOWEVENT_CLOSE:
          SDL_Log("Window %d closed", event->window.windowID);
          win->destroy_event();
          break;
#if SDL_VERSION_ATLEAST(2, 0, 5)
        case SDL_WINDOWEVENT_TAKE_FOCUS:
          SDL_Log("Window %d is offered a focus", event->window.windowID);
          break;
        case SDL_WINDOWEVENT_HIT_TEST:
          SDL_Log("Window %d has a special hit test", event->window.windowID);
          break;
#endif
        default:
          SDL_Log("Window %d got unknown event %d",
                  event->window.windowID, event->window.event);
          break;
      } // switch (event->window.event)
      break;
    } // case SDL_WINDOWEVENT:
    case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEBUTTONUP:
    {
      update_mouse_state ( SDL_BUTTON(event->button.button) );
      // we need to take into account explicitly the current button
#if 0
      if (event->type == SDL_MOUSEBUTTONDOWN)
        mouse_state = mouse_state | SDL_BUTTON (event->button.button);
      else
        mouse_state = mouse_state & ~SDL_BUTTON (event->button.button);
#endif
      cout << "new mouse state " << mouse_state << LF;
      sdl_window win= get_window_from_ID (event->button.windowID);
      if (win == NULL) break;
      unmap_balloon ();
      string action = event->button.type == SDL_MOUSEBUTTONDOWN ? "press-" : "release-";
//      set_button_state (event->button.state ^ get_button_mask (&ev->xbutton));
      win->mouse_event (action * lookup_mouse (event->button.button),
            event->button.x, event->button.y,  texmacs_time ());
      break;
    } // case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEWHEEL:
    {
      SDL_Log("Window %d got wheel event event %f %f",
              event->window.windowID, event->wheel.preciseX, event->wheel.preciseY);

      sdl_window win= get_window_from_ID (event->button.windowID);
      if (win == NULL) break;
      unmap_balloon ();
      update_mouse_state ();
      int x,y, ox,oy;
      SDL_GetGlobalMouseState (&x, &y);
      SDL_GetWindowPosition(win->win, &ox, &oy);
      x -= ox; y -= oy;
      float deltaX= event->wheel.preciseX;
      float deltaY= event->wheel.preciseY;
      if (deltaY >= 0.5) {
        win->mouse_event ("press-up", x, y, texmacs_time ());
      } else if (deltaY <= -0.5) {
        win->mouse_event ("press-down", x, y, texmacs_time ());
      }
      break;
    } // case SDL_MOUSEWHEEL:
    case SDL_MOUSEMOTION:
    {
      unmap_balloon ();
      update_mouse_state ();
      sdl_window win= get_window_from_ID (event->motion.windowID);
      if (win == NULL) break;
//      set_button_state (event->button.state ^ get_button_mask (&ev->xbutton));
      win->mouse_event ("move",
            event->motion.x, event->motion.y, event->motion.timestamp);
      break;
    } // case SDL_MOUSEMOTION:
    case SDL_KEYDOWN:
    {
      SDL_Log("Keydown: %s key acting as %s key", SDL_GetScancodeName(event->key.keysym.scancode), SDL_GetKeyName(event->key.keysym.sym));
      unmap_balloon ();
      sdl_window win= get_window_from_ID (event->key.windowID);
      if (win == NULL) break;
      string key= lookup_key (&event->key.keysym);
      //cout << "Press " << key << " at " << (time_t) ev->xkey.time
      //<< " (" << texmacs_time() << ")\n";
      kbd_count++;
      synchronize_time (event->key.timestamp);
      if (texmacs_time () - remote_time (event->key.timestamp) < 100 ||
          (kbd_count & 15) == 0)
        request_partial_redraw= true;
      //cout << "key   : " << key << "\n";
      //cout << "redraw: " << request_partial_redraw << "\n";
      if (N(key)>0) win->key_event (key);
      break;
    } // case SDL_KEYDOWN:
  } // switch (event->type)
}



/******************************************************************************
* Selections
******************************************************************************/

void
sdl_gui_rep::created_window (SDL_Window* win) {
  windows_l << win;
}

void
sdl_gui_rep::deleted_window (SDL_Window* win) {
  windows_l= remove (windows_l, win);
}

void
sdl_gui_rep::focussed_window (SDL_Window* win) {
  windows_l= list<SDL_Window*> (win, remove (windows_l, win));
}

bool
sdl_gui_rep::get_selection (string key, tree& t, string& s) {
  t= "none";
  s= "";
  bool res=false;

  if (selection_t->contains (key)) {
    t= copy (selection_t [key]);
    s= copy (selection_s [key]);
    return true;
  }
}

bool
sdl_gui_rep::set_selection (string key, tree t, string s) {
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
  return true;
}

void
sdl_gui_rep::clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
}

/******************************************************************************
* Mouse pointers
******************************************************************************/

void
sdl_gui_rep::set_mouse_pointer (widget w, string name) {
  // FIXME: implement
}

void
sdl_gui_rep::set_mouse_pointer (widget w, string name, string mask_name) {
  // FIXME: implement
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void
sdl_gui_rep::show_help_balloon (widget wid, SI x, SI y) {
  unmap_balloon ();
  balloon_wid = wid;
  balloon_win = NULL;
  balloon_x   = x;
  balloon_y   = y;
  balloon_time= texmacs_time ();
}

void
sdl_gui_rep::map_balloon () {
  widget win_wid= popup_window_widget (balloon_wid, "Balloon");
  set_position (win_wid, balloon_x, balloon_y);
  balloon_win= (window) get_sdl_window (win_wid);
  balloon_win->set_visibility (true);
}

void
sdl_gui_rep::unmap_balloon () {
  if (!is_nil (balloon_wid)) {
    if (balloon_win != NULL) {
      balloon_win->set_visibility (false);
      tm_delete (balloon_win);
      balloon_win= NULL;
    }
    balloon_wid= widget ();
  }
}

void
sdl_gui_rep::show_wait_indicator (widget w, string message, string arg) {
  // NOTE: the wait indicator is directly displayed inside the window
  // corresponding to w. We explicitly shortcut the main event loop
  // by invalidating the wait widget and requesting a redraw.
  // Using a popup window does not work, because it would be necessary
  // to return to the main loop to map and redraw it.
  sdl_window ww= get_sdl_window (w);
  if (ww == NULL || message == "") return;
  if (arg != "") message= message * " " * arg * "...";
  SI width= 400*PIXEL, height= 160*PIXEL;
  widget wait_wid= wait_widget (width, height, message);
  SI mid_x= (ww->win_w>>1)*PIXEL, mid_y= -(ww->win_h>>1)*PIXEL + height;
  SI x= mid_x- width/2, y= mid_y- height/2;
  widget old_wid= ww->w;
  ww->w= wait_wid;
  set_position (wait_wid, x, y);
  set_identifier (wait_wid, ww->id);
  send_invalidate_all (wait_wid);
  ww->repaint_invalid_regions ();
  ww->w= old_wid;
  send_invalidate_all (old_wid);
}

void
sdl_gui_rep::external_event (string type, time_t t) {
  (void) t;
  if (!is_nil (windows_l)) {
    SDL_Window* win= windows_l->item;
    sdl_window sdl_win= (sdl_window) Window_to_window[win];
    sdl_win->key_event (type);
  }
}

bool
sdl_gui_rep::check_event (int type) {
  bool status;
  switch (type) {
  case INTERRUPT_EVENT:
    if (interrupted) return true;
    else {
      int n=1; // n=XPending (dpy);
      time_t now= texmacs_time ();
      if (now - interrupt_time < 0) return false;
      else interrupt_time= now + (100 / (n + 1));
      interrupted= (SDL_HasEvent (SDL_KEYDOWN) == SDL_TRUE) ||
                   (SDL_HasEvent (SDL_MOUSEBUTTONDOWN) == SDL_TRUE);
      return interrupted;
    }
  case INTERRUPTED_EVENT:
    return interrupted;
  case ANY_EVENT:
    return (SDL_HasEvents(SDL_FIRSTEVENT, SDL_LASTEVENT) == SDL_TRUE);
  case MOTION_EVENT:
    status= (SDL_HasEvent (SDL_MOUSEMOTION) == SDL_TRUE);
    return status;
  case DRAG_EVENT:
    {
      status= false;
      SDL_Event event;
      if (SDL_PeepEvents (&event, 1, SDL_PEEKEVENT,
                          SDL_MOUSEMOTION, SDL_MOUSEMOTION)) {
        if (event.motion.state) {
          status= true;
        }
      }
    }
    return status;
  case MENU_EVENT:
    status= (SDL_HasEvent (SDL_MOUSEBUTTONUP) == SDL_TRUE);
    if (!status) {
      SDL_Event event;
      if (SDL_PeepEvents (&event, 1, SDL_PEEKEVENT,
                          SDL_MOUSEMOTION, SDL_MOUSEMOTION)) {
        status=  (event.motion.state != 0);
      }
    }
    return status;
  }
  return interrupted;
}


static string the_default_font ("");
font the_default_wait_font;

void
sdl_gui_rep::set_default_font (string name) {
  the_default_font= name;
}

font
sdl_gui_rep::default_font_sub (bool tt, bool mini, bool bold) {
  string s= the_default_font;
  string series= (bold? string ("bold"): string ("medium"));
  if (s == "") s= "ecrm11@300";
  int i, j, n= N(s);
  for (j=0; j<n; j++) if (is_digit (s[j])) break;
  string fam= s (0, j);
  if (mini && fam == "ecrm") fam= "ecss";
  if (bold && fam == "ecrm") fam= "ecbx";
  if (bold && fam == "ecss") fam= "ecsx";
  for (i=j; j<n; j++) if (s[j] == '@') break;
  int sz= (j<n? as_int (s (i, j)): 10);
  if (j<n) j++;
  int dpi= (j<n? as_int (s (j, n)): 300);
  if (mini) { sz= (int) (0.6 * sz); dpi= (int) (1.3333333 * dpi); }
  if (use_macos_fonts ()) {
    tree lucida_fn= tuple ("apple-lucida", "ss", series, "right");
    lucida_fn << as_string (sz) << as_string ((int) (0.95 * dpi));
    return find_font (lucida_fn);
  }
  if (N(fam) >= 2) {
    string ff= fam (0, 2);
    string out_lan= get_output_language ();
    if (((out_lan == "bulgarian") || (out_lan == "russian") ||
   (out_lan == "ukrainian")) &&
  ((ff == "cm") || (ff == "ec"))) {
      fam= "la" * fam (2, N(fam)); ff= "la"; if (sz<100) sz *= 100; }
    if (out_lan == "japanese" || out_lan == "korean") {
      tree modern_fn= tuple ("modern", "ss", series, "right");
      modern_fn << as_string (sz) << as_string (dpi);
      return find_font (modern_fn);
    }
    if (out_lan == "chinese" || out_lan == "taiwanese")
      return unicode_font ("fireflysung", sz, dpi);
    if (out_lan == "greek")
      return unicode_font ("Stix", sz, dpi);
    //if (out_lan == "japanese")
    //return unicode_font ("ipagui", sz, dpi);
    //if (out_lan == "korean")
    //return unicode_font ("UnDotum", sz, dpi);
    if (ff == "ec")
      return tex_ec_font (tt? ff * "tt": fam, sz, dpi);
    if (ff == "la")
      return tex_la_font (tt? ff * "tt": fam, sz, dpi, 1000);
    if (ff == "pu") tt= false;
    if ((ff == "cm") || (ff == "pn") || (ff == "pu"))
      return tex_cm_font (tt? ff * "tt": fam, sz, dpi);
  }
  return tex_font (fam, sz, dpi);
  // if (out_lan == "german") return tex_font ("ygoth", 14, 300, 0);
  // return tex_font ("rpagk", 10, 300, 0);
  // return tex_font ("rphvr", 10, 300, 0);
  // return ps_font ("b&h-lucidabright-medium-r-normal", 11, 300);
}

font
sdl_gui_rep::default_font (bool tt, bool mini, bool bold) {
  font fn= default_font_sub (tt, mini, bold);
  if (!tt && !mini) the_default_wait_font= fn;
  return fn;
}

/******************************************************************************
* Interface
******************************************************************************/

void
gui_open (int& argc2, char** argv2) {
  ASSERT (the_gui == NULL, "gui already open");
  the_gui= tm_new<sdl_gui_rep> (argc2, argv2);
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
  iterator<SDL_Window*> it= iterate (Window_to_window);
  while (it->busy()) {
    sdl_window win= (sdl_window) Window_to_window [it->next()];
    if (get_sdl_window (win->w) != NULL)
      send_update (win->w);
  }
}

string
gui_version () {
  return "sdl";
}


void
beep () {
  // FIXME: implement
}

void
show_help_balloon (widget wid, SI x, SI y) {
  the_gui->show_help_balloon (wid, x, y);
}

void
show_wait_indicator (widget w, string message, string arg) {
  the_gui->show_wait_indicator (w, message, arg);
}

void
external_event (string type, time_t t) {
  the_gui->external_event (type, t);
}

void
needs_update () {
}

bool
check_event (int type) {
  return the_gui->check_event (type);
}

bool
set_selection (string key, tree t,
               string s, string sv, string sh, string format) {
  (void) format;
  return the_gui->set_selection (key, t, s);
}

bool
get_selection (string key, tree& t, string& s, string format) {
  (void) format;
  return the_gui->get_selection (key, t, s);
}

void
clear_selection (string key) {
  the_gui->clear_selection (key);
}

void gui_interpose (void (*r) (void)) {
  the_interpose_handler= r;
}

void
set_default_font (string name) {
  the_gui->set_default_font (name);
}

font
get_default_font (bool tt, bool mini, bool bold) {
  return the_gui->default_font (tt, mini, bold);
}


