/******************************************************************************
* MODULE     : QTMKeyboard.cpp
* DESCRIPTION: Qt TeXmacs class, that handle global keyboard settings,
*              keymaps, and event handling.
* COPYRIGHT  : (C) 2024 Massimiliano Gubinelli, Grégoire Lecerf, Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMKeyboard.hpp"
#include "boot.hpp"

inline void
QTMKeyboard::map (int code, string name) {
  qtkeymap (code) = name;
}

inline void
QTMKeyboard::deadmap (int code, string name) {
  qtdeadmap (code) = name;
}

void
QTMKeyboard::initkeymap () {
  static bool fInit= false;
  if (fInit) return;
  fInit= true;
  if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "Initializing keymap\n";
  map (Qt::Key_Space     , "space");
  map (Qt::Key_Tab       , "tab");
  map (Qt::Key_Backtab   , "tab");
  map (Qt::Key_Return    , "return");
  map (Qt::Key_Enter     , "enter");
  map (Qt::Key_Escape    , "escape");
  map (Qt::Key_Backspace , "backspace");
  map (Qt::Key_Up        , "up" );
  map (Qt::Key_Down      , "down" );
  map (Qt::Key_Left      , "left" );
  map (Qt::Key_Right     , "right" );
  map (Qt::Key_F1        , "F1" );
  map (Qt::Key_F2        , "F2" );
  map (Qt::Key_F3        , "F3" );
  map (Qt::Key_F4        , "F4" );
  map (Qt::Key_F5        , "F5" );
  map (Qt::Key_F6        , "F6" );
  map (Qt::Key_F7        , "F7" );
  map (Qt::Key_F8        , "F8" );
  map (Qt::Key_F9        , "F9" );
  map (Qt::Key_F10       , "F10" );
  map (Qt::Key_F11       , "F11" );
  map (Qt::Key_F12       , "F12" );
  map (Qt::Key_F13       , "F13" );
  map (Qt::Key_F14       , "F14" );
  map (Qt::Key_F15       , "F15" );
  map (Qt::Key_F16       , "F16" );
  map (Qt::Key_F17       , "F17" );
  map (Qt::Key_F18       , "F18" );
  map (Qt::Key_F19       , "F19" );
  map (Qt::Key_F20       , "F20" );
  map (Qt::Key_F21       , "F21" );
  map (Qt::Key_F22       , "F22" );
  map (Qt::Key_F23       , "F23" );
  map (Qt::Key_F24       , "F24" );
  map (Qt::Key_F25       , "F25" );
  map (Qt::Key_F26       , "F26" );
  map (Qt::Key_F27       , "F27" );
  map (Qt::Key_F28       , "F28" );
  map (Qt::Key_F29       , "F29" );
  map (Qt::Key_F30       , "F30" );
  map (Qt::Key_F31       , "F31" );
  map (Qt::Key_F32       , "F32" );
  map (Qt::Key_F33       , "F33" );
  map (Qt::Key_F34       , "F34" );
  map (Qt::Key_F35       , "F35" );
  map (Qt::Key_Insert    , "insert" );
  map (Qt::Key_Delete    , "delete" );
  map (Qt::Key_Home      , "home" );
  map (Qt::Key_End       , "end" );
  map (Qt::Key_PageUp    , "pageup" );
  map (Qt::Key_PageDown  , "pagedown" );
  map (Qt::Key_ScrollLock, "scrolllock" );
  map (Qt::Key_Pause     , "pause" );
  map (Qt::Key_SysReq    , "sysreq" );
  map (Qt::Key_Stop      , "stop" );
  map (Qt::Key_Menu      , "menu" );
  map (Qt::Key_Print     , "print" );
  map (Qt::Key_Select    , "select" );
  map (Qt::Key_Execute   , "execute" );
  map (Qt::Key_Help      , "help" );
  map (Qt::Key_section   , "section" );

  deadmap (Qt::Key_Dead_Acute     , "acute");
  deadmap (Qt::Key_Dead_Grave     , "grave");
  deadmap (Qt::Key_Dead_Diaeresis , "umlaut");
  deadmap (Qt::Key_Dead_Circumflex, "hat");
  deadmap (Qt::Key_Dead_Tilde     , "tilde");

  // map (0x0003              , "K-enter");
  // map (Qt::Key_Begin       , "begin" );
  // map (Qt::Key_PrintScreen , "printscreen" );
  // map (Qt::Key_Break       , "break" );
  // map (Qt::Key_User        , "user" );
  // map (Qt::Key_System      , "system" );
  // map (Qt::Key_Reset       , "reset" );
  // map (Qt::Key_ClearLine   , "clear" );
  // map (Qt::Key_ClearDisplay, "cleardisplay" );
  // map (Qt::Key_InsertLine  , "insertline" );
  // map (Qt::Key_DeleteLine  , "deleteline" );
  // map (Qt::Key_InsertChar  , "insert" );
  // map (Qt::Key_DeleteChar  , "delete" );
  // map (Qt::Key_Prev        , "prev" );
  // map (Qt::Key_Next        , "next" );
  // map (Qt::Key_Undo        , "undo" );
  // map (Qt::Key_Redo        , "redo" );
  // map (Qt::Key_Find        , "find" );
  // map (Qt::Key_ModeSwitchFunctionKey, "modeswitch" );
}

void
QTMKeyboard::setShiftPreference (int key_code, char shifted) {
  set_user_preference ("shift-" * as_string (key_code), string (shifted));
}

bool
QTMKeyboard::hasShiftPreference (int key_code) {
  return has_user_preference ("shift-" * as_string (key_code));
}

string
QTMKeyboard::getShiftPreference (char key_code) {
  return get_user_preference ("shift-" * as_string (key_code));
}
