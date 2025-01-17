/******************************************************************************
* MODULE     : QTMKeyboardEvent.cpp
* DESCRIPTION: Qt TeXmacs keyboard event handling class, that converts
*              Qt key events into TeXmacs key combinations.
* COPYRIGHT  : (C) 2024 Massimiliano Gubinelli, Grégoire Lecerf, Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMKeyboardEvent.hpp"

#include <QApplication>
#include "analyze.hpp"
#include "basic.hpp"

#if QT_VERSION < 0x060000
inline bool inputMethodIsNotAnyTerritory() {
  return QApplication::inputMethod()->locale().country() 
         != QLocale::AnyTerritory;
}
#else
inline bool inputMethodIsNotAnyTerritory() {
  return QApplication::inputMethod()->locale().territory()
         != QLocale::AnyCountry;
}
#endif

void QTMKeyboardEvent::printDebugInformations() const {
  debug_qt << "keypressed" << LF;
  int key = mEvent.key();
  debug_qt << "key  : " << key << LF;
  debug_qt << "text : " << mEvent.text().toUtf8().data() << LF;
#if QT_VERSION >= 0x060000      
  debug_qt << "count: " << mEvent.text().size() << LF;
#else
  debug_qt << "count: " << mEvent.text().count() << LF;
#endif
  debug_qt << "unic : " << mEvent.text().data()[0].unicode() << LF;

#ifdef OS_MINGW
  debug_qt << "nativeScanCode: " << mEvent.nativeScanCode() << LF; 
  debug_qt << "nativeVirtualKey: " << mEvent.nativeVirtualKey() << LF;
  debug_qt << "nativeModifiers: " << mEvent.nativeModifiers() << LF;
#endif
  if (isShift()) debug_qt << "shift" << LF;
  if (isMeta()) debug_qt << "meta" << LF;
  if (isControl()) debug_qt << "control" << LF;
  if (isKeyPad()) debug_qt << "keypad" << LF;
  if (isAlt()) debug_qt << "alt" << LF;
}

void QTMKeyboardEvent::patchForMingw() {
#ifdef OS_MINGW
  /* "Qt::Key_AltGr On Windows, when the KeyDown event for this key is sent,
  * the Ctrl+Alt modifiers are also set." (excerpt from Qt doc)
  * However the AltGr key is used to obtain many symbols 
  * which should not be regarded as C-A- shortcuts.
  * (e.g. \ or @ on a French keyboard) 
  * 
  * Hence, when "native modifiers" are (ControlLeft | AltRight) 
  * we clear Qt's Ctrl+Alt modifiers
  */
  quint32 controlAlt = ControlLeft | AltRight;
  if ((mEvent.nativeModifiers() & controlAlt) == controlAlt) {
    if (DEBUG_QT && DEBUG_KEYBOARD) {
      debug_qt << "assuming it's an AltGr key code" << LF;
    }
    removeAlt();
    removeControl();
  }
#endif
}

bool QTMKeyboardEvent::patchForShift() {
  bool shifted = unic < 32 && isAscii();
#ifdef Q_OS_WIN
  shifted = (shifted && unic > 0) 
          || (unic > 0 && unic < 255 && mKey > 32 && isShift() && isControl());
#endif

  if (!shifted) {
    return false;
  }
  // NOTE: For some reason, the 'shift' modifier key is not applied
  // to 'key' when 'control' is pressed as well.  We perform some
  // dirty hacking to figure out the right shifted variant of a key
  // by ourselves...
  bool upcase = is_upcase((char) mKey);

  if (upcase && !isShift()) {
      mKey= (int) locase ((char) mKey);
  }

  if (!upcase && mKeyboard.hasShiftPreference (kc) && isShift() && isControl()) {
    string pref= mKeyboard.getShiftPreference (kc);
    if (N(pref) > 0) mKey= (int) (unsigned char) pref [0];
    if (DEBUG_QT && DEBUG_KEYBOARD) {
      debug_qt << "Control+Shift " << kc << " -> " << mKey << LF;
    }
  }
  removeShift();
  mTexmacsKeyCombination= string ((char) mKey);
  return true;
  
}

void QTMKeyboardEvent::computeUnicodeToCork() {
  switch (unic) {
    case 96:   mTexmacsKeyCombination= "`"; 
    // unicode to cork conversion not appropriate for this case...
#ifdef Q_OS_MAC
    // CHECKME: are these two MAC exceptions really needed?
      if (isAlt()) mTexmacsKeyCombination= "grave";
#endif
      break;
    case 168:  mTexmacsKeyCombination= "umlaut"; break;
    case 180:  mTexmacsKeyCombination= "acute"; break;
      // the following combining characters should be caught by qtdeadmap
    case 0x300: mTexmacsKeyCombination= "grave"; break;
    case 0x301: mTexmacsKeyCombination= "acute"; break;
    case 0x302: mTexmacsKeyCombination= "hat"; break;
    case 0x308: mTexmacsKeyCombination= "umlaut"; break;
    case 0x33e: mTexmacsKeyCombination= "tilde"; break;
    default:
      QByteArray buf= nss.toUtf8();
      string rr (buf.constData(), buf.size());
      string tstr= utf8_to_cork (rr);
      // HACK! The encodings defined in langs/encoding and which
      // utf8_to_cork uses (via the converters loaded in
      // converter_rep::load()), enclose the texmacs symbols in "< >", 
      // but this format is not used for keypresses, so we must remove
      // them.
      int len= N (tstr);
      if (len >= 1 && tstr[0] == '<' && tstr[1] != '#' && tstr[len-1] == '>') {
        mTexmacsKeyCombination= tstr (1, len-1);
      }
      else {
        mTexmacsKeyCombination= tstr;
      }
      if (mTexmacsKeyCombination == "less") {
        mTexmacsKeyCombination= "<";
      }
      else if (mTexmacsKeyCombination == "gtr") {
        mTexmacsKeyCombination= ">";
      }
  }
}

void QTMKeyboardEvent::patchForMac() {
  bool modifiersAlt = isAlt();
#if QT_VERSION >= 0x060000
  modifiersAlt = modifiersAlt && inputMethodIsNotAnyTerritory();
#endif
  if (!modifiersAlt) {
    return;
  }

  bool mustAlter = mKey >= 32 && mKey < 128 && (
                      N(mTexmacsKeyCombination) != 1 
                      || ((int) (unsigned char) mTexmacsKeyCombination[0]) < 32 
                      || ((int) (unsigned char) mTexmacsKeyCombination[0]) >= 128
                   );
  // todo : check commit 14560
#if QT_VERSION < 0x060000
  mustAlter = mustAlter 
            && ((mModifiers & (Qt::MetaModifier + Qt::ControlModifier)) == 0);
#endif
  if (mustAlter) {
    if (!isShift() && isUpcase()) {
      toLower();
    }
    mKeyboard.composemap()(mKey) = mTexmacsKeyCombination;
    mTexmacsKeyCombination= string ((char) mKey);
  }
  else {
    removeAlt();
  }
}

void
QTMKeyboardEvent::handleKeyboardByTexmacs() {
    // We need to use text(): Alt-{5,6,7,8,9} are []|{} under MacOS, etc.
  nss = mEvent.text();
  kc  = mEvent.nativeVirtualKey();
  if (nss.size() == 0) {
    return;
  }
  unic= nss.data()[0].unicode();

  if (unic > 32 && unic < 255 && 
      isShift() && !isControl() && !isAlt() && !isMeta()) {
    mKeyboard.setShiftPreference (kc, (char) unic);
  }

  if (patchForShift()) {
    return;
  }

  computeUnicodeToCork();

#ifdef Q_OS_MAC
  patchForMac();
#endif

  removeShift();
      
}

void QTMKeyboardEvent::computeModifiers() {
  if (isShift()) {
    mTexmacsKeyCombination= "S-" * mTexmacsKeyCombination;
  }
#if defined(Q_OS_MAC) && QT_VERSION >= 0x060000
  if (inputMethodIsNotAnyTerritory()) {
    if (isAlt()) {
      mTexmacsKeyCombination= "A-" * mTexmacsKeyCombination;
    }
  }
#else
  if (isAlt()) {
    mTexmacsKeyCombination= "A-" * mTexmacsKeyCombination;
  }
#endif
  //if (isKeyPad()) mTexmacsKeyCombination= "K-" * mTexmacsKeyCombination;
#ifdef Q_OS_MAC
  if (isMeta()) {
    // The "Control" key
    mTexmacsKeyCombination= "C-" * mTexmacsKeyCombination;
  }
  if (isControl()) {
    // The "Command" key
    mTexmacsKeyCombination= "M-" * mTexmacsKeyCombination;
  }
#else
  if (isControl()) {
    mTexmacsKeyCombination= "C-" * mTexmacsKeyCombination;
  }
  if (isMeta()) {
    // The "Windows" key
    mTexmacsKeyCombination= "M-" * mTexmacsKeyCombination;
  }
#endif
}
    

void QTMKeyboardEvent::handleKeyboardEvent() {
  if (DEBUG_QT && DEBUG_KEYBOARD) {
    printDebugInformations();
  }

  patchForMingw();

  if (!handleQtKeyAny()) {
    handleKeyboardByTexmacs();
  }

  if (mTexmacsKeyCombination == "") return;

  computeModifiers();
}


bool QTMKeyboardEvent::handleQtKeyAny () {
    if (handleQtKeyMap ()) {
        return true;
    }
    return handleQtDeadMap ();
}

bool QTMKeyboardEvent::handleQtKeyMap () {
  if (!mKeyboard.keymap()->contains (mKey)) {
    return false;
  }
  mTexmacsKeyCombination = mKeyboard.keymap()[mKey];
#if defined(OS_MINGW) && QT_VERSION >= 0x060000
  // e.g. azerty keyboard: AltGr tilde followed by Space
  if (mKey == 32) {
    QByteArray buf = mEvent.text().toUtf8();
    mTexmacsKeyCombination = string (buf.data(), buf.size());
  }
#endif
  return true;
}

bool QTMKeyboardEvent::handleQtDeadMap () {
  if (!mKeyboard.deadmap()->contains(mKey)) {
    return false;
  }
  removeShift();
  mTexmacsKeyCombination = mKeyboard.deadmap()[mKey];
  return true;
}