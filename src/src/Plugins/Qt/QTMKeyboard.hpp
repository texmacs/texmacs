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

#ifndef QTMKEYBOARD_HPP
#define QTMKEYBOARD_HPP

#include <QKeyEvent>
#include <QKeySequence>

#include "string.hpp"
#include "hashmap.hpp"

#ifdef OS_MINGW
enum WindowsNativeModifiers {
    ShiftLeft            = 0x00000001,
    ControlLeft          = 0x00000002,
    AltLeft              = 0x00000004,
    MetaLeft             = 0x00000008,
    ShiftRight           = 0x00000010,
    ControlRight         = 0x00000020,
    AltRight             = 0x00000040,
    MetaRight            = 0x00000080,
    CapsLock             = 0x00000100,
    NumLock              = 0x00000200,
    ScrollLock           = 0x00000400,
    ExtendedKey          = 0x01000000,
};
#endif

class QTMKeyboard {

public:
    QTMKeyboard () : qtkeymap (0), qtdeadmap (0), qtcomposemap (0) { 
        initkeymap (); 
    }

    void setShiftPreference (int key_code, char shifted);
    bool hasShiftPreference (int key_code);
    string getShiftPreference (char key_code);

    void map (int code, string name);
    void deadmap (int code, string name);

    inline void getMappingIfExist (int key, string &name) {
        if (qtkeymap->contains (key)) name = qtkeymap[key];
    }

    inline hashmap<int,string> &composemap () { 
        return qtcomposemap; 
    }

    inline hashmap<int,string> &keymap () {
        return qtkeymap; 
    }

    inline hashmap<int,string> &deadmap () {
        return qtdeadmap; 
    }

protected:
    void initkeymap ();

private:
    hashmap<int,string> qtkeymap;
    hashmap<int,string> qtdeadmap;
    hashmap<int,string> qtcomposemap;

};

#endif // QTMKEYBOARD_HPP