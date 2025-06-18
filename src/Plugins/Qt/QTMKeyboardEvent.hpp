/******************************************************************************
* MODULE     : QTMKeyboardEvent.hpp
* DESCRIPTION: Qt TeXmacs keyboard event handling class, that converts
*              Qt key events into TeXmacs key combinations.
* COPYRIGHT  : (C) 2024 Massimiliano Gubinelli, Grégoire Lecerf, Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMKEYBOARDEVENT_HPP
#define QTMKEYBOARDEVENT_HPP

#include <QObject>
#include <QEvent>
#include <QKeyEvent>

#include "QTMKeyboard.hpp"
#include "string.hpp"

class QTMKeyboardEvent {

public:
    QTMKeyboardEvent(QTMKeyboard &keyboard, const QKeyEvent &event) 
    : mKeyboard(keyboard), mEvent(event) {
        mKey = mEvent.key();
        mModifiers = mEvent.modifiers();
        handleKeyboardEvent();
    }

    void printDebugInformations() const;

    string texmacsKeyCombination() const {
        return mTexmacsKeyCombination;
    }

    inline bool isAlt() const {
        return (mModifiers & Qt::AltModifier) != 0;
    }

    inline bool isControl() const {
        return (mModifiers & Qt::ControlModifier) != 0;
    }

    inline bool isShift() const {
        return (mModifiers & Qt::ShiftModifier) != 0;
    }

    inline bool isMeta() const {
        return (mModifiers & Qt::MetaModifier) != 0;
    }

    inline bool isKeyPad() const {
        return (mModifiers & Qt::KeypadModifier) != 0;
    }

    inline bool removeAlt() {
        return mModifiers &= ~Qt::AltModifier;
    }

    inline bool removeControl() {
        return mModifiers &= ~Qt::ControlModifier;
    }

    inline bool removeShift() {
        return mModifiers &= ~Qt::ShiftModifier;
    }

    inline bool removeMeta() {
        return mModifiers &= ~Qt::MetaModifier;
    }

    inline bool removeKeyPad() {
        return mModifiers &= ~Qt::KeypadModifier;
    }

    inline bool isUpcase() const {
        return mKey >= 'A' && mKey <= 'Z';
    }

    inline bool toLower() {
        mKey += 32;
    }

    inline bool isAscii() const {
        return mKey > 0 && mKey < 128;
    }

protected:
    void patchForMingw();
    bool patchForShift();
    void computeUnicodeToCork();
    void patchForMac();
    void computeModifiers();

    bool handleQtKeyAny ();
    bool handleQtKeyMap ();
    bool handleQtDeadMap ();

    void handleKeyboardByTexmacs();

    void handleKeyboardEvent();

private:
    QTMKeyboard &mKeyboard;
    const QKeyEvent &mEvent;
    int mKey;
    Qt::KeyboardModifiers mModifiers;
    string mTexmacsKeyCombination;
    QString nss;
    unsigned int   kc;
    unsigned short unic;
    

};

#endif // QTMKEYBOARD_HPP