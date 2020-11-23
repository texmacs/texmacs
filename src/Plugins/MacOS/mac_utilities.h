/******************************************************************************
 * MODULE     : mac_utilities.h
 * DESCRIPTION: Cocoa related utilites (also for TeXmacs/Qt)
 * COPYRIGHT  : (C) 2010  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef MAC_UTILITIES_H
#define MAC_UTILITIES_H

#include "string.hpp"

bool mac_alternate_startup();

void mac_begin_remote();
void mac_end_remote();
void mac_remote_button (string button, bool pressed); 
double mac_screen_scale_factor();

void mac_fix_yosemite_bug();

void mac_begin_server ();
void mac_end_server ();

#endif // MAC_UTILITIES_H
