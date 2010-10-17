/******************************************************************************
 * MODULE     : mac_app.h
 * DESCRIPTION: NSApplication related function for the X11 interface
 * COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef MAC_APP_H
#define MAC_APP_H


void init_mac_application ();

void finalize_mac_application ();

void process_mac_events ();

bool mac_alternate_startup();

#endif // MAC_APP_H
