
/******************************************************************************
* MODULE     : mac_spellservice.h
* DESCRIPTION: interface with the MacOSX standard spell service
* COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MAC_SPELLSERVICE_H
#define MAC_SPELLSERVICE_H
#include "tree.hpp"

string mac_spell_start (string lan);
tree   mac_spell_check (string lan, string s);
void   mac_spell_accept (string lan, string s);
void   mac_spell_insert (string lan, string s);
void   mac_spell_done (string lan);

#endif // MAC_SPELLSERVICE_H
