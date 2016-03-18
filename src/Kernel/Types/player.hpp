
/******************************************************************************
* MODULE     : player.hpp
* DESCRIPTION: animation players
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PLAYER_H
#define PLAYER_H
#include "tree.hpp"

class player_rep: concrete_struct {
public:
  double started;
  double speed;

  player_rep (double started, double speed);

  void   set_elapsed (double t);
  double get_elapsed ();
  void   set_speed (double s);
  double get_speed ();
  double get_refresh_time (double dt);

  friend class player;
};

class player {
  CONCRETE(player);
  player ();
  player (double started, double speed);
  operator tree ();
};
CONCRETE_CODE(player);

player copy (player spc);
bool operator == (player p1, player p2);
bool operator != (player p1, player p2);
tm_ostream& operator << (tm_ostream& out, player p);

#endif // defined PLAYER_H
