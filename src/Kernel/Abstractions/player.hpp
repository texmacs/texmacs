
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

class player;
extern int player_count;

class player_rep: public abstract_struct {
public:
  inline player_rep () { TM_DEBUG(player_count++); }
  inline virtual ~player_rep () { TM_DEBUG(player_count--); }

  virtual void   set_speed (double s) = 0;
  virtual double get_speed () = 0;
  virtual void   set_started (double t) = 0;
  virtual double get_started () = 0;
  virtual void   set_duration (double l) = 0;
  virtual double get_duration () = 0;

  virtual void   set_elapsed (double t) = 0;
  virtual double get_elapsed () = 0;
  virtual double get_refresh_time (double dt) = 0;

  virtual player duplicate () = 0;
  virtual tree   expression () = 0;
  virtual void   print (tm_ostream& out) = 0;

  friend class player;
};

class player {
  ABSTRACT(player);
  player ();
  player (double started, double speed);
  operator tree ();
  friend bool operator == (player p1, player p2);
  friend bool operator != (player p1, player p2);
};
ABSTRACT_CODE(player);

player copy (player spc);
bool operator == (player p1, player p2);
bool operator != (player p1, player p2);
tm_ostream& operator << (tm_ostream& out, player p);

player reverse_player (player base);
player fade_in_player (player base);
player fade_out_player (player base);
player faded_player (player base);
player bump_player (player base);

#endif // defined PLAYER_H
