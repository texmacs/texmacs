
/******************************************************************************
* MODULE     : player.cpp
* DESCRIPTION: animation players
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "player.hpp"
#include "timer.hpp"

/******************************************************************************
* Basic players
******************************************************************************/

class basic_player_rep: public player_rep {
public:
  double started;
  double speed;

  basic_player_rep (double started, double speed);

  void   set_started (double t);
  double get_started ();
  void   set_speed (double s);
  double get_speed ();

  void   set_elapsed (double t);
  double get_elapsed ();
  double get_refresh_time (double dt);

  player duplicate ();
  tree   expression ();
  void   print (tm_ostream& out);

  friend class player;
};

basic_player_rep::basic_player_rep (double started2, double speed2) {
  started= started2;
  speed  = speed2;
}

void
basic_player_rep::set_started (double t) {
  started= t;
}

double
basic_player_rep::get_started () {
  return started;
}

void
basic_player_rep::set_speed (double s) {
  double t= get_elapsed ();
  speed= s;
  set_elapsed (t);
}

double
basic_player_rep::get_speed () {
  return speed;
}

void
basic_player_rep::set_elapsed (double t) {
  started= ((double) texmacs_time ()) - (t / speed);
}

double
basic_player_rep::get_elapsed () {
  return (((double) texmacs_time ()) - started) * speed;
}

double
basic_player_rep::get_refresh_time (double dt) {
  return texmacs_time () + fabs (dt / speed) + 1.0;
}

player
basic_player_rep::duplicate () {
  return player (started, speed);
}

tree
basic_player_rep::expression () {
  return tree (TUPLE, as_string (started), as_string (speed));
}

void
basic_player_rep::print (tm_ostream& out) {
  out << "player (" << started << ", " << speed << ")";
}

/******************************************************************************
* Abstract players
******************************************************************************/

int player_count= 0;

player::player ():
  rep (tm_new<basic_player_rep> ((double) texmacs_time (), 1.0)) {
    INC_COUNT (rep); }

player::player (double started, double speed):
  rep (tm_new<basic_player_rep> (started, speed)) { INC_COUNT (rep); }

player::operator tree () {
  return rep->expression ();
}

player
copy (player p) {
  return p->duplicate ();
}

bool
operator == (player p1, player p2) {
  return p1.rep == p2.rep;
}

bool
operator != (player p1, player p2) {
  return p1.rep != p2.rep;
}

tm_ostream&
operator << (tm_ostream& out, player p) {
  p->print (out);
  return out;
}
