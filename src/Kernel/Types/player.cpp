
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
* Constructors
******************************************************************************/

player_rep::player_rep (double started2, double speed2) {
  started= started2;
  speed  = speed2;
}

player::player () {
  rep= tm_new<player_rep> ((double) texmacs_time (), 1.0);
}

player::player (double started, double speed) {
  rep= tm_new<player_rep> (started, speed);
}

player::operator tree () {
  return tree (TUPLE,
	       as_string (rep->started),
	       as_string (rep->speed));
}

player
copy (player p) {
  return player (p->started, p->speed);
}

/******************************************************************************
* Functions on players
******************************************************************************/

void
player_rep::set_elapsed (double t) {
  started= ((double) texmacs_time ()) - (t / speed);
}

double
player_rep::get_elapsed () {
  return (((double) texmacs_time ()) - started) * speed;
}

void
player_rep::set_speed (double s) {
  double t= get_elapsed ();
  speed= s;
  set_elapsed (t);
}

double
player_rep::get_speed () {
  return speed;
}

bool
operator == (player p1, player p2) {
  return p1->started == p2->started && p1->speed == p2->speed;
}

bool
operator != (player p1, player p2) {
  return !(p1 == p2);
}

tm_ostream&
operator << (tm_ostream& out, player p) {
  out << "player (" << p->started << ", " << p->speed << ")";
  return out;
}

/******************************************************************************
* Refreshing
******************************************************************************/

static double next_refresh= 1.0e12;

void
player_rep::request_refresh (double dt) {
  double t= texmacs_time () + fabs (dt / speed) + 1.0;
  next_refresh= min (next_refresh, t);
}

double
get_next_refresh () {
  return max (next_refresh, ((double) texmacs_time ()) + 40.0);
}

void
clear_next_refresh () {
  next_refresh= 1.0e12;
}
