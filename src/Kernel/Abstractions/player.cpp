
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
  double duration;

  basic_player_rep (double started, double speed);

  void   set_started (double t);
  double get_started ();
  void   set_speed (double s);
  double get_speed ();
  void   set_duration (double l);
  double get_duration ();

  bool   is_progressing ();
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
basic_player_rep::set_duration (double l) {
  duration= l;
}

double
basic_player_rep::get_duration () {
  return duration;
}

bool
basic_player_rep::is_progressing () {
  return speed > 0;
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
* Accelerations
******************************************************************************/

class accelerated_player_rep: public player_rep {
public:
  player base;
  double duration;

  accelerated_player_rep (player base2):
    base (base2), duration (1000.0) {}

  virtual tree   get_name () = 0;
  virtual double transform_direct (double t) = 0;
  virtual double transform_inverse (double t, double ref) = 0;

  double accelerate (double t) {
    return duration * transform_direct (t / duration); }
  double decelerate (double t, double ref) {
    return duration * transform_inverse (t / duration, ref / duration); }
  
  void   set_started (double t) { base->set_started (t); }
  double get_started () { return base->get_started (); }
  void   set_speed (double s) { base->set_speed (s); }
  double get_speed () { return base->get_speed (); }
  void   set_duration (double l) { duration= l; base->set_duration (l); }
  double get_duration () { return duration; }

  bool   is_progressing ();
  void   set_elapsed (double t) { base->set_elapsed (t); }
  double get_elapsed () { return accelerate (base->get_elapsed ()); }
  double get_refresh_time (double dt);

  tree   expression () {
    return tuple (get_name (), as_tree (duration), (tree) base); }
  void   print (tm_ostream& out) {
    out << get_name () << " (" << duration << ", ";
    base->print (out);
    out << ")"; }
  
  friend class player;
};

bool
accelerated_player_rep::is_progressing () {
  double a0= base->get_elapsed ();
  return
    (accelerate (a0 + 0.001) >= accelerate (a0)) ^
    !base->is_progressing ();
}

double
accelerated_player_rep::get_refresh_time (double dt) {
  if (is_progressing ()) {
    double a0= base->get_elapsed ();
    double t0= accelerate (a0);
    double t1= t0 + fabs (dt);
    double a1= decelerate (t1, a0);
    return base->get_refresh_time (a1 - a0);
  }
  else {
    double a0= base->get_elapsed ();
    double t0= accelerate (a0);
    double t1= t0 - fabs (dt);
    double a1= decelerate (t1, a0);
    return base->get_refresh_time (a0 - a1);
  }
}

class reverse_player_rep: public accelerated_player_rep {
public:
  reverse_player_rep (player base): accelerated_player_rep (base) {}
  tree   get_name () { return "reverse"; }
  double transform_direct (double t) { return 1.0 - t; }
  double transform_inverse (double t, double ref) { return 1.0 - t; }
  player duplicate () { return reverse_player (copy (base)); }
};

player
reverse_player (player base) {
  return tm_new<reverse_player_rep> (base);
}

class fade_in_player_rep: public accelerated_player_rep {
public:
  fade_in_player_rep (player base): accelerated_player_rep (base) {}
  tree   get_name () { return "fade_in"; }
  double transform_direct (double t) {
    if (t <= 0.0 || t >= 1.0) return t;
    else return t * t; }
  double transform_inverse (double t, double ref) {
    if (t <= 0.0 || t >= 1.0) return t;
    else return sqrt (t); }
  player duplicate () { return fade_in_player (copy (base)); }
};

player
fade_in_player (player base) {
  return tm_new<fade_in_player_rep> (base);
}

class fade_out_player_rep: public accelerated_player_rep {
public:
  fade_out_player_rep (player base): accelerated_player_rep (base) {}
  tree   get_name () { return "fade_out"; }
  double transform_direct (double t) {
    if (t <= 0.0 || t >= 1.0) return t;
    else return t * (2.0 - t); }
  double transform_inverse (double t, double ref) {
    if (t <= 0.0 || t >= 1.0) return t;
    else return 1.0 - sqrt (1.0 - t); }
  player duplicate () { return fade_out_player (copy (base)); }
};

player
fade_out_player (player base) {
  return tm_new<fade_out_player_rep> (base);
}

class faded_player_rep: public accelerated_player_rep {
public:
  faded_player_rep (player base): accelerated_player_rep (base) {}
  tree   get_name () { return "faded"; }
  double transform_direct (double t) {
    if (t <= 0.0 || t >= 1.0) return t;
    else return 0.5 - 0.5 * cos (3.14159265359 * t); }
  double transform_inverse (double t, double ref) {
    if (t <= 0.0 || t >= 1.0) return t;
    else return acos (1.0 - 2.0 * t) / 3.14159265359; }
  player duplicate () { return faded_player (copy (base)); }
};

player
faded_player (player base) {
  return tm_new<faded_player_rep> (base);
}

class bump_player_rep: public accelerated_player_rep {
public:
  bump_player_rep (player base): accelerated_player_rep (base) {}
  tree   get_name () { return "bump"; }
  double transform_direct (double t) {
    if (t <= 0.0) return t;
    if (t >= 1.0) return 1.0 - t;
    return 4.0 * t * (1.0 - t); }
  double transform_inverse (double t, double ref) {
    if (t >= 1.0) return 0.5;
    double r;
    if (t <= 0.0) r= t;
    else r= 0.5 * (1.0 - sqrt (1.0 - t));
    if (ref <= 0.5) return r;
    else return 1.0 - r; }
  player duplicate () { return bump_player (copy (base)); }
};

player
bump_player (player base) {
  return tm_new<bump_player_rep> (base);
}

class fixed_player_rep: public accelerated_player_rep {
public:
  double position;
  fixed_player_rep (player base, double pos):
    accelerated_player_rep (base), position (pos) {}
  tree   get_name () { return tuple ("fixed", as_string (position)); }
  double transform_direct (double t) {
    (void) t;
    return position; }
  double transform_inverse (double t, double ref) {
    if (t < position) return 0.0;
    else return 1.0; }
  player duplicate () { return fixed_player (copy (base), position); }
};

player
fixed_player (player base, double position) {
  return tm_new<fixed_player_rep> (base, position);
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
