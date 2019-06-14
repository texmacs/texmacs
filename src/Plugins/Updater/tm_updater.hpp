/******************************************************************************
 * MODULE     : tm_updater.hpp
 * DESCRIPTION: Base class for auto-update frameworks like (Win)Sparkle
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TM_UPDATER_HPP
#define TM_UPDATER_HPP

#include <time.h>
#include "url.hpp"

class tm_updater
{
protected:
  static const int MinimumCheckInterval = 24;     //<! in hours
  static const int MaximumCheckInterval = 24*31;  //<! in hours
  
  url appcast;
  int interval;
  
  tm_updater () : interval (0) { }
  tm_updater (const tm_updater&);
  void operator= (const tm_updater&);
  virtual ~tm_updater () { };
  
public:
  static tm_updater* instance ();
  
  virtual bool checkInBackground () { return false; }  // non-blocking
  virtual bool checkInForeground () { return false; }  // non-blocking
  virtual bool isRunning () const   { return false; }
   
  virtual time_t lastCheck () const { return 0; }
  virtual bool getCheckInterval () const { return interval; }
  virtual bool setCheckInterval (int hours) { (void) hours; return false; }
};


/******************************************************************************
 * Scheme interface
 ******************************************************************************/

bool updater_supported ();
bool updater_is_running ();
bool updater_check_background ();
bool updater_check_foreground ();
bool updater_set_interval (int hours);
time_t updater_last_check ();

#endif    // TM_UPDATER_HPP
