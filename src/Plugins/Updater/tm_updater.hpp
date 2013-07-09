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

#include "url.hpp"

class tm_updater
{
protected:
  url appcast;
  
public:
  tm_updater (url _appcast_url) : appcast(_appcast_url) { }
  virtual ~tm_updater () { };
  
  static tm_updater* instance (url _appcast_url);
  
  virtual bool checkInBackground () { return false; }  // non-blocking
  virtual bool checkInForeground () { return false; }  // blocking
  virtual bool isRunning () const   { return false; }
  
  virtual url getAppcast () const { return appcast; }
  virtual bool setAppcast (url _appcast_url) {
    appcast = _appcast_url;
    return true;
  }
};


/******************************************************************************
 * Scheme interface
 ******************************************************************************/

bool check_updates_background (url appcast);
bool check_updates_foreground (url appcast);

#endif    // TM_UPDATER_HPP
