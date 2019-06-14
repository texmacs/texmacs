/******************************************************************************
 * MODULE     : tm_winsparkle.hpp
 * DESCRIPTION: Manager class for the autoupdater WinSparkle framework
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_updater.hpp"

class tm_winsparkle : public tm_updater
{
  bool running;
  
  tm_winsparkle () : tm_updater (), running (false) { }
  ~tm_winsparkle ();
  friend class tm_updater;

public:
  bool checkInBackground ();
  bool checkInForeground ();
  bool isRunning () const { return running; }
  time_t lastCheck () const;
  bool setCheckInterval (int hours);
  bool setAppcast (url _appcast_url);
};
