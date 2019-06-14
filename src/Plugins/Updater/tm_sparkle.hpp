/******************************************************************************
 * MODULE     : tm_sparkle.hpp
 * DESCRIPTION: Manager class for the autoupdater Sparkle framework
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_updater.hpp"

class tm_sparkle : public tm_updater
{
    // We have to hide the SUUPdater type to avoid C++/Obj-C clashes
  class tm_suupdater;
  tm_suupdater* updater;

  tm_sparkle ();
  ~tm_sparkle ();
  friend class tm_updater;
  
public:
  bool checkInBackground ();
  bool checkInForeground ();
  
  bool isRunning () const;
  time_t lastCheck () const;
  bool setCheckInterval (int hours);
};
