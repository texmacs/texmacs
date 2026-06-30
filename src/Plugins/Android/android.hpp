/******************************************************************************
* MODULE     : android.hpp
* DESCRIPTION: android specific functions
* COPYRIGHT  : (C) 2024 Liza BELOS
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PLUGINS_ANDROID_HPP
#define PLUGINS_ANDROID_HPP

/*
 * @brief Initialize the android plugin. Must be called after the QApplication
 * object has been created.
 */
void init_android();

/*
 * @brief Start the android background service. This function will start 
 * the service if it is not already running.
 */
void start_android_service();

/*
 * @brief Stop the android background service. This function will stop 
 * the service if it is running.
 */
void stop_android_service();

string android_suffix_from_mime(string tm_uriString);

#endif