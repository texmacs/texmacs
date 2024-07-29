/******************************************************************************
* MODULE     : windows64_entrypoint.hpp
* DESCRIPTION: Windows entry point for TeXmacs
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TEXMACS_WINDOWS_ENTRYPOINT_HPP
#define TEXMACS_WINDOWS_ENTRYPOINT_HPP

/*
 * @brief The main function of texmacs
 * On Windows, the argc and argv are AINSI encoded or UTF-16 encoded.
 * To make the AINSI to UTF-8 conversion seamless, we rename the texmacs main
 * function to texmacs_main. A new main function is created to convert 
 * the arguments to UTF-8 and call the texmacs_main function.
*/
int texmacs_main(int argc, char** argv);

/*
 * @brief Attach the console to the current process.
 *
 * By default on Windows, cout and cerr will display nothing. This function
 * attaches the console to command line it was started from. This command 
 * line car be a cmd, a powershell or a msys2 shell.
 */
void texmacs_attach_console();

/*
 * @brief Initialize the guile hooks to make guile compatible 
 * with unicode strings.
 * 
 * The texmacs-guile provides a set of hooks to intercept the system calls.
 * This function initializes the guile hooks to incercept each string and
 * make them compatible with unicode.
 */
void texmacs_init_guile_hooks();

/**
  * @brief Initialize the TEXMACS_DISPLAYNAME environment variable with the
  * full username.
  */
void texmacs_initialize_displayname();

#endif