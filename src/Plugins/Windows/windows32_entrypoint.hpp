/******************************************************************************
* MODULE     : windows_main.hpp
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
 * function to texmacs_main. A new main function is created to convert the
 * arguments to UTF-8 and call the texmacs_main function.
 */
int texmacs_main(int argc, char** argv);

#endif