/******************************************************************************
* MODULE     : windows64_encoding.hpp
* DESCRIPTION: A set of functions to convert strings 
               between UTF-8 and the system encoding
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TEXMACS_WINDOWS_STRING_ENCODING_HPP
#define TEXMACS_WINDOWS_STRING_ENCODING_HPP

/*
 * This file contains a set of functions to convert strings between UTF-8 and 
 * the system encoding. The windows system encoding is the locale encoding of
 * the system. By default, a narrow filename string is interpreted using the 
 * ANSI codepage (CP_ACP). Source: 
 * //learn.microsoft.com/en-us/cpp/c-runtime-library/reference/fopen-wfopen
 *
 * This AINSI codepage depends on the language of the system during the 
 * installation of Windows. It can therefore be different on different 
 * computers.
 *
 * To avoid memory leaks error, we prefer to use classes that encapsulate 
 * the char* and wchar_t* pointers. We use this convention : 
 *  - texmacs string are UTF-8 encoded
 *  - standard string are system AINSI encoded
 *  - standard wide string are UTF-16 encoded
 */
#include "string.hpp"
#include <string>

/**
  * @brief The wide version of @ref texmacs_utf8_string_to_system_string.
  * @param utf8_string The UTF-8 string to convert.
  * @return A wide string with the system encoding.
  * @see texmacs_utf8_string_to_system_string(string utf8_string)
  *
  * This function converts a UTF-8 string to a wide string with the locale 
  * system encoding. The returned string can be used with the standard C 
  * functions, but should not be used for texmacs document contents.
 */
std::wstring texmacs_utf8_to_wide(string utf8_str);

/**
  * @brief Convert a string with the system encoding to a UTF-8 string.
  * @param ainsi_string The string with the system encoding to convert.
  * @return A UTF-8 string.
  * @see texmacs_ainsi_to_utf8(c_string ainsi_string)
  *
  * This function converts a string with the locale system encoding to a 
  * UTF-8 string. The returned string should not be used with the standard
  *  C functions, but can be used for texmacs document contents.
 */
string texmacs_ainsi_to_utf8(const std::string &ainsi_str);

/**
  * @brief The wide version of @ref texmacs_ainsi_to_utf8.
  * @param wide_str The string with the system wide encoding to convert.
  * @return A wide UTF-8 string.
  * @see texmacs_ainsi_to_utf8(c_string ainsi_string)
  *
  * This function converts a string with the locale system encoding to a wide
  * UTF-8 string. The returned string should not be used with the standard
  * C functions, but can be used for texmacs document contents.
 */
string texmacs_wide_to_utf8(const std::wstring &wide_str);

#endif // TEXMACS_WINDOWS_STRING_ENCODING_HPP