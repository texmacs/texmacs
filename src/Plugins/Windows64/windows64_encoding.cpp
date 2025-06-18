/******************************************************************************
* MODULE     : windows64_encoding.cpp
* DESCRIPTION: A set of functions to convert strings 
               between UTF-8 and the system encoding
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#include "windows64_encoding.hpp"

#include <windows.h>

// The ANSI code pages can be different on different computers, 
// or can be changed for a single computer, leading to data corruption.
// Source: 
// https://learn.microsoft.com/en-us/windows/win32/api/winnls/nf-winnls-getacp
inline UINT get_code_page() {
  UINT code_page = GetACP();
  return code_page;
}

std::wstring texmacs_utf8_to_wide(string utf8_str) {
  c_string _utf8_str = utf8_str;
  int wide_size = MultiByteToWideChar(CP_UTF8, 0, _utf8_str, 
                                      N(utf8_str), NULL, 0);
  if (wide_size == 0) {
    return L"";
  }
  std::wstring wide_str(wide_size, 0);
  MultiByteToWideChar(CP_UTF8, 0, _utf8_str, N(utf8_str), 
                      &wide_str[0], wide_size);
  return wide_str;
}

string texmacs_ainsi_to_utf8(const std::string &ainsi_str) {
  // Step 1: convert the string to multibyte wide char
  int wide_size = MultiByteToWideChar(get_code_page(), 0, ainsi_str.c_str(),
                                      ainsi_str.size(), NULL, 0);
  if (wide_size == 0) return "";
  
  std::wstring wide_str(wide_size, 0);
  MultiByteToWideChar(get_code_page(), 0, ainsi_str.c_str(), 
                      ainsi_str.size(), &wide_str[0], wide_size);

  // Step 2: convert the wide char to utf8
  int utf8_size = WideCharToMultiByte(CP_UTF8, 0, wide_str.c_str(), 
                                      wide_size, NULL, 0, NULL, NULL);
  if (utf8_size == 0) return "";
  
  std::string utf8_str(utf8_size, 0);
  WideCharToMultiByte(CP_UTF8, 0, wide_str.c_str(), wide_size, &utf8_str[0],
                      utf8_size, NULL, NULL);
  return string(utf8_str.data(), utf8_size);
}

string texmacs_wide_to_utf8(const std::wstring &wide_str) {
  int utf8_size = WideCharToMultiByte(CP_UTF8, 0, wide_str.c_str(), 
                                      wide_str.size(), NULL, 0, NULL, NULL);
  if (utf8_size == 0) {
    return "";
  }
  std::string utf8_str(utf8_size, 0);
  WideCharToMultiByte(CP_UTF8, 0, wide_str.c_str(), wide_str.size(), 
                      &utf8_str[0], utf8_size, NULL, NULL);
  return string(utf8_str.data(), utf8_size);
}
