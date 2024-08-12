/******************************************************************************
* MODULE     : unix_system.cpp
* DESCRIPTION: Unix system function proxies
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "unix_system.hpp"
#include "config.h"
#ifdef QTTEXMACS
#include <QGuiApplication>
#include <QStyleHints>
#endif

inline std::string texmacs_utf8_string_to_system_string(string utf8_string) {
  return std::string(
    &utf8_string[0], 
    (std::basic_string<char>::size_type)N(utf8_string)
  );
}

inline string texmacs_ainsi_to_utf8(const std::string &local_string) {
  return string(
    local_string.data(),
    (int)local_string.size()
  );
}

void texmacs_lock_file(FILE *&file) {
  int file_descriptor = fileno(file);
  if (flock(file_descriptor, LOCK_EX) == -1) {
    fclose(file);
    file = nullptr;
  }
}

void texmacs_unlock_file(FILE *&file) {
  int file_descriptor = fileno(file);
  flock(file_descriptor, LOCK_UN);
}


FILE* texmacs_fopen(string filename, string mode, bool lock) {
  c_string c_mode = mode;
  FILE *file = fopen(texmacs_utf8_string_to_system_string(filename).c_str(), c_mode);
  if (file == nullptr) {
    return nullptr;
  }
  if (lock) {
    texmacs_lock_file(file);
  }
  return file;
}

int texmacs_fwrite(const char *string, size_t size, FILE *stream) {
  if (stream != stdout && stream != stderr) {
      return fwrite(string, size, 1, stream);
  }
  std::string system_string = texmacs_utf8_string_to_system_string(string);
  return fwrite(system_string.c_str(), system_string.size(), 1, stream);
}

void texmacs_fclose(FILE *&file, bool unlock) {
  if (unlock) {
    texmacs_unlock_file(file);
  }
  fclose(file);
  file = nullptr;
}

TEXMACS_DIR texmacs_opendir(string dirname) {
  return (TEXMACS_DIR)opendir(texmacs_utf8_string_to_system_string(dirname).c_str());
}

void texmacs_closedir(TEXMACS_DIR dir) {
  closedir((DIR*)dir);
  dir = nullptr;
}

texmacs_dirent texmacs_readdir(TEXMACS_DIR dirp) {
  struct dirent* entry = readdir(dirp);
  if (entry == NULL) {
    return {false, ""};
  }
  return {true, texmacs_ainsi_to_utf8(entry->d_name)};
}

int texmacs_stat(string filename, struct_stat* buf) {
  return stat(texmacs_utf8_string_to_system_string(filename).c_str(), buf);
}

bool texmacs_mkdir(string dirname, int mode) {
  return mkdir(texmacs_utf8_string_to_system_string(dirname).c_str(), mode) == 0;
}

bool texmacs_rmdir(string dirname) {
  return rmdir(texmacs_utf8_string_to_system_string(dirname).c_str()) == 0;
}

bool texmacs_rename(string oldname, string newname) {
  return rename(
    texmacs_utf8_string_to_system_string(oldname).c_str(),
    texmacs_utf8_string_to_system_string(newname).c_str()
  ) == 0;
}

bool texmacs_chmod(string filename, int mode) {
  return chmod(texmacs_utf8_string_to_system_string(filename).c_str(), mode) == 0;
}

bool texmacs_remove(string filename) {
  return remove(texmacs_utf8_string_to_system_string(filename).c_str()) == 0;
}

bool texmacs_getenv(string variable_name, string &variable_value) {
    char *value = getenv(texmacs_utf8_string_to_system_string(variable_name).c_str());
    if (value == nullptr) {
        return false;
    }
    variable_value = texmacs_ainsi_to_utf8(value);
    return true;
}

bool texmacs_setenv(string variable_name, string new_value) {
    return setenv(
      texmacs_utf8_string_to_system_string(variable_name).c_str(),
      texmacs_utf8_string_to_system_string(new_value).c_str(), 1
    ) == 0;
}

string get_default_theme() {
#if defined(OS_MACOS) && !defined(__arm64__)
  return "";
#endif
#ifdef qt_no_fontconfig
  return "native";
#endif
#if defined(QTTEXMACS) && QT_VERSION >= 0x060500
  if (QGuiApplication::styleHints()->colorScheme() == Qt::ColorScheme::Dark) {
    return "dark";
  } else {
    return "light";
  }
#endif
  return "light";
}