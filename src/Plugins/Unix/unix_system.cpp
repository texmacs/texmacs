
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

#ifdef OS_MACOS
#include <mach-o/dyld.h>
#endif

inline std::string
texmacs_utf8_string_to_system_string (string utf8_string) {
  return std::string (&utf8_string[0], 
		      (std::basic_string<char>::size_type) N(utf8_string));
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
  FILE *file = fopen(texmacs_utf8_string_to_system_string(filename).c_str(),
		     c_mode);
  if (file == nullptr) {
    return nullptr;
  }
  if (lock) {
    texmacs_lock_file(file);
  }
  return file;
}

ssize_t texmacs_fsize (FILE *stream) {
  // get the current position of the file pointer
  long current = ftell(stream);
  if (current == -1) {
    return -1;
  }
  // seek to the end of the file
  if (fseek(stream, 0, SEEK_END) != 0) {
    return -1;
  }
  // get the position of the file pointer
  long size = ftell(stream);
  if (size == -1) {
    return -1;
  }
  // restore the position of the file pointer
  if (fseek(stream, current, SEEK_SET) != 0) {
    return -1;
  }
  return size;
}

ssize_t texmacs_fread (char *z, size_t n, FILE *stream) {
  return fread(z, 1, n, stream);
}

ssize_t texmacs_fwrite (const char *s, size_t n, FILE *stream) {
  if (stream != stdout && stream != stderr) {
    size_t ret= fwrite (s, n, 1, stream);
    return ret < 1 ? 0 : n;
  }
  std::string system_string= texmacs_utf8_string_to_system_string (string (s, n));
  size_t ret= fwrite (system_string.c_str(), system_string.size(), 1, stream);
  return ret < 1 ? 0 : n;
}

void texmacs_fclose(FILE *&file, bool unlock) {
  if (unlock) {
    texmacs_unlock_file(file);
  }
  fclose(file);
  file = nullptr;
}

TEXMACS_DIR texmacs_opendir(string dirname) {
  return (TEXMACS_DIR)
    opendir(texmacs_utf8_string_to_system_string(dirname).c_str());
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

url texmacs_get_application_directory() {
#ifdef OS_GNU_LINUX
  // use proc self exe to get the path of the executable
  char path[PATH_MAX];
  ssize_t len = readlink("/proc/self/exe", path, sizeof(path) - 1);
  if (len == -1) {
    return url();
  }
  path[len] = '\0';
  string exe_path = path;
  return url_system(exe_path) * "..";
#elif defined(OS_MACOS)
  char path[PATH_MAX];
  uint32_t size = sizeof(path);
  if (_NSGetExecutablePath(path, &size) != 0) {
    return url();
  }
  string exe_path = path;
  return url_system(exe_path) * "..";
#endif
}