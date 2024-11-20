/******************************************************************************
* MODULE     : windows32_system.hpp
* DESCRIPTION: Windows system functions with UTF-8 input/output instead of ANSI
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TEXMACS_WINDOWS_SYSTEM_HPP
#define TEXMACS_WINDOWS_SYSTEM_HPP

#include "string.hpp"
#include "array.hpp"

#include <string>

typedef void* TEXMACS_DIR;
typedef struct _stat32 struct_stat;

typedef struct texmacs_dirent {
  bool           is_valid;     /* entry is valid */
  string         d_name;       /* nom du fichier */
} texmacs_dirent;

/*
 * @brief Proxy function to call the fopen function 
 * with UTF-8 encoded strings
 * @param lock: not used on Windows
 */
FILE* texmacs_fopen(string filename, string mode, bool lock = true);

/*
 * @brief Return the size of a file in bytes
 */
ssize_t texmacs_fsize (FILE *stream);

/*
 * @brief Proxy function to the fread function
 */
ssize_t texmacs_fread (char *z, size_t n, FILE *stream);

/*
 * @brief Proxy function to the fwirte function.
 * If the stream is cout or cerr, the function will do
 * the necessary conversion.
 * Otherwise, it will call the fputs function withou any conversion.
 */
ssize_t texmacs_fwrite(const char *string, size_t size, FILE *stream);

/*
 * @brief Proxy function to the fclose function.
 * @param unlock: not used on Windows
 */
void texmacs_fclose(FILE *&file, bool unlock = true);

/*
 * @brief Proxy function to the opendir function with UTF-8 encoded strings
 */
TEXMACS_DIR texmacs_opendir(string dirname);

/*
 * @brief Proxy function to the closedir function
 */
void texmacs_closedir(TEXMACS_DIR dir);

/*
 * @brief Proxy function to the readdir function with UTF-8 encoded strings
 */
texmacs_dirent texmacs_readdir(TEXMACS_DIR dirp);

/*
 * @brief Proxy function to the stat function with UTF-8 encoded strings
 * @return true if the file was found, false otherwise
 */
int texmacs_stat(string filename, struct_stat* buf);

/*
 * @brief Proxy function to the getenv function with UTF-8 encoded strings
 * @param variable_name: the name of the environment variable
 * @param variable_value: a string that will be filled with the value 
 *                        of the environment variable
 * @return true if the environment variable was found, false otherwise
 */
bool texmacs_getenv(string variable_name, string &variable_value);

/*
 * @brief Proxy function to the setenv function with UTF-8 encoded strings
 */
bool texmacs_setenv(string variable_name, string new_value);

/*
 * @brief Proxy function to the mkdir function with UTF-8 encoded strings
 * @return true if the directory was created successfully, false otherwise
 */
bool texmacs_mkdir(string dirname, int mode);

/*
 * @brief Proxy function to the rmdir function with UTF-8 encoded strings
 * @return true if the directory was removed successfully, false otherwise
 */
bool texmacs_rmdir(string dirname);

/*
 * @brief Proxy function to the rename function with UTF-8 encoded strings
 * @return true if the file was renamed successfully, false otherwise
 */
bool texmacs_rename(string oldname, string newname);

/*
 * @brief Proxy function to the chmod function with UTF-8 encoded strings
 * @return true if the file permissions were changed successfully,
 * false otherwise
 */
bool texmacs_chmod(string filename, int mode);

/*
 * @brief Proxy function to the remove function with UTF-8 encoded strings
 * @return true if the file was removed successfully, false otherwise
 */
bool texmacs_remove(string filename);

/*
 * @brief A function to get the default theme according to the way texmacs
 * has been compiled, and the system configuration
 */
string get_default_theme();

#endif