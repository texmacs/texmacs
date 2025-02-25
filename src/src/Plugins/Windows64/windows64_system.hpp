/******************************************************************************
* MODULE     : windows64_system.cpp
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
#ifndef WINDOWS_HEADERS_FIX
#include "url.hpp"
#endif

#include <string>

#include "windows64_encoding.hpp"

typedef struct _stat64 struct_stat;

struct texmacs_dir_t;

typedef struct texmacs_dir_t* TEXMACS_DIR;

/*
 * @brief Structure to represent a directory entry
 */
typedef struct texmacs_dirent {
    bool           is_valid;     /* entry is valid */
    string         d_name;       /* nane of the file in UTF-8 */
} texmacs_dirent;


/*
 * @brief Proxy function to call the fopen function with UTF-8 encoded strings
 * The lock parameter is used to lock the file when it is opened.
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
 * @brief Proxy function to the fputs function.
 * If the stream is cout or cerr, the function will do the necessary
 * conversion. Otherwise, it will call the fputs function withou any
 * conversion.
 */
ssize_t texmacs_fwrite(const char *string, size_t size, FILE *stream);

/*
 * @brief Proxy function to the fclose function.
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
 * @return true if the file was chmoded successfully, false otherwise
 */
bool texmacs_chmod(string filename, int mode);

/*
 * @brief Proxy function to the remove function with UTF-8 encoded strings
 * @return true if the file was removed successfully, false otherwise
 */
bool texmacs_remove(string filename);

/*
 * @brief Proxy function to the spawnvp function with UTF-8 encoded strings
 */
intptr_t texmacs_spawnvp(int mode, string name, ::array<::string> args);

/*
 * @brief A function to get the default theme according to the way texmacs
 * has been compiled, and the system configuration
 */
string get_default_theme();

/*
 * @brief Proxy function to the system function with UTF-8 encoded strings
 */
int mingw_system (array<string> arg,
                  array<int> fd_in, array<string> str_in,
                  array<int> fd_out, array<string*> str_out);


/* 
 * @brief Launch an executable with arguments, or open a link, a PDF
 * file, etc. with the default application.
 * If not program is associated with the file, the system will ask
 * the user to choose a program.
 * 
 * This version will wait for 
 * the process to finish before returning.
 * 
 * @param cmd: the command to execute
 * @param cmdout: the output of the command
 * @param cmderr: the error output of the command
 * 
 * @return the exit code of the process
 */
int windows_system(string cmd, string &cmdout, string &cmderr);

/* 
 * @brief Launch an executable with arguments, or open a link, a PDF
 * file, etc. with the default application.
 * If not program is associated with the file, the system will ask
 * the user to choose a program.
 * 
 * This version will wait for 
 * the process to finish before returning.
 * 
 * @param cmd: the command to execute
 * @param cmdout: the output of the command
 * 
 * @return the exit code of the process
 */
int windows_system(string cmd, string &cmdout);

/* 
 * @brief Launch an executable with arguments, or open a link, a PDF
 * file, etc. with the default application.
 * If not program is associated with the file, the system will ask
 * the user to choose a program.
 * 
 * This version will NOT wait for 
 * the process to finish before returning.
 * 
 * @param cmd: the command to execute
 * 
 * @return the exit code of the process
 */
int windows_system(string cmd);

/*
 * @brief A function to get the directory string where the texmacs
 * executable is located
 */
string texmacs_get_application_directory_str();

#ifndef WINDOWS_HEADERS_FIX
/*
 * @brief A function to get the directory where the texmacs executable is
 * located
 */
inline url texmacs_get_application_directory() {
    return url_system(texmacs_get_application_directory_str());
}
#endif

/*
 * @brief Tell TeXmacs system that a long task is starting. This will
 * allow TeXmacs to process the essential user events, and make TeXmacs
 * responsive while the task is running.
 */
void texmacs_system_start_long_task();

/*
 * @brief Tell TeXmacs system that a long task is ending.
 * @see texmacs_system_start_long_task
 */
void texmacs_system_end_long_task();

#endif