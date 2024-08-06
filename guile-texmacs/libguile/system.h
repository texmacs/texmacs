/**
 * @file system.h
 * @brief Header file for guile that defines system function hooks for libguile.
 *
 * The guile code is old and may not work directly on newer systems. To 
 * accommodate this, users have the possibility to create hooks for the system 
 * functions (such as fstat, fopen, ...). The Guile functions will always call
 * the corresponding hooks if they are defined.
 */

#ifndef SCM_SYSTEM_H
#define SCM_SYSTEM_H

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <stdint.h>
#include <fcntl.h>

#if SCM_USE_64_CALLS
#ifdef __MINGW32__
#define CHOOSE_LARGEFILE(foo,foo64,foowin64)     foowin64
#else
#define CHOOSE_LARGEFILE(foo,foo64,foowin64)     foo64
#endif
#else
#define CHOOSE_LARGEFILE(foo,foo64,foowin64)     foo
#endif

#if SCM_USE_64_CALLS
#ifdef __MINGW32__
typedef struct _stat64 guile_stat_t;
#else
typedef struct stat64 guile_stat_t;
#endif
#if SCM_HAVE_STRUCT_DIRENT64 == 1
typedef struct dirent64 guile_dirent_t;
#else
typedef struct dirent guile_dirent_t;
#endif
// if this is mac, off64_t is off_t
#ifdef __APPLE__
typedef off_t guile_off_t;
#else
typedef off64_t guile_off_t;
#endif
#else
typedef struct stat guile_stat_t;
typedef struct dirent guile_dirent_t;
typedef off_t guile_off_t;
#endif

#define scm_from_off_t_or_off64_t       CHOOSE_LARGEFILE(scm_from_off_t,scm_from_int64,scm_from_int64)
#define scm_from_ino_t_or_ino64_t       CHOOSE_LARGEFILE(scm_from_nat,scm_from_uint64,scm_from_uint64)
#define scm_from_blkcnt_t_or_blkcnt64_t CHOOSE_LARGEFILE(scm_from_nat,scm_from_uint64,scm_from_uint64)
#define scm_to_off_t_or_off64_t         CHOOSE_LARGEFILE(scm_to_off_t,scm_to_int64,scm_to_int64)


extern char *(*guile_utf8_string_to_system_string)(const char *utf8_string);
extern char *(*guile_system_string_to_utf8_string)(const char *system_string);
extern void (*guile_utf8_string_to_system_string_path)(char *utf8_string);

extern int (*guile_fstat)(int fd, guile_stat_t *buf);
extern int (*guile_ftruncate)(int fd, guile_off_t length);
extern guile_off_t (*guile_lseek)(int fd, guile_off_t offset, int whence);
extern int (*guile_stat)(const char *path, guile_stat_t *buf);
extern int (*guile_lstat)(const char *path, guile_stat_t *buf);
extern int (*guile_open)(const char *pathname, int flags, mode_t mode);
extern DIR *(*guile_opendir)(const char *name);
extern guile_dirent_t *(*guile_readdir)(DIR *dirp);
#if HAVE_READDIR_R
extern int (*guile_readdir_r)(DIR *dirp, guile_dirent_t *entry, guile_dirent_t **result);
#endif
extern int (*guile_truncate)(const char *path, guile_off_t length);
extern char *(*guile_getenv)(const char *name);

#endif /* SCM_SYSTEM_H */
