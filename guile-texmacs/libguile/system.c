#include "_scm.h"

#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#define fstat_or_fstat64                CHOOSE_LARGEFILE(fstat,fstat64,_fstat64)
#define ftruncate_or_ftruncate64        CHOOSE_LARGEFILE(ftruncate,ftruncate64,ftruncate64)
#define lseek_or_lseek64                CHOOSE_LARGEFILE(lseek,lseek64,lseek64)
#define lstat_or_lstat64                CHOOSE_LARGEFILE(lstat,lstat64,_stat64)
#define stat_or_stat64                  CHOOSE_LARGEFILE(stat,stat64,_stat64)
#define open_or_open64                  CHOOSE_LARGEFILE(open,open64,open)
#if SCM_HAVE_STRUCT_DIRENT64 == 1
#define readdir_or_readdir64            CHOOSE_LARGEFILE(readdir,readdir64,readdir)
#else
#define readdir_or_readdir64            readdir
#endif
#if SCM_HAVE_READDIR64_R == 1
#define readdir_r_or_readdir64_r        CHOOSE_LARGEFILE(readdir_r,readdir64_r,readdir_r)
#else
#define readdir_r_or_readdir64_r        readdir_r
#endif
#define truncate_or_truncate64          CHOOSE_LARGEFILE(truncate,truncate64,truncate64)


char *guile_default_utf8_string_to_system_string(const char *utf8_string) {
    return (char*)utf8_string;
}

char *guile_default_system_string_to_utf8_string(const char *system_string) {
    return (char*)system_string;
}

void guile_default_utf8_string_to_system_string_path(char *utf8_string) {

}

int guile_default_fstat(int fd, guile_stat_t *buf) {
    return fstat_or_fstat64(fd, buf);
}

int guile_default_ftruncate(int fd, guile_off_t length) {
    return ftruncate_or_ftruncate64(fd, length);
}

guile_off_t guile_default_lseek(int fd, guile_off_t offset, int whence) {
    return lseek_or_lseek64(fd, offset, whence);
}

int guile_default_stat(const char *path, guile_stat_t *buf) {
    path = guile_utf8_string_to_system_string(path);
    return stat_or_stat64(path, buf);
}

int guile_default_lstat(const char *path, guile_stat_t *buf) {
    path = guile_utf8_string_to_system_string(path);
    return lstat_or_lstat64(path, buf);
}

int guile_default_open(const char *pathname, int flags, mode_t mode) {
    pathname = guile_utf8_string_to_system_string(pathname);
    return open_or_open64(pathname, flags, mode);
}

// guile_readdir
DIR *guile_default_opendir(const char *name) {
    name = guile_utf8_string_to_system_string(name);
    return opendir(name);
}

guile_dirent_t *guile_default_readdir(DIR *dirp) {
    guile_dirent_t *next = readdir_or_readdir64(dirp);
    if (next) {
        guile_utf8_string_to_system_string_path(next->d_name);
    }
    return next;
}

#if HAVE_READDIR_R
int guile_default_readdir_r(DIR *dirp, guile_dirent_t *entry, guile_dirent_t **result) {
    int res = readdir_r_or_readdir64_r(dirp, entry, result);
    if (res == 0 && *result) {
        guile_utf8_string_to_system_string_path((*result)->d_name);
    }
    return res;
}
#endif

int guile_default_truncate(const char *path, off_t length) {
    path = guile_utf8_string_to_system_string(path);
    return truncate_or_truncate64(path, length);
}

char *guile_default_getenv(const char *name) {
    name = guile_utf8_string_to_system_string(name);
    return guile_system_string_to_utf8_string(getenv(name));
}

/* Function pointers initialization */
char *(*guile_utf8_string_to_system_string)(const char *utf8_string) = guile_default_utf8_string_to_system_string;
char *(*guile_system_string_to_utf8_string)(const char *system_string) = guile_default_system_string_to_utf8_string;
void (*guile_utf8_string_to_system_string_path)(char *utf8_string) = guile_default_utf8_string_to_system_string_path;
int (*guile_fstat)(int fd, guile_stat_t *buf) = guile_default_fstat;
int (*guile_ftruncate)(int fd, guile_off_t length) = guile_default_ftruncate;
guile_off_t (*guile_lseek)(int fd, guile_off_t offset, int whence) = guile_default_lseek;
int (*guile_stat)(const char *path, guile_stat_t *buf) = guile_default_lstat;
int (*guile_lstat)(const char *path, guile_stat_t *buf) = guile_default_lstat;
int (*guile_open)(const char *pathname, int flags, mode_t mode) = guile_default_open;
DIR *(*guile_opendir)(const char *name) = guile_default_opendir;
guile_dirent_t *(*guile_readdir)(DIR *dirp) = guile_default_readdir;
#if HAVE_READDIR_R
int (*guile_readdir_r)(DIR *dirp, guile_dirent_t *entry, guile_dirent_t **result) = guile_default_readdir_r;
#endif
int (*guile_truncate)(const char *path, guile_off_t length) = guile_default_truncate;
char *(*guile_getenv)(const char *name) = guile_default_getenv;