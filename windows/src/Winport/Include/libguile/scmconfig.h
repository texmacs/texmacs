/* libguile/scmconfig.h.  Generated automatically by configure.  */
/* libguile/scmconfig.h.in.  Generated automatically from configure.in by autoheader.  */

/* Define if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
/* #undef _ALL_SOURCE */
#endif

/* Define if using alloca.c.  */
/* #undef C_ALLOCA */

/* Define to empty if the keyword does not work.  */
/* #undef const */

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP systems.
   This function is required for alloca.c support on those systems.  */
/* #undef CRAY_STACKSEG_END */

/* Define to the type of elements in the array set by `getgroups'.
   Usually this is either `int' or `gid_t'.  */
#define GETGROUPS_T int

/* Define to `int' if <sys/types.h> doesn't define.  */
#define gid_t int

/* Define if you have alloca, as a function or macro.  */
#define HAVE_ALLOCA 1

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
/* #undef HAVE_ALLOCA_H */

/* Define if system calls automatically restart after interruption
   by a signal.  */
/* #undef HAVE_RESTARTABLE_SYSCALLS */

/* Define if your struct stat has st_blksize.  */
/* #undef HAVE_ST_BLKSIZE */

/* Define if your struct stat has st_blocks.  */
/* #undef HAVE_ST_BLOCKS */

/* Define if your struct stat has st_rdev.  */
#define HAVE_ST_RDEV 1

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
/* #undef HAVE_SYS_WAIT_H */

/* Define if your struct tm has tm_zone.  */
/* #undef HAVE_TM_ZONE */

/* Define if you don't have tm_zone but do have the external array
   tzname.  */
/* #undef HAVE_TZNAME */

/* Define as __inline if that's what the C compiler calls it.  */
/* #undef inline */

/* Define if on MINIX.  */
/* #undef _MINIX */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef mode_t */

/* Define if the system does not provide POSIX.1 features except
   with this defined.  */
/* #undef _POSIX_1_SOURCE */

/* Define if you need to in order for stat and other things to work.  */
/* #undef _POSIX_SOURCE */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
 STACK_DIRECTION > 0 => grows toward higher addresses
 STACK_DIRECTION < 0 => grows toward lower addresses
 STACK_DIRECTION = 0 => direction of growth unknown
 */
/* #undef STACK_DIRECTION */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
/* #undef TIME_WITH_SYS_TIME */

/* Define if your <sys/time.h> declares struct tm.  */
/* #undef TM_IN_SYS_TIME */

/* Define to `int' if <sys/types.h> doesn't define.  */
#define uid_t int

/* Define these two if you want support for debugging of Scheme
   programs.  */
#define DEBUG_EXTENSIONS 1
#define READER_EXTENSIONS 1

/* Define this if you want to debug the free list (helps w/ GC bugs) */
/* #undef GUILE_DEBUG_FREELIST */

/* Define this if you want to debug scm_must_malloc/realloc/free calls */
/* #undef GUILE_DEBUG_MALLOC */

/* Define this if your system defines S_ISLNK in sys/stat.h */
/* #undef HAVE_S_ISLNK */

/* Define this if your system defines struct linger, for use with the
   getsockopt and setsockopt system calls.  */
#define HAVE_STRUCT_LINGER 1

/* Define this if your system defines struct timespec via <time.h>. */
/* #undef HAVE_STRUCT_TIMESPEC */

/* Define this if floats are the same size as longs.  */
#define SCM_SINGLES 1

/* Define this if a callee's stack frame has a higher address than the
   caller's stack frame.  On most machines, this is not the case.  */
/* #undef SCM_STACK_GROWS_UP */

/* Define this if <utime.h> doesn't define struct utimbuf unless
   _POSIX_SOURCE is #defined.  See GUILE_STRUCT_UTIMBUF in aclocal.m4.  */
#define UTIMBUF_NEEDS_POSIX 1

/* Define this if we should #include <libc.h> when we've already
   #included <unistd.h>.  On some systems, they conflict, and libc.h
   should be omitted.  See GUILE_HEADER_LIBC_WITH_UNISTD in
   aclocal.m4.  */
/* #undef LIBC_H_WITH_UNISTD_H */

/* Define this to include various undocumented functions used to debug
   the Guile library itself.  */
#define GUILE_DEBUG 1

/* Define to implement scm_internal_select */
/* #undef GUILE_ISELECT */

/* Define to enable workaround for COOP-linuxthreads compatibility */
/* #undef GUILE_PTHREAD_COMPAT */

/* Define if using cooperative multithreading.  */
/* #undef USE_COOP_THREADS */

/* Define if using any sort of threads.  */
/* #undef USE_THREADS */

/* Define if you want support for dynamic linking. */
#define DYNAMIC_LINKING 1

/* Define if localtime caches the TZ setting.  */
#define LOCALTIME_CACHE 1

/* Define if the system supports Unix-domain (file-domain) sockets.  */
/* #undef HAVE_UNIX_DOMAIN_SOCKETS */

/* Define this if you want support for arrays and uniform arrays.  */
#define HAVE_ARRAYS 1

/* This is included as part of a workaround for a autoheader bug. */
#define HAVE_REGCOMP 1

/* Define this if you want support for POSIX system calls in Guile.  */
#define HAVE_POSIX 1

/* Define this if you want support for networking in Guile.  */
#define HAVE_NETWORKING 1

/* Define if the operating system supplies bzero without declaring it. */
#define MISSING_BZERO_DECL 1

/* Define if the operating system supplies strptime without declaring it. */
#define MISSING_STRPTIME_DECL 1

/* Define if the operating system supplies sleep without declaring it. */
#define MISSING_SLEEP_DECL 1

/* Define if the operating system supplies usleep without declaring it. */
#define MISSING_USLEEP_DECL 1

/* Define if the system headers declare usleep to return void.  */
/* #undef USLEEP_RETURNS_VOID */

/* Define if the compiler supports long longs.  */
#define HAVE_LONG_LONGS 1

/* The number of bytes in a int.  */
#define SIZEOF_INT 4

/* The number of bytes in a long.  */
#define SIZEOF_LONG 4

/* Define if you have the atexit function.  */
#define HAVE_ATEXIT 1

/* Define if you have the bzero function.  */
/* #undef HAVE_BZERO */

/* Define if you have the connect function.  */
#define HAVE_CONNECT 1

/* Define if you have the ctermid function.  */
/* #undef HAVE_CTERMID */

/* Define if you have the endhostent function.  */
/* #undef HAVE_ENDHOSTENT */

/* Define if you have the endnetent function.  */
/* #undef HAVE_ENDNETENT */

/* Define if you have the endprotoent function.  */
/* #undef HAVE_ENDPROTOENT */

/* Define if you have the endservent function.  */
/* #undef HAVE_ENDSERVENT */

/* Define if you have the fchown function.  */
/* #undef HAVE_FCHOWN */

/* Define if you have the ftime function.  */
#define HAVE_FTIME 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the geteuid function.  */
/* #undef HAVE_GETEUID */

/* Define if you have the getgroups function.  */
/* #undef HAVE_GETGROUPS */

/* Define if you have the gethostbyname function.  */
#define HAVE_GETHOSTBYNAME 1

/* Define if you have the gethostent function.  */
/* #undef HAVE_GETHOSTENT */

/* Define if you have the getnetbyaddr function.  */
/* #undef HAVE_GETNETBYADDR */

/* Define if you have the getnetbyname function.  */
/* #undef HAVE_GETNETBYNAME */

/* Define if you have the getnetent function.  */
/* #undef HAVE_GETNETENT */

/* Define if you have the getprotoent function.  */
/* #undef HAVE_GETPROTOENT */

/* Define if you have the getservent function.  */
/* #undef HAVE_GETSERVENT */

/* Define if you have the gettimeofday function.  */
/* #undef HAVE_GETTIMEOFDAY */

/* Define if you have the hstrerror function.  */
/* #undef HAVE_HSTRERROR */

/* Define if you have the inet_aton function.  */
/* #undef HAVE_INET_ATON */

/* Define if you have the inet_lnaof function.  */
/* #undef HAVE_INET_LNAOF */

/* Define if you have the inet_makeaddr function.  */
/* #undef HAVE_INET_MAKEADDR */

/* Define if you have the inet_netof function.  */
/* #undef HAVE_INET_NETOF */

/* Define if you have the lstat function.  */
/* #undef HAVE_LSTAT */

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the mkdir function.  */
#define HAVE_MKDIR 1

/* Define if you have the mknod function.  */
/* #undef HAVE_MKNOD */

/* Define if you have the nice function.  */
/* #undef HAVE_NICE */

/* Define if you have the on_exit function.  */
/* #undef HAVE_ON_EXIT */

/* Define if you have the pause function.  */
/* #undef HAVE_PAUSE */

/* Define if you have the putenv function.  */
#define HAVE_PUTENV 1

/* Define if you have the readlink function.  */
/* #undef HAVE_READLINK */

/* Define if you have the rename function.  */
#define HAVE_RENAME 1

/* Define if you have the rmdir function.  */
#define HAVE_RMDIR 1

/* Define if you have the select function.  */
#define HAVE_SELECT 1

/* Define if you have the setegid function.  */
/* #undef HAVE_SETEGID */

/* Define if you have the seteuid function.  */
/* #undef HAVE_SETEUID */

/* Define if you have the sethostent function.  */
/* #undef HAVE_SETHOSTENT */

/* Define if you have the setlocale function.  */
#define HAVE_SETLOCALE 1

/* Define if you have the setnetent function.  */
/* #undef HAVE_SETNETENT */

/* Define if you have the setpgid function.  */
/* #undef HAVE_SETPGID */

/* Define if you have the setprotoent function.  */
/* #undef HAVE_SETPROTOENT */

/* Define if you have the setpwent function.  */
/* #undef HAVE_SETPWENT */

/* Define if you have the setservent function.  */
/* #undef HAVE_SETSERVENT */

/* Define if you have the setsid function.  */
/* #undef HAVE_SETSID */

/* Define if you have the sigaction function.  */
/* #undef HAVE_SIGACTION */

/* Define if you have the siginterrupt function.  */
/* #undef HAVE_SIGINTERRUPT */

/* Define if you have the socketpair function.  */
/* #undef HAVE_SOCKETPAIR */

/* Define if you have the strdup function.  */
#define HAVE_STRDUP 1

/* Define if you have the strerror function.  */
#define HAVE_STRERROR 1

/* Define if you have the strftime function.  */
#define HAVE_STRFTIME 1

/* Define if you have the strptime function.  */
/* #undef HAVE_STRPTIME */

/* Define if you have the symlink function.  */
/* #undef HAVE_SYMLINK */

/* Define if you have the sync function.  */
/* #undef HAVE_SYNC */

/* Define if you have the system function.  */
#define HAVE_SYSTEM 1

/* Define if you have the tcgetpgrp function.  */
/* #undef HAVE_TCGETPGRP */

/* Define if you have the tcsetpgrp function.  */
/* #undef HAVE_TCSETPGRP */

/* Define if you have the times function.  */
/* #undef HAVE_TIMES */

/* Define if you have the tzset function.  */
#define HAVE_TZSET 1

/* Define if you have the uname function.  */
/* #undef HAVE_UNAME */

/* Define if you have the usleep function.  */
/* #undef HAVE_USLEEP */

/* Define if you have the waitpid function.  */
/* #undef HAVE_WAITPID */

/* Define if you have the <dirent.h> header file.  */
/* #undef HAVE_DIRENT_H */

/* Define if you have the <io.h> header file.  */
#define HAVE_IO_H 1

/* Define if you have the <libc.h> header file.  */
/* #undef HAVE_LIBC_H */

/* Define if you have the <limits.h> header file.  */
#define HAVE_LIMITS_H 1

/* Define if you have the <malloc.h> header file.  */
#define HAVE_MALLOC_H 1

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <regex.h> header file.  */
#define HAVE_REGEX_H 1

/* Define if you have the <rx/rxposix.h> header file.  */
/* #undef HAVE_RX_RXPOSIX_H */

/* Define if you have the <rxposix.h> header file.  */
/* #undef HAVE_RXPOSIX_H */

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/ioctl.h> header file.  */
/* #undef HAVE_SYS_IOCTL_H */

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/select.h> header file.  */
/* #undef HAVE_SYS_SELECT_H */

/* Define if you have the <sys/time.h> header file.  */
/* #undef HAVE_SYS_TIME_H */

/* Define if you have the <sys/timeb.h> header file.  */
#define HAVE_SYS_TIMEB_H 1

/* Define if you have the <sys/times.h> header file.  */
/* #undef HAVE_SYS_TIMES_H */

/* Define if you have the <sys/types.h> header file.  */
#define HAVE_SYS_TYPES_H 1

/* Define if you have the <sys/utime.h> header file.  */
#define HAVE_SYS_UTIME_H 1

/* Define if you have the <time.h> header file.  */
#define HAVE_TIME_H 1

/* Define if you have the <unistd.h> header file.  */
/* #undef HAVE_UNISTD_H */

/* Define if you have the <utime.h> header file.  */
/* #undef HAVE_UTIME_H */

/* Define if you have the m library (-lm).  */
#define HAVE_LIBM 1

/* Define if you have the nsl library (-lnsl).  */
/* #undef HAVE_LIBNSL */

/* Define if you have the pthread library (-lpthread).  */
/* #undef HAVE_LIBPTHREAD */

/* Define if you have the rx library (-lrx).  */
/* #undef HAVE_LIBRX */

/* Define if you have the socket library (-lsocket).  */
/* #undef HAVE_LIBSOCKET */
