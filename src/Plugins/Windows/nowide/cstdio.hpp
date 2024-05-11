//
//  Copyright (c) 2012 Artyom Beilis (Tonkikh)
//
//  Distributed under the Boost Software License, Version 1.0. (See
//  accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
#ifndef NOWIDE_CSTDIO_H_INCLUDED
#define NOWIDE_CSTDIO_H_INCLUDED
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>

#include <cstdio>
#include <stdio.h>
#include <nowide/config.hpp>
#include <nowide/convert.hpp>
#include <nowide/stackstring.hpp>
#include <errno.h>

#ifdef NOWIDE_MSVC
#  pragma warning(push)
#  pragma warning(disable : 4996)
#endif



namespace nowide {
#if !defined(NOWIDE_WINDOWS) && !defined(NOWIDE_DOXYGEN)
    using std::fopen;
    using std::freopen;
    using std::remove;
    using std::rename;
#else

///
/// \brief Same as freopen but file_name and mode are UTF-8 strings
///
/// If invalid UTF-8 given, NULL is returned and errno is set to EINVAL
///
inline FILE *freopen(char const *file_name,char const *mode,FILE *stream)
{
    wstackstring wname;
    wshort_stackstring wmode;
    if(!wname.convert(file_name) || !wmode.convert(mode)) {
        errno = EINVAL;
        return 0;
    }
    return _wfreopen(wname.c_str(),wmode.c_str(),stream);
}
///
/// \brief Same as fopen but file_name and mode are UTF-8 strings
///
/// If invalid UTF-8 given, NULL is returned and errno is set to EINVAL
///
inline FILE *fopen(char const *file_name,char const *mode)
{
    wstackstring wname;
    wshort_stackstring wmode;
    if(!wname.convert(file_name) || !wmode.convert(mode)) {
        errno = EINVAL;
        return 0;
    }
    return _wfopen(wname.c_str(),wmode.c_str());
}
///
/// \brief Same as rename but old_name and new_name are UTF-8 strings
///
/// If invalid UTF-8 given, -1 is returned and errno is set to EINVAL
///
inline int rename(char const *old_name,char const *new_name)
{
    wstackstring wold,wnew;
    if(!wold.convert(old_name) || !wnew.convert(new_name)) {
        errno = EINVAL;
        return -1;
    }
    return _wrename(wold.c_str(),wnew.c_str());
}
///
/// \brief Same as remove but name is UTF-8 string
///
/// If invalid UTF-8 given, -1 is returned and errno is set to EINVAL
///
inline int remove(char const *name)
{
    wstackstring wname;
    if(!wname.convert(name)) {
        errno = EINVAL;
        return -1;
    }
    int retval;
    retval = _wremove(wname.c_str());
    return (retval == -1)? _wrmdir (wname.c_str()) : retval;
}
///
/// \brief Same as chmod but file_name and mode are UTF-8 strings
///
/// If invalid UTF-8 given, -1 is returned and errno is set to EINVAL
///
inline int chmod(char const *name, int const mode)
{
    wstackstring wname;
    if(!wname.convert(name) ) {
        errno = EINVAL;
        return -1;
    }
    return _wchmod (wname.c_str(), mode);
}
	
///
/// \brief Same as mkdir but file_name and mode are UTF-8 strings
///
/// If invalid UTF-8 given, -1 is returned and errno is set to EINVAL
///
inline int mkdir(char const *name, int const mode)
{
    wstackstring wname;
    if(!wname.convert(name) ) {
        errno = EINVAL;
        return -1;
    }
    return _wmkdir (wname.c_str());
}
#include <sys/stat.h>
inline int stat(char const *name, struct_stat *buf)
{
    wstackstring wname;
    if(!wname.convert(name)) {
        errno = EINVAL;
        return -1;
    }
#ifdef __MINGW64__
    return _wstat64(wname.c_str(), buf);
#else
    return _wstat32(wname.c_str(), buf);
#endif
}
typedef _WDIR DIR;
inline DIR* opendir(char const *name)
{
    wstackstring wname;
    if(!wname.convert(name) ) {
        errno = EINVAL;
        return NULL;
    }
    return _wopendir (wname.c_str());
}

inline char* readir_entry(DIR* dir)
{
  struct _wdirent *wentry;
  wentry = _wreaddir (dir);
  while (wentry 
	     && (0 == wcscmp (wentry->d_name, L".") ||
		 0 == wcscmp (wentry->d_name, L"..")))
	     wentry = _wreaddir (dir);
  if (wentry == NULL) return NULL;
  else { 
  stackstring entry;
    if(!entry.convert(wentry->d_name) ) {
        errno = EINVAL;
        return NULL;
    }
    else return entry.c_str();
  }
} 

#endif
} // nowide


#ifdef NOWIDE_MSVC
#pragma warning(pop)
#endif

#endif
///
// vim: tabstop=4 expandtab shiftwidth=4 softtabstop=4
