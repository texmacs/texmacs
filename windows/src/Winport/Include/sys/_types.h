#ifndef SYSTYPES_H
#define SYSTYPES_H

#include <sys/types.h>

#ifndef _WINSOCKAPI_
typedef void* HANDLE;
typedef unsigned long DWORD;
typedef char CHAR;
typedef int BOOL;
typedef unsigned char BYTE;
typedef unsigned short USHORT;

typedef unsigned short u_short;
typedef unsigned char u_char;
typedef unsigned long u_long;
typedef unsigned int u_int;
#endif

#endif