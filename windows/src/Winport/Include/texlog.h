#ifndef TEXLOG_H
#define TEXLOG_H

#define LEVEL_DEBUG	0
#define LEVEL_FORCE	5

bool TEXLOG_Initialize(int level, char *logFilePath);

bool TEXLOG_Log(int level, char *format, ...);

#endif