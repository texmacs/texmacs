#include "texlog.h"
#include <stdio.h>
#include <time.h>
//#include <string.h>
#include <windows.h>

int logLevel;
FILE *logFile;
#define MAX_MESSAGE_SIZE	4096


bool TEXLOG_Initialize(int level, char *logFilePath){

	char path[MAX_PATH];

	logLevel = level;

	logFile = NULL;

	if(logFilePath)
		logFile = fopen(logFilePath, "wb");

	if(!logFile){
		GetCurrentDirectory(MAX_PATH, path);
		sprintf(path, "%s\\debug.log", path);

		logFile = fopen(path, "wb");

		if(!logFile)
			return false;
	}

	TEXLOG_Log(LEVEL_FORCE, "Log File Initialized");
	return true;
}

bool TEXLOG_Log(int level, char *format, ...){

	char message[MAX_MESSAGE_SIZE];
	char toPrint[MAX_MESSAGE_SIZE + 100];
	char timeStamp[100];
	va_list argList;
	time_t t;

	if(!logFile || (level < logLevel)){
		return false;
	}

	va_start(argList, format);

	t = time(NULL);
	strftime(timeStamp, 100, "%b %d %Y %H:%M:%S - ", localtime(&t)); 
	vsprintf(message, format, argList);
	sprintf(toPrint, "%s%s\n", timeStamp, message);
	fwrite(toPrint, sizeof(char), strlen(toPrint), logFile);
	fflush(logFile);

	va_end(argList);

	return true;
}