/******************************************************************************
* MODULE     : sysmisc.cpp
* DESCRIPTION: Windows version of various POSIX functions
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <stdlib.h>
#include <windows.h>
#include <time.h>
#include <stdio.h>
#include <shellapi.h>
#include <sys/misc.h>
#include <sys/socket.h>

#define BUFFER_SIZE	1024

typedef struct BUFFER_STRUCT_{
	char *buffer;
	DWORD bufferSize;
}BUFFER_STRUCT;

BUFFER_STRUCT getEnvBuffer = {NULL, 0};

void GetLocaleLanguage(char *buffer);
int ShellOut(char *file, char *operation);
int WaitForProcess(HANDLE process);
int HandlePkCompilation(char *command);
int HandleTfmCompilation(char *command);
char* stristr(char *theString, char *toSearch);
int RunGhostScript(char *cmd);
int HandleModifierMapRequest(char *commandLine);
void ReplaceArg(char *theString, char *argString, char *replaceString);

bool SysMiscInitialize(){

/*	char path[1024], texHome[1024], *temp;

	temp = getenv("Path");
	sprintf(path, "%s;", temp);
	temp = getenv("TEX_HOME");

	if(!temp)
		return false;

	sprintf(texHome, "%s\\miktex\\bin", temp);
	strcat(path, texHome);

	printf("%s", path);
	setenv("Path", path, 0);
*/
	return true;
}

void ConvertPathing(char *toConvert){

	if(!toConvert)
		return;

	for(int i = 0; i < strlen(toConvert); i++){
		if(toConvert[i] == '/')
			toConvert[i] = '\\';
	}

}

int gettimeofday(struct timeval *tp, void *tzp){
	SYSTEMTIME sysTime;

	GetSystemTime(&sysTime);
	
	tp->tv_sec = time(NULL);
	tp->tv_usec = sysTime.wMilliseconds * 1000;

	return 0;
}

int setenv(const char *name, const char *value, int rewrite){

//	printf("Call to setenv, %s=%s\n", name, value);
	if(SetEnvironmentVariable(name, value))
		return 1;
	else
		return 0;

	return 0;
}

OS_VERSION GetOsVersion(){

	OSVERSIONINFO vInfo;

	vInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	GetVersionEx(&vInfo);

	switch(vInfo.dwMajorVersion){

		case 3: return OS_VERSION_WIN_NT351;
		case 4:
				switch(vInfo.dwMinorVersion){
					case 0: 
						switch(vInfo.dwPlatformId){
							case VER_PLATFORM_WIN32_WINDOWS: 
								return OS_VERSION_WIN_95;
							case VER_PLATFORM_WIN32_NT:
								return OS_VERSION_WIN_NT40;
							default:
								return OS_VERSION_UNSUPPORTED;
						}

					case 10: return OS_VERSION_WIN_98;
					case 90: return OS_VERSION_WIN_ME;
					default: return OS_VERSION_UNSUPPORTED;
				}
		case 5:

				switch(vInfo.dwMinorVersion){
					case 0: return OS_VERSION_WIN_2000;
					case 1: return OS_VERSION_WIN_XP;
					case 2: return OS_VERSION_WIN_SERVER_2003;
					default: return OS_VERSION_UNSUPPORTED;
				}
		default: return OS_VERSION_UNSUPPORTED;
	}

}

bool SetBufferSize(BUFFER_STRUCT *buffer, DWORD size){

	if(buffer->bufferSize >= size)
		return true;

	if((buffer->buffer = (char*)realloc(buffer->buffer, size)) == NULL)
		return false;

	buffer->bufferSize = size;
	memset(buffer->buffer, 0, buffer->bufferSize);

	return true;
}

char* getenv(char *name){

	char temp[BUFFER_SIZE];
	OS_VERSION version;

	//printf("Call to getenv, %s\n", name);
	memset(getEnvBuffer.buffer, 0, getEnvBuffer.bufferSize);
	version = GetOsVersion();
	strcpy(temp, name);

	if(strcmp(name, "TEXMACS_STYLE_PATH") == 0)
		int x = 1;

	if(temp[0] == '$')
		memcpy(temp, &temp[1], strlen(temp));

	if(strcmp(temp, "HOME") == 0){

		if((version == OS_VERSION_WIN_2000)
			|| (version == OS_VERSION_WIN_XP)
			|| (version == OS_VERSION_WIN_SERVER_2003)
			|| (version == OS_VERSION_WIN_NT40))
			strcpy(temp, "USERPROFILE");
		else
			strcpy(temp, "TEXMACS_PATH");
	}

	if(strcmp(temp, "LANG") == 0){
		SetBufferSize(&getEnvBuffer, 100);
		GetLocaleLanguage(getEnvBuffer.buffer);
		//printf("Returning value %s=%s\n", name, getEnvBuffer.buffer);
		return getEnvBuffer.buffer;
	}

	else{

getVariable:
		int retVal = GetEnvironmentVariable(temp, getEnvBuffer.buffer, getEnvBuffer.bufferSize);

		if((retVal > 0) && (retVal <= getEnvBuffer.bufferSize)){
			//printf("Returning value %s=%s\n", name, getEnvBuffer.buffer);	
			return getEnvBuffer.buffer;
		}
		else if((retVal > getEnvBuffer.bufferSize) && SetBufferSize(&getEnvBuffer, (DWORD)retVal)){
			goto getVariable;
		}

		else{
			//printf("Invalid Environment Variable, returning NULL");
			return NULL;
		}
	}
}

char* ExpandEnvString(char *toExpand){

	static char returnBuffer[MAX_PATH];
	char tempBuffer[MAX_PATH];
	int len;

	strcpy(tempBuffer, toExpand);
	len = strlen(tempBuffer);

	for(int i = 0; i < len; i++){
		if(tempBuffer[i] == '$'){
			tempBuffer[i] = '%';
			for(int x = i; x <= len; x++){
				if((tempBuffer[x] == '\\') 
					|| (tempBuffer[x] == '\"')
					|| (x == len)){
					memcpy(&tempBuffer[x + 1], &tempBuffer[x], (len - x) + 1);
					tempBuffer[x] = '%';
					len++;
					break;
				}
				i++;
			}
		}
	}

	ExpandEnvironmentStrings(tempBuffer, returnBuffer, MAX_PATH);

	return returnBuffer;
}

char* GetWorkingDirectory(){

	static char buffer[1024];

	GetCurrentDirectory(1024, buffer);

	return buffer;
}

void GetLocaleLanguage(char *buffer){

	LANGID langid;
	char *ptr;

	langid = GetSystemDefaultLangID();

	switch(langid){
		case 0x0809: ptr = "en_GB"; break;
		case 0x0405: ptr = "cs_CZ"; break;
		case 0x0413:  
		case 0x0813: ptr = "nl_NL"; break;
		case 0x0409:
		case 0x0c09:
		case 0x1009:
		case 0x1409:
		case 0x1809:
		case 0x1c09:
		case 0x2009:
		case 0x2409:
		case 0x2809:
		case 0x2c09:
		case 0x3009:
		case 0x3409: ptr = "en_US"; break;
		case 0x040c:
		case 0x080c:
		case 0x0c0c:
		case 0x100c:
		case 0x140c:
		case 0x180c: ptr = "fr_FR"; break;
		case 0x0407:
		case 0x0807:
		case 0x0c07:
		case 0x1007:
		case 0x1407: ptr = "de_DE"; break;
		case 0x040e: ptr = "hu_HU"; break;
		case 0x0410:
		case 0x0810: ptr = "it_IT"; break;
		case 0x0415: ptr = "pl_PL"; break;
		case 0x0416:
		case 0x0816: ptr = "pt_PT"; break;
		case 0x0418: ptr = "ro_RO"; break;
		case 0x0419: ptr = "ru_RU"; break;
		case 0x040a:
		case 0x080a:
		case 0x0c0a:
		case 0x100a:
		case 0x140a:
		case 0x180a:
		case 0x1c0a:
		case 0x200a:
		case 0x240a:
		case 0x280a:
		case 0x2c0a:
		case 0x300a:
		case 0x340a:
		case 0x380a:
		case 0x3c0a:
		case 0x400a:
		case 0x440a:
		case 0x480a:
		case 0x4c0a:
		case 0x500a: ptr = "es_ES"; break;
		case 0x041d:
		case 0x081d: ptr = "sv_SV"; break;
		case 0x0422: ptr = "uk_UA"; break;
		default: ptr = "en_US"; break;
	}

	strcpy(buffer, ptr);
}

void QuoteArgs(char **toQuote, int numArgs){

	char *ptr;
	int len;

	len = strlen(*toQuote);
	ptr = *toQuote;

	for(int i = 0; i < numArgs; i++){
		
		while(isalpha(*ptr)) ptr++;
		while(isspace(*ptr)) ptr++;

		if(*ptr == '\"'){
				ptr++;
				while(*ptr != '\"') ptr++;
		}
		
		else{
			memcpy(ptr + 1, ptr, len - (ptr - *toQuote) + 1);
			len++;

			*ptr = '\"';

			while(!isspace(*ptr) && (*ptr != '\0')) ptr++;

			memcpy(ptr + 1, ptr, len - (ptr - *toQuote) + 1);
			len++;
			*ptr = '\"';
		}
		ptr++;
		*toQuote = ptr;
	}

}

int _system(char *command){

	char temp[1024];
	char *ptr, *ptr2, *ptr3;
	int len;
	bool doneParsing = false;

	strcpy(temp, command);
	len = strlen(temp);
	ptr = temp;

	//printf("Call to _system to run \"%s\"\n", command);

	for(int i = 0; i < len; i++){
		if(temp[i] == '$'){
			temp[i] = '%';
			for(int x = i; x <= len; x++){
				if((temp[x] == '\\') 
					|| (temp[x] == '\"')
					|| (x == len)){
					memcpy(&temp[x + 1], &temp[x], (len - x) + 1);
					temp[x] = '%';
					len++;
					break;
				}
				i++;
			}
		}
	}

	while(!doneParsing){

		len = strlen(ptr);
		/*if((ptr2 = strstr(ptr, "mkdir")) != NULL){

			ptr2 += 6;
			memcpy(ptr2 + 1, ptr2, len - (ptr2 - temp) + 1);
			len++;
			*ptr2 = '\"';
			ptr2++;

			while((*ptr2 != '\0') 
					&& (*ptr2 != '>') 
					&& (*ptr2 != '<')
					&& (*ptr2 != '|'))
					ptr2++;

			memcpy(ptr2 + 1, ptr2, len - (ptr2 - temp) + 1);
			len++;
			*ptr2 = '\"';
			ptr = ptr2;
		}*/

		if((ptr2 = strstr(ptr, "mkdir ")) != NULL){

//			QuoteArgs(&ptr2, 1);
		}


		else if((ptr2 = strstr(ptr, "cp ")) != NULL){
			
			memcpy(ptr2 + 2, ptr2, len - (ptr2 - temp) + 1);
			ptr2[0] = 'c';
			ptr2[1] = 'o';
			ptr2[2] = 'p';
			ptr2[3] = 'y';
			ptr2[4] = ' ';

//			QuoteArgs(&ptr2, 2);


		}

		else if((ptr2 = strstr(ptr, "cat ")) != NULL){

			memcpy(ptr2 + 1, ptr2, len - (ptr2 - temp) + 1);
			ptr2[0] = 't';
			ptr2[1] = 'y';
			ptr2[2] = 'p';
			ptr2[3] = 'e';
			ptr2[4] = ' ';

//			QuoteArgs(&ptr2, 1);

			while((*ptr2 == ' ') || (*ptr2 == '>') && (*ptr2 != '\0')) ptr2++;

//			QuoteArgs(&ptr2, 1);
		}
		
		else if((ptr2 = strstr(ptr, "pwd ")) != NULL){
		
			memcpy(ptr2 + 2, ptr2 + 3, len - (ptr2 - temp) + 1);
			ptr2[0] = 'c';
			ptr2[1] = 'd';
			ptr2[2] = ' ';
		}

		else if((ptr2 = strstr(ptr, "rm ")) != NULL){

			ReplaceArg(ptr2, "-f", "/Q");
				
			memcpy(ptr2 + 1, ptr2, len - (ptr2 - temp) + 1);
			ptr2[0] = 'd';
			ptr2[1] = 'e';
			ptr2[2] = 'l';
			ptr2[3] = ' ';

	//		QuoteArgs(&ptr2, 1);
		}

		if((ptr2 = strstr(ptr, "lpr ")) != NULL){

			return ShellOut(ptr2 + 4, "print");
		}

		else if ((ptr2 = strstr(ptr, "ghostview ")) != NULL){

			return ShellOut(ptr2 + 10, "open");
		}

		else if ((ptr2 = strstr(ptr, "gs ")) != NULL){

			return RunGhostScript(ptr2 + 3);
		}

		else if ((ptr2 = stristr(ptr, "makepk ")) != NULL){

			return HandlePkCompilation(ptr2);
		}

		else if ((ptr2 = stristr(ptr, "maketfm ")) != NULL){

			return HandleTfmCompilation(ptr2);
		}

		else if((ptr2 = stristr(ptr, "xmodmap ")) != NULL){
			
			return HandleModifierMapRequest(ptr2);
		}

		else
			doneParsing = true;

		ptr = ptr2;
	}

	//printf("_system converted UNIX command \"%s\" to Windows command \"%s\"\n", command, temp);
	return system(temp);
	//return system(ExpandEnvString(temp));
}

void ReplaceArg(char *theString, char *argString, char *replaceString){

	char *ptr;
	int length1, length2, length3;

	ptr = strstr(theString, argString);

	if(ptr == NULL) return;

	length3 = strlen(theString);
	length1 = strlen(argString);
	length2 = strlen(replaceString);

	if(length1 < length2)
		memcpy(ptr, ptr + (length2 - length1), length3 - (ptr - theString));

	else if(length1 > length2)
		memcpy(ptr + (length1 - length2), ptr - (length1 - length2), length3 - (ptr - theString));
	
	memcpy(ptr, replaceString, length2);
}

char* stristr(char *theString, char *toSearch){

	int length = strlen(theString);
	char *copyString;
	char *copySearch;
	char *ret;
	int i;

	copyString = (char*)malloc(strlen(theString) + 1);
	copySearch = (char*)malloc(strlen(toSearch) + 1);
	strcpy(copyString, theString);
	strcpy(copySearch, toSearch);

	for(i = 0; i < length; i++)
		copyString[i] = tolower(copyString[i]);

	length = strlen(toSearch);

	for(i = 0; i < length; i++)
		copySearch[i] = tolower(copySearch[i]);

	ret = strstr(copyString, copySearch);

	free(copySearch);
	free(copyString);

	if(!ret)
		return NULL;

	return theString + (ret - copyString);
}

int RunGhostScript(char *cmd){

	char command[MAX_PATH + 100];
	int length = strlen(cmd);
	char *cmdPtr, *commandPtr;

	strcpy(command, "\"%GS_PATH%\\gs*\\bin\\gswin32.exe\" -dNODISPLAY ");
	cmdPtr = cmd;
	commandPtr = command + strlen(command);

	while(*cmdPtr != '\0'){
			
		if(!((cmdPtr + 3) > (cmd + length))
			&& (*cmdPtr == '=')
			&& (*(cmdPtr + 1) == 'x')
			&& (*(cmdPtr + 2) == '1')
			&& (*(cmdPtr + 3) == '1')
			&& (*(cmdPtr + 4) == ' ')){
			memcpy(commandPtr, "#mswindll ", strlen("#mswindll "));
			cmdPtr += strlen("x11 ");
			commandPtr += strlen("#mswindll ");
			continue;
		}

		*commandPtr = *cmdPtr;
		commandPtr++;
		cmdPtr++;
	}
	*commandPtr = '\0';

	return system(ExpandEnvString(ExpandEnvString(command)));
}

int HandleTfmCompilation(char *command){

	system(command);
	return 1;
}

int HandlePkCompilation(char *command){

	char *ptr;
	char directory[MAX_PATH];
	char dpi[10];
	char name[MAX_PATH];
	char oldFileName[MAX_PATH];
	char newFileName[MAX_PATH];
	int i, argCount, x;
	char endChar;

	endChar = ' ';
	if(ptr = strstr(command, "--dest-dir ")){

		i = 0;
		ptr += strlen("--dest-dir ");

		if(ptr[i] == '"'){
			endChar = '"';
			ptr++;
		}

		while(ptr[i] != endChar){
			directory[i] = ptr[i];
			i++;
		}

		directory[i] = '\0';
	}

	else if(ptr = strstr(command, "-D ")){
		i = 0;
		ptr += strlen("-D ");

		while(ptr[i] != endChar){
			directory[i] = ptr[i];
			i++;
		}

		directory[i] = '\0';
	}

	else {
		return system(command);
	}

	ptr = stristr(command, "makepk ");
	ptr += strlen("makepk ");

	i = 0;
	argCount = 1;
	while(true){
		while(isspace(ptr[i])) i++;
		if(ptr[i] == '-'){
			while(!isspace(ptr[i])) i++;
			while(isspace(ptr[i])) i++;
			while(ptr[i] == endChar) i++;
			while(ptr[i] != endChar) i++;
			while(ptr[i] == endChar) i++;
		}

		else{
			argCount++;
			if(argCount == 2){
				x = 0;
				while(!isspace(ptr[i])) {
					name[x] = ptr[i];
					i++;
					x++;
				}

				name[x] = '\0';
			}

			if(argCount == 3){
				x = 0;
				while(isalnum(ptr[i])) {
					dpi[x] = ptr[i];
					i++;
					x++;
				}

				dpi[x] = '\0';
				break;
			}

			while(!isspace(ptr[i])) i++;
			while(isspace(ptr[i])) i++;
		}
	}

	sprintf(oldFileName, "%s\\%s.pk", directory, name);
	sprintf(newFileName, "%s\\%s.%spk", directory, name, dpi);

	system(command);

	if(!MoveFileEx(oldFileName, newFileName, 
		MOVEFILE_WRITE_THROUGH | MOVEFILE_COPY_ALLOWED)){
		DeleteFile(oldFileName);
		return 0;
	}

	return 1;
}

int ShellOut(char *file, char *operation){

	char *fileName;
	SHELLEXECUTEINFO executeInfo;

	memset(&executeInfo, 0, sizeof(SHELLEXECUTEINFO));
	fileName = file;

	int i = 0;

//	while(isspace(fileName[i])) i++;
//	while(!isspace(fileName[i]) && (fileName[i] != '\0')) i++;
//	fileName[i] = '\0';

	executeInfo.cbSize = sizeof(SHELLEXECUTEINFO);
	executeInfo.fMask = SEE_MASK_DOENVSUBST | SEE_MASK_FLAG_DDEWAIT | SEE_MASK_NOCLOSEPROCESS;
	executeInfo.hwnd = GetDesktopWindow();
	executeInfo.lpVerb = operation;
	executeInfo.lpFile = fileName;
	executeInfo.nShow = SW_SHOWNORMAL;
	//if((int)ShellExecute(GetDesktopWindow(), "print", fileName, NULL, NULL, SW_SHOWNORMAL) < 32){
	
	ShellExecuteEx(&executeInfo);

	if((int)executeInfo.hInstApp < 32){
		MessageBox(NULL, "Unable to print postscript file, please install ghostscript",
					"Print Failure", MB_OK | MB_ICONERROR);
		return 0;
	}

	WaitForProcess(executeInfo.hProcess);
	return 1;
}

int WaitForProcess(HANDLE process){

	DWORD status;

	while(GetExitCodeProcess(process, &status) != 0){

		if(status != STILL_ACTIVE){
			return 1;
		}

		Sleep(1000);
	}

	return 0;
}

int HandleModifierMapRequest(char *commandLine){

	return 1;
}

int select(int  n,fd_set *readfds,fd_set *writefds,
		   fd_set *exceptfds, timeval *timeout){

	Sleep(timeout->tv_sec * 100 + timeout->tv_usec / 100);
	return 0;
}

//int execve(const char *filename,char *const argv [],
//		   char *const envp[]);
int pipe(int *fildes){
	return 0;
}

int fork(){
	return 0;
}

int close(int fildes){
	
	if(SOCKET_IsSocket(fildes))
		return SOCKET_close(fildes);
	else
		return ERROR;
}

size_t read(int d, void *buf, size_t nbytes){
	
	if(SOCKET_IsSocket(d))
		return SOCKET_read(d, buf, nbytes);

	return ERROR;
}

int wait(int *status){
	return 0;
}

size_t write(int d, const void *buf, size_t nbytes){
	
	if(SOCKET_IsSocket(d))
		return SOCKET_write(d, buf, nbytes);

	return ERROR;
}

int kill(pid_t pid, int sig){
	return 0;
}
