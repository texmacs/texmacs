/******************************************************************************
* MODULE     : dirent.cpp
* DESCRIPTION: Windows implementation of POSIX dirent.h
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <windows.h>
#include <stdlib.h>
#include <sys/misc.h>
#include <dirent.h>

static dirent staticDirent;

int closedir(DIR *toClose){

	if(!toClose || !toClose->isOpen)
		return -1;

	FindClose(toClose->findHandle);
	memset(toClose, 0, sizeof(DIR));
	free(toClose);
	return 0;
}

DIR* opendir(const char *pathName){

	HANDLE findHandle;
	WIN32_FIND_DATA findData;
	char path[MAX_PATH];
	char rootPath[MAX_PATH];
	DIR *newDir;

	memset(path, 0, MAX_PATH);
	strncpy(rootPath, pathName, MAX_PATH);
	strcpy(rootPath, ExpandEnvString(rootPath));

	strcpy(path, rootPath);
	strcat(path, "\\*.*");
	memset(&findData, 0, sizeof(findData));
	findHandle = FindFirstFile(path, &findData);

	if(findHandle == INVALID_HANDLE_VALUE)
		return NULL;

	newDir = (DIR*)malloc(sizeof(DIR));
	memset(newDir, 0, sizeof(DIR));
	newDir->position = 0;
	newDir->isOpen = 1;
	newDir->findDataValid = true;
	memcpy(&newDir->findData, &findData, sizeof(WIN32_FIND_DATA));
	strcpy(newDir->d_name, rootPath);
	newDir->findHandle = findHandle;

	return newDir;

}

struct dirent *readdir(DIR *toRead){

	dirent *temp;

	memset(&staticDirent, 0, sizeof(dirent));
	readdir_r(toRead, &staticDirent, &temp);

	if(temp != &staticDirent)
		return NULL;

	return &staticDirent;
}

int readdir_r(DIR *toRead, struct dirent *entry, struct dirent **result){

	if(!toRead || !toRead->isOpen){
		*result = NULL;
		return -1;
	}

	if(!toRead->findDataValid){
		*result = NULL;
		return 0;
	}

	memset(entry, 0, sizeof(dirent));
	entry->d_ino = toRead->position;
	entry->d_off = toRead->position;
	entry->d_reclen = 1;
	strcpy(entry->d_name, toRead->findData.cFileName);

	(!FindNextFile(toRead->findHandle, &toRead->findData)) ? 
		(toRead->findDataValid = false) : (toRead->position++);

	*result = entry;
	return 0;
}

void rewinddir(DIR *toRewind){

	if(!toRewind || !toRewind->isOpen)
		return;

	closedir(toRewind);
	opendir(toRewind->d_name);
}

void seekdir(DIR *toSeek, long int location){

	if(!toSeek || !toSeek->isOpen)
		return;

	rewinddir(toSeek);

	while(toSeek->position != location){
		
		if(!readdir(toSeek))
			return;
	}

	return;
}

long int telldir(DIR *toTell){

	if(!toTell || !toTell->isOpen)
		return -1;

	return toTell->position;
}