/******************************************************************************
* MODULE     : dlfcn.cpp
* DESCRIPTION: Windows version of POSIX dlfcn.cpp
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "dlfcn.h"
#include "windows.h"

#define ERROR_STRING_LEN	255

char error[ERROR_STRING_LEN];

void dlcaptureMessage();
void dlclearMessage();

void *dlopen(const char *file, int mode){

	HMODULE theLibrary;

	dlclearMessage();
	if(file == NULL){
		return GetModuleHandle(NULL);
	}

	theLibrary = LoadLibrary(file);

	if(!theLibrary){
		dlcaptureMessage();
		return NULL;
	}

	return theLibrary;

}

void *dlsym(void *handle, const char *name){

	HANDLE procAddress;

	dlclearMessage();

	procAddress = GetProcAddress((HINSTANCE)handle, name);

	if(!procAddress){
		dlcaptureMessage();
		return NULL;
	}

	return procAddress;
}

char *dlerror(void){

	if(strlen(error) == 0)
		return NULL;

	return error;
}

void dlclose(void *handle){

	if(!FreeLibrary((HINSTANCE)handle)){
		dlcaptureMessage();
		return;
	}
}

void dlclearMessage(){

	memset(error, 0, ERROR_STRING_LEN);
}

void dlcaptureMessage(){

	if (!FormatMessage( 
    FORMAT_MESSAGE_ALLOCATE_BUFFER | 
    FORMAT_MESSAGE_FROM_SYSTEM | 
    FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    GetLastError(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
    (LPTSTR) error,
    0,
    NULL ))
	{
		// Handle the error.
		return;
	}
}