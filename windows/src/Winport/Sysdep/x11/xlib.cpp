
/******************************************************************************
* MODULE     : xlib.cpp
* DESCRIPTION: TeXmacs XWindow subset for Win32 GDI
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#pragma warning(disable: 4786);

#include <windows.h>
#include <commctrl.h>
#include <X11/xlib.h>
#include <sys/_types.h>
#include <iostream.h>
#include <texlog.h>
#include <sys/misc.h>
#include <freeImage.h>
#include <CXimage/ximage.h>
#include <X11/XColorTable.h>
#include <X11/XAtom.h>
#include <map>
#include <vector>
using namespace std;
//nclude "..\..\texmacswin32\resource.h"

#define DEBUGXCOPYAREA			0
#define DEBUG

#define DEFAULT_COLORMAP		-1
#define EVENT_LIST_SIZE		4096
#define WAKEUP_MESSAGE			(WM_USER + 500)
#define TITLE_BAR_WIDTH			0
#define MAX_SLEEP_TIME			1000
#define MAX_NUM_CHILDREN		255

#define LOCK(criticalSection){	\
    EnterCriticalSection(&criticalSection);	\
}

#define RELEASE(criticalSection){	\
    LeaveCriticalSection(&criticalSection);	\
}

typedef enum {
    TYPE_DISPLAY_INFO = 0,
    TYPE_WINDOW_INFO,
    TYPE_GC_INFO,
    TYPE_COLORMAP_INFO,
    TYPE_SIZEHINTS_INFO,
    TYPE_WMHINTS_INFO,
    TYPE_CLASSHINTS_INFO,
    TYPE_PIXMAP_INFO,
    TYPE_REGION_INFO,
    TYPE_KEYBOARD_INFO,
    TYPE_ATOM_INFO,
    TYPE_BACKING_STORE_INFO,
    TYPE_END_LIST
}STRUCT_TYPE;

typedef struct INTERNAL_XEVENT_{
    int mask;
    INTERNAL_XEVENT_ *next;
    INTERNAL_XEVENT_ *previous;
    XEvent event;
}INTERNAL_XEVENT;

typedef struct INFO_BASE_{
    STRUCT_TYPE type;
    XID handle;
}INFO_BASE;

typedef struct GC_INFO_{
    INFO_BASE base;
    Drawable d;
    HPEN pen;
    Bool graphics_exposure;
    DWORD penStyle;
    DWORD lineWidth;
    HRGN clippingRgn;
    unsigned int foregroundPixel;
    unsigned int backgroundPixel;
}GC_INFO;

typedef struct BACKING_STORE_INFO_{
    INFO_BASE base;
    HWND window;
    HBITMAP bitmap;
    HDC dc;
    int x;
    int y;
    int width;
    int height;
}BACKING_STORE_INFO;

typedef struct SIZEHINTS_INFO_{
    INFO_BASE base;
    XSizeHints sizeHints;
}SIZEHINTS_INFO;

typedef struct WMHINTS_INFO_{
    INFO_BASE base;
    XWMHints wmHints;
}WMHINTS_INFO;

typedef struct CLASSHINTS_INFO_{
    INFO_BASE base;
    XClassHint classHint;
}CLASSHINTS_INFO;

typedef struct COLORMAP_INFO_{
    INFO_BASE base;
    HPALETTE palette;
    HDC theDC;
    UINT numEntries;
}COLORMAP_INFO;

typedef struct REGION_INFO_{
    INFO_BASE base;
    HRGN region;
    GC_INFO *theGC;
}REGION_INFO;

typedef struct WINDOW_INFO_{
    INFO_BASE base;
    HWND window;
    XID xid;
    DWORD eventMask;
    int border_width;
    bool isDesktopWindow;
    Bool override_direct;
    int maxWidth;
    int maxHeight;
    int minWidth;
    int minHeight;
    Display *theDisplay;
    int x;
    int y;
    int width;
    int height;
    BOOL containsMouse;
    COLORMAP_INFO *theColormap;
    Window children[MAX_NUM_CHILDREN];
    Window parent;
    int numChildren;
    HICON smallIcon;
    HICON largeIcon;
    UINT lastShown;
}WINDOW_INFO;

typedef struct PIXMAP_INFO_{
    INFO_BASE base;
    HBITMAP bitmap;
    HDC theDC;
    COLORMAP_INFO *colorMap;
    int width;
    int height;
    int depth;
    int bitsPerPixel;
}PIXMAP_INFO;

typedef struct XEVENT_LIST_{
    INTERNAL_XEVENT eventAlloc[EVENT_LIST_SIZE];
    INTERNAL_XEVENT eventListHead;
    INTERNAL_XEVENT eventListTail;
    INTERNAL_XEVENT freeListHead;
    int numEvents;
//	CRITICAL_SECTION listLock;
//	HANDLE eventSignal;
}XEVENT_LIST;

typedef struct DISPLAY_INFO_{
    INFO_BASE base;
    Display disp;
    ATOM wndClass;
    XEVENT_LIST eventList;
//	CMapPtrToPtr *windowMap;
//	HANDLE eventThread;
//	DWORD eventThreadID;
//	DWORD mainThreadID;
//	bool runEventThread;
}DISPLAY_INFO;

typedef struct MOUSE_INFO_{
    INFO_BASE base;
    Window currentWindow;
    POINT clientPoint;
    int buttonMask;
}MOUSE_INFO;

typedef struct KEYBOARD_INFO_{
    INFO_BASE base;
    HKL hkb;
    LANGID langId;
} KEYBOARD_INFO;

typedef struct ATOM_INFO_{
    INFO_BASE base;
    ATOM atomID;
    char *name;
    void *value;
    int valueType;
    long valueSize;
    Window owner;
    Time time;
    XID xid;
}ATOM_INFO;

typedef struct POINTER_CAPTURE_{
    HWND currentWindow;
    HWND confineTo;
    unsigned int event_mask;
}POINTER_CAPTURE;

const UINT STRUCT_SIZES[] = {
    sizeof(DISPLAY_INFO),
    sizeof(WINDOW_INFO),
    sizeof(GC_INFO),
    sizeof(COLORMAP_INFO),
    sizeof(SIZEHINTS_INFO),
    sizeof(WMHINTS_INFO),
    sizeof(CLASSHINTS_INFO),
    sizeof(PIXMAP_INFO),
    sizeof(REGION_INFO),
    sizeof(KEYBOARD_INFO),
    sizeof(ATOM_INFO),
    sizeof(BACKING_STORE_INFO)
};

POINTER_CAPTURE captureData = {0, 0, 0};

extern int main(int argc, char**argv);
char progName[255];
HINSTANCE saveInstance;

unsigned int eventCounter = 0;
XID handleCounter = 0;
map<XID, void*> handleMap;
map<HWND, WINDOW_INFO*> fastWindowMap;
map<Atom, ATOM_INFO*> atomMap;
map<HWND, BACKING_STORE_INFO*> backingStoreMap;
WINDOW_INFO *rootWindow;
Display *rootDisplay;
MOUSE_INFO theMouse;
KEYBOARD_INFO theKeyboard;
HCURSOR cursor;

CRITICAL_SECTION fastWindowMapLock;
CRITICAL_SECTION handleMapLock;

void EnumerateAllWindows(vector<WINDOW_INFO*> &windowList);
BOOL CALLBACK EnumWindowsCallback(HWND hwnd, LPARAM lParam);
LRESULT CALLBACK XWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
XID XCreateInfoStruct(STRUCT_TYPE type);
unsigned int XGetStructSize(STRUCT_TYPE type);
bool XIsValidHandle(XID to
				    , STRUCT_TYPE type);
void* XGetInfoStruct(XID toGet);
bool XFreeInfoStruct(XID toFree);
//bool XStartEventThread(DISPLAY_INFO *theDisplay);
//bool XStopEventThread(DISPLAY_INFO *theDisplay);
//DWORD _stdcall XEventThread(void *param);
void XProcessMessageQueue(DISPLAY_INFO *display);
Status XSendCreateWindowEvent(Display *theDisplay, Window window);
BYTE XApproximateShortToByte(USHORT toApprox);
USHORT XApproximateByteToShort(BYTE toApprox);
int XGetDefaultPalette(HDC theDC, COLORREF defaultColorsArray[256]);
WINDOW_INFO* XGetWindowInfoStruct(Window w);
bool XSaveWindow(WINDOW_INFO *toStore);
bool XDeleteWindow(WINDOW_INFO *toDelete);
bool XRemoveWindow(Window);
BOOL XCreateIcon(Window theWindow, HBITMAP iconBitmap, HDC iconDC, HBITMAP maskBitmap, HDC maskDC, 
			     HICON *iconReturn, 
			     int bitmapWidth, int bitmapHeight, int bitsPerPixel, int numPlanes,
			     int iconWidth, int iconHeight);
int XGenerateGraphicsExposureEvents(Display *display, Drawable source, GC gc, int src_x, int src_y, 
								     int width, int height);
Status XSendNoExposeEvent(Display *display, Drawable source, GC eventGC, Bool doCheck);
Status XSendInternalEvent(Display*	display, Window	w, Bool propagate, long event_mask, XEvent*	event_send);
Status XSendGraphicsExposureEvent(Display *display, Drawable source, GC eventGC, int x, int y, 
							      int height, int width, Bool doCheck);
Status XSendMapEvent(Display *display, Window window);
//void ShiftEventQueueDown(DISPLAY_INFO *display, int startIndex, int numToShift);
//void ShiftEventQueueUp(DISPLAY_INFO *display, int startIndex, int numToShift);
void XTranslateWindowCoords(Window window, int *x, int *y);
void XTranslateClientCoordsToWindow(Window window, int *x, int *y); 
Status XSendConfigureNotifyEvent(Display *theDisplay, Window window,
							     int x, int y, int width, int height);
Status XSendDestroyNotifyEvent(Display *display, Window window);
Status XSendExposeEvent(Display *theDisplay, Window window, int x, int y, int width, int height);
Status XSendCrossingEvent(Display *display, Window window, int x, int y, int type, int mode, 
					      int state);
Status XSendMotionEvent(Display *display, Window window, int x, int y, int mode, int state);
Status XSendButtonEvent(Display *display, Window window, int x, int y, int type, int button, int state);
Status XAddChildToParent(Window parent, Window child);
Status XRemoveChildFromParent(Window parent, Window child);
void XGetWindowPositionRelativeToParent(Window toGet, int *x, int *y);
int XAddEventToEventList(XEVENT_LIST *theList, INTERNAL_XEVENT *toAdd, int position);
Status XInitializeEventList(XEVENT_LIST *toInitialize);
Status XRemoveEventFromEventList(XEVENT_LIST *theList, INTERNAL_XEVENT *buffer, int position);
Status XSendKeyboardEvent(Display *display, Window window, int type, int keycode, int state, Bool unicode);
Status XSendSelectionClearEvent(Display *display, Window window, Atom selection, Time time);
bool XDeleteObject(HANDLE toDelete);
bool XClearAtom(Atom toClear);
XID XCreateAtom(const char *name, Atom requestedID);
Status XSendSelectionNotifyEvent(Display *display, Window requestor, Atom selection, 
							     Atom target, Atom property, Time time);
Status XSendSelectionRequestEvent(Display *display, Window owner, Window requestor, Atom selection, 
							     Atom target, Atom property, Time time);
bool XIsPhantomWindow(Window toCheck);
int XUpdatePen(GC_INFO *toUpdate);
Bool XCopyAreaWithClipMask(GC_INFO *gcInfo, HDC srcDC, HDC destDC, int srcX, int srcY,
			   int width, int height, int dest_x, int dest_y);

map<USHORT, USHORT> keyMap;

USHORT keySymMap_normal[255];
USHORT keySymMap_shift[255];
USHORT keySymMap_lock[255];

extern HBITMAP SHLoadImageFile(LPCTSTR pszFileName);

WINDOW_INFO* XGetWindowInfoStruct(Window w){

    WINDOW_INFO *theWindow;
    map<HWND, WINDOW_INFO*>::iterator iter;
    BOOL retVal;

    LOCK(fastWindowMapLock);
    iter = fastWindowMap.find((HWND)w);
    
    if(iter != fastWindowMap.end()){
	    retVal = true;
	    theWindow = iter->second;
    }
    else{
	    retVal = false;
    }

//	retVal = fastWindowMap.Lookup((void*)w, (void*&)theWindow);
    RELEASE(fastWindowMapLock);

    return (retVal) ? theWindow : NULL;
}

HDC XGetDC(Drawable d){

    PIXMAP_INFO *pixInfo;
    WINDOW_INFO *winInfo;
    HDC temp;

    if(winInfo = XGetWindowInfoStruct(d)){
	    temp = GetDC((HWND)d);
	    if(!temp){
		    DWORD error = GetLastError();
	    }
	    return temp;
    }
    if(!(pixInfo = (PIXMAP_INFO*)XGetInfoStruct(d)))
	    return NULL;

    return pixInfo->theDC;

    
}

void XReleaseDC(Drawable d, HDC toRelease){

    WINDOW_INFO *winInfo;
    INFO_BASE* base;

    if(winInfo = XGetWindowInfoStruct(d)){
	    ReleaseDC((HWND)d, toRelease);
	    return;
    } 

}

Bool XQueryPointer(
Display* display		/* display */,
Window	window	/* w */,
Window*	root/* root_return */,
Window*	child	/* child_return */,
int* root_x	/* root_x_return */,
int* root_y		/* root_y_return */,
int* win_x		/* win_x_return */,
int* win_y		/* win_y_return */,
unsigned int* mask       /* mask_return */
){

//	WINDOW_INFO *windowInfo;
    POINT mousePoint;

    if(!theMouse.currentWindow)
	    return False;

    *root = (Window)rootWindow->window;
    *child = theMouse.currentWindow;
    
    GetCursorPos(&mousePoint);

    *win_x = theMouse.clientPoint.x;
    *win_y = theMouse.clientPoint.y;
    *mask = theMouse.buttonMask;

    return True;
    /*CPtrArray windowList;
    CPtrArray childWindowList;
    RECT origin;
    POINT cursorPos;
    bool found;
    int i;
    WINDOW_INFO *temp;

    EnumWindows(EnumWindowsCallback, (LPARAM)&windowList);
    GetWindowRect((HWND)window, &origin);
    GetCursorPos(&cursorPos);

    found = false;
    *root = *child = *root_x = *root_y = *win_x = *win_y = *mask = 0;
    for(i = 0; (i < windowList.GetSize()) && !found; i++){

	    temp = (WINDOW_INFO*)windowList.GetAt(i);

	    if((cursorPos.x >= temp->x) && (cursorPos.x <= (temp->x + temp->width))
		    && (cursorPos.y >= temp->y) && (cursorPos.y <= (temp->y + temp->height))){
			    *root = (Window)temp->window;
			    found = true;
			    EnumChildWindows((HWND)root, EnumWindowsCallback, (LPARAM)&childWindowList);

			    for(int x = 0; x < childWindowList.GetSize(); x++){
				    WINDOW_INFO *childTemp = (WINDOW_INFO*)childWindowList.GetAt(x);

				    if((cursorPos.x >= childTemp->x) && (cursorPos.x <= (childTemp->x + childTemp->width))
					    && (cursorPos.y >= childTemp->y) && (cursorPos.y <= (childTemp->y + childTemp->height))){
					    *child = (Window)childTemp->window;
					    break;
				    }
			    }
	    }
    }

    *root_x = (cursorPos.x - temp->x);
    *root_y = (cursorPos.y - temp->y);
    *win_x = (cursorPos.x - origin.left);
    *win_y = (cursorPos.y - origin.top);

    for(i = 0; i < windowList.GetSize(); i++){
	    free(windowList.GetAt(i));
    }

    for(i = 0; i < childWindowList.GetSize(); i++){
	    free(childWindowList.GetAt(i));
    }

    return found;
*/
}

BOOL CALLBACK EnumWindowsCallback(HWND hwnd, LPARAM lParam){

    vector<WINDOW_INFO*> *theList = (vector<WINDOW_INFO*>*)lParam;
    WINDOW_INFO *newInfo = (WINDOW_INFO*)malloc(sizeof(WINDOW_INFO));
    RECT temp;

    newInfo->window = hwnd;

    GetWindowRect(hwnd, &temp);
    newInfo->x = temp.left;
    newInfo->y = temp.top;
    newInfo->width = temp.right - temp.left;
    newInfo->height = temp.bottom - temp.top;
    theList->insert(&newInfo);

    return TRUE;
}

int XGrabPointer(
Display*	theDisplay	/* display */,
Window		grab_window/* grab_window */,
Bool		owner_events/* owner_events */,
unsigned int	event_mask/* event_mask */,
int			pointer_mode/* pointer_mode */,
int			keyboard_mode/* keyboard_mode */,
Window		confine_to/* confine_to */,
Cursor		cursor/* cursor */,
Time		time/* time */
    ){

    XCrossingEvent event;

    if(!SetCapture((HWND)grab_window))
	    return False;

    return True;
/*	if(captureData.currentWindow){
	    ReleaseCapture();
	    memset(&captureData, 0, sizeof(POINTER_CAPTURE));
    }

    captureData.currentWindow = (HWND)grab_window;
    captureData.confineTo = (HWND)confine_to;
    captureData.event_mask = event_mask;

    SetCapture((HWND)grab_window);

    event.type = EnterNotify;
    event.display = theDisplay;
    event.window = event.root = grab_window;
//	event.
//	XSendEvent(
    return GrabSuccess;*/
}

/*unsigned int XGetButtonMask(){

    unsigned int mask;
    unsigned char state[256];

    GetKeyboardState(&state);

    mask = (state[
    #define ShiftMask		(1<<0)
#define LockMask		(1<<1)
#define ControlMask		(1<<2)
#define Mod1Mask		(1<<3)
#define Mod2Mask		(1<<4)
#define Mod3Mask		(1<<5)
#define Mod4Mask		(1<<6)
#define Mod5Mask		(1<<7)
#define Button1Mask		(1<<8)
#define Button2Mask		(1<<9)
#define Button3Mask		(1<<10)
#define Button4Mask		(1<<11)
#define Button5Mask		(1<<12)
}*/

int XUngrabPointer(
Display*		display/* display */,
Time		time/* time */
    ){

    ReleaseCapture();
    return GrabSuccess;
}

Window XGetSelectionOwner(
Display*		display/* display */,
Atom		selection/* selection */
    ){

    ATOM_INFO *value;
    map<Atom,ATOM_INFO*>::iterator iter;

    iter = atomMap.find(selection);

    if(iter != atomMap.end()){
	    value = iter->second;

	    if(!XGetWindowInfoStruct(value->owner)){
		    XClearAtom(selection);
		    //atomMap.RemoveKey((void*)selection);
		    //XFreeInfoStruct(value->xid);
	    }

	    return value->owner;
    }

    return None;
}

bool XClearAtom(Atom toClear){

    ATOM_INFO *info;
    map<Atom, ATOM_INFO*>::iterator iter;

    iter = atomMap.find(toClear);

    if(iter == atomMap.end())
	    return false;

    info = iter->second;

    if(info->valueSize > 0){
	    free(info->value);
    }

    info->owner = None;
    info->time = 0;
    info->value = NULL;
    info->valueSize = 0;
    info->valueType = 0;

    return true;
}

bool XInitializeAtoms(){

    return XCreateAtom(NULL, XA_PRIMARY) && XCreateAtom(NULL, XA_SECONDARY);
}

Atom XInternAtom(
Display*		display/* display */,
_Xconst char*	atom_name/* atom_name */,
Bool		only_if_exists/* only_if_exists */		 
    ){

    ATOM_INFO *info;
    XID atomID;
    map<Atom, ATOM_INFO*>::iterator iter;

    for(iter = atomMap.begin(); iter != atomMap.end(); iter++){
	    atomID = iter->first;
	    info = iter->second;
	    if(info->name && (strcmp(atom_name, info->name)))
		    return info->xid;
    }

    if(!only_if_exists){

	    if(!(atomID = XCreateAtom(atom_name, None)))
		    return None;

	    return atomID;
    }

    return None;
}

ATOM_INFO* XGetAtom(Atom toGet){

    map<Atom, ATOM_INFO*>::iterator iter;

    iter = atomMap.find(toGet);

    if(iter == atomMap.end())
	    return NULL;

    return iter->second;
}

XID XCreateAtom(const char *name, Atom requestedID){

    XID atomID;
    ATOM_INFO *info;
    map<Atom, ATOM_INFO*>::iterator iter;

    iter = atomMap.find(requestedID);

    if(iter != atomMap.end()){
	    return None;
    }
    
    if(!(atomID = XCreateInfoStruct(TYPE_ATOM_INFO))){
	    return None;
    }

    if(!(info = (ATOM_INFO*)XGetInfoStruct(atomID))){
	    return None;
    }

    if(name)
	    info->name = strdup(name);

    info->atomID = (requestedID != None) ? requestedID : atomID;
    info->xid = atomID;

    atomMap[info->atomID] = info;
    return atomID;
}

int XConvertSelection(
Display*		display/* display */,
Atom		selection/* selection */,
Atom 		target/* target */,
Atom		property/* property */,
Window		requestor/* requestor */,
Time		time/* time */
    ){

    ATOM_INFO *selectionInfo;
    ATOM_INFO *propertyInfo;

    if(!(selectionInfo = XGetAtom(selection))){
	    return 0;
    }

    if(!(propertyInfo = XGetAtom(property))){
	    return 0;
    }

    XClearAtom(property);

    propertyInfo->time = time;

    if(selectionInfo->valueSize > 0){
	    propertyInfo->value = malloc(selectionInfo->valueSize);
	    memcpy(propertyInfo->value, selectionInfo->value, selectionInfo->valueSize);
	    propertyInfo->valueType = selectionInfo->valueType;
    }
    
    if(selectionInfo->owner)
	    XSendSelectionRequestEvent(display, selectionInfo->owner, requestor, 
							    selection, target, property, time);
    else
	    XSendSelectionNotifyEvent(display, requestor, selection, target, property, time);

    return 0;
}

Bool XCheckIfEvent(
Display*		display/* display */,
XEvent*		event_return/* event_return */,
Bool predicate(
	   Display*			display/* display */,
           XEvent*			event/* event */,
           XPointer			arg/* arg */
         )/* predicate */,
XPointer		arg/* arg */
    ){

    INTERNAL_XEVENT *current, buffer;
    DISPLAY_INFO *dispInfo;
    XEvent theEvent;
    int x;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)display)))
	    return False;

    XProcessMessageQueue(dispInfo);
    current = dispInfo->eventList.eventListHead.next;

    x = 0;
    while(current != &dispInfo->eventList.eventListTail){
    
	    if(predicate(display, &current->event, arg)){
		    XRemoveEventFromEventList(&dispInfo->eventList, &buffer, x);
		    memcpy(event_return, &buffer.event, sizeof(XEvent));
		    return True;
	    }
	    x++;
	    current = current->next;
    }
    return False;
}

bool XIsPhantomWindow(Window toCheck){

    return XGetWindowInfoStruct(toCheck) == NULL;
}

int XGetWindowProperty(
Display*		/* display */,
Window		/* w */,
Atom		/* property */,
long		/* long_offset */,
long		/* long_length */,
Bool		/* delete */,
Atom		/* req_type */,
Atom*		/* actual_type_return */,
int*		/* actual_format_return */,
unsigned long*	/* nitems_return */,
unsigned long*	/* bytes_after_return */,
unsigned char**	/* prop_return */
    ){

    return 0;
}

int XFree(
void*		/* data */
    ){

    return 0;
}

int XSetSelectionOwner(
Display*		display/* display */,
Atom	        selection/* selection */,
Window		owner/* owner */,
Time		time/* time */
    ){

    ATOM_INFO* atomInfo;
    XID selectID;

    if(atomInfo = XGetAtom(selection)){
	    if(atomInfo->time < time)
		    return 0;

	    if(atomInfo->owner != None && atomInfo->owner != owner)
		    XSendSelectionClearEvent(display, owner, selection, time);
	    
	    atomInfo->owner = owner;
	    atomInfo->time = time;
    }
    else
	    return 0;
}

Status XSendSelectionClearEvent(Display *display, Window window, Atom selection, Time time){

    XEvent toSend;
    
    toSend.type = SelectionClear;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = SelectionClear;
    toSend.xany.window = window;
    toSend.xselectionclear.display = display;
    toSend.xselectionclear.selection = selection;
    toSend.xselectionclear.send_event = False;
    toSend.xselectionclear.time = GetTickCount();
    toSend.xselectionclear.type = SelectionClear;
    toSend.xselectionclear.window = window;

    return XSendInternalEvent(display, window, False, PropertyChangeMask, &toSend);

}

Status XSendSelectionNotifyEvent(Display *display, Window requestor, Atom selection, 
							     Atom target, Atom property, Time time){

    XEvent toSend;
    
    toSend.type = SelectionNotify;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = SelectionNotify;
    toSend.xany.window = requestor;
    toSend.xselection.display = display;
    toSend.xselection.property = property;
    toSend.xselection.requestor = requestor;
    toSend.xselection.selection = selection;
    toSend.xselection.target = target;
    toSend.xselection.type = SelectionNotify;

    return XSendInternalEvent(display, requestor, False, PropertyChangeMask, &toSend);

}

Status XSendSelectionRequestEvent(Display *display, Window owner, Window requestor, Atom selection, 
							     Atom target, Atom property, Time time){

    XEvent toSend;
    
    toSend.type = SelectionRequest;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = SelectionRequest;
    toSend.xany.window = requestor;
    toSend.xselectionrequest.owner = owner;
    toSend.xselectionrequest.property = property;
    toSend.xselectionrequest.requestor = requestor;
    toSend.xselectionrequest.selection = selection;
    toSend.xselectionrequest.target = target;
    toSend.xselectionrequest.type = SelectionRequest;

    return XSendInternalEvent(display, owner, False, PropertyChangeMask, &toSend);

}

int XFlush(
Display*		/* display */
    ){

    return 0;
}

Pixmap XCreatePixmap(
Display*		display/* display */,
Drawable		d/* d */,
unsigned int	width/* width */,
unsigned int	height/* height */,
unsigned int	depth/* depth */		        
    ){

    PIXMAP_INFO *pixInfo;
    XID pixID;
    HBITMAP bitmap;
    HDC newDC, oldDC;
    int bitsPerPixel;
    WINDOW_INFO *theWindow;

    if(!(theWindow = XGetWindowInfoStruct(d)))
	    return False;


    oldDC = XGetDC(d);
    newDC = CreateCompatibleDC(oldDC);

    if(!newDC){
	    XReleaseDC(d, oldDC);
	    return False;
    }

    if(!(bitmap = CreateCompatibleBitmap(oldDC, width, height))){
	    DeleteDC(newDC);
	    XReleaseDC(d, oldDC);
	    return False;
    }
    
    bitsPerPixel = GetDeviceCaps(oldDC, BITSPIXEL);
    XReleaseDC(d, oldDC);

    if(!(pixID = XCreateInfoStruct(TYPE_PIXMAP_INFO))){
	    DeleteDC(newDC);
	    XDeleteObject(bitmap);
	    XReleaseDC(d, oldDC);
	    return False;
    }

    pixInfo = (PIXMAP_INFO*)XGetInfoStruct(pixID);

    pixInfo->bitmap = bitmap;
    pixInfo->depth = depth;
    pixInfo->height = height;
    pixInfo->width = width;
    pixInfo->theDC = newDC;
    pixInfo->bitsPerPixel = bitsPerPixel;
    
    if(!SelectObject(newDC, bitmap)){
	    DeleteDC(newDC);
	    XDeleteObject(bitmap);
	    XReleaseDC(d, oldDC);
	    XFreeInfoStruct(pixID);
	    return False;
    }

    if(theWindow->theColormap){
	    pixInfo->colorMap = theWindow->theColormap;
//		if(!SelectPalette(newDC, theWindow->theColormap->palette, TRUE)
//			|| (RealizePalette(newDC) == GDI_ERROR))
//			return False;
    }

    return pixID;
}

int XGetImageSize(char *fileName, int *x, int *y, int *w, int *h){

    ConvertPathing(fileName);
    HDC theDc;
    SIZE size;
    FREE_IMAGE_FORMAT fif;
    FIBITMAP *freeBitmap;
    HBITMAP temp;


    char *expandedName;

    expandedName = ExpandEnvString(fileName);

    *x = *y = *w = *h = 0;
    
    try{
	    fif = FreeImage_GetFIFFromFilename(expandedName);

	    if( fif == FIF_UNKNOWN ){
		    throw -1;
	    }

	    freeBitmap = FreeImage_Load(fif, expandedName);

	    if(!freeBitmap){
		    throw -1;
	    }
    
    
	    *w = (int) FreeImage_GetWidth(freeBitmap);
	    *h = (int) FreeImage_GetHeight(freeBitmap);

	    FreeImage_Unload(freeBitmap);
    }
    catch(...){

	    CxImage theImage(expandedName, CXIMAGE_FORMAT_UNKNOWN);

	    if(!theImage.IsValid())
		    return 0;

	    *w = theImage.GetWidth();
	    *h = theImage.GetHeight();
    }

    return 1;

}

int XLoadImage(char *fileName, Pixmap loadInto){

    PIXMAP_INFO *info;
    ConvertPathing(fileName);
    HDC theDc;
    SIZE size;
    FREE_IMAGE_FORMAT fif;
    FIBITMAP *freeBitmap;
    HBITMAP temp;
    double dpiX;
    double dpiY;
    double width;
    double height;
    double sizeX;
double sizeY;
    BYTE *pData;


    char *expandedName;

    if(!(info = (PIXMAP_INFO*)XGetInfoStruct(loadInto))){
	    return 0;
    }

    expandedName = ExpandEnvString(fileName);

    freeBitmap = NULL;

    try{
    
	    fif = FreeImage_GetFIFFromFilename(expandedName);
	    
	    if( fif == FIF_UNKNOWN ){
		    throw -1;
	    }

	    freeBitmap = FreeImage_Load(fif, expandedName);

	    if(!freeBitmap){
		    throw -1;
	    }
    
    
	    dpiX   = FreeImage_GetDotsPerMeterX(freeBitmap);
    dpiY   = FreeImage_GetDotsPerMeterY(freeBitmap);
    width  = FreeImage_GetWidth(freeBitmap);
    height = FreeImage_GetHeight(freeBitmap);

	    pData    = FreeImage_GetBits(freeBitmap);

	    if( dpiX==0.0 )    dpiX = 72.0 * 100.0 / 2.54;
	    if( dpiY==0.0 )    dpiY = 72.0 * 100.0 / 2.54;

	    sizeX = 1.0 * 100.0 * 1000.0 * width  / dpiX;
	    sizeY = 1.0 * 100.0 * 1000.0 * height / dpiY;

	    // stretched 
	    StretchDIBits(info->theDC,
		    // x-y-coord of destination upper-left corner
		    0, 0,                       
		    // width-height of destination rectangle
		    info->width, info->height, 
		    // x-y-coord of source upper-left corner
		    0, 0,                                
		    // width-height of source rectangle
		    width, height, 
		    pData,                               // bitmap bits
		    FreeImage_GetInfo(freeBitmap),    // bitmap data
		    DIB_RGB_COLORS,                      // usage options
		    SRCCOPY                              // raster operation code
    );
    
	    FreeImage_Unload(freeBitmap);
    }
catch(...){

    CxImage theImage(expandedName, CXIMAGE_FORMAT_UNKNOWN);

    if(!theImage.IsValid())
	    return 0;

    StretchDIBits(info->theDC,
		    // x-y-coord of destination upper-left corner
		    0, 0,                       
		    // width-height of destination rectangle
		    info->width, info->height, 
		    // x-y-coord of source upper-left corner
		    0, 0,                                
		    // width-height of source rectangle
		    theImage.GetWidth(), theImage.GetHeight(), 
		    theImage.GetBits(),                               // bitmap bits
		    (BITMAPINFO*)theImage.GetDIB(),    // bitmap data
		    DIB_RGB_COLORS,                      // usage options
		    SRCCOPY                              // raster operation code
   );
    //newBitmap = theImage.MakeBitmap(info->theDC);
    //newDc = CreateCompatibleDC(newDc);


    //BitBlt(
    //info->bitmapm_picture.SetBitmap(m_bitmap);
}
    return 1;

}

int XFreePixmap(
Display*		display/* display */,
Pixmap		pixmap/* pixmap */
    ){

    PIXMAP_INFO *info;

    if(!(info = (PIXMAP_INFO*)XGetInfoStruct(pixmap))){
	    return False;
    }

    DeleteDC(info->theDC);
    XDeleteObject(info->bitmap);

    XFreeInfoStruct(pixmap);
    return True;
}

Region XCreateRegion(){

    REGION_INFO *theRegion;
    Region region;

    if(!(region = XCreateInfoStruct(TYPE_REGION_INFO)))
	    return False;

//	if(!(theRegion = (REGION_INFO*)XGetInfoStruct(region)))
//		return False;

//	if(!(theRegion->region = CreateRectRgn(0, 0, 0, 0))
//		return False;

    return region;
}

int XUnionRectWithRegion(
XRectangle*		rectangle/* rectangle */,
Region		src_region/* src_region */,
Region		dest_region_return/* dest_region_return */
    ){

    REGION_INFO *srcRegion, *destRegion;
    RECT theRect;
    HRGN tempRegion;
    HRGN newRegion;

//	rectangle->y += TITLE_BAR_WIDTH;
    theRect.bottom = rectangle->height + rectangle->y;
    theRect.top = rectangle->y;
    theRect.right = rectangle->width + rectangle->x;
    theRect.left = rectangle->x;

    if(!(srcRegion = (REGION_INFO*)XGetInfoStruct(src_region)))
	    return False;

    if(!(destRegion = (REGION_INFO*)XGetInfoStruct(dest_region_return)))
	    return False;

    if(destRegion->region){
	    XDeleteObject(destRegion->region);
	    destRegion->region = NULL;
    }
    if(!(tempRegion = CreateRectRgnIndirect(&theRect)))
	    return False;

    if(srcRegion->region == NULL){
	    destRegion->region = tempRegion;
	    return True;
    }

    if(!(newRegion = CreateRectRgn(0, 0, 0 ,0))){
    //if(!(newRegion = CreateRectRgnIndirect(&theRect)))
	    XDeleteObject(tempRegion);
	    return False;
    }

    if(CombineRgn(newRegion, srcRegion->region, tempRegion, RGN_OR) == ERROR){
	    XDeleteObject(tempRegion);
	    return False;
    }

    destRegion->region = newRegion;
    XDeleteObject(tempRegion);

    return True;
}

bool XDeleteObject(HANDLE toDelete){

    if(!DeleteObject(toDelete)){
	    DWORD error = GetLastError();
	    return false;
    }

    return true;
}

int XSetRegion(
Display*		display/* display */,
GC			gc/* gc */,
Region		r/* r */
    ){

    GC_INFO *gcInfo;
    HDC theDC;
    REGION_INFO *regionInfo;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    if(!(regionInfo = (REGION_INFO*)XGetInfoStruct(r)))
	    return False;

//	if(!(theDC = XGetDC(gcInfo->d)))
//		return False;
    regionInfo->theGC = gcInfo;
/*	if(SelectClipRgn(theDC, regionInfo->region) == ERROR){
	    XReleaseDC(gcInfo->d, theDC);
	    return False;
    }

    switch(GetClipRgn(theDC, gcInfo->clippingRgn)){
	    case -1:return False;
	    case 0: return False;
	    case 1: break;
    }*/

    CombineRgn(gcInfo->clippingRgn, regionInfo->region, NULL, RGN_COPY);
    
//	XReleaseDC(gcInfo->d, theDC);

    return True;
}

int XDestroyRegion(
Region		r/* r */
    ){

    REGION_INFO *regionInfo;
    GC_INFO *gcInfo;
    HDC theDC;

    if(!(regionInfo = (REGION_INFO*)XGetInfoStruct(r)))
	    return False;
/*
    //if(regionInfo->theGC){
	    if(!(theDC = XGetDC(regionInfo->theGC->d)))
		    return False;

//		SelectClipRgn(theDC, NULL);
	    XReleaseDC(regionInfo->theGC->d, theDC);
//	}*/

    XDeleteObject(regionInfo->region);
    XFreeInfoStruct(r);

    return True;
}

int XSetForeground(
Display*		display/* display */,
GC			gc/* gc */,
unsigned long	foreground/* foreground */
    ){

    GC_INFO *gcInfo;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    gcInfo->foregroundPixel = foreground;

    return True;
}

int XSetBackground(
Display*		display/* display */,
GC			gc/* gc */,
unsigned long	background/* background */
    ){

    GC_INFO *gcInfo;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    gcInfo->backgroundPixel = background;

    return True;
}

int XSetLineAttributes(
Display*		display/* display */,
GC			gc/* gc */,
unsigned int	line_width/* line_width */,
int			line_style/* line_style */,
int			cap_style/* cap_style */,
int			join_style/* join_style */
    ){

    GC_INFO *gcInfo;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    gcInfo->penStyle = 0;
    gcInfo->penStyle |= (line_style & LineSolid) ? PS_SOLID : 0;
    gcInfo->penStyle |= (line_style & LineOnOffDash) ? PS_DASH : 0;
    gcInfo->penStyle |= (line_style & LineDoubleDash) ? PS_DASHDOTDOT : 0;

    gcInfo->penStyle |= (cap_style & CapNotLast) ? PS_ENDCAP_FLAT : 0;
    gcInfo->penStyle |= (cap_style & CapButt) ? PS_ENDCAP_SQUARE : 0;
    gcInfo->penStyle |= (cap_style & CapRound) ? PS_ENDCAP_ROUND : 0;
    
    gcInfo->penStyle |= (join_style & JoinMiter) ? PS_JOIN_MITER : 0;
    gcInfo->penStyle |= (join_style & JoinRound) ? PS_JOIN_ROUND : 0;
    gcInfo->penStyle |= (join_style & JoinBevel) ? PS_JOIN_BEVEL : 0;

    return XUpdatePen(gcInfo);
}

int XUpdatePen(GC_INFO *toUpdate){

    LOGBRUSH logBrush;

    logBrush.lbColor = toUpdate->foregroundPixel;
    logBrush.lbHatch = 0;
    logBrush.lbStyle = BS_SOLID;

    if(toUpdate->pen){
	    XDeleteObject(toUpdate->pen);
	    toUpdate->pen = NULL;
    }

    if(!(toUpdate->pen = ExtCreatePen(PS_GEOMETRIC | toUpdate->penStyle, toUpdate->lineWidth,  &logBrush, 0, 0)))
	    return False;

    return True;
}

int XDrawLine(
Display*		display/* display */,
Drawable		d/* d */,
GC			gc/* gc */,
int			x1/* x1 */,
int			y1/* y1 */,
int			x2/* x2 */,
int			y2/* y2 */
    ){

    GC_INFO *gcInfo;
    HDC theDC;

//	y1 += TITLE_BAR_WIDTH;
//	y2 += TITLE_BAR_WIDTH;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    theDC = XGetDC(d);

    XUpdatePen(gcInfo);

    if(gcInfo->pen && !SelectObject(theDC, gcInfo->pen)){
	    XReleaseDC(d, theDC);
	    return False;
    }
    
    SelectClipRgn(theDC, gcInfo->clippingRgn);

    if(!MoveToEx(theDC, x1, y1, NULL)){
	    XReleaseDC(d, theDC);
	    return False;
    }

    if(!LineTo(theDC, x2, y2)){
	    XReleaseDC(d, theDC);
	    return False;
    }

    XReleaseDC(d, theDC);
    return True;
}

int XFillRectangle(
Display*		display/* display */,
Drawable		d/* d */,
GC			gc/* gc */,
int			x/* x */,
int			y/* y */,
unsigned int	width/* width */,
unsigned int	height/* height */
    ){

    GC_INFO *gcInfo;
    HBRUSH brush;
    HDC theDC;
    RECT theRect;

//	y += TITLE_BAR_WIDTH;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    theDC = XGetDC(d);
    
    XUpdatePen(gcInfo);

    if(!(brush = CreateSolidBrush((gcInfo->penStyle & LineDoubleDash) ? 
	    gcInfo->backgroundPixel : gcInfo->foregroundPixel))){
		    XReleaseDC(d, theDC);
		    return False;
    }

    //if(!SelectObject(theDC, brush))
    //	return False;

    theRect.top = y;
    theRect.left = x;
    theRect.bottom = height + y;
    theRect.right = width + x;

    SelectClipRgn(theDC, gcInfo->clippingRgn);

    if(!FillRect(theDC, &theRect, brush)){
	    XReleaseDC(d, theDC);
	    return False;
    }

    XReleaseDC(d, theDC);
    XDeleteObject(brush);
    return True;
}

int XDrawArc(
Display*		/* display */,
Drawable		/* d */,
GC			/* gc */,
int			/* x */,
int			/* y */,
unsigned int	/* width */,
unsigned int	/* height */,
int			/* angle1 */,
int			/* angle2 */
    ){

    return 0;
}

int XFillPolygon(
Display*		display/* display */,
Drawable		d/* d */,
GC			gc/* gc */,
XPoint*		points/* points */,
int			npoints/* npoints */,
int			shape/* shape */,
int			mode/* mode */
    ){

    GC_INFO *gcInfo;
    HBRUSH brush;
    HDC theDC;
    POINTS *gdiPoints;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    theDC = XGetDC(d);
    
    XUpdatePen(gcInfo);

    if(!(brush = CreateSolidBrush((gcInfo->penStyle & LineDoubleDash) ? 
	    gcInfo->backgroundPixel : gcInfo->foregroundPixel))){
		    XReleaseDC(d, theDC);
		    return False;
    }

    if(!SelectObject(theDC, brush)){
	    XDeleteObject(brush);
	    XReleaseDC(d, theDC);
	    return False;
    }

/*	gdiPoints = malloc(sizeof(POINT) * npoints);

    for(int i = 0; i < npoints; i++){
	    gdiPoints[i].x = points[i].x;
	    gdiP*/

    SelectClipRgn(theDC, gcInfo->clippingRgn);

/*	printf("being polygon\n");
    if(!Polygon(theDC, (POINT*)points, npoints)){
	    XDeleteObject(brush);
	    XReleaseDC(d, theDC);
	    return False;
    }
    
    printf("end polygon\n");
*/	XReleaseDC(d, theDC);
    XDeleteObject(brush);
    return True;
}

Status XLookupColor(
Display*		display/* display */,
Colormap		colormap/* colormap */,
_Xconst char*	color_name/* color_name */,
XColor*		exact_def_return/* exact_def_return */,
XColor*		screen_def_return/* screen_def_return */
    ){

    unsigned int value;
    COLORREF exact, screen;
    COLORMAP_INFO *info;

    if(!(info = (COLORMAP_INFO*)XGetInfoStruct(colormap)))
	    return False;

    value = XGetColorValue(color_name);

//	if((value & 0x000000FF) == 0x000000FF)
//		printf("blue\n");
//	if(value == 0)
//		printf("black\n");

    exact = RGB((value & 0x00FF0000) >> 16, (value & 0x0000FF00) >> 8, value & 0x000000FF);
    screen = GetNearestColor(info->theDC, exact);

    exact_def_return->blue = XApproximateByteToShort((exact & 0x00FF0000) >> 16);
    exact_def_return->flags = DoRed | DoGreen | DoBlue;
    exact_def_return->green = XApproximateByteToShort((exact & 0x0000FF00) >> 8);
    exact_def_return->pixel = exact;
    exact_def_return->red = XApproximateByteToShort(exact & 0x000000FF);

    screen_def_return->blue = XApproximateByteToShort((screen & 0x00FF0000) >> 16);
    screen_def_return->flags = DoRed | DoGreen | DoBlue;
    screen_def_return->green = XApproximateByteToShort((screen & 0x0000FF00) >> 8);
    screen_def_return->pixel = screen;
    screen_def_return->red = XApproximateByteToShort(exact & 0x000000FF);

    return True;
}

BYTE XApproximateShortToByte(USHORT toApprox){

    double ratio;
    BYTE approximation;

    ratio = (double)toApprox / 65536.0;

    if(ratio == 1.0)
	    return 0xFF;

    approximation = (BYTE)((ratio * 255.0) + 0.5);

    return approximation;
}

USHORT XApproximateByteToShort(BYTE toApprox){

    double ratio;
    USHORT approximation;

    ratio = (double)toApprox / 255.0;

    if(ratio == 1.0)
	    return 0xFFFF;

    approximation = (USHORT)((ratio * 65536.0) + 0.5);
    
    return approximation;
}

BYTE XApproximateByte12ToByte(USHORT toApprox){

    double ratio;
    BYTE approximation;

    ratio = (double)toApprox / 4096.0;

    if(ratio == 1.0)
	    return 0xFF;

    approximation = (BYTE)((ratio * 255.0) + 0.5);

    return approximation;
}

BYTE XApproximateByte4ToByte(BYTE toApprox){

    double ratio;
    BYTE approximation;

    ratio = (double)toApprox / 16.0;

    if(ratio == 1.0)
	    return 0xFF;

    approximation = (BYTE)((ratio * 255.0) + 0.5);

    return approximation;
}

Status XAllocColor(
Display*		display/* display */,
Colormap		colormap/* colormap */,
XColor*		screen_in_out/* screen_in_out */
    ){

    COLORMAP_INFO *cmapInfo;
    PALETTEENTRY newEntry, testEntry;
    COLORREF pixel, pixel2;
    UINT palIndex;

    if(!(cmapInfo = (COLORMAP_INFO*)XGetInfoStruct(colormap)))
	    return 0;

    newEntry.peFlags = NULL;
    newEntry.peBlue = XApproximateShortToByte(screen_in_out->blue);
    newEntry.peGreen = XApproximateShortToByte(screen_in_out->green);
    newEntry.peRed = XApproximateShortToByte(screen_in_out->red);

    pixel = RGB(newEntry.peRed, newEntry.peGreen, newEntry.peBlue);

    palIndex = GetNearestPaletteIndex(cmapInfo->palette, pixel);

    GetPaletteEntries(cmapInfo->palette, palIndex, 1, &testEntry);

    pixel2 = RGB(testEntry.peRed, testEntry.peGreen, testEntry.peBlue);

    if(pixel == pixel2){
	    screen_in_out->pixel = pixel;
	    return 1;
    }

    if(ResizePalette(cmapInfo->palette, cmapInfo->numEntries + 1) == GDI_ERROR)
	    return 0;

    if(!SetPaletteEntries(cmapInfo->palette, cmapInfo->numEntries, 1, &newEntry)){
	    ResizePalette(cmapInfo->palette, cmapInfo->numEntries);
	    return 0;
    }
    cmapInfo->numEntries++;

    UnrealizeObject(cmapInfo->palette);
    RealizePalette(cmapInfo->theDC);

    pixel = RGB(newEntry.peRed, newEntry.peGreen, newEntry.peBlue);

    palIndex = GetNearestPaletteIndex(cmapInfo->palette, pixel);
    GetPaletteEntries(cmapInfo->palette, palIndex, 1, &testEntry);

    pixel = RGB(testEntry.peRed, testEntry.peGreen, testEntry.peBlue);
    
    screen_in_out->red = XApproximateByteToShort((BYTE)(pixel & 0x000000FF));
    screen_in_out->green = XApproximateByteToShort((BYTE)((pixel & 0x0000FF00) >> 8));
    screen_in_out->blue = XApproximateByteToShort((BYTE)((pixel & 0x00FF0000) >> 16));
    screen_in_out->pixel = pixel;

    return 1;
}

int XDrawPoint(
Display*		display/* display */,
Drawable		d/* d */,
GC			gc/* gc */,
int			x/* x */,
int			y/* y */
    ){

    HDC theDC;
    INFO_BASE *base;
    PIXMAP_INFO *pixInfo;
    GC_INFO *gcInfo;
    COLORREF retVal;
    PALETTEENTRY palEntry;
    int index;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    if(!XIsValidHandle(d, TYPE_PIXMAP_INFO)){
	    theDC = XGetDC(d);

	    retVal = SetPixel(theDC, x, y, gcInfo->foregroundPixel);
	    XReleaseDC(d, theDC);
    
	    return retVal != -1;
    }
    else{
	    if(!(base = (INFO_BASE*)XGetInfoStruct(d)))
		    return False;

	    if(base->type != TYPE_PIXMAP_INFO)
		    return False;

	    pixInfo = (PIXMAP_INFO*)base;
	    index = GetNearestPaletteIndex(pixInfo->colorMap->palette, gcInfo->foregroundPixel);
	    
	    if(!GetPaletteEntries(pixInfo->colorMap->palette, index, 1, &palEntry))
		    return false;

	    retVal = RGB(palEntry.peRed, palEntry.peGreen, palEntry.peBlue);
	    retVal = GetNearestColor(pixInfo->theDC, retVal);
	    retVal = SetPixel(pixInfo->theDC, x, y, retVal);
	    
	    return retVal != -1;
    }

    return 0;
}

Pixmap XCreateBitmapFromData(
Display*		display/* display */,
Drawable		d/* d */,
_Xconst char*	data/* data */,
unsigned int	width/* width */,
unsigned int	height/* height */
    ){

    
   PIXMAP_INFO *pixInfo;
    XID pixID;
    HBITMAP bitmap;

    if(!(bitmap = CreateBitmap(width, height, 1, 1, data)))
	    return False;

    if(!(pixID = XCreateInfoStruct(TYPE_PIXMAP_INFO))){
	    return False;
    }

    pixInfo = (PIXMAP_INFO*)XGetInfoStruct(pixID);

    if(pixInfo->bitmap){
	    DeleteObject(pixInfo->bitmap);
	    pixInfo->bitmap = NULL;
    }

    if(pixInfo->theDC){
	    DeleteDC(pixInfo->theDC);
	    pixInfo->theDC = NULL;
    }

    pixInfo->bitmap = bitmap;
    pixInfo->depth = 1;
    pixInfo->height = height;
    pixInfo->width = width;
    pixInfo->theDC = NULL;
    pixInfo->bitmap = bitmap;
    pixInfo->bitsPerPixel = 1;

    return pixID;

}

int XChangeProperty(
Display*		/* display */,
Window		/* w */,
Atom		/* property */,
Atom		/* type */,
int			/* format */,
int			/* mode */,
_Xconst unsigned char*	/* data */,
int			/* nelements */
    ){

    return 0;
}

int XSync(
Display*		/* display */,
Bool		/* discard */
    ){

    return 0;
}

int XCopyArea(
Display*		display/* display */,
Drawable		src/* src */,
Drawable		dest/* dest */,
GC			gc/* gc */,
int			src_x/* src_x */,
int			src_y/* src_y */,
unsigned int	width/* width */,
unsigned int	height/* height */,
int			dest_x/* dest_x */,
int			dest_y/* dest_y */
    ){

    HDC srcDC, destDC;
    WINDOW_INFO *srcInfo, *destInfo;
    PIXMAP_INFO *srcPixInfo, *destPixInfo;
    REGION_INFO *regionInfo;
    GC_INFO *gcInfo;
    PIXMAP_INFO *pixInfo;
    BOOL releaseSrc, releaseDest;
    DWORD error;

    releaseSrc = releaseDest = TRUE;

#if(DEBUGXCOPYAREA)
    printf("XCopyArea-----src x=%d, src y=%d, dest x=%d, desty=%d, width=%d, height=%d\n",
		    src_x, src_y, dest_x, dest_y, width, height);
#endif

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    if(!(srcInfo = XGetWindowInfoStruct(src))){

	    if(!(srcPixInfo = (PIXMAP_INFO*)XGetInfoStruct(src)))
		    return False;
	    
	    srcDC = srcPixInfo->theDC;
	    releaseSrc = FALSE;
    }
    else{
	    if(!(srcDC = XGetDC(src)))
		    return False;
    }

    if(!(destInfo = XGetWindowInfoStruct(dest))){

	    if(!(destPixInfo = (PIXMAP_INFO*)XGetInfoStruct(dest))){
		    if(releaseSrc)
			    XReleaseDC(src, srcDC);
		    return False;
	    }

	    destDC = destPixInfo->theDC;
	    releaseDest = FALSE;
    }
    else{
	    if(!(destDC = XGetDC(dest))){
		    if(releaseSrc)
			    XReleaseDC(src, srcDC);
		    return False;
	    }
    }

    /*HRGN saved = CreateRectRgn(0, 0, 0, 0);
    HRGN saved2 = CreateRectRgn(0, 0, 0, 0);
    
    switch(GetClipRgn(destDC, saved)){
    
    case 1: 
	    CombineRgn(saved2, saved, gcInfo->clippingRgn, RGN_OR);
	    SelectClipRgn(destDC, saved2);
    case -1:
    case 0:
	    DeleteObject(saved);
	    DeleteObject(saved2);
    }*/
//	RECT temp;
//	GetRgnBox(gcInfo->clippingRgn, &temp);

 /*  if(gcInfo->clipMask){
     if(!(pixInfo = (PIXMAP_INFO*)XGetInfoStruct(gcInfo->clipMask))){
      return False;
    }

    if(!MaskBlt(destDC, dest_x, dest_y, width, height, srcDC, src_x, src_y, 
	  pixInfo->bitmap, 0, 0, MAKEROP4(PATCOPY, SRCCOPY))){
      error = GetLastError();
      printf("Error %d\n", error);
    }
    //  XCopyAreaWithClipMask(gcInfo, srcDC, destDC, src_x, src_y, width, height, dest_x, dest_y);
   }
    else{
*/ //     SelectClipRgn(destDC, gcInfo->clippingRgn);
	SelectClipRgn(destDC, gcInfo->clippingRgn);

      if(!BitBlt(destDC, dest_x, dest_y, width, height, srcDC, src_x, src_y, SRCCOPY)){
	    if(releaseSrc)
		    XReleaseDC(src, srcDC);
    
	    if(releaseDest)
		    XReleaseDC(dest, destDC);
	    return False;
      }
//    }

    if(releaseSrc)
	    XReleaseDC(src, srcDC);
    
    if(releaseDest)
	    XReleaseDC(dest, destDC);

    //XSendExposeEvent(display, src, src_x, src_y, width, height); 
    //if(gcInfo->graphics_exposure && !XGenerateGraphicsExposureEvents(display, dest, gc, dest_x, dest_y, width, height))
    //	XSendNoExposeEvent(display, dest, gc, False);
    //XSendGraphicsExposureEvent(display, dest, gc, dest_x, dest_y, height, width, False);

    return True;
}

Bool XCopyAreaWithClipMask(GC_INFO *gcInfo, HDC srcDC, HDC destDC, int srcX, int srcY,
			   int width, int height, int dest_x, int dest_y){

/*  HBITMAP hDestBitmap;
  BITMAP destBitmap;
  BITMAPINFO bitmapInfo;
  int x, y, x1, y1;
  int currentPixel;
  int pixelIndex;
  int pixelShift;
  UINT pixelVal;
  UINT pixelMask;
  PIXMAP_INFO *pixInfo;
  UINT *data;
  DWORD error;

  if(!(pixInfo = (PIXMAP_INFO*)XGetInfoStruct(gcInfo->clipMask))){
    return False;
  }

  if(!MaskBlt(destDC, dest_x, dest_y, width, height, srcDC, srcX, srcY, 
	  pixInfo->bitmap, gcInfo->maskX, gcInfo->maskY, MAKEROP4(SRCCOPY, SRCCOPY))){
    error = GetLastError();
    printf("Error %d\n", error);
  }

//  hDestBitmap = (HBITMAP)GetCurrentObject(pixInfo->theDC, OBJ_BITMAP);

 // if(hDestBitmap == NULL)
   // return False;

//  GetObject(hDestBitmap, sizeof(BITMAP), &destBitmap);

//  data = (UINT*)malloc(destBitmap.bmWidthBytes * destBitmap.bmHeight);
  //GetBitmapBits(hDestBitmap, destBitmap.bmWidthBytes * destBitmap.bmHeight, data);
//  GetDIBits(pixInfo->theDC, hDestBitmap, 0, destBitmap.bmHeight, data, &bitmapInfo, DIB_RGB_COLORS); 

//  pixelMask = 0;
//  for(int i = 0; i < destBitmap.bmBitsPixel; i++){
//    pixelMask = (pixelMask << 1) | 0x01;
//  }

/*  HPEN test;

/*  DeleteObject(gcInfo->clippingRgn);
  gcInfo->clippingRgn = CreateRectRgn(0, 0, width, height);
  SelectObject(destDC, gcInfo->clippingRgn);
  OffsetClipRgn(destDC, gcInfo->maskX, gcInfo->maskY);
  for(y = srcY; y < height; y++){
    for(x = srcX; x < width; x++){

     // x1 = x;
      //y1 = y;

      if((pixelVal = GetPixel(pixInfo->theDC, srcX + x, srcY + y)))
	SetPixel(destDC, dest_x + x, dest_y + y, GetPixel(srcDC, srcX + x, srcY + y));

 /*     while((pixelVal = GetPixel(pixInfo->theDC, x, y))
	&& (x <= (srcX + width))){
//	printf("*");
	x++;
      }
      
      if(x > (srcX + width)){
//	printf("\n");
      }
      else{
//	printf(" ");
      }
      //currentPixel = (width * x) + y;
      //pixelIndex = currentPixel * destBitmap.bmBitsPixel;
      //pixelIndex = pixelIndex / 32;
      //pixelShift = (pixelIndex % 32) * destBitmap.bmBitsPixel;
      //pixelVal = data[pixelIndex];
      //pixelVal = (pixelVal >> pixelShift) & pixelMask;

      if(x != x1){
	test = CreatePen(PS_SOLID, 1, GetPixel(srcDC, x1, y1));
	SelectObject(destDC, test);
	MoveToEx(destDC, dest_x + x1, y1 + dest_y, NULL);
	LineTo(destDC, dest_x + x, y + dest_y);
	DeleteObject(test);
      }
      */
 //   }
 // }*/

  return False;
}

Status XSendNoExposeEvent(Display *display, Drawable source, GC eventGC, Bool doCheck){

    XEvent toSend;
    GC_INFO *gcInfo;
    
    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(eventGC)))
	    return False;

    if(doCheck && gcInfo->graphics_exposure)
	    return False;

    toSend.type = NoExpose;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = NoExpose;
    toSend.xany.window = source;
    toSend.xnoexpose.display = display;
    toSend.xnoexpose.drawable = source;
    toSend.xnoexpose.major_code = 1;
    toSend.xnoexpose.send_event = False;
    toSend.xnoexpose.type = NoExpose;

    return XSendInternalEvent(display, source, False, ExposureMask, &toSend);
}

int XGenerateGraphicsExposureEvents(Display *display, Drawable source, GC gc, int src_x, int src_y, 
								     int width, int height){

    map<XID, void*>::iterator iter;
    REGION_INFO *regionInfo;
    GC_INFO *gcInfo;
    void *key;
    RECT rect, temp;
    HRGN region;
    int numEventsSent = 0;

    rect.left = src_x;
    rect.right = src_x + width;
    rect.top = src_y;
    rect.bottom = src_y + height;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return 0;

    if(!gcInfo->graphics_exposure)
	    return 0;

    EnterCriticalSection(&handleMapLock);

    for(iter = handleMap.begin(); iter != handleMap.end(); iter++){

	    regionInfo = (REGION_INFO*)iter->second;

	    if(regionInfo->base.type == TYPE_REGION_INFO)
		    region = regionInfo->region;
	    else if(regionInfo->base.handle == gc){
		    gcInfo = (GC_INFO*)regionInfo;
		    
		    if(gcInfo->clippingRgn)
			    region = gcInfo->clippingRgn;
		    else
			    continue;
	    }
	    else
		    continue;

	    if(RectInRegion(region, &rect)){
		    GetRgnBox(region, &temp);
		    XSendGraphicsExposureEvent(display, source, gc, temp.left, temp.top, temp.right - temp.left, 
									    temp.bottom - temp.top, False);
		    numEventsSent++;
	    }
    }
    LeaveCriticalSection(&handleMapLock);
    return numEventsSent;

}

Status XSendGraphicsExposureEvent(Display *display, Drawable source, GC eventGC, int x, int y, 
							      int width, int height, Bool doCheck){

    XEvent toSend;
    GC_INFO *gcInfo;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(eventGC)))
	    return False;

    if(doCheck && !gcInfo->graphics_exposure)
	    return False;

    toSend.type = GraphicsExpose;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = GraphicsExpose;
    toSend.xany.window = source;
    toSend.xgraphicsexpose.count = 0;
    toSend.xgraphicsexpose.display = display;
    toSend.xgraphicsexpose.drawable = source;
    toSend.xgraphicsexpose.height = height;
    toSend.xgraphicsexpose.major_code = 1;
    toSend.xgraphicsexpose.send_event = False;
    toSend.xgraphicsexpose.type = GraphicsExpose;
    toSend.xgraphicsexpose.width = width;
    toSend.xgraphicsexpose.x = x;
    toSend.xgraphicsexpose.y = y;

    return XSendInternalEvent(display, source, False, ExposureMask, &toSend);
}

int XPending(
Display*		display/* display */
    ){

    DISPLAY_INFO *dispInfo;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)display)))
	    return 0;

    if(dispInfo->eventList.numEvents == 0){
	    XProcessMessageQueue(dispInfo);
    }

    return dispInfo->eventList.numEvents;
}

Bool XCheckMaskEvent(
Display*		display/* display */,
long		event_mask/* event_mask */,
XEvent*		event_return/* event_return */
    ){

    INTERNAL_XEVENT *current;
    INTERNAL_XEVENT buffer;
    DISPLAY_INFO *dispInfo;
    int x;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)display)))
	    return False;

//beginCheck:

    XProcessMessageQueue(dispInfo);
    current = dispInfo->eventList.eventListHead.next;

    x = 0;
    while(current != &dispInfo->eventList.eventListTail){

	    //if(event_mask == ButtonMotionMask|ButtonReleaseMask)
	    //	DebugBreak();

	    if(current->mask & event_mask){
		    XRemoveEventFromEventList(&dispInfo->eventList, &buffer, x);
		    memcpy(event_return, &buffer.event, sizeof(XEvent));
		    return True;
	    }
	    x++;
	    current = current->next;
    }
    return False;
}

int XPutBackEvent(
Display*		display/* display */,
XEvent*		event/* event */
    ){

    DISPLAY_INFO *dispInfo;
    INTERNAL_XEVENT newEvent;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)display)))
	    return False;

    newEvent.mask = event->type;
    memcpy(&newEvent.event, event, sizeof(XEvent));
    return XAddEventToEventList(&dispInfo->eventList, &newEvent, 0);
}

int XAddEventToEventList(XEVENT_LIST *theList, INTERNAL_XEVENT *toAdd, int position){

    INTERNAL_XEVENT *current, *free;

    if(theList->numEvents == 0)
	    position = 0;
    
    if(position < 0)
	    position = 0;

    if(position >= theList->numEvents)
	    position = theList->numEvents;

    free = theList->freeListHead.next;

    if(!free){
	    printf("EVENT LIST OVERFLOW!!!\n");
	    return -1;
    }

    free->previous->next = free->next;

    if(free->next){
	    free->next->previous = free->previous;
    }

    memcpy(free, toAdd, sizeof(INTERNAL_XEVENT));

    if(position <= (theList->numEvents / 2)){
	    current = &theList->eventListHead;

	    for(int i = 0; i < position; i++)
		    current = current->next;
    }
    else{
	    current = theList->eventListTail.previous;

	    for(int i = (theList->numEvents - 1); i > position; i--)
		    current = current->previous;
    }

    free->next = current->next;
    free->previous = current;
    current->next = free;
    free->next->previous = free;
    theList->numEvents++;

    return position;
}

Status XInitializeEventList(XEVENT_LIST *toInitialize){

    INTERNAL_XEVENT *current;

    memset(toInitialize, 0, sizeof(XEVENT_LIST));
    toInitialize->eventListHead.next = &toInitialize->eventListTail;
    toInitialize->eventListTail.previous = &toInitialize->eventListHead;

    for(int i = 0; i < EVENT_LIST_SIZE; i++){
    
	    if(i == 0){
		    toInitialize->freeListHead.next = &toInitialize->eventAlloc[i];
		    toInitialize->eventAlloc[i].previous = &toInitialize->freeListHead;
	    }
	    else{
		    toInitialize->eventAlloc[i - 1].next = &toInitialize->eventAlloc[i];
		    toInitialize->eventAlloc[i].previous = &toInitialize->eventAlloc[i - 1];
	    }
    }

    return True;
}

Status XRemoveEventFromEventList(XEVENT_LIST *theList, INTERNAL_XEVENT *buffer, int position){

    INTERNAL_XEVENT *current;

    if(theList->numEvents == 0)
	    return False;

    if(position < 0)
	    position = 0;

    if(position > (theList->numEvents - 1))
	    position = theList->numEvents - 1;

    if(position <= (theList->numEvents / 2)){
	    current = theList->eventListHead.next;

	    for(int i = 0; i < position; i++)
		    current = current->next;
    }
    else{
	    current = theList->eventListTail.previous;

	    for(int i = (theList->numEvents - 1); i > position; i--)
		    current = current->previous;
    }

    memcpy(buffer, current, sizeof(INTERNAL_XEVENT));
    current->previous->next = current->next;
    current->next->previous = current->previous;
    memset(current, 0, sizeof(INTERNAL_XEVENT));
    current->next = theList->freeListHead.next;
    current->previous = &theList->freeListHead;

    if(theList->freeListHead.next){
	    theList->freeListHead.next->previous = current;
    }

    theList->freeListHead.next = current;
    theList->numEvents--;
    return True;
}

int XSetClipMask(
Display*		display/* display */,
GC			gc/* gc */,
Pixmap		pixmap/* pixmap */
    ){

    PIXMAP_INFO *pixInfo;
    GC_INFO *gcInfo;
    UCHAR *bits;
    HRGN appendedRgn;
    UCHAR temp;
    int totalBits, x, y, arrayIndex, bitIndex, height, width;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;
    
    if(!(pixInfo = (PIXMAP_INFO*)XGetInfoStruct(pixmap))){
	    XDeleteObject(gcInfo->clippingRgn);
	    gcInfo->clippingRgn = CreateRectRgn(0, 0, 0, 0);
//		gcInfo->clippingRgn = CreatePolygonRgn(NULL, 0, ALTERNATE);
//		theDC = XGetDC(gcInfo->d);
//		SelectClipRgn(theDC, NULL);
//		XReleaseDC(gcInfo->d, theDC);

//		if(gcInfo->clippingRgn){
//			XDeleteObject(gcInfo->clippingRgn);
//			gcInfo->clippingRgn = NULL;
//		}

	    return True;
    }

    if(gcInfo->clippingRgn){
	    XDeleteObject(gcInfo->clippingRgn);
    }

    gcInfo->clippingRgn = CreateRectRgn(0, 0, 0, 0);

    bits = (UCHAR*)malloc(pixInfo->height * pixInfo->width);

    if(!GetBitmapBits(pixInfo->bitmap, pixInfo->height, bits)){
	    free(bits);
	    return False;
    }


    totalBits = pixInfo->height * pixInfo->width;

//	theDC = XGetDC(gcInfo->d);

    for(int i = 0; i < totalBits; i++){

	    arrayIndex = i / 8;
	    bitIndex = i % 8;

	    temp = bits[arrayIndex];

//		temp = (0x01 & (temp >> bitIndex));
	    temp = 1;

//		if((i / pixInfo->width) != height)
//			printf("\n/");

	    if(!temp){
//			printf(" ");
		    continue;
	    }

	    height = i / pixInfo->width;
	    width = i % pixInfo->width;

//		printf("*");
	    if(!(appendedRgn = CreateRectRgn(width, height, width+1, height+1))){
		    continue;
	    }

	    if(CombineRgn(gcInfo->clippingRgn, gcInfo->clippingRgn, appendedRgn, RGN_OR) == ERROR){
		    width = width;
	    }

	    XDeleteObject(appendedRgn);
    }

    free(bits);
    /*theDC = XGetDC(gcInfo->d);
    if(SelectClipRgn(theDC, existingRgn) == ERROR){
	    XReleaseDC(gcInfo->d, theDC);
	    return False;
    }

    if(gcInfo->clippingRgn){
	    DeleteObject(gcInfo->clippingRgn);
	    gcInfo->clippingRgn = NULL;
    }
    gcInfo->clippingRgn = existingRgn;
    XReleaseDC(gcInfo->d, theDC);
    */
    return True;
}

int XSetClipOrigin(
Display*		display/* display */,
GC			gc/* gc */,
int			clip_x_origin/* clip_x_origin */,
int			clip_y_origin/* clip_y_origin */
    ){

    GC_INFO *gcInfo;
    RECT theRect;
    int offX, offY;
    HDC theDC;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return False;

    if(!gcInfo->clippingRgn)
	    return False;

//	theDC = XGetDC(gcInfo->d);

    GetRgnBox(gcInfo->clippingRgn, &theRect);

//	if(!GetClipBox(theDC, &theRect)){
//		XReleaseDC(gcInfo->d, theDC);
//		return False;
//	}

    offX = clip_x_origin - theRect.left;
    offY = clip_y_origin - theRect.top;

    if(!OffsetRgn(gcInfo->clippingRgn, offX, offY)){
	    return False;
    }

/*	if(!OffsetClipRgn(theDC, offX, offY)){
	    XReleaseDC(gcInfo->d, theDC);
	    return False;
    }

    XReleaseDC(gcInfo->d, theDC);
*/
    return 1;
}


int XSetFont(
Display*		/* display */,
GC			/* gc */,
Font		/* font */
    ){

    return 0;
}

int XDrawString(
Display*		/* display */,
Drawable		/* d */,
GC			/* gc */,
int			/* x */,
int			/* y */,
_Xconst char*	/* string */,
int			/* length */
    ){

    return 0;
}

int XQueryTextExtents(
Display*		/* display */,
XID			/* font_ID */,
_Xconst char*	/* string */,
int			/* nchars */,
int*		/* direction_return */,
int*		/* font_ascent_return */,
int*		/* font_descent_return */,
XCharStruct*	/* overall_return */    
    ){

    return 0;
}

XImage *XGetImage(
Display*		/* display */,
Drawable		/* d */,
int			/* x */,
int			/* y */,
unsigned int	/* width */,
unsigned int	/* height */,
unsigned long	/* plane_mask */,
int			/* format */
    ){

    return 0;
}

Font XLoadFont(
Display*		/* display */,
_Xconst char*	/* name */
    ){

    return 0;
}

XFontStruct *XQueryFont(
Display*		/* display */,
XID			/* font_ID */
    ){

    return 0;
}

KeySym XKeycodeToKeysym(
Display*		/* display */,
KeyCode		/* keycode */,
int			/* index */
    ){

    return 0;
}

XModifierKeymap	*XGetModifierMapping(
Display*		/* display */
    ){

    return 0;
}

int XFreeModifiermap(
XModifierKeymap*	/* modmap */
    ){

    return 0;
}

Display *XOpenDisplay(
_Xconst char*	display_name){

    WNDCLASSEX wndClass;
    DISPLAY_INFO *dispInfo;
    GC_INFO *gcInfo;
    XID dispID, gcID;
    ATOM temp;
    HDC tempDC;

//    printf("Batch Limit is %d", GdiGetBatchLimit());
//	GdiSetBatchLimit(1);
    cursor = LoadCursor(NULL, IDC_ARROW);
    memset(&wndClass, 0, sizeof(WNDCLASSEX));
    wndClass.cbSize = sizeof(WNDCLASSEX);
    wndClass.style = CS_HREDRAW | CS_VREDRAW;
    wndClass.lpfnWndProc = XWindowProc;
    wndClass.hInstance = saveInstance;
    wndClass.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wndClass.hIconSm = LoadIcon(NULL, IDI_APPLICATION);
    wndClass.hCursor = cursor;
    wndClass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
//	wndClass.lpszClassName = (display_name) ? display_name : progName;
    wndClass.lpszClassName = progName;
    
    if(!(temp = RegisterClassEx(&wndClass)))
	    return 0;

    dispID = XCreateInfoStruct(TYPE_DISPLAY_INFO);
    gcID = XCreateInfoStruct(TYPE_GC_INFO);

    if(!dispID || !gcID)
	    return 0;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct(dispID)))
	    return 0;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gcID)))
	    return 0;

    dispInfo->wndClass = temp;
    tempDC = XGetDC((XID)rootWindow->window);
    memset(dispInfo->disp.screens, 0, sizeof(Screen));
//	gcInfo->handle = tempDC;
    
    dispInfo->disp.default_screen = 0;
    dispInfo->disp.screens[0].root = (Window)rootWindow->window;
    dispInfo->disp.screens[0].default_gc = gcID;
//	dispInfo->disp.screens[0].default_gc = (ULONG)CreateDC("DISPLAY", NULL, NULL, NULL);
    dispInfo->disp.screens[0].height = GetDeviceCaps(tempDC, VERTRES);
    dispInfo->disp.screens[0].width = GetDeviceCaps(tempDC, HORZRES);
    dispInfo->disp.screens[0].mheight = GetDeviceCaps(tempDC, HORZSIZE);
    dispInfo->disp.screens[0].mwidth = GetDeviceCaps(tempDC, VERTSIZE);
    dispInfo->disp.screens[0].root_depth = GetDeviceCaps(tempDC, BITSPIXEL);
    dispInfo->disp.screens[0].ndepths = GetDeviceCaps(tempDC, PLANES);	
    dispInfo->disp.screens[0].cmap = XCreateColormap((Display*)dispID, (Window)rootWindow->window, NULL, AllocAll);
//	dispInfo->wndClass = malloc(sizeof(WNDCLASS));
//	memcpy(dispInfo->wndClass, &wndClass, 
    XReleaseDC((Window)rootWindow->window, tempDC);

    XInitializeEventList(&dispInfo->eventList);
//	InitializeCriticalSection(&dispInfo->queueLock);
//	dispInfo->eventSignal = CreateEvent(NULL, FALSE, FALSE, NULL);
//	dispInfo->windowMap = new CMapPtrToPtr();

//	if(!XStartEventThread(dispInfo)){
//		XCloseDisplay((Display*)dispInfo->base.handle);
//		return 0;
//	}

    rootDisplay = (Display*)dispInfo->base.handle;
    return (Display*)dispInfo->base.handle;
}

Colormap XCreateColormap(Display *display, Window w, Visual *visual, int alloc){

    COLORMAP_INFO *cmapInfo;
    WINDOW_INFO *theWindow;
    XID cmapID;
    HDC theDC;
    PALETTEENTRY *palEntry;
    LOGPALETTE newPalette;
    COLORREF defaultColors[256];
    int numDefaultColors;

    cmapID = XCreateInfoStruct(TYPE_COLORMAP_INFO);

    if(!cmapID)
	    return 0;

    if(!(cmapInfo = (COLORMAP_INFO*)XGetInfoStruct(cmapID)))
	    return 0;

    memset(&newPalette, 0, sizeof(LOGPALETTE));
    newPalette.palVersion = 0x300;
    newPalette.palNumEntries = 1;

    if(!(cmapInfo->palette = CreatePalette(&newPalette))){
	    int error = GetLastError();
	    return 0;
    }
    

    theWindow = XGetWindowInfoStruct(w);

    theWindow->theColormap = cmapInfo;
    theDC = XGetDC(w);

    if(alloc == AllocNone){
	    
	    if(!SelectPalette(theDC, cmapInfo->palette, TRUE)
		    || !RealizePalette(theDC)){
		    XReleaseDC(w, theDC);
		    return 0;
	    }

	    return cmapID;
    }

    else if(alloc == AllocAll){

	    numDefaultColors = XGetDefaultPalette(theDC, defaultColors);

	    if(numDefaultColors == -1){
		    XReleaseDC(w, theDC);
		    return 0;
	    }

	    palEntry = (PALETTEENTRY*)malloc(sizeof(PALETTEENTRY) * numDefaultColors);
	    memset(palEntry, 0, sizeof(PALETTEENTRY) * numDefaultColors);

	    for(int i = 0; i < numDefaultColors; i++){

		    DWORD temp = defaultColors[i];

		    palEntry[i].peFlags = NULL;
		    palEntry[i].peRed = (BYTE)temp;
		    palEntry[i].peGreen = (BYTE)(temp >> 8);
		    palEntry[i].peBlue = (BYTE)(temp >> 16);
	    }

	    if(!ResizePalette(cmapInfo->palette, numDefaultColors)
		    || !SetPaletteEntries(cmapInfo->palette, 0, numDefaultColors, palEntry)){
		    free(palEntry);
		    XReleaseDC(w, theDC);
		    return 0;
	    }
	    free(palEntry);

	    if(!SelectPalette(theDC, cmapInfo->palette, TRUE)
		    || (RealizePalette(theDC) == GDI_ERROR)){
		    XReleaseDC(w, theDC);
		    return 0;
	    }

	    cmapInfo->theDC = theDC;
	    cmapInfo->numEntries = numDefaultColors;

	    return cmapID;
    }
    else{
	    XReleaseDC(w, theDC);
	    XFreeInfoStruct(cmapID);
	    return 0;
    }

}

int _stdcall XEnumObjectProc(LPVOID gdiObj, LPBYTE param){

    COLORREF *colorArray = (COLORREF*)param;
    LOGPEN *thePen = (LOGPEN*)gdiObj;
    int numColors = (int)colorArray[255];

    if(thePen->lopnStyle==PS_SOLID){
	    colorArray[numColors] = thePen->lopnColor;
	    colorArray[255] = (COLORREF)(++numColors);
    }

    return 1;

}

int XGetDefaultPalette(HDC theDC, COLORREF defaultColorsArray[256]){

    int numColors;

    memset(defaultColorsArray, 0, sizeof(COLORREF) * 256);

    if(!EnumObjects(theDC, OBJ_PEN, (GOBJENUMPROC)XEnumObjectProc, (LPARAM)defaultColorsArray))
	    return -1;

    numColors = (int)defaultColorsArray[255];

    return numColors;

}

GC XCreateGC(
Display*		display/* display */,
Drawable		d/* d */,
unsigned long	valuemask/* valuemask */,
XGCValues*		values/* values */
    ){

//	HDC oldDC;
//	HDC newDC;
    GC newGC;
    GC_INFO *gcInfo;
//	HBITMAP bitmap;

//	oldDC = XGetDC((HWND)d);
    
//	newDC = CreateCompatibleDC(oldDC);
//	bitmap = CreateCompatibleBitmap(newDC, GetDeviceCaps(oldDC, HORZRES), GetDeviceCaps(oldDC, VERTRES));
//	SelectObject(newDC, bitmap);

//	XReleaseDC(d, oldDC);


    newGC = XCreateInfoStruct(TYPE_GC_INFO);

    if(!newGC)
	    return 0;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(newGC)))
	    return 0;

    gcInfo->d = d;
    gcInfo->graphics_exposure = false;
    gcInfo->clippingRgn = CreateRectRgn(0, 0, 0, 0);

    return gcInfo->base.handle;
}

int XSetGraphicsExposures(
Display*		display/* display */,
GC			gc/* gc */,
Bool		graphics_exposures/* graphics_exposures */
    ){

    GC_INFO *gcInfo;

    if(!(gcInfo = (GC_INFO*)XGetInfoStruct(gc)))
	    return 0;

    gcInfo->graphics_exposure = graphics_exposures;

    return 1;
}

int XFreeGC(
Display*		/* display */,
GC			/* gc */
    ){

    return 0;
}

int XCloseDisplay(
Display*		/* display */
    ){

    return 0;
}

KeySym XLookupKeysym(
    XKeyEvent *ev,
    int index
    ) {

  map<USHORT, USHORT>::iterator iter;

  if(ev->unicode)
    return ev->keycode;

  iter = keyMap.find(ev->keycode);

  if(iter == keyMap.end())
    return 0;

  return iter->second;
 
}

int XSetupKeyboard(void) {
   // theKeyboard.hkb = LoadKeyboardLayout("00000409", KLF_NOTELLSHELL);
   // theKeyboard.langId = GetUserDefaultLangID();


    keyMap[VK_BACK] = XK_BackSpace;
    keyMap[VK_TAB] = XK_Tab;
    keyMap[VK_CLEAR] = XK_Clear;
    keyMap[VK_RETURN] = XK_Return;
    keyMap[VK_SHIFT] = XK_Shift_L;
    keyMap[VK_CONTROL] = XK_Control_L;
    keyMap[VK_MENU] = XK_Alt_L;
    keyMap[VK_PAUSE] = XK_Pause;
    keyMap[VK_CAPITAL] = XK_Caps_Lock;
    keyMap[VK_KANA] = 0;
    keyMap[VK_HANGEUL] = 0;
    keyMap[VK_HANGUL] = 0;
    keyMap[VK_JUNJA] = 0;
    keyMap[VK_FINAL] = 0;
    keyMap[VK_HANJA] = 0;
    keyMap[VK_KANJI] = 0;
    keyMap[VK_ESCAPE] = XK_Escape;
    keyMap[VK_CONVERT] = 0;
    keyMap[VK_NONCONVERT] = 0;
    keyMap[VK_ACCEPT] = 0;
    keyMap[VK_MODECHANGE] = 0;
    keyMap[VK_SPACE] = XK_space;
    keyMap[VK_PRIOR] = XK_Page_Up;
    keyMap[VK_NEXT] = XK_Page_Down;
    keyMap[VK_END] = XK_End;
    keyMap[VK_HOME] = XK_Home;
    keyMap[VK_LEFT] = XK_Left;
    keyMap[VK_UP] = XK_Up;
    keyMap[VK_RIGHT] = XK_Right;
    keyMap[VK_DOWN] = XK_Down;
    keyMap[VK_SELECT] = XK_Select;
    keyMap[VK_PRINT ] = XK_Print;
    keyMap[VK_EXECUTE] = XK_Execute;
    keyMap[VK_SNAPSHOT] = XK_Print;
    keyMap[VK_INSERT] = XK_Insert;
    keyMap[VK_DELETE] = XK_Delete;
    keyMap[VK_HELP] = XK_Help	;
    keyMap[VK_LWIN] = 0;
    keyMap[VK_RWIN] = 0;
    keyMap[VK_APPS] = 0;
    keyMap[VK_SLEEP] = 0;
    keyMap[VK_NUMPAD0] = XK_KP_0;
    keyMap[VK_NUMPAD1] = XK_KP_1;
    keyMap[VK_NUMPAD2] = XK_KP_2;
    keyMap[VK_NUMPAD3] = XK_KP_3;
    keyMap[VK_NUMPAD4] = XK_KP_4;
    keyMap[VK_NUMPAD5] = XK_KP_5;
    keyMap[VK_NUMPAD6] = XK_KP_6;
    keyMap[VK_NUMPAD7] = XK_KP_7;
    keyMap[VK_NUMPAD8] = XK_KP_8;
    keyMap[VK_NUMPAD9] = XK_KP_9;
    keyMap[VK_MULTIPLY] = XK_KP_Multiply;
    keyMap[VK_ADD] = XK_KP_Add;
    keyMap[VK_SEPARATOR] = XK_KP_Separator;
    keyMap[VK_SUBTRACT] = XK_KP_Subtract;
    keyMap[VK_DECIMAL] = XK_KP_Decimal;
    keyMap[VK_DIVIDE] = XK_KP_Divide;
    keyMap[VK_F1] = XK_F1;
    keyMap[VK_F2] = XK_F2;
    keyMap[VK_F3] = XK_F3;
    keyMap[VK_F4] = XK_F4;
    keyMap[VK_F5] = XK_F5;
    keyMap[VK_F6] = XK_F6;
    keyMap[VK_F7] = XK_F7;
    keyMap[VK_F8] = XK_F8;
    keyMap[VK_F9] = XK_F9;
    keyMap[VK_F10] = XK_F10;
    keyMap[VK_F11] = XK_F11;
    keyMap[VK_F12] = XK_F12;
    keyMap[VK_F13] = XK_F13;
    keyMap[VK_F14] = XK_F14;
    keyMap[VK_F15] = XK_F15;
    keyMap[VK_F16] = XK_F16;
    keyMap[VK_F17] = XK_F17;
    keyMap[VK_F18] = XK_F18;
    keyMap[VK_F19] = XK_F19;
    keyMap[VK_F20] = XK_F20;
    keyMap[VK_F21] = XK_F21;
    keyMap[VK_F22] = XK_F22;
    keyMap[VK_F23] = XK_F23;
    keyMap[VK_F24] = XK_F24;
    keyMap[VK_NUMLOCK] = XK_Num_Lock;
    keyMap[VK_SCROLL] = XK_Scroll_Lock;
    keyMap[VK_LSHIFT] = XK_Shift_L;
    keyMap[VK_RSHIFT] = XK_Shift_R;
    keyMap[VK_LCONTROL] = XK_Control_L;
    keyMap[VK_RCONTROL] = XK_Control_R;
    keyMap[VK_LMENU] = XK_Alt_L;
    keyMap[VK_RMENU] = XK_Alt_R;
    //keyMap[VK_OEM_1] = XK_semicolon;	// ';:' key 
    //keyMap[VK_OEM_PLUS] = XK_equal;
    //keyMap[VK_OEM_COMMA] = XK_comma;	// ',<' key
    //keyMap[VK_OEM_MINUS] = XK_minus;
    //keyMap[VK_OEM_PERIOD] = XK_period;	// '.>' key
    //keyMap[VK_OEM_2] = XK_slash;		// '/?' key 
    //keyMap[VK_OEM_3] = XK_quoteleft;	// '`~' key 
    //keyMap[VK_OEM_4] = XK_bracketleft;	// '[{' key
    //keyMap[VK_OEM_5] = XK_backslash;	// '\|' key
    //keyMap[VK_OEM_6] = XK_bracketright;	// ']}' key
    //keyMap[VK_OEM_7] = XK_quoteright;	// ''"' key
    keyMap[VK_OEM_8] = 0;				// miscellaneous characters
    keyMap[VK_OEM_102] = 0;				// Either the angle bracket key or the backslash key on the RT 102-key keyboard
    keyMap[VK_PROCESSKEY] = 0;
    keyMap[VK_ATTN] = 0;
    keyMap[VK_CRSEL] = 0;
    keyMap[VK_EXSEL] = 0;
    keyMap[VK_EREOF] = 0;
    keyMap[VK_PLAY] = 0;
    keyMap[VK_ZOOM] = 0;
    keyMap[VK_NONAME] = 0;
    keyMap[VK_PA1] = 0;
    keyMap[VK_OEM_CLEAR] = 0;

    return True;
}

Status XSendInternalEvent(Display*	display/* display */,
Window		w/* w */,
Bool		propagate/* propagate */,
long		event_mask/* event_mask */,
XEvent*		event_send/* event_send */){

    DISPLAY_INFO *dispInfo;
    INTERNAL_XEVENT toSend;

    //printf("Sending event %d with mask event_mask %d\n", event_send->type, event_mask); 
    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)display)))
	    return False;

//	EnterCriticalSection(&dispInfo->queueLock);

    event_send->xany.serial = ++eventCounter;

    toSend.mask = event_mask;
    memcpy(&toSend.event, event_send, sizeof(XEvent));
    return XAddEventToEventList(&dispInfo->eventList, &toSend, EVENT_LIST_SIZE);

//	SetEvent(dispInfo->eventSignal);
//	LeaveCriticalSection(&dispInfo->queueLock);

//	return True;
}

Status XSendEvent(
Display*	display/* display */,
Window		w/* w */,
Bool		propagate/* propagate */,
long		event_mask/* event_mask */,
XEvent*		event_send/* event_send */
    ){

    event_send->xany.send_event = True;

    return XSendInternalEvent(display, w, propagate, event_mask, event_send);
}

int XNextEvent(
Display*	display/* display */,
XEvent*		event_return/* event_return */
    ){

    DISPLAY_INFO *dispInfo;
    INTERNAL_XEVENT theEvent;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)display)))
	    return False;
    
    while(!dispInfo->eventList.numEvents){
	    Sleep(0);
	    XProcessMessageQueue(dispInfo);

    }

    if(!XRemoveEventFromEventList(&dispInfo->eventList, &theEvent, 0))
	    return False;

    memcpy(event_return, &theEvent.event, sizeof(XEvent));

//	WaitForSingleObject(dispInfo->eventSignal, INFINITE);
//	EnterCriticalSection(&dispInfo->queueLock);

/*	memcpy(event_return, &dispInfo->eventQueue[0].event, sizeof(XEvent));

    ShiftEventQueueDown(dispInfo, 0, 1);

    if(dispInfo->queueSize > 0){
	    SetEvent(dispInfo->eventSignal);
    }

    LeaveCriticalSection(&dispInfo->queueLock);
*/
    return True;
}

XSizeHints *XAllocSizeHints (){

    XID sizeHintsID;
    SIZEHINTS_INFO *sizeHints;

    if(!(sizeHintsID = XCreateInfoStruct(TYPE_SIZEHINTS_INFO)))
	    return NULL;

    sizeHints = (SIZEHINTS_INFO*)XGetInfoStruct(sizeHintsID);

    return &sizeHints->sizeHints;
}

XWMHints *XAllocWMHints (
void
    ){

    XID wmHintsID;
    WMHINTS_INFO *wmHints;

    if(!(wmHintsID = XCreateInfoStruct(TYPE_WMHINTS_INFO)))
	    return NULL;

    wmHints = (WMHINTS_INFO*)XGetInfoStruct(wmHintsID);

    return &wmHints->wmHints;
}

XClassHint *XAllocClassHint (
void
    ){

    XID classHintsID;
    CLASSHINTS_INFO *classHints;

    if(!(classHintsID = XCreateInfoStruct(TYPE_CLASSHINTS_INFO)))
	    return NULL;

    classHints = (CLASSHINTS_INFO*)XGetInfoStruct(classHintsID);

    return &classHints->classHint;
}

Status XStringListToTextProperty(
char**		list/* list */,
int			count/* count */,
XTextProperty*	text_prop_return/* text_prop_return */
    ){

    int totalSize;
    int i, x, z;

    text_prop_return->format = 8;
    text_prop_return->nitems = count;
    text_prop_return->encoding = XStringStyle;
    
    totalSize = 0;

    for(i = 0; i < count; i++){
	    totalSize += strlen(list[i]);
	    totalSize++;
    }

    text_prop_return->value = (UCHAR*)malloc(totalSize);
    memset(text_prop_return->value, 0, totalSize);

    z = 0;
    for(i = 0; i < count; i++){
	    for(x = 0; list[i][x] != '\0'; x++)
		    text_prop_return->value[z++] = list[i][x];
	    text_prop_return->value[z++] = '\0';
    }

    return True;
}

void XSetWMProperties(
Display*		display/* display */,
Window		w/* w */,
XTextProperty*	window_name/* window_name */,
XTextProperty*	icon_name/* icon_name */,
char**		argv/* argv */,
int			argc/* argc */,
XSizeHints*		normal_hints/* normal_hints */,
XWMHints*		wm_hints/* wm_hints */,
XClassHint*		class_hints/* class_hints */
){

    if(window_name)
	    XSetWMName(display, w, window_name);

    if(icon_name)
	    XSetWMIconName(display, w, icon_name);

    if(normal_hints)
	    XSetWMNormalHints(display, w, normal_hints);

    if(wm_hints)
	    XSetWMHints(display, w, wm_hints);

    if(class_hints)
	    XSetClassHint(display, w, class_hints);
}

BOOL XCreateIcons(Window theWindow, HBITMAP iconBitmap, HDC iconDC, HBITMAP maskBitmap, HDC maskDC,
			      HICON *largeIconReturn, 
			      HICON *smallIconReturn,
			      int bitmapWidth, int bitmapHeight, int bitsPerPixel, int numPlanes){

    return XCreateIcon(theWindow, iconBitmap, iconDC, maskBitmap, maskDC, smallIconReturn, 
					    bitmapWidth, bitmapHeight, bitsPerPixel, numPlanes,
					    32, 32) &&
		    XCreateIcon(theWindow, iconBitmap, iconDC, maskBitmap, maskDC, largeIconReturn,
					    bitmapWidth, bitmapHeight, bitsPerPixel, numPlanes,
					    16, 16);
}

BOOL XCreateIcon(Window theWindow, HBITMAP iconBitmap, HDC iconDC, HBITMAP maskBitmap, HDC maskDC, 
			     HICON *iconReturn, 
			     int bitmapWidth, int bitmapHeight, int bitsPerPixel, int numPlanes,
			     int iconWidth, int iconHeight){


    ICONINFO iconInfo;
    HBITMAP bitmap, mask;
    HICON icon;
    UCHAR *genMask;
    HDC bitmapDC, iconMaskDC, screenDC;
    int i;
    BOOL retVal;

    genMask = (UCHAR*)malloc(iconHeight);
    for(i = 0; i < iconHeight; i++) genMask[i] = 0x01;

    screenDC = XGetDC(theWindow);
    bitmapDC = CreateCompatibleDC(screenDC);
    iconMaskDC = CreateCompatibleDC(screenDC);

    //Create bitmaps
    if(!(bitmap =  CreateCompatibleBitmap(screenDC, iconWidth, iconHeight))){
	    XReleaseDC(theWindow, screenDC);
	    goto end;
    }

    XReleaseDC(theWindow, screenDC);

    if(!(mask = CreateBitmap(iconWidth, iconHeight, 1, 1, genMask)))
	    goto end;

    if(!SelectObject(bitmapDC, bitmap))
	    goto end;
    

    if(!StretchBlt(bitmapDC, 0, 0, iconWidth, iconHeight, iconDC, 0, 0, 
					    bitmapWidth, bitmapHeight, SRCCOPY)){
	    goto end;
    }	
    
    if(maskBitmap){

	    SelectObject(iconMaskDC, mask);

	    if(!StretchBlt(iconMaskDC, 0, 0, iconWidth, iconHeight, maskDC, 
					    0, 0, bitmapWidth, bitmapHeight, SRCCOPY)){
		    goto end;
	    }
    }

    iconInfo.fIcon = TRUE;
    iconInfo.xHotspot = 0;
    iconInfo.yHotspot = 0;
    iconInfo.hbmMask = mask;
    iconInfo.hbmColor = bitmap;

    if(!(icon = CreateIconIndirect(&iconInfo))){
	    goto end;
    }

    *iconReturn = icon;
    retVal = TRUE;
end:

    free(genMask);
    DeleteDC(bitmapDC);
    DeleteDC(iconMaskDC);
    XDeleteObject(bitmap);
    XDeleteObject(mask);

    return retVal;
}

int XSetWMHints(
Display*		display/* display */,
Window		w/* w */,
XWMHints*		wm_hints/* wm_hints */
    ){

    PIXMAP_INFO *pixInfo, *maskInfo;
    HICON smallIcon, largeIcon;
    WINDOW_INFO *winInfo;

    winInfo = XGetWindowInfoStruct(w);

    if(wm_hints->flags & IconPixmapHint){
	    
	    if(!(pixInfo = (PIXMAP_INFO*)XGetInfoStruct(wm_hints->icon_pixmap)))
		    return False;

	    if(wm_hints->flags & IconMaskHint){
	    
		    if(!XCreateIcons(w, pixInfo->bitmap, pixInfo->theDC, maskInfo->bitmap, maskInfo->theDC,
						    &largeIcon, &smallIcon,
						    pixInfo->width, pixInfo->height, pixInfo->bitsPerPixel, pixInfo->depth))
						    return FALSE;
	    }
	    
	    else{

		    if(!XCreateIcons(w, pixInfo->bitmap, pixInfo->theDC, NULL, NULL, &largeIcon, &smallIcon,
						    pixInfo->width, pixInfo->height, pixInfo->bitsPerPixel, pixInfo->depth))
						    return FALSE;
	    }

	    if(!SetClassLong((HWND)w, GCL_HICON, (long)largeIcon))
		    return False;

	    if(!SetClassLong((HWND)w, GCL_HICONSM, (long)smallIcon)){
		    DWORD error = GetLastError();
		    return False;
	    }
	    winInfo->largeIcon = largeIcon;
	    winInfo->smallIcon = smallIcon;
    }


    return True;
}

void XSetTextProperty(
Display*		display/* display */,
Window		w/* w */,
XTextProperty*	text_prop/* text_prop */,
Atom		property/* property */
    ){

    WINDOW_INFO *winInfo;

    if((text_prop->encoding != XStringStyle) || (text_prop->format != 8))
	    return;

//	if(!(winInfo = (WINDOW_INFO*)XGetInfoStruct(w)))
//		return;

    switch(property){

	    case WM_ICON_NAME: return;
	    case WM_NAME: SetWindowText((HWND)w, (char*)text_prop->value); return;
	    default: return;
    }
			    
}

void XSetWMIconName(
Display*		display/* display */,
Window		w/* w */,
XTextProperty*	text_prop/* text_prop */
    ){

    XSetTextProperty(display, w, text_prop, WM_ICON_NAME);
}

void XSetWMName(
Display*		display/* display */,
Window		w/* w */,
XTextProperty*	text_prop/* text_prop */
    ){

    XSetTextProperty(display, w, text_prop, WM_NAME);
}

void XSetWMNormalHints(
Display*		display/* display */,
Window		w/* w */,
XSizeHints*		hints/* hints */
    ){

    RECT windowRect;
    WINDOW_INFO *info;

    if(!(info = XGetWindowInfoStruct(w)))
	    return;

/*	if(hints->flags & PPosition){
	    if(!SetWindowPos((HWND)w, HWND_TOP, hints->x, hints->y, 0, 0, SWP_NOSIZE))
		    return;
    }*/
	    
    GetWindowRect((HWND)w, &windowRect);

/*	if(hints->flags & PSize){
	    if(!SetWindowPos((HWND)w, HWND_TOP, windowRect.left, windowRect.top, 
					    hints->width, hints->height, NULL))
		    return;
    }
*/
    if(hints->flags & PMinSize){
	    info->minWidth = hints->min_width;
	    info->minHeight = hints->min_height;
    }

    if(hints->flags & PMaxSize){
	    info->maxWidth = hints->max_width;
	    info->maxHeight = hints->max_height;
    }
    
}

int XSetClassHint(
Display*		display/* display */,
Window		w/* w */,
XClassHint*		class_hints/* class_hints */
    ){

    return True;
}

DWORD __stdcall DebugThread(void *p){

    Sleep(2000);
    while(true){
	    DWORD temp;
	    
	    if(!SendMessageTimeout((HWND)p, WM_USER + 10, 0, 0, SMTO_ABORTIFHUNG, 1000, &temp))
		    DebugBreak();

	    Sleep(1000);
	    
    }

    return 0;
}

Bool XGetWindowPos(Display *display, Window window, int *x, int *y){

  POINT point;

  point.x = point.y = 0;
  ClientToScreen((HWND)window, &point);

  *x = point.x;
  *y = point.y;

  return True;
}

Window XCreateWindow(
Display*		display/* display */,
Window		parent/* parent */,
int			x/* x */,
int			y/* y */,
unsigned int	width/* width */,
unsigned int	height/* height */,
unsigned int	border_width/* border_width */,
int			depth/* depth */,
unsigned int	_class/* class */,
Visual*		visual/* visual */,
unsigned long	valuemask/* valuemask */,
XSetWindowAttributes*	attributes/* attributes */
){

    WINDOWINFO info;
    XID winID;
    WINDOW_INFO *winInfo;
    WINDOW_INFO *parentInfo;
    bool topLevel = false;
    HWND theWindow;
    DWORD exType, type;
    RECT windowRect, clientRect;
    POINT point;
    int diffX, diffY;

    /*
    printf("Create Window\n");
    if(x < 0 || y < 0)
	    printf("negative creation\n");
	    */

    if(!attributes->override_redirect){
    //	exType = WS_EX_OVERLAPPEDWINDOW;
	    exType = WS_EX_WINDOWEDGE;
	    type = WS_OVERLAPPEDWINDOW  | WS_CLIPCHILDREN | WS_CLIPSIBLINGS;
    }
    else{
	    exType = WS_EX_TOPMOST | WS_EX_WINDOWEDGE;
//	    exType = WS_EX_WINDOWEDGE;
//	    type = WS_CHILD | WS_CLIPSIBLINGS;
	    type = WS_POPUP;
	    //point.x = x;
	    //point.y = y;
	    //ClientToScreen((HWND)parent, &point);
	    //x = point.x;
	    //y = point.y;

	    if(parent == (Window)rootWindow->window)
//		    parent = (Window)rootWindow->window;
		    parent = (Window)GetActiveWindow();
//			parent = rootWindow->children[0];
//		point.x = x;
//		point.y = y;
//		ScreenToClient((HWND)parent, &point);
//		x = point.x;
//		y = point.y;

    }

/*	if(parent){
	    info.cbSize = sizeof(WINDOWINFO);
	    GetWindowInfo((HWND)parent, &info);
	    
	    if(visual == CopyFromParent){
		    exType = info.dwExStyle;
		    type = info.dwStyle;
	    }
    }
*/

    if(!(parentInfo = XGetWindowInfoStruct(parent)))
	    return False;

    if(parentInfo->numChildren >= MAX_NUM_CHILDREN)
	    return False;

    if(!(theWindow = CreateWindowEx(exType, progName, progName, type, x, y, width, height, 
				(HWND)parent, NULL, saveInstance, NULL)))
	    return 0;

    //printf("window %d created, parent is %d\n", theWindow, parent);
    GetWindowRect(theWindow, &windowRect);
    GetClientRect(theWindow, &clientRect);

    diffX = (windowRect.right - windowRect.left) - clientRect.right;
    diffY = (windowRect.bottom - windowRect.top) - clientRect.bottom;

    if(diffX || diffY)
	    SetWindowPos(theWindow, NULL, x, y, width + diffX, height + diffY, 
		    SWP_NOACTIVATE | SWP_NOOWNERZORDER);

    GetClientRect(theWindow, &clientRect);
/*	RECT temp;

    GetWindowRect(theWindow, &temp);*/

    winID = XCreateInfoStruct(TYPE_WINDOW_INFO);

    winInfo = (WINDOW_INFO*)XGetInfoStruct(winID);

    winInfo->window = theWindow;
    winInfo->xid = winID;
    winInfo->border_width = border_width;
    winInfo->override_direct = attributes->override_redirect & (valuemask & CWOverrideRedirect);
    winInfo->eventMask = KeyPressMask | KeyReleaseMask | ButtonPressMask
						    | ButtonReleaseMask | EnterWindowMask | LeaveWindowMask
						    | PointerMotionMask | PointerMotionHintMask | Button1MotionMask
						    | Button2MotionMask | Button3MotionMask | Button4MotionMask
						    | Button5MotionMask | ButtonMotionMask | KeymapStateMask
						    | ExposureMask | VisibilityChangeMask | StructureNotifyMask
						    | ResizeRedirectMask | SubstructureNotifyMask | SubstructureRedirectMask
						    | FocusChangeMask | PropertyChangeMask | ColormapChangeMask
						    | OwnerGrabButtonMask;
    winInfo->theDisplay = display;
    winInfo->width = width;
    winInfo->height = height;
    winInfo->x = x;
    winInfo->y = y;

    XSaveWindow(winInfo);
    XAddChildToParent(parent, (Window)theWindow);
    XSendCreateWindowEvent(display, (Window)theWindow);
//    BringWindowToTop(theWindow);

/*	if(!attributes->override_redirect){
    //	exType = WS_EX_OVERLAPPEDWINDOW;
	    DWORD temp;
	    CreateThread(NULL, NULL, DebugThread, theWindow, 0, &temp);
	    SetThreadPriority(&temp, THREAD_PRIORITY_HIGHEST);
    }
*/
    return (Window)theWindow;
} 

Status XAddChildToParent(Window parent, Window child){

    WINDOW_INFO *parentInfo, *childInfo;

    if(!(parentInfo = XGetWindowInfoStruct(parent)))
	    return False;

    if(!(childInfo = XGetWindowInfoStruct(child)))
	    return False;

    if(parentInfo->numChildren >= MAX_NUM_CHILDREN)
	    return False;

    parentInfo->children[parentInfo->numChildren] = child;
    parentInfo->numChildren++;
    childInfo->parent = parent;
}

Status XRemoveChildFromParent(Window parent, Window child){

    WINDOW_INFO *parentInfo, *childInfo;

    if(!(parentInfo = XGetWindowInfoStruct(parent)))
	    return False;

    if(!(childInfo = XGetWindowInfoStruct(child)))
	    return False;

    for(int i = 0; i < parentInfo->numChildren; i++){
	    if(parentInfo->children[i] == child){
		    memcpy(&parentInfo->children[i], &parentInfo->children[i + 1], 
				    sizeof(Window) * (parentInfo->numChildren - (i + 1)));
		    parentInfo->children[parentInfo->numChildren - 1] = 0;
		    childInfo->parent = 0;
		    parentInfo->numChildren--;
		    return True;
	    }
    }

    return False;
}

Status XSendCreateWindowEvent(Display *theDisplay, Window window){

    XEvent toSend;
    WINDOW_INFO *windowInfo;
    int x, y;
    RECT rect;
    
    windowInfo = XGetWindowInfoStruct(window);
    
    if(!windowInfo || !(windowInfo->eventMask & SubstructureNotifyMask))
	    return false;

    GetClientRect((HWND)window, &rect);
    XGetWindowPositionRelativeToParent(window, &x, &y);
//	GetWindowRect((HWND)window, &rect);
    
    toSend.xany.display = theDisplay;
    toSend.xany.send_event = False;
    toSend.xany.type = CreateNotify;
    toSend.xany.window = window;
    toSend.xcreatewindow.window = window;
    toSend.xcreatewindow.type = CreateNotify;
    toSend.xcreatewindow.border_width = windowInfo->border_width;
    toSend.xcreatewindow.display = theDisplay;
    toSend.xcreatewindow.height = rect.bottom - rect.top;
    toSend.xcreatewindow.override_redirect = windowInfo->override_direct;
//	toSend.xcreatewindow.parent = (Window)windowInfo->parent;
    toSend.xcreatewindow.send_event = False;
    toSend.xcreatewindow.width = rect.right - rect.left;
    toSend.xcreatewindow.x = x;
    toSend.xcreatewindow.y = y;
    
    return XSendInternalEvent(theDisplay, window, False, SubstructureNotifyMask, &toSend); 
}

Status XSendExposeEvent(Display *theDisplay, Window window, int x, int y, int width, int height){

    XEvent toSend;
    WINDOW_INFO *windowInfo;
    DISPLAY_INFO *dispInfo;
    INTERNAL_XEVENT *current;
    
    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)theDisplay)))
	    return False;

    current = dispInfo->eventList.eventListHead.next;

    while(current != &dispInfo->eventList.eventListTail){
	    
	    if((current->event.type == Expose) 
		    && (current->event.xany.window == window)
		    && (current->event.xexpose.x == x)
		    && (current->event.xexpose.y == y)
		    && (current->event.xexpose.width == width)
		    && (current->event.xexpose.height == height))
		    return True;
	    current = current->next;
    }

    if(!(windowInfo = (WINDOW_INFO*)XGetWindowInfoStruct(window)))
	    return False;

    if(!(windowInfo->eventMask & ExposureMask))
	    return False;

    toSend.xany.display = theDisplay;
    toSend.xany.send_event = False;
    toSend.xany.type = Expose;
    toSend.xany.window = window;
    toSend.xexpose.count = 0;
    toSend.xexpose.display = theDisplay;
    toSend.xexpose.height = height;
    toSend.xexpose.width = width;
    toSend.xexpose.send_event = False;
    toSend.xexpose.type = Expose;
    toSend.xexpose.window = window;
    toSend.xexpose.x = x;
    toSend.xexpose.y = y;

    return XSendInternalEvent(theDisplay, window, False, ExposureMask, &toSend);
}

int XSelectInput(
Display*		display/* display */,
Window		w/* w */,
long		event_mask/* event_mask */
){

    WINDOW_INFO *info;

    if(!(info = XGetWindowInfoStruct(w)))
	    return False;

    info->eventMask = event_mask;
    return True;
}

Status XSetWMProtocols(
Display*		display/* display */,
Window		w/* w */,
Atom*		protocols/* protocols */,
int			count/* count */
){

    //not sure what to do for this one yet???
    return True;
}

/*void ShiftEventQueueDown(DISPLAY_INFO *display, int startIndex, int numToShift){

    if(startIndex == 0)
	    startIndex = 1;

    if((startIndex - numToShift) < 0)
	    numToShift = startIndex + 1;

    memcpy(&display->eventQueue[startIndex - numToShift], &display->eventQueue[startIndex], 
	    sizeof(INTERNAL_XEVENT) * (display->queueSize - startIndex));
    memset(&display->eventQueue[display->queueSize - numToShift], 0, sizeof(INTERNAL_XEVENT) * numToShift);
    display->queueSize--;
}

void ShiftEventQueueUp(DISPLAY_INFO *display, int startIndex, int numToShift){

    if(startIndex >= EVENT_QUEUE_SIZE)
	    startIndex = EVENT_QUEUE_SIZE - 1;

    if((startIndex + numToShift) > EVENT_QUEUE_SIZE)
	    numToShift = EVENT_QUEUE_SIZE - startIndex;

    memcpy(&display->eventQueue[startIndex + numToShift], &display->eventQueue[startIndex],
		    sizeof(INTERNAL_XEVENT) * (display->queueSize - startIndex));
    memset(&display->eventQueue[startIndex], 0, sizeof(INTERNAL_XEVENT) * numToShift);
    display->queueSize++;
}*/

Bool XCheckWindowEvent(
Display*		display/* display */,
Window		w/* w */,
long		event_mask/* event_mask */,
XEvent*		event_return/* event_return */
){

    DISPLAY_INFO *dispInfo;
    INTERNAL_XEVENT *current;
    INTERNAL_XEVENT buffer;
    BOOL found;
    int x;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)display)))
	    return False;

    XProcessMessageQueue(dispInfo);

    current = dispInfo->eventList.eventListHead.next;

    x = 0;
    while(current != &dispInfo->eventList.eventListTail){

	    if((current->event.xany.window == w) && (current->mask & event_mask)){
		    XRemoveEventFromEventList(&dispInfo->eventList, &buffer, x);
		    memcpy(event_return, &buffer.event, sizeof(XEvent));
		    return True;
	    }
	    x++;
	    current = current->next;
    }

/*	EnterCriticalSection(&dispInfo->queueLock);

    for(int i = 0; i < dispInfo->queueSize; i++){
	    event = &dispInfo->eventQueue[i];
	    if(event->mask & event_mask){
		    memcpy(event_return, &event->event, sizeof(XEvent));

		    ShiftEventQueueDown(dispInfo, i + 1, 1);

		    if(dispInfo->queueSize > 0){
			    SetEvent(dispInfo->eventSignal);
		    }

		    LeaveCriticalSection(&dispInfo->queueLock);
		    return True;
	    }
    }
    LeaveCriticalSection(&dispInfo->queueLock);
*/
    return False;
}

int XDestroyWindow(
Display*		display/* display */,
Window		window/* w */
){

    WINDOW_INFO *windowInfo;

    if(!(windowInfo = XGetWindowInfoStruct(window)))
	    return False;

    while(windowInfo->numChildren > 0){
	    XDestroyWindow(display, window);
    }

    XRemoveChildFromParent(windowInfo->parent, window);
    DestroyWindow((HWND)window);
    DestroyIcon(windowInfo->smallIcon);
    DestroyIcon(windowInfo->largeIcon);
    XDeleteWindow(windowInfo);
    XFreeInfoStruct(windowInfo->xid);
//	InvalidateRect((HWND)windowInfo->parent, NULL, FALSE);
    XSendDestroyNotifyEvent(display, window);
    return True;
}

Status XSendUnmapNotifyEvent(Display *theDisplay, Window window){

    XEvent toSend;

    toSend.type = UnmapNotify;
    toSend.xany.display = theDisplay;
    toSend.xany.send_event = False;
    toSend.xany.type = UnmapNotify;
    toSend.xany.window = window;
    toSend.xunmap.display = theDisplay;
    toSend.xunmap.event = window;
    toSend.xunmap.from_configure = False;
    toSend.xunmap.send_event = False;
    toSend.xunmap.type = UnmapNotify;
    toSend.xunmap.window = window;

    return XSendInternalEvent(theDisplay, window, False, StructureNotifyMask, &toSend);
}

int XMoveWindow(
Display*		/* display */,
Window		/* w */,
int			/* x */,
int			/* y */
){

    return 0;
}

int XResizeWindow(
Display*		/* display */,
Window		/* w */,
unsigned int	/* width */,
unsigned int	/* height */
){

    return 0;
}

int XStoreName(
Display*		display/* display */,
Window		w/* w */,
_Xconst char*	window_name/* window_name */
){

    SetWindowText((HWND)w, window_name);
    return True;
}

int XSetIconName(
Display*		display/* display */,
Window		w/* w */,
_Xconst char*	icon_name/* icon_name */
){

    SetWindowText((HWND)w, icon_name);
    return True;
}

void XBufferBacking(Window w){

    WINDOW_INFO *info, *parentInfo;
    BACKING_STORE_INFO *backing;
    RECT windowRect;
    POINT parentPoint;
    HDC parentDC, desktopDC;
    XID backingId;
    HWND desktopWindow;

    if(!(info = XGetWindowInfoStruct(w))){
	    return;
    }

    //if(info->parent == (Window)rootWindow->window){
//	    return;
  //  }

    backingId = XCreateInfoStruct(TYPE_BACKING_STORE_INFO);
    backing = (BACKING_STORE_INFO*)XGetInfoStruct(backingId);

//	parentDC = GetDC((HWND)info->parent);
    desktopWindow = GetDesktopWindow();
    parentDC = GetDC(NULL);
//    desktopDC = GetWindowDC((HWND)info->parent);
    desktopDC = GetWindowDC(desktopWindow);

    parentPoint.x = info->x;
    parentPoint.y = info->y;

//    ClientToScreen((HWND)info->parent, &parentPoint);
    ClientToScreen(desktopWindow, &parentPoint);
    backing->dc = CreateCompatibleDC(parentDC);
    //backing->dc = GetWindowDC((HWND)info->parent);
    backing->bitmap = CreateCompatibleBitmap(desktopDC, info->width, info->height);
    SelectObject(backing->dc, backing->bitmap);
    BitBlt(backing->dc, 0, 0, info->width, info->height, parentDC, parentPoint.x, parentPoint.y, SRCCOPY);
    
    ReleaseDC(NULL, parentDC);
    XReleaseDC(info->parent, desktopDC);

    backingStoreMap[(HWND)w] = backing;
}

void XRestoreBacking(Window w){

    map<HWND, BACKING_STORE_INFO*>::iterator iter;
    BACKING_STORE_INFO* backing;
    WINDOW_INFO *info;
    HDC parentDC;
    HWND deskWindow;
    POINT screenPoint;


    iter = backingStoreMap.find((HWND)w);

    if(iter == backingStoreMap.end())
	    return;

    backing = iter->second;

    if(!(info = XGetWindowInfoStruct(w))){
	    return;
    }

    screenPoint.x = info->x;
    screenPoint.y = info->y;
//    ClientToScreen((HWND)info->parent, &screenPoint);
    ClientToScreen(GetDesktopWindow(), &screenPoint);
    //deskWindow = GetDesktopWindow();
    parentDC = GetDC(NULL);
    BitBlt(parentDC, screenPoint.x, screenPoint.y, info->width, info->height, backing->dc, 0, 0, SRCCOPY);
    ReleaseDC(NULL, parentDC);
    DeleteObject(backing->bitmap);
    DeleteDC(backing->dc);

    backingStoreMap.erase((HWND)w);
    XFreeInfoStruct(backing->base.handle);
}

int XMapRaised(
Display*		display/* display */,
Window		w/* w */
){

    WINDOW_INFO *info;

    info = XGetWindowInfoStruct(w);

    XBufferBacking(w);
    //printf("Raising Window\n");
    
    if(info->parent != (Window)rootWindow->window)
      ShowWindow((HWND)w, SW_SHOWNOACTIVATE);
    else
      ShowWindow((HWND)w, SW_SHOWNORMAL);

//    SetFocus((HWND)rootWindow->children[0]);
//    BringWindowToTop((HWND)w);
//    BringWindowToTop((HWND)rootWindow->children[0]);  
    XSendMapEvent(display, w);
//	SetCursor(LoadCursor(
    return True;
}

Status XSendMapEvent(Display *display, Window window){

    XEvent toSend;
    WINDOW_INFO *winInfo;

    if(!(winInfo = (WINDOW_INFO*)XGetWindowInfoStruct(window)))
	    return False;

    if(!(winInfo->eventMask & StructureNotifyMask))
	    return False;

    toSend.type = MapNotify;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = MapNotify;
    toSend.xany.window = window;
    toSend.xmap.display = display;
    toSend.xmap.event = window;
    toSend.xmap.override_redirect = winInfo->override_direct;
    toSend.xmap.send_event = False;
    toSend.xmap.type = MapNotify;
    toSend.xmap.window = window;

    return XSendInternalEvent(display, window, False, StructureNotifyMask, &toSend);
}

int XUnmapWindow(
Display*		display/* display */,
Window		window/* w */
){

    RECT theRect, invalidRect;
    POINT temp;
    int x, y;
    WINDOW_INFO *windowInfo;
/*	DISPLAY_INFO *dispInfo;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)display)))
	    return False;
*/
    //printf("Unmapping Window\n");
    if(!(windowInfo = XGetWindowInfoStruct(window)))
	    return False;

    ShowWindow((HWND)window, SW_HIDE);	
    XRestoreBacking(window);
    XSendUnmapNotifyEvent(display, window);

    XGetWindowPositionRelativeToParent(window, &x, &y);
    GetClientRect((HWND)window, &theRect);
    
    temp.x = x;
    temp.y = y;
    ScreenToClient((HWND)windowInfo->parent, &temp);
    invalidRect.left = temp.x;
    invalidRect.top = temp.y;
    temp.x = x + theRect.right;
    temp.y = y + theRect.bottom;
    ScreenToClient((HWND)windowInfo->parent, &temp);
    invalidRect.right = temp.x;
    invalidRect.bottom = temp.y;
    /*invalidRect.left = x;
    invalidRect.right = x + theRect.right;
    invalidRect.top = y;
    invalidRect.bottom = y + theRect.bottom;
*/
//	InvalidateRect((HWND)windowInfo->parent, &invalidRect, TRUE); 
//	UpdateWindow((HWND)windowInfo->parent);

//	XProcessMessageQueue(dispInfo);
    return True;
}

int XChangeWindowAttributes(
Display*		/* display */,
Window		/* w */,
unsigned long	/* valuemask */,
XSetWindowAttributes* /* attributes */
){

    return 0;
}

int XSetInputFocus(
Display*		/* display */,
Window		/* focus */,
int			/* revert_to */,
Time		/* time */
){

    return 0;
}

int XSetWindowBorderWidth(
Display*		/* display */,
Window		/* w */,
unsigned int	/* width */
){

    return 0;
}

int XMoveResizeWindow(
Display*		/* display */,
Window		/* w */,
int			/* x */,
int			/* y */,
unsigned int	/* width */,
unsigned int	/* height */
){

    return 0;
}

void XSleep(unsigned long milliseconds){

    Sleep((DWORD)milliseconds);
}

/*bool XStartEventThread(DISPLAY_INFO *theDisplay){

    if(theDisplay->eventThread)
	    return true;

    theDisplay->runEventThread = true;
    theDisplay->mainThreadID = GetCurrentThreadId();
    if(!(theDisplay->eventThread = CreateThread(NULL, NULL, XEventThread, theDisplay, CREATE_SUSPENDED,
										    &theDisplay->eventThreadID)))
	    return false;
    
    SetThreadPriority(theDisplay->eventThread, THREAD_PRIORITY_HIGHEST);
    ResumeThread(theDisplay->eventThread);

    return true;
}*/

/*bool XStopEventThread(DISPLAY_INFO *theDisplay){

    if(!theDisplay->eventThread)
	    return true;

    theDisplay->runEventThread = false;
    PostThreadMessage(theDisplay->eventThreadID, WAKEUP_MESSAGE, NULL, NULL);

    if(WaitForSingleObject(theDisplay->eventThread, 5000) != WAIT_OBJECT_0){
	    if(!TerminateThread(theDisplay->eventThread, 0))
		    return false;
    }
	    
    theDisplay->eventThread = NULL;
    theDisplay->eventThreadID = 0;

    return true;
}*/

/*DWORD _stdcall XEventThread(void *param){

    DISPLAY_INFO *display;
    MSG message;
    POSITION pos;
    void *key;
    HWND window;
    DWORD idleCount;
    BOOL messageProcessed;
    DWORD sleepTime;

    display = (DISPLAY_INFO*)param;

    GetMessagePos();

    if(!AttachThreadInput(GetCurrentThreadId(), display->mainThreadID, TRUE))
	    return 0;

    idleCount = 0;
    while(display->runEventThread){

	    messageProcessed = FALSE;
	    if(PeekMessage(&message, NULL, 0, 0, PM_REMOVE)){

		    messageProcessed = TRUE;
	    
		    if(message.message != WAKEUP_MESSAGE)
			    DispatchMessage(&message);
	    }

	    pos = fastWindowMap.GetStartPosition();

	    while(pos){
		    fastWindowMap.GetNextAssoc(pos, key, (void*&)window);

		    if(window == rootWindow->window)
			    continue;

		    while(PeekMessage(&message, window, 0, 0, PM_REMOVE)){
				    messageProcessed = TRUE;
				    DispatchMessage(&message);
		    }

	    }

	    idleCount = (messageProcessed) ? 0 : idleCount + 1;

	    sleepTime = idleCount * 10;

	    Sleep((sleepTime > MAX_SLEEP_TIME) ? MAX_SLEEP_TIME : sleepTime);

    }

    return 0;
}*/

void XProcessMessageQueue(DISPLAY_INFO *display){

    MSG message;

    //if(display->eventList.numEvents > 0)
    //	return;

    if(PeekMessage(&message, NULL, 0, 0, PM_REMOVE)){

	    if(display->eventList.numEvents >= EVENT_LIST_SIZE){
		    PostMessage(message.hwnd, message.message, message.wParam, message.lParam);
		    return;
	    }

//		GetMessage(&message, NULL, 0, 0);
		TranslateMessage(&message);
	    DispatchMessage(&message);
    }

    /*pos = fastWindowMap.GetStartPosition();

    while(pos){
	    fastWindowMap.GetNextAssoc(pos, key, (void*&)window);

	    if(window == rootWindow->window)
		    continue;

	    while(PeekMessage(&message, window, 0, 0, PM_REMOVE)){
			    DispatchMessage(&message);
	    }

    }*/
}

bool XInit(){

    WINDOW_INFO *winInfo;
    XID windowID;

    InitializeCriticalSection(&handleMapLock);
    InitializeCriticalSection(&fastWindowMapLock);
    XInitializeAtoms();

    if (!XSetupKeyboard()) {
	    printf("I failed to initialize the keyboard",
	    "x_display_rep::x_display_rep");
	    return false;
    }

    windowID = XCreateInfoStruct(TYPE_WINDOW_INFO);

    winInfo = (WINDOW_INFO*)XGetInfoStruct(windowID);

    winInfo->window = GetDesktopWindow();
    winInfo->isDesktopWindow = true;

    XSaveWindow(winInfo);

    rootWindow = winInfo;
    
    return true;
}

bool XSaveWindow(WINDOW_INFO *toStore){

    LOCK(fastWindowMapLock);
    fastWindowMap[toStore->window] = toStore;
    RELEASE(fastWindowMapLock);

    return true;
}

bool XDeleteWindow(WINDOW_INFO *toUnstore){

    LOCK(fastWindowMapLock);
    fastWindowMap.erase(toUnstore->window);
    RELEASE(fastWindowMapLock);

    return true;
}

bool XDestroy(){

    map<XID, void*>::iterator iter;
    void *key, *value;
    
    for(iter = handleMap.begin(); iter != handleMap.end(); iter++){

	    XFreeInfoStruct(iter->first);
    }

    DeleteCriticalSection(&handleMapLock);
    DeleteCriticalSection(&fastWindowMapLock);
    return true;
}

int _stdcall WinMain(HINSTANCE hInstance,
HINSTANCE hPrevInstance,
LPSTR lpCmdLine,
int nCmdShow
    ){

    char *commandLine[255];
    string temp;
    /*HMODULE mod;
    int (*addr) (int argc, char** argv);
    //find main method

    mod = GetModuleHandle(NULL);

    addr = (int (*) (int, char**))GetProcAddress(mod, "Xmain");

    if(addr == NULL){
	    MessageBox(NULL, "No main method found in program...exiting", 
		    "Fatal Error", MB_OK | MB_ICONERROR);
	    return 1;
    }

    addr(1, &lpCmdLine);*/

    saveInstance = hInstance;

    if(!XInit()){
	    MessageBox(NULL, "Could not initialize X subsystem", "Fatal Error", MB_OK | MB_ICONERROR);
	    return 0;
    }

    GetModuleFileName(GetModuleHandle(NULL), progName, 255);
    
    temp = progName;
    temp.erase(0, temp.find_last_of('\\') + 1);
    strcpy(progName, temp.c_str());
    commandLine[0] = progName;

    XOpenConsole(lpCmdLine);
	    SysMiscInitialize();
    main(1, commandLine);
    XCloseConsole();

    XDestroy();

    return 0;
}

_iobuf storeout, storeerr, storein;
void XOpenConsole(char * commandLine){

//	FILE temp;

    AllocConsole();

    memcpy(&storeout, stdout, sizeof(_iobuf));
    memcpy(&storeerr, stderr, sizeof(_iobuf));
    memcpy(&storein, stdin, sizeof(_iobuf));
    
    if(strstr(commandLine, "--debuglog") != NULL){
	    freopen("debug.log", "w", stdout);
	    freopen("debug.log", "w", stderr);
	    freopen("CONIN$", "r", stdin);
    }
    else{
	    freopen("CONOUT$", "w", stdout);
	    freopen("CONOUT$", "w", stderr);
	    freopen("CONIN$", "r", stdin);
    }
    cout.sync_with_stdio();
    cerr.sync_with_stdio();
    cin.sync_with_stdio();
//	TEXLOG_Initialize(LEVEL_DEBUG, NULL);
//	temp = CreateFile("CONOUT$", GENERIC_WRITE, NULL, NULL, OPEN_EXISTING, NULL, NULL);

//	SetStdHandle(STD_OUTPUT_HANDLE, temp);

//	stdout->_file = (int)temp;
}

void XCloseConsole(){

    FreeConsole();
    memcpy(stdout, &storeout, sizeof(_iobuf));
    memcpy(stderr, &storeerr, sizeof(_iobuf));
    memcpy(stdin, &storein, sizeof(_iobuf));
    cout.sync_with_stdio();
    cerr.sync_with_stdio();
    cin.sync_with_stdio();
}

void XDebugBreak(){

    DebugBreak();
}

void XGetMousePosInfo(MOUSE_INFO &info){

  POINT cursor;
  POINT hitTest;
  HWND window = NULL;
  UINT shownTime = 0;
  WINDOW_INFO *temp;

  map<HWND, WINDOW_INFO*>::iterator iter;

  GetCursorPos(&cursor);

  for(iter = fastWindowMap.begin(); iter != fastWindowMap.end(); iter++){

    if(iter->first == (HWND)rootWindow->window)
      continue;

    memcpy(&hitTest, &cursor, sizeof(POINT));
    ScreenToClient(iter->first, &hitTest);
    temp = iter->second;

    if((hitTest.x < 0) || (hitTest.y < 0) || (hitTest.x > (temp->width + temp->x)) 
	|| (hitTest.y > (temp->height + temp->y))){
    //  printf("Hit x=%d,y=%d Window x=%d,y=%d,outerX=%d, outerY=%d\n",
//		hitTest.x, hitTest.y, temp->x, temp->y, temp->x + temp->width, temp->y + temp->height);
      continue;
    }

 //   printf("Testing window %d with shown time of %d\n", iter->first, temp->lastShown);
    if(shownTime < temp->lastShown){
      window = iter->first;
      shownTime = temp->lastShown;
    }
  }

  if(!window){
    info.clientPoint.x = 0;
    info.clientPoint.y = 0;
    info.currentWindow = NULL;
  }
  else{
    ScreenToClient(window, &cursor);
    info.clientPoint.x = cursor.x;
    info.clientPoint.y = cursor.y;
    info.currentWindow = (Window)window;
  }
}

void XUpdateMouseMotion(){

  MOUSE_INFO oldMouseInfo;
  WINDOW_INFO *windowInfo;
  TRACKMOUSEEVENT track;

  memcpy(&oldMouseInfo, &theMouse, sizeof(MOUSE_INFO));
  XGetMousePosInfo(theMouse);
		    
  if((theMouse.clientPoint.x == oldMouseInfo.clientPoint.x)
      && (theMouse.clientPoint.y == oldMouseInfo.clientPoint.y)){
    return;
  } 

 // printf("The mouse is in window %d\n at x=%d, y=%d\n", 
  //    theMouse.currentWindow, theMouse.clientPoint.x, theMouse.clientPoint.y);

  track.cbSize = sizeof(TRACKMOUSEEVENT);
  track.dwFlags = TME_HOVER | TME_LEAVE;
  track.dwHoverTime = HOVER_DEFAULT;
  track.hwndTrack = (HWND)theMouse.currentWindow;
  _TrackMouseEvent(&track);

  if(theMouse.currentWindow == NULL){
   //  printf("Message - Mouse Leave window=%d\n", oldMouseInfo.currentWindow);
     windowInfo = XGetWindowInfoStruct(oldMouseInfo.currentWindow);
    windowInfo->containsMouse = FALSE;
    XSendCrossingEvent(rootDisplay, (Window)windowInfo->window, 
		      theMouse.clientPoint.x, 
		      theMouse.clientPoint.y, LeaveNotify, NotifyNormal, 
		      theMouse.buttonMask);
  }
  else if(theMouse.currentWindow != oldMouseInfo.currentWindow){
    windowInfo = XGetWindowInfoStruct(oldMouseInfo.currentWindow);
  
    if(windowInfo){
    //  printf("Message - Mouse Leave window=%d\n", oldMouseInfo.currentWindow);
      windowInfo->containsMouse = FALSE;
  
      XSendCrossingEvent(rootDisplay, oldMouseInfo.currentWindow, 
		      theMouse.clientPoint.x, 
		      theMouse.clientPoint.y, LeaveNotify, NotifyNormal, 
		      theMouse.buttonMask);
    }

   // printf("Message - Mouse Enter Window=%d\n", theMouse.currentWindow);
      windowInfo = XGetWindowInfoStruct(theMouse.currentWindow);
      windowInfo->containsMouse = TRUE;
    XSendCrossingEvent(rootDisplay, theMouse.currentWindow, 
			    theMouse.clientPoint.x, theMouse.clientPoint.y,
			    EnterNotify, NotifyNormal, theMouse.buttonMask);
	  
    XSendMotionEvent(rootDisplay, theMouse.currentWindow,
			  theMouse.clientPoint.x, theMouse.clientPoint.y,
			  NotifyNormal, theMouse.buttonMask);
  }
  else{
  //  printf("Message - Mouse Move window=%d\n", theMouse.currentWindow);
    XSendMotionEvent(rootDisplay, theMouse.currentWindow,
		      theMouse.clientPoint.x, theMouse.clientPoint.y,
		      NotifyNormal, theMouse.buttonMask);
  }
}

LRESULT CALLBACK XWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam){

    WINDOW_INFO *windowInfo;
    BOOL retVal;
    PAINTSTRUCT paintStruct;
    RECT rect;
    MINMAXINFO *minMaxInfo;
    WINDOWPOS *windowPos;
    DWORD var;
    TRACKMOUSEEVENT track;
    POINT point;
    HWND tempWnd;
    BYTE ks[256], tempChar;
    UINT word, i;
    Bool unicode = FALSE;
    Window currentWindow;
    map<HWND, WINDOW_INFO*>::iterator iter;

    windowInfo = XGetWindowInfoStruct((Window)hwnd);

    if(!windowInfo)
	    return DefWindowProc(hwnd, uMsg, wParam, lParam);

    switch(uMsg){

	    case WM_CREATE: 
	//	    printf("Message - Create window=%d\n", hwnd);
		    break;//return 0;
	    case WM_NCCREATE: break;//return TRUE;
	    case WM_SHOWWINDOW: windowInfo->lastShown = GetTickCount();
	    case WM_NCCALCSIZE:
				    if(!wParam)
					    break;//return 0;
				    else
					    break;//return WVR_ALIGNLEFT | WVR_ALIGNTOP | WVR_REDRAW;
	    case WM_NCPAINT:
    /*		if(wParam == 1){
				    GetWindowRect(hwnd, &rect);
				    XSendExposeEvent(rootDisplay, (Window)hwnd, rect.left, rect.top,
									    rect.right - rect.left,
									    rect.bottom - rect.top);
		    }
				    break;//return 0;*/
	    case WM_PAINT:
				    //GetWindowRect(GetDesktopWindow(), &paintStruct.rcPaint);
				    BeginPaint(hwnd, &paintStruct);
/*					printf("Message - Paint window=%d, x1=%d, y1=%d, x2=%d, y2=%d\n",
						    hwnd,
						    paintStruct.rcPaint.left, paintStruct.rcPaint.top,
						    paintStruct.rcPaint.right, paintStruct.rcPaint.bottom);*/
				    XSendExposeEvent(rootDisplay, (Window)hwnd, paintStruct.rcPaint.left,
								    paintStruct.rcPaint.top, 
								    paintStruct.rcPaint.right - paintStruct.rcPaint.left,
								    paintStruct.rcPaint.bottom - paintStruct.rcPaint.top);
				    EndPaint(hwnd, &paintStruct);
				    break;//return 0;
	    case WM_GETMINMAXINFO:
		    //	printf("Message - Get Min/Max Info\n");
			    if(!windowInfo){
				    break;//return 0;
			    }
			    else{
				    minMaxInfo = (MINMAXINFO*)lParam;
				    minMaxInfo->ptMaxSize.x = windowInfo->maxWidth;
				    minMaxInfo->ptMaxSize.y = windowInfo->maxHeight;
				    minMaxInfo->ptMaxTrackSize.x = windowInfo->maxWidth;
				    minMaxInfo->ptMaxTrackSize.y = windowInfo->maxHeight;
				    minMaxInfo->ptMinTrackSize.x = windowInfo->minWidth;
				    minMaxInfo->ptMinTrackSize.y = windowInfo->minHeight;
				    break;//return 0;
			    }				
	    case WM_WINDOWPOSCHANGING:
		    //	printf("Message - Pos Changing window=%d\n", hwnd);
			    //DefWindowProc(hwnd, uMsg, wParam, lParam);
			    windowPos = (WINDOWPOS*)lParam;
/*				windowInfo->x = (windowPos->flags & SWP_NOMOVE) ? windowInfo->x : windowPos->x;
			    windowInfo->y = (windowPos->flags & SWP_NOMOVE) ? windowInfo->y : windowPos->y;
			    windowInfo->width = (windowPos->flags & SWP_NOSIZE) 
				    ? windowInfo->width : windowPos->cx;
			    windowInfo->height = (windowPos->flags & SWP_NOSIZE) 
				    ? windowInfo->height : windowPos->cy;*/

			    if((windowPos->x < 0) || (windowPos->y < 0)){
	    //			printf("negative\n");
			    }
	    //		printf("Position Changing x=%d, y=%d, width=%d, height=%d\n", windowInfo->x, windowInfo->y,
	    //				windowInfo->width, windowInfo->height);
			    break;//return 0;
	    case WM_WINDOWPOSCHANGED:
		    //	printf("Message - Pos Changed window=%d\n", hwnd);
			    GetClientRect(hwnd, &rect);
			    if((windowInfo->y < 0) || (windowInfo->x < 0)){
    //				printf("negative\n");
			    }
//				XGetWindowPositionRelativeToParent((Window)hwnd, &windowInfo->x, &windowInfo->y);
			    if((windowInfo->y < 0) || (windowInfo->x < 0)){
				    printf("negative\n");
			    }
			    //DefWindowProc(hwnd, uMsg, wParam, lParam);
/*				windowPos = (WINDOWPOS*)lParam;
			    windowInfo->x = (windowPos->flags & SWP_NOMOVE) ? windowInfo->x : windowPos->x;
			    windowInfo->y = (windowPos->flags & SWP_NOMOVE) ? windowInfo->y : windowPos->y;
			    windowInfo->width = (windowPos->flags & SWP_NOSIZE) 
				    ? windowInfo->width : windowPos->cx;
			    windowInfo->height = (windowPos->flags & SWP_NOSIZE) 
				    ? windowInfo->height : windowPos->cy; */
			    windowInfo->width = rect.right;
			    windowInfo->height = rect.bottom;
			    XSendConfigureNotifyEvent(rootDisplay, (Window)hwnd, windowInfo->x, windowInfo->y, 
									    windowInfo->width, windowInfo->height);
	    //		printf("Position Changed x=%d, y=%d, width=%d, height=%d\n", windowInfo->x, windowInfo->y,
	    //				windowInfo->width, windowInfo->height);
			    break;//return 0;
	    case WM_SETFOCUS:
		  windowInfo->lastShown = GetTickCount();  
		  //	printf("Message - Set Focus window=%d\n", hwnd);
			    SetCursor(cursor);
			    break;//return 0;
	    case WM_ERASEBKGND:
//				printf("Message - Erase Background window=%d\n", hwnd);
			    GetClientRect(hwnd, &rect);
//				XSendExposeEvent(rootDisplay, (Window)hwnd, rect.left, rect.top,
//										rect.right - rect.left,
//										rect.bottom - rect.top);
			    break;
	    case WM_SIZE:
		    //	printf("Message - Size window=%d\n", hwnd);
    //			XGetWindowPositionRelativeToParent((Window)hwnd, &windowInfo->x, &windowInfo->y);
			    GetClientRect(hwnd, &rect);
			    windowInfo->width = rect.right - rect.left;
			    windowInfo->height = rect.bottom - rect.top;
//				windowInfo->x = LOWORD(lParam);
//				windowInfo->y = HIWORD(lParam);
			    XSendConfigureNotifyEvent(rootDisplay, (Window)hwnd, windowInfo->x, windowInfo->y, 
								    windowInfo->width, windowInfo->height);
		    //	UpdateWindow(hwnd);
			    //XSendExposeEvent(rootDisplay, (Window)hwnd, windowInfo->x, windowInfo->y, windowInfo->width, 
			    //					windowInfo->height);
			    break;
	    case WM_MOVE:
	     /* for(i = 0; i < windowInfo->numChildren; i++){
		
		GetWindowRect((HWND)windowInfo->children[i], &rect);
		MoveWindow((HWND)windowInfo->children[i], rect.left + (windowInfo->x - LOWORD(lParam)), 
			  rect.top + (windowInfo->x - HIWORD(lParam)), rect.right - rect.left,
			    rect.bottom - rect.top, TRUE);
	      }*/
		    //	printf("Message - Move window=%d\n", hwnd);
			    XGetWindowPositionRelativeToParent((Window)hwnd, &windowInfo->x, &windowInfo->y);
			//	windowInfo->x = LOWORD(lParam);
			//	windowInfo->y = HIWORD(lParam);
			    XSendConfigureNotifyEvent(rootDisplay, (Window)hwnd, windowInfo->x, windowInfo->y, 
								    windowInfo->width, windowInfo->height);
		    
		    //	InvalidateRgn(hwnd, NULL, TRUE);
		    //	UpdateWindow(hwnd);
	    //		printf("Move x=%d, y=%d, width=%d, height=%d\n", windowInfo->x, windowInfo->y,
		    //			windowInfo->width, windowInfo->height);
    //			XSendExposeEvent(rootDisplay, (Window)hwnd, windowInfo->x, windowInfo->y, windowInfo->width, 
    //								windowInfo->height); 
			    break;
	    case WM_QUIT:
	    case WM_DESTROY:
	    case WM_CLOSE:
			    //printf("Message - Destroy window=%d\n", hwnd);
			    XSendDestroyNotifyEvent(rootDisplay, (Window)hwnd);
			    return 0;
	    case WM_MOUSEMOVE:
		    var = 0;
		    if(wParam & MK_LBUTTON)
			    var |= Button1Mask;
		    if(wParam & MK_MBUTTON)
			    var |= Button2Mask;
		    if(wParam & MK_RBUTTON)
			    var |= Button3Mask;

		    XUpdateMouseMotion();
		    theMouse.buttonMask = var;
		    break;
	    case WM_MOUSELEAVE:
	      XUpdateMouseMotion();
	      break;

	    case WM_LBUTTONDOWN:
		//	printf("Message - Mouse Button Down window=%d\n", hwnd);
		    var = 0;
		    if(wParam & MK_LBUTTON)
			    var |= Button1Mask;
		    if(wParam & MK_MBUTTON)
			    var |= Button2Mask;
		    if(wParam & MK_RBUTTON)
			    var |= Button3Mask;

		    XUpdateMouseMotion();

		    //if(IsWindowEnabled((HWND)theMouse.currentWindow)){
		    //  BringWindowToTop((HWND)theMouse.currentWindow);
		    //}

		    XSendButtonEvent(rootDisplay, theMouse.currentWindow, 
		      		      theMouse.clientPoint.x, theMouse.clientPoint.y,
				      ButtonPress, Button1, theMouse.buttonMask);
		    theMouse.buttonMask = var;
		    break;
	    case WM_RBUTTONDOWN:
		//	printf("Message - Mouse Button Down window=%d\n", hwnd);
		    var = 0;
		    if(wParam & MK_LBUTTON)
			    var |= Button1Mask;
		    if(wParam & MK_MBUTTON)
			    var |= Button2Mask;
		    if(wParam & MK_RBUTTON)
			    var |= Button3Mask;

		    XUpdateMouseMotion();
		    
		    if(IsWindowEnabled((HWND)theMouse.currentWindow)){
		      BringWindowToTop((HWND)theMouse.currentWindow);
		    }
		    
		    XSendButtonEvent(rootDisplay, theMouse.currentWindow,
				  theMouse.clientPoint.x, theMouse.clientPoint.y,
						    ButtonPress, Button3, theMouse.buttonMask);
		    theMouse.buttonMask = var;
		    break;
	    case WM_MBUTTONDOWN:
		//	printf("Message - Mouse Button Down window=%d\n", hwnd);
		    var = 0;
		    if(wParam & MK_LBUTTON)
			    var |= Button1Mask;
		    if(wParam & MK_MBUTTON)
			    var |= Button2Mask;
		    if(wParam & MK_RBUTTON)
			    var |= Button3Mask;

		    XUpdateMouseMotion();
		    
		    if(IsWindowEnabled((HWND)theMouse.currentWindow)){
		      BringWindowToTop((HWND)theMouse.currentWindow);
		    }
		    
		    XSendButtonEvent(rootDisplay, theMouse.currentWindow, 
				      theMouse.clientPoint.x, theMouse.clientPoint.y,
						    ButtonPress, Button2, theMouse.buttonMask);
		    theMouse.buttonMask = var;
		    break;

	    case WM_RBUTTONUP:
		    var = 0;
		//	printf("Message - Mouse Button Up, window=%d\n", hwnd);
    
		    if(wParam & MK_LBUTTON)
			    var |= Button1Mask;
		    if(wParam & MK_MBUTTON)
			    var |= Button2Mask;
		    if(wParam & MK_RBUTTON)
			    var |= Button3Mask;

		    XUpdateMouseMotion();
		    

		    XSendButtonEvent(rootDisplay, theMouse.currentWindow, 
				    theMouse.clientPoint.x, theMouse.clientPoint.y,
		    				    ButtonRelease, Button3, theMouse.buttonMask);
		    theMouse.buttonMask = var;
		    break;
	    case WM_LBUTTONUP:
		    var = 0;
		//	printf("Message - Mouse Button Up, window=%d\n", hwnd);
    
		    if(wParam & MK_LBUTTON)
			    var |= Button1Mask;
		    if(wParam & MK_MBUTTON)
			    var |= Button2Mask;
		    if(wParam & MK_RBUTTON)
			    var |= Button3Mask;

		    XUpdateMouseMotion();

		    XSendButtonEvent(rootDisplay, theMouse.currentWindow,
				  theMouse.clientPoint.x, theMouse.clientPoint.y,
						    ButtonRelease, Button1, theMouse.buttonMask);
		    theMouse.buttonMask = var;
		    break;
	    case WM_MBUTTONUP:
		    var = 0;
		//	printf("Message - Mouse Button Up, window=%d\n", hwnd);
    
		    if(wParam & MK_LBUTTON)
			    var |= Button1Mask;
		    if(wParam & MK_MBUTTON)
			    var |= Button2Mask;
		    if(wParam & MK_RBUTTON)
			    var |= Button3Mask;

		    XUpdateMouseMotion();

		    XSendButtonEvent(rootDisplay, theMouse.currentWindow,
				    theMouse.clientPoint.x, theMouse.clientPoint.y,
						    ButtonRelease, Button2, theMouse.buttonMask);
		    theMouse.buttonMask = var;
		    break;
	    case WM_SYSKEYDOWN:
	    case WM_KEYDOWN:
		    var = 0;
		    word = 0;
		    tempChar = 0;
		    GetKeyboardState(ks);
		    retVal = 0;

		    if(ks[VK_CONTROL] > 0x00){
		      ks[VK_CONTROL] = 0x00;
		    }

		    if(keyMap.find(wParam) == keyMap.end()){
		      retVal = ToUnicode(wParam, 0,ks,(USHORT*)&word, 1, 0);
		      // printf("Unicode returned %d\n", retVal); 
		    }

		    if(retVal == -1){
		      // printf("Dead Key On\n");
		    }

		    else if(retVal == 0){
		      // printf("Cannot translate.....%d\n", word);
		      word = wParam;
		      unicode = FALSE;
		    }
		    else{
		      unicode = TRUE;
		    }
	
		    if ((tempChar & 0x01) || (GetKeyState(VK_SHIFT) < 0)){
			    // printf("Shift key on\n");
			    var |= ShiftMask;
		    }
		    if (tempChar & 0x02 || (GetKeyState(VK_CONTROL) < 0)){
			    // printf("Control key on\n");
			    var |= ControlMask;
		    }
		    if (GetKeyState(VK_CAPITAL) < 0){
			    // printf("Caps lock on\n");
			    var |= LockMask;
		    }

		    if ((tempChar & 0x04) || (GetKeyState(VK_MENU) < 0)){
			    // printf("Alt key on\n");
			    var |= Mod1Mask;
		    }

		    XSendKeyboardEvent (rootDisplay, (Window)hwnd, KeyPress, word, var, unicode);
		    break;
	    case WM_SYSKEYUP:
	    case WM_KEYUP:
		   var = 0;
		    word = 0;
		    tempChar = 0;
		    GetKeyboardState(ks);
		    retVal = 0;

		    if(ks[VK_CONTROL] > 0x00){
		      ks[VK_CONTROL] = 0x00;
		    }

		    if(keyMap.find(wParam) == keyMap.end()){
		      retVal = ToUnicode(wParam, 0,ks,(USHORT*)&word, 1, 0);
		      // printf("Unicode returned %d\n", retVal); 
		    }

		    if(retVal == -1){
		      // printf("Dead Key On\n");
		    }

		    else if(retVal == 0){
		      // printf("Cannot translate.....%d\n", word);
		      word = wParam;
		      unicode = FALSE;
		    }
		    else{
		      unicode = TRUE;
		    }
	
		    if ((tempChar & 0x01) || (GetKeyState(VK_SHIFT) < 0)){
			    // printf("Shift key on\n");
			    var |= ShiftMask;
		    }
		    if (tempChar & 0x02 || (GetKeyState(VK_CONTROL) < 0)){
			    // printf("Control key on\n");
			    var |= ControlMask;
		    }
		    if (GetKeyState(VK_CAPITAL) < 0){
			    // printf("Caps lock on\n");
			    var |= LockMask;
		    }

		    if ((tempChar & 0x04) || (GetKeyState(VK_MENU) < 0)){
			    // printf("Alt key on\n");
			    var |= Mod1Mask;
		    }
		    XSendKeyboardEvent (rootDisplay, (Window)hwnd, KeyRelease, word, var, unicode);
		    break;

	    default:
		    return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

Status XSendKeyboardEvent(Display *display, Window window, int type, int keycode, int state, Bool unicode) {

    WINDOW_INFO *windowInfo;
    DISPLAY_INFO *dispInfo;
    XEvent toSend;
    POINT screen, client;

    GetCursorPos(&screen);
    client.x = screen.x;
    client.y = screen.y;

    ScreenToClient((HWND)window, &client);

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)display)))
	    return False;

    if(!(windowInfo = (WINDOW_INFO*)XGetWindowInfoStruct(window)))
	    return False;

    toSend.xkey.type = type;
//	toSend.xkey.serial = 0;
    toSend.xkey.send_event = False;
    toSend.xkey.display = display;
    toSend.xkey.window = window;
    toSend.xkey.root = (Window)rootWindow->window;
//	toSend.xkey.subwindow = NULL;
    toSend.xkey.time = GetTickCount();
    toSend.xkey.x = client.x;
    toSend.xkey.y = client.y;
    toSend.xkey.x_root = screen.x;
    toSend.xkey.y_root = screen.y;
    toSend.xkey.state = state;
    toSend.xkey.keycode = keycode;
    toSend.xkey.same_screen = True;
    toSend.xkey.unicode = unicode;

    return XSendInternalEvent(display, window, False, 
	    (type == KeyPress) ? KeyPressMask : KeyReleaseMask, &toSend);
}

void XGetWindowPositionRelativeToParent(Window toGet, int *x, int *y){

    RECT clientRect, windowRect, parentRect;
    WINDOW_INFO *winInfo;
    POINT clientPoint, parentPoint;

    if(!(winInfo = XGetWindowInfoStruct(toGet)))
	    return;

    clientPoint.x = clientPoint.y = parentPoint.x = parentPoint.y = 0;
    //GetClientRect((HWND)toGet, &clientRect);
    //GetWindowRect((HWND)toGet, &windowRect);
//	ClientToScreen((HWND)toGet, &clientPoint);
    ClientToScreen((HWND)winInfo->parent, &parentPoint);
//	GetWindowRect((HWND)winInfo->parent, &parentRect);

    *x = clientPoint.x - parentPoint.x;
    *y = clientPoint.y - parentPoint.y;
    
}

Status XSendButtonEvent(Display *display, Window window, int x, int y, int type, int button, int state){

    XEvent toSend;
    POINT temp;

    temp.x = x;
    temp.y = y;

    ClientToScreen((HWND)window, &temp); 

    toSend.type = type;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = type;
    toSend.xany.window = window;
    toSend.xbutton.button = button;
    toSend.xbutton.display = display;
    toSend.xbutton.root = (Window)rootWindow->window;
    toSend.xbutton.same_screen = True;
    toSend.xbutton.send_event = False;
    toSend.xbutton.state = state;
    toSend.xbutton.subwindow = window;
    toSend.xbutton.time = GetTickCount();
    toSend.xbutton.type = type;
    toSend.xbutton.window = window;
    toSend.xbutton.x = x;
    toSend.xbutton.x_root = temp.x;
    toSend.xbutton.y = y;
    toSend.xbutton.y_root = temp.y;

    return XSendInternalEvent(display, window, False, 
		    (type == ButtonPress) ? ButtonPressMask : ButtonReleaseMask, &toSend);
}

Status XSendCrossingEvent(Display *display, Window window, int x, int y, int type, int mode, 
					      int state){

    XEvent toSend;
    POINT temp;

    temp.x = x;
    temp.y = y;

    ClientToScreen((HWND)window, &temp);

    toSend.type = type;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = type;
    toSend.xany.window = window;
    toSend.xcrossing.detail = 0;
    toSend.xcrossing.display = display;
    toSend.xcrossing.focus = True;
    toSend.xcrossing.mode = mode;
    toSend.xcrossing.root = (Window)rootWindow->window;
    toSend.xcrossing.same_screen = True;
    toSend.xcrossing.send_event = False;
    toSend.xcrossing.state = state;
    toSend.xcrossing.subwindow = window;
    toSend.xcrossing.time = GetTickCount();
    toSend.xcrossing.type = type;
    toSend.xcrossing.window = window;
    toSend.xcrossing.x = x;
    toSend.xcrossing.x_root = temp.x;
    toSend.xcrossing.y = y;
    toSend.xcrossing.y_root = temp.y;
    
    return XSendInternalEvent(display, window, False, 
						    (type == EnterNotify) ? EnterWindowMask : LeaveWindowMask, &toSend);
}

Status XSendMotionEvent(Display *display, Window window, int x, int y, int mode, int state){

    XEvent toSend;
    POINT temp;

    temp.x = x;
    temp.y = y;

    ClientToScreen((HWND)window, &temp);

    toSend.type = MotionNotify;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = MotionNotify;
    toSend.xany.window = window;
    toSend.xmotion.display = display;
    toSend.xmotion.is_hint = (mode == NotifyNormal) ? NotifyNormal : NotifyHint;
    toSend.xmotion.root = (Window)rootWindow->window;
    toSend.xmotion.same_screen = True;
    toSend.xmotion.send_event = False;
    toSend.xmotion.state = state;
    toSend.xmotion.subwindow = window;
    toSend.xmotion.time = GetTickCount();
    toSend.xmotion.type = MotionNotify;
    toSend.xmotion.window = window;
    toSend.xmotion.x = x;
    toSend.xmotion.x_root = temp.x;
    toSend.xmotion.y = y;
    toSend.xmotion.y_root = temp.y;

    return XSendInternalEvent(display, window, False, 
	    PointerMotionMask | (state) ? (ButtonMotionMask | state) : 0, &toSend);

}

Status XSendDestroyNotifyEvent(Display *display, Window window){

    XEvent toSend;

    toSend.type = DestroyNotify;
    toSend.xany.display = display;
    toSend.xany.send_event = False;
    toSend.xany.type = DestroyNotify;
    toSend.xany.window = window;
    toSend.xdestroywindow.display = display;
    toSend.xdestroywindow.event = window;
    toSend.xdestroywindow.send_event = False;
    toSend.xdestroywindow.type = DestroyNotify;
    toSend.xdestroywindow.window = window;

    return XSendInternalEvent(display, window, False, StructureNotifyMask, &toSend);
}

Status XSendConfigureNotifyEvent(Display *theDisplay, Window window,
							     int x, int y, int width, int height){

    XEvent toSend;
    WINDOW_INFO *windowInfo;
    DISPLAY_INFO *dispInfo;
    INTERNAL_XEVENT *current;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)theDisplay)))
	    return False;

    if(!(windowInfo = (WINDOW_INFO*)XGetWindowInfoStruct(window)))
	    return False;

    toSend.type = ConfigureNotify;
    toSend.xany.display = theDisplay;
    toSend.xany.send_event = False;
    toSend.xany.type = ConfigureNotify;
    toSend.xany.window = window;
    toSend.xconfigure.above = (Window)GetParent((HWND)window);
    toSend.xconfigure.border_width = windowInfo->border_width;
    toSend.xconfigure.display = theDisplay;
    toSend.xconfigure.event = window;
    toSend.xconfigure.height = height;
    toSend.xconfigure.override_redirect = windowInfo->override_direct;
    toSend.xconfigure.send_event = False;
    toSend.xconfigure.type = ConfigureNotify;
    toSend.xconfigure.width = width;
    toSend.xconfigure.window = window;
    toSend.xconfigure.x = x;
    toSend.xconfigure.y = y;

    current = dispInfo->eventList.eventListHead.next;
    
    while(current != &dispInfo->eventList.eventListTail){
	    
	    if((current->event.type == ConfigureNotify)){
		    toSend.xany.serial = current->event.xany.serial;
		    toSend.xconfigure.serial = current->event.xconfigure.serial;
		    memcpy(&current->event, &toSend, sizeof(XEvent));
		    return True;
	    }
	    current = current->next;
    }

    return XSendInternalEvent(theDisplay, window, False, StructureNotifyMask, &toSend);
}

XID XCreateInfoStruct(STRUCT_TYPE type){

    void *newStruct;
    unsigned int size;
    void *temp;
    map<XID, void*>::iterator iter;

    size = XGetStructSize(type);

    newStruct = malloc(size);

    if(!newStruct)
	    return NULL;

    memset(newStruct, 0, size);

    LOCK(handleMapLock);

    ((INFO_BASE*)newStruct)->type = type;
    
    while(handleMap.find(handleCounter) != handleMap.end())
		    handleCounter++;

    ((INFO_BASE*)newStruct)->handle = handleCounter;
    handleCounter++;

    handleMap[handleCounter - 1] = newStruct;
    RELEASE(handleMapLock);

    return ((INFO_BASE*)newStruct)->handle;
}

unsigned int XGetStructSize(STRUCT_TYPE type){

    if((type < 0) || (type >= TYPE_END_LIST))
	    return 0;

    return STRUCT_SIZES[type];
}

bool XIsValidHandle(XID toCheck, STRUCT_TYPE type){

    map<XID, void*>::iterator iter;

    if(!toCheck)
	    return false;

    LOCK(handleMapLock);
    iter = handleMap.find(toCheck);
    RELEASE(handleMapLock);

    if(iter == handleMap.end())
	    return false;

    if(((INFO_BASE*)iter->second)->type != type)
	    return false;

    return true;
}

void* XGetInfoStruct(XID toGet){

    map<XID, void*>::iterator iter;

    LOCK(handleMapLock);
    iter = handleMap.find(toGet);
    RELEASE(handleMapLock);

    if(iter == handleMap.end())
	    return NULL;

    return iter->second;
}

bool XFreeInfoStruct(XID toFree){

    void *infoStruct;
    BOOL retVal;

    if(!(infoStruct = XGetInfoStruct(toFree)))
	    return false;

    LOCK(handleMapLock);
    handleMap.erase(toFree);
    RELEASE(handleMapLock);

    if(!retVal)
	    return false;

    //add struct specific deallocation code here....when you feel like it

    free(infoStruct);

    return true;
}

Screen* ScreenOfDisplay(Display *dpy, int scr){

    DISPLAY_INFO *dispInfo;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)dpy)))
	    return 0;

    if(dispInfo->base.type != TYPE_DISPLAY_INFO)
	    return 0;

    return &dispInfo->disp.screens[scr];
}

int DefaultScreen(Display *dpy){

    DISPLAY_INFO *dispInfo;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)dpy)))
	    return 0;

    if(dispInfo->base.type != TYPE_DISPLAY_INFO)
	    return 0;

    return dispInfo->disp.default_screen;	
}

Window RootWindow(Display *dpy, int scr){
    
    DISPLAY_INFO *dispInfo;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)dpy)))
	    return 0;

    if(dispInfo->base.type != TYPE_DISPLAY_INFO)
	    return 0;

    return dispInfo->disp.screens[dispInfo->disp.default_screen].root;	
}

UINT DefaultDepth(Display *dpy, int scr){

    DISPLAY_INFO *dispInfo;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)dpy)))
	    return 0;

    if(dispInfo->base.type != TYPE_DISPLAY_INFO)
	    return 0;

    return dispInfo->disp.screens[dispInfo->disp.default_screen].root_depth;	
}

UINT DisplayWidth(Display *dpy, int scr){

    DISPLAY_INFO *dispInfo;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)dpy)))
	    return 0;

    if(dispInfo->base.type != TYPE_DISPLAY_INFO)
	    return 0;

    return dispInfo->disp.screens[dispInfo->disp.default_screen].width;	
}

UINT DisplayHeight(Display *dpy, int scr){

    DISPLAY_INFO *dispInfo;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)dpy)))
	    return 0;

    if(dispInfo->base.type != TYPE_DISPLAY_INFO)
	    return 0;

    return dispInfo->disp.screens[dispInfo->disp.default_screen].height;	
}

Colormap DefaultColormap(Display *dpy, int scr){

    DISPLAY_INFO *dispInfo;

    if(!(dispInfo = (DISPLAY_INFO*)XGetInfoStruct((XID)dpy)))
	    return 0;

    if(dispInfo->base.type != TYPE_DISPLAY_INFO)
	    return 0;

    return dispInfo->disp.screens[dispInfo->disp.default_screen].cmap;	
}

int XDialogChooseFile(char *directoryDefault, char *fileDefault){

    return True;
}

UINT XGetDrivesMask(){

    return GetLogicalDrives();
}

int XLookupString(
    XKeyEvent *event_struct, 
    char *buffer_return, 
    int bytes_buffer, 
    KeySym *keysym_return, 
    XComposeStatus *status_in_out){

    *keysym_return = XLookupKeysym(event_struct, 0); 
    return 1;
}

void* XLoadFunc(char *name){

    HMODULE lib = LoadLibrary("libguile.dll");

    void* funcPtr = GetProcAddress(lib, name);

    FreeLibrary(lib);

    return funcPtr;
}

/*bool XGetString(char *name, char *buffer, int bufferSize){

}

bool XStoreString(char *name, char *buffer){

}*/