/******************************************************************************
* MODULE     : windows32_stacktrace.cpp
* DESCRIPTION: Windows get_stacktrace function
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "string.hpp"
#include <windows.h>
#include <dbghelp.h>
#include <sstream>
#include <vector>

string get_stacktrace(unsigned int max_frames) {
  HANDLE process = GetCurrentProcess();
  SymInitialize(process, NULL, TRUE);
  SymSetOptions(SYMOPT_LOAD_LINES | SYMOPT_UNDNAME);

  // Capture the stack frames
  std::vector<void*> stack(max_frames);
  WORD num_frames = CaptureStackBackTrace(0, max_frames, stack.data(), nullptr);

  // Resolve the addresses into function names
  SYMBOL_INFO *symbol = (SYMBOL_INFO*)calloc(sizeof(SYMBOL_INFO) + 256, 1);
  symbol->MaxNameLen = 255;
  symbol->SizeOfStruct = sizeof(SYMBOL_INFO);

  char undecorated_name[256];

  std::stringstream ss;
  for (WORD frame_index = 0; frame_index < num_frames; frame_index++) {

    // Load module for the address if necessary
    DWORD address = (DWORD)(stack[frame_index]);

    IMAGEHLP_MODULE moduleInfo;
    moduleInfo.SizeOfStruct = sizeof(IMAGEHLP_MODULE);
    if (!SymGetModuleInfo(process, address, &moduleInfo)) {
      SymLoadModule(process, NULL, NULL, NULL, address, 0);
    }

    bool res = SymFromAddr(process, (DWORD)(stack[frame_index]), 0, symbol);
    if (!res) {
      // get and show the error
      DWORD error = GetLastError();
      LPVOID lpMsgBuf;
      FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                    FORMAT_MESSAGE_FROM_SYSTEM |
                    FORMAT_MESSAGE_IGNORE_INSERTS,
                    NULL, error, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
                    (LPTSTR)&lpMsgBuf, 0, NULL);
      ss << "  " << frame_index << ": <unknown error: " 
         << (char*)lpMsgBuf << ">" << std::endl;
      continue;
    }
    res = UnDecorateSymbolName(symbol->Name, undecorated_name, 
                               256, UNDNAME_COMPLETE);
    if (!res) {
      ss << "  " << frame_index << ": <unknown function: " 
         << symbol->Name << ">";
      continue;
    }
    ss << "  " << frame_index << ": " << undecorated_name;

    // get filename and line number
    DWORD displacement;
    IMAGEHLP_LINE line;
    line.SizeOfStruct = sizeof(IMAGEHLP_LINE);
    res = SymGetLineFromAddr(process, (DWORD)(stack[frame_index]), 
                             &displacement, &line);
    if (res) {
      ss << " (" << line.FileName << ":" << line.LineNumber << ")" << std::endl;
    } else {
      ss << std::endl;
    }
  
  }

  // Clean up
  SymCleanup(process);
  free(symbol);

  std::string result = ss.str();

  return string(result.c_str(), result.size());
}