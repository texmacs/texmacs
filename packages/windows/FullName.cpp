
/******************************************************************************
* MODULE     : FullNmae.cpp
* DESCRIPTION: Get user's full name. 
*              Must be complied with Microsoft visual studio
* COPYRIGHT  : (C) 2015  Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/


#define _WIN32_DCOM
#include <iostream>
using namespace std;
#include <comdef.h>
#include <Wbemidl.h>

# pragma comment(lib, "wbemuuid.lib")

int main(int argc, char **argv)
{
  int ret = 0; //
  string serr;
  HRESULT hres;

  if(argc < 2) {serr="Usage : fullname username [-d]"; ret=1; } 
  else {
    // Step 1: Initialize COM. ------------------------------------------
    hres =  CoInitializeEx(0, COINIT_MULTITHREADED); 
    if (FAILED(hres)) { serr = "Failed to initialize COM library."; ret=12; }
    else {
      // Step 2: Set general COM security levels --------------------------
      hres =  CoInitializeSecurity(
        NULL, 
        -1,                          // COM authentication
        NULL,                        // Authentication services
        NULL,                        // Reserved
        RPC_C_AUTHN_LEVEL_DEFAULT,   // Default authentication 
        RPC_C_IMP_LEVEL_IMPERSONATE, // Default Impersonation  
        NULL,                        // Authentication info
        EOAC_NONE,                   // Additional capabilities 
        NULL                         // Reserved
      );
      if (FAILED(hres)) { serr = "Failed to initialize security."; ret=13; }
      else {
        // Step 3: Obtain the initial locator to WMI -------------------------
        IWbemLocator *pLoc = NULL;
        hres = CoCreateInstance(CLSID_WbemLocator, 0, CLSCTX_INPROC_SERVER, IID_IWbemLocator, (LPVOID *) &pLoc);
        if (FAILED(hres)) { serr =  "Failed to create IWbemLocator object."; ret=14; }
        else {
          // Step 4: Connect to WMI through the IWbemLocator::ConnectServer method
          IWbemServices *pSvc = NULL;
          // Connect to the root\cimv2 namespace with the current user and obtain pointer pSvc
          // to make IWbemServices calls.
          hres = pLoc->ConnectServer(
            _bstr_t(L"ROOT\\CIMV2"), // Object path of WMI namespace
            NULL,                    // User name. NULL = current user
            NULL,                    // User password. NULL = current
            0,                       // Locale. NULL indicates current
            NULL,                    // Security flags.
            0,                       // Authority (for example, Kerberos)
            0,                       // Context object 
            &pSvc                    // pointer to IWbemServices proxy
            );

          if (FAILED(hres)) { serr =  "Could not connect."; ret=15; }
          else {
            // Step 5: Set security levels on the proxy -------------------------
            hres = CoSetProxyBlanket(
              pSvc,                        // Indicates the proxy to set
              RPC_C_AUTHN_WINNT,           // RPC_C_AUTHN_xxx
              RPC_C_AUTHZ_NONE,            // RPC_C_AUTHZ_xxx
              NULL,                        // Server principal name 
              RPC_C_AUTHN_LEVEL_CALL,      // RPC_C_AUTHN_LEVEL_xxx 
              RPC_C_IMP_LEVEL_IMPERSONATE, // RPC_C_IMP_LEVEL_xxx
              NULL,                        // client identity
              EOAC_NONE                    // proxy capabilities 
              );
            if (FAILED(hres)) { serr =  "Could not set proxy blanket."; ret=16; }
            else {
              // Step 6: Use the IWbemServices pointer to make requests of WMI ----
              IEnumWbemClassObject* pEnumerator = NULL;
              string qs("SELECT * FROM Win32_UserAccount where Name='");
              qs += argv[1]; qs += "'";
              hres = pSvc->ExecQuery(
                bstr_t("WQL"), 
                bstr_t(qs.c_str()),
                WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY, 
                NULL,
                &pEnumerator);
                if (FAILED(hres)) { serr =  "Query failed."; ret=17; }
                else {
                  // Step 7: Get the data from the query in step 6 -------------------
                  IWbemClassObject *pclsObj;
                  ULONG uReturn = 0;
                  if (!pEnumerator)  { serr = "No iterator"; ret=18; }
                  else {
                    hres = pEnumerator->Next(WBEM_INFINITE, 1, &pclsObj, &uReturn);
                    if (FAILED(hres)) { serr =  "Query failed."; ret=19; }
                    else {
                      if(1 == uReturn) {
                        VARIANT vtProp;
                        hres = pclsObj->Get(L"FullName", 0, &vtProp, 0, 0);
                        if (FAILED(hres)) { serr =  "Get methode failed."; ret=20; }
                        else {
                          wcout << vtProp.bstrVal;
                          VariantClear(&vtProp);
                        }
                        pclsObj->Release();
                      } else  { serr = "No item found"; ret=3; }
                    }
                  }
                  pEnumerator->Release();
                }
            }
            pSvc->Release();
          }
          pLoc->Release();
        }
        CoUninitialize();
      }
    }
  }
  if(ret && argc > 2 && !strcmp(argv[2], "-d")) {
    cerr << serr.data();
    if(ret >= 10) cerr << " Error code = 0x" << hex << hres;
    cerr << endl;
  }
  return ret;
}

