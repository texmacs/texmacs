/******************************************************************************
* MODULE     : winwallet.cpp
* DESCRIPTION: To store TeXmacs password into the operating system.
*              Must be complied with Microsoft visual studio
* COPYRIGHT  : (C) 2015  Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <windows.h>
#include <wincred.h>
#include <tchar.h>
#include <io.h>
#include <stdio.h>
#include <winbase.h>

#pragma hdrstop

#define ARG_CMD		1
#define ARG_APP		2
#define ARG_USER	3
#define CMD_ADD		"ADD"			// Read password on standard input
#define CMD_GETP	"GETP"
#define CMD_GETN	"GETN"
#define CMD_RM		"RM"

#define ERR_OK				0
#define ERR_GENE			1 
#define ERR_CMD				2
#define ERR_ARG				3
#define ERR_TOOLONG		4
#define ERR_CRED			5
#define ERR_WR				6
#define ERR_RD				7

int _return(int err) {
	char *msg = NULL;
	switch(err) {
	case ERR_OK: break;
	case ERR_CMD:		msg= "Unknown command\n";break;
	case ERR_ARG:		msg= "Bad arguments count\nusage: winwallet CMD Application [user] ";break;
	case ERR_TOOLONG:	msg= "Password too long\n";break;
	case ERR_CRED:	msg= "Credential interface error\n";break;
	case ERR_WR:		msg= "Cannot output info\n";break;
	case ERR_RD:		msg= "CAnnot read password\n";break;
	case ERR_GENE:
	default :				msg= "General error\n";break;
	}
	if(msg) _write(2, msg, strlen(msg));
	return(err);
}

int  main (int argc, char *argv[])
{
	int ret= ERR_OK;

  if(argc < 2) ret= ERR_ARG;
  else {
    if(!strcmp(argv[ARG_CMD], CMD_ADD)) {
      if(argc != 4) ret= ERR_ARG;
      else {
        char buf[CRED_MAX_CREDENTIAL_BLOB_SIZE+1];
        int cnt;
        cnt= _read(0, buf, sizeof(buf));
        if(cnt == -1) ret= ERR_RD;
        else if(cnt == sizeof(buf)) ret= ERR_TOOLONG;
        else {
          CREDENTIAL cred = {0};
          cred.Type = CRED_TYPE_GENERIC;
          cred.TargetName = LPSTR(argv[ARG_APP]);
          cred.CredentialBlobSize= cnt;
          cred.CredentialBlob= (LPBYTE)buf;
          cred.Persist= CRED_PERSIST_LOCAL_MACHINE;
          cred.UserName= LPSTR(argv[ARG_USER]);
          ret= ::CredWrite (&cred, 0)? ERR_OK:ERR_CRED;
        }
      }
    } else if(argc != 3) ret= ERR_ARG;
    else if(!strcmp(argv[ARG_CMD], CMD_GETP) || !strcmp(argv[ARG_CMD], CMD_GETN)) {
      PCREDENTIAL pcred;
      if(::CredRead (LPSTR(argv[ARG_APP]), CRED_TYPE_GENERIC, 0, &pcred)) {
        if(!strcmp(argv[ARG_CMD], CMD_GETP)) {
          if(_write(1, pcred->CredentialBlob,pcred->CredentialBlobSize) == -1) ret= ERR_WR;

        } else {
          if(_write(1, pcred->UserName, strlen(pcred->UserName)) == -1) ret= ERR_WR;
        }
        // must free memory allocated by CredRead()!
        ::CredFree (pcred);
      } else ret= ERR_CRED;
    } else if(!strcmp(argv[ARG_CMD], CMD_RM)) {
      if(::CredDelete (LPCSTR(argv[ARG_APP]), CRED_TYPE_GENERIC, 0) == false) ret= ERR_CRED;
    } else ret= ERR_CMD;
  }
  return(_return(ret));
}