
/******************************************************************************
* MODULE     : WINPrint.hpp
* DESCRIPTION: routines for the editor
* COPYRIGHT  : (C) 2013  Denis Raux
*  printing for windows using poppler and Qt
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/


#if defined (QTTEXMACS) && defined (OS_MINGW)

class WINPrint {
public:
   WINPrint(QString url,bool &IsLandscape);
   ~WINPrint();
   int first_page,last_page;
   bool doit;   
private:
   static QPrinter *Prt;  // static to keep user choice along texmacs session
   QString file;
   
};
 #endif
