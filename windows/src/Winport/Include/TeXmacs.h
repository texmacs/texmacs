
/******************************************************************************
* MODULE     : TeXmacs.h
* DESCRIPTION: Include file for communication of extern packages with TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*******************************************************************************
* The TeXmacs communication protocol works as follows:
*   - First of all, the protocol is dynamic. All protocols define
*     the abstract structures 'TeXmacs_exports' and 'package_exports'.
*     These structures contain the version of the protocol used
*     and the versions of TeXmacs and the package
*   - Version n of the concrete protocol implements two structures
*     'TeXmacs_exports_n' and 'package_exports_n'.
*     The first structure contains all routines and data of TeXmacs,
*     which may be necessary for the package.
*     The structure 'package_exports' contains all routines and data
*     of the package, which should be visible for TeXmacs
*   - Now the package has to be a dynamically linkable library,
*     which implements a function
*        package_exports* get_my_package (int version);
*     In order to link your package to TeXmacs, you will now have to
*     call the TeXmacs function
*        void package_declare (string name, string lib, string f, string init);
*     Here 'name' is the name of your package under TeXmacs,
*     'lib' the name of the library, 'f' the function 'get_my_package' and
*     'init' an initialization string.
******************************************************************************/

#ifndef __TEXMACS_H
#define __TEXMACS_H

#if defined (__cplusplus)
extern "C" {
#endif

/******************************************************************************
* Any communication protocol contains at least the following
******************************************************************************/

typedef struct TeXmacs_exports {
  char* version_protocol;
  char* version_TeXmacs;
} TeXmacs_exports;

typedef struct package_exports {
  char* version_protocol;
  char* version_package;
} package_exports;

/******************************************************************************
* The first TeXmacs <-> package communication protocol from September 1999
******************************************************************************/

typedef struct TeXmacs_exports_1 {
  char* version_protocol; /* "TeXmacs communication protocol 1" */
  char* version_TeXmacs;
} TeXmacs_exports_1;

typedef struct package_exports_1 {
  char* version_protocol; /* "TeXmacs communication protocol 1" */
  char* version_package;
  
  char* (*install) (TeXmacs_exports_1* TeXmacs,
		    char* options, char** errors);
  /* Installation routine for extern package.
     TeXmacs: pointer to routines exported by TeXmacs
     options: a string with installation option (freed by TeXmacs)
     *errors: contains error and warning messages (freed by TeXmacs)
     returned string: status of installation (freed by TeXmacs)
                    : NULL indicates a pure error */

  char* (*evaluate) (char* what, char* session, char** errors);
  /* Interactive evaluation routine for shells.
     what: string to be evaluated (freed by TeXmacs)
     session: name of your session ("default" by default, freed by TeXmacs)
     *errors: contains error and warning messages (freed by TeXmacs)
     returned string: result of the evaluation (freed by TeXmacs)
                    : NULL indicates a pure error */

  char* (*execute) (char* what, char* session, char** errors);
  /* Alternative routine for executing strings,
     used for controlling the interaction between TeXmacs and the package.
     what: string to be executed (freed by TeXmacs)
     session: name of your session ("default" by default, freed by TeXmacs)
     *errors: contains error and warning messages (freed by TeXmacs)
     returned string: result of the evaluation (freed by TeXmacs)
                    : NULL indicates a pure error */
} package_exports_1;

#if defined (__cplusplus)
}
#endif

#endif /* __TEXMACS_H */
