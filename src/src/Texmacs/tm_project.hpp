
/******************************************************************************
* MODULE     : tm_project.hpp
* DESCRIPTION: Buffer management for TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_PROJECT_H
#define TM_PROJECT_H
#include "server.hpp"
#include "tm_buffer.hpp"

class tm_project_rep: public tm_data_rep {
protected:
  tm_buffer prj;                // current project being processed
  tree et;                      // the edit tree of the project
  path tp;                      // current path where the project is being read
  hashmap<string,tree> global;  // the global project environment

  void globalize_variable (string var); 
  void localize_variable (string var);
  void assign_variable (string var, tree t);
  void include_document (string s);
  void include_project (string s);

  bool start (tm_buffer buf);
  tree next ();
  void process (tree t);

  void project_update_menu ();
  void project_update_view (tm_view vw);
  void project_update_buffer (tm_buffer buf);
  void project_process_buffer (tm_buffer buf);

public:
  tm_project_rep ();
  ~tm_project_rep ();

  void project_attach (string prj_name);
  bool project_attached ();
  void project_compile_all ();
  void project_compile_buffer ();
};

#endif // defined TM_PROJECT_H
