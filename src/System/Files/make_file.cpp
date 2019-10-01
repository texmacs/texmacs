
/******************************************************************************
* MODULE     : make_file.cpp
* DESCRIPTION: automatically make and cache auxiliary files using commands
* COPYRIGHT  : (C) 2019  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "web_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "convert.hpp"
#include "image_files.hpp"

/******************************************************************************
* Make
******************************************************************************/

static hashmap<tree,tree> make_cache;

url
make_file (int cmd, tree data, array<url> args) {
  // check whether result has been cached
  tree key (TUPLE, as_string (cmd), data);
  for (int i=0; i<N(args); i++)
    key << as_string (args[i]);
  if (make_cache->contains (key))
    return as_url (make_cache[key]);

  // generate target and check names
  url make_dir= "$TEXMACS_HOME_PATH/system/make";
  int code= hash (key) & 0x7fffffff;
  string hex= as_hexadecimal (code);
  url make_target= make_dir * url (hex);
  url make_check = make_dir * url (hex * ".check");
  switch (cmd) {
  case CMD_GET_FROM_WEB:
    make_target= make_dir * (hex * "." * suffix (args[0]));
    break;
  case CMD_GET_FROM_SERVER:
    // FIXME: we should determine the appropriate format
    make_target= make_dir * (hex * "." * suffix (args[0]));
    break;
  case CMD_APPLY_EFFECT:
    make_target= make_dir * (hex * ".png");
    break;
  }
  make_cache (key)= as_tree (make_target);

  // check whether the result has been cached on disk
  tree check= copy (key);
  for (int i=0; i<N(args); i++) {
    if (is_rooted_web (args[i]) || is_rooted_tmfs (args[i]))
      check << "#f";
    else if (exists (args[i]))
      check << as_string (last_modified (args[i]));
    else
      check << "#f";
  }
  if (exists (make_target) && exists (make_check)) {
    string s;
    if (!load_string (make_check, s, false)) {
      tree old_check= scheme_to_tree (s);
      if (is_tuple (old_check) && N(old_check) == N(check)) {
        bool ok= true;
        for (int i=0; i<N(check); i++)
          ok= ok && (old_check[i] == check[i]);
        if (ok) return make_target;
      }
    }
  }
  save_string (make_check, tree_to_scheme (check));
  //cout << "Make " << cmd << ", " << data << ", " << args << LF;

  // fetch files that are not on disk
  if (cmd != CMD_GET_FROM_WEB && cmd != CMD_GET_FROM_SERVER) {
    array<url> bis;
    for (int i=0; i<N(args); i++)
      if (is_rooted_web (args[i]))
        bis << make_file (CMD_GET_FROM_WEB, "", range (args, i, i+1));
      else if (is_rooted_tmfs (args[i])) {
        string name= as_string (args[i]);
        if (starts (name, "tmfs://artwork/")) {
          url local ("$TEXMACS_HOME_PATH/misc/" * name (15, N(name)));
          if (exists (local)) { bis << local; continue; }
        }
        bis << make_file (CMD_GET_FROM_SERVER, "", range (args, i, i+1));
      }
      else bis << args[i];
    args= bis;
  }
  
  // make the actual target
  switch (cmd) {
  case CMD_GET_FROM_WEB:
    {
      system_wait ("Fetching web file", as_string (args[0]));
      url local= get_from_web (args[0]);
      if (!is_none (local)) move (local, make_target);
      system_wait ("");
      break;
    }
  case CMD_GET_FROM_SERVER:
    {
      url local= get_from_server (args[0]);
      if (!is_none (local)) move (local, make_target);
      break;
    }
  case CMD_APPLY_EFFECT:
    {
      system_wait ("Applying image effect");
      int w= 300, h= 300;
      if (N(args) > 0) native_image_size (args[0], w, h);
      //cout << "Size " << args[0] << " ~> " << w << ", " << h << LF;
      //cout << "Apply " << data << " to " << args
      //     << " ~> " << make_target << LF;
      apply_effect (data, args, make_target, w, h);
      system_wait ("");
      break;
    }
  }
  return make_target;
}
