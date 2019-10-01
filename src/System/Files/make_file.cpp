
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
  case CMD_APPLY_EFFECT:
    make_target= make_dir * (hex * ".png");
    break;
  }
  make_cache (key)= as_tree (make_target);

  // check whether the result has been cached on disk
  tree check= copy (key);
  for (int i=0; i<N(args); i++) {
    if (exists (args[i])) check << as_string (last_modified (args[i]));
    else check << "#f";
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

  // make the actual target
  switch (cmd) {
  case CMD_APPLY_EFFECT:
    {
      int w= 300, h= 300;
      if (N(args) > 0) native_image_size (args[0], w, h);
      //cout << "Size " << args[0] << " ~> " << w << ", " << h << LF;
      //cout << "Apply " << data << " to " << args
      //     << " ~> " << make_target << LF;
      apply_effect (data, args, make_target, w, h);
      break;
    }
  }
  return make_target;
}
