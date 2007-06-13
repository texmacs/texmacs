
/******************************************************************************
* MODULE     : disk_table.cpp
* DESCRIPTION: large size string->string tables stored on disk
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"
#include "file.hpp"

transaction read_table_from_disk (url u);

/******************************************************************************
* Subroutines for hashing
******************************************************************************/

string
mix (string s) {
  unsigned char c= 0;
  int i, n= N(s);
  string r= copy (s);
  for (i=n-1; i>=0; i--) {
    c= s[i] ^ (c<<1) ^ (c>>7) ^ (c<<3) ^ (c>>5);
    r[i]= c;
  }
  return r;
}

int
hash_index (string s, int level) {
  int i= level >> 1;
  if (i >= N(s)) return 0;
  string r= mix (s);
  int h= ((unsigned char) r[i]);
  if ((level&1) == 0) return h&15;
  else return h>>4;
}

url
subdir (url u, int index) {
  return u * string ((char) ('A' + index));
}

/******************************************************************************
* Encoding of a tables on disk
******************************************************************************/

string
disk_encode_string (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (((unsigned char) s[i]) < 32) r << '%' << (s[i]+64);
    else if (s[i] == '%') r << '%' << '%';
    else r << s[i];
  return r;
}

string
disk_decode_string (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] != '%') r << s[i];
    else {
      if (++i >= n) break;
      if (s[i] == '%') r << '%';
      else r << (s[i]-64);
    }
  return r;
}

string
disk_encode_collection (collection t) {
  string r;
  iterator<string> it= iterate (t);
  while (it->busy ()) {
    string s= it->next ();
    r << '\t';
    if (t[s] < 0) r << '!';
    if (N(s) != 0 && s[0] == '!') r << '%';
    r << s;
  }
  return r;
}

collection
disk_decode_collection (string s) {
  int i, n= N(s);
  collection t;
  for (i=0; i<n; ) {
    int eps= 1;
    if (s[i] == '\t') i++;
    if (i<n && s[i] == '!') { i++; eps= -eps; }
    if (i+1<n && s[i] == '%' && s[i+1] == '!') i++;
    int start= i;
    for (; i<n; i++)
      if (s[i] == '\t') break;
    t (disk_decode_string (s (start, i)))= eps;
  }
  return t;
}

string
disk_encode_transaction (transaction t) {
  string r;
  iterator<string> it= iterate (t);
  while (it->busy()) {
    string s= it->next();
    r << disk_encode_string (s) << disk_encode_collection (t[s]) << "\n";
  }
  return r;
}

transaction
disk_decode_transaction (string s) {
  int i, n= N(s);
  transaction t;
  for (i=0; i<n; i++) {
    int start= i;
    for (; i<n; i++)
      if (s[i] == '\t' || s[i] == '\n') break;
    string key= disk_decode_string (s (start, i));
    start= i;
    for (; i<n; i++)
      if (s[i] == '\n') break;
    t(key)= disk_decode_collection (s (start, i));
  }
  return t;
}

/******************************************************************************
* Writing values to disk
******************************************************************************/

void
write_to_disk (url u, transaction t, int level) {
  //cout << "Write " << u << ", " << t << ", " << level << "\n";
  if (exists (u * "index")) {
    transaction h= read_table_from_disk (u);
    merge (h, t);
    t= h;
  }
  if (total_size (t) < (1000 << level)) {
    if (!exists (u)) mkdir (u);
    save_string (u * "index", disk_encode_transaction (t), false);
  }
  else {
    transaction h[16];
    iterator<string> it= iterate (t);
    while (it->busy()) {
      string s= it->next();
      int i= hash_index (s, level);
      h[i](s)= t[s];
    }
    for (int i=0; i<16; i++)
      if (N(h[i]) != 0)
	write_to_disk (subdir (u, i), h[i], level+1);
    remove (u * "index");
  }
}

void
write_to_disk (url u, string key, string val, int eps, int level) {
  //cout << "Write " << u << ", " << key << ", " << val << ", " << eps
  //<< ", " << level << "\n";
  if (!exists (u)) mkdir (u);
  if (eps < 0) {
    remove (u * key);
    int index= hash_index (key, level);
    if (exists (subdir (u, index)))
      write_to_disk (subdir (u, index), key, val, eps, level + 1);
  }
  else {
    bool error_flag;
    if (N (read_directory (u, error_flag)) <= 32)
      save_string (u * key, val, false);
    else {
      int index= hash_index (key, level);
      write_to_disk (subdir (u, index), key, val, eps, level + 1);
    }
  }
}

void
write_to_disk (url u, transaction t) {
  write_to_disk (u, filter (t, false), 0);
  transaction t2= filter (t, true);
  iterator<string> it= iterate (t2);
  while (it->busy ()) {
    string key= it->next ();
    string val= first (t2[key]);
    write_to_disk (u, key, val, t2[key][val], 0);
  }
}

void
disk_table_rep::write (transaction t) {
  write_to_disk (root, t);
}

/******************************************************************************
* Reading values from disk
******************************************************************************/

transaction
read_table_from_disk (url u) {
  string contents;
  load_string (u * "index", contents, false);
  return disk_decode_transaction (contents);
}

transaction
read_from_disk (url u, collection keys, int level) {
  transaction t;
  if (N(keys) == 0 || !exists (u)) return t;
  //cout << "Read " << u << ", " << keys << ", " << level << "\n";

  collection c[16];
  iterator<string> it= iterate (keys);
  while (it->busy()) {
    string s= it->next();
    int i= hash_index (s, level);
    c[i](s)= keys[s];
  }
  for (int i=0; i<16; i++) {
    transaction d= read_from_disk (subdir (u, i), c[i], level+1);
    merge (t, d);
  }

  transaction d;
  if (exists (u * "index")) d= read_table_from_disk (u);
  it= iterate (keys);
  while (it->busy ()) {
    string s= it->next ();
    if (d->contains (s)) merge (t, atom (s, d[s]));
    if (keys[s] == 3 && exists (u * s)) {
      string contents;
      if (!load_string (u * s, contents, false))
	merge (t, atom (s, singleton (contents, 3)));
    }
  }

  //cout << "Return " << u << ", " << keys << ", " << t << "\n";
  return t;
}

transaction
disk_table_rep::read (collection keys) {
  return read_from_disk (root, keys, 0);
}
