
/******************************************************************************
* MODULE     : web_files.hpp
* DESCRIPTION: file handling via the web
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*                  2026  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WEB_FILES_H
#define WEB_FILES_H
#include "url.hpp"
#include "array.hpp"
#include "scheme.hpp"

void web_cache_invalidate (url u);

url get_from_web (url u);
url get_from_server (url u);
url get_from_ramdisc (url u);

bool save_to_server (url u, string s);

// HTTP requests
#if defined(QTTEXMACS) && AC_QT_MAJOR_VERSION >= 6

tree qt_http_from_json (string s);
int qt_http_post (string& ret, string url, array<string> headers_attr,
		  string data);
int qt_http_post (string& ret, string url, array<string> headers_attr,
		  tree data);
int qt_http_post (string& ret, string url, array<string> headers_attr,
		  array<string> attr);
bool qt_async_http_post (string url, array<string> headers_attr,
			 string data, object callback);
bool qt_async_http_post (string url, array<string> headers_attr,
			 tree data, object callback);
bool qt_async_http_post (string url, array<string> headers_attr,
			 array<string> attr, object callback);

inline tree
http_from_json (string s) {
  return qt_http_from_json (s); }

inline int
http_post (string& ret, string url,
	   array<string> headers_attr, string data) {
  return qt_http_post (ret, url, headers_attr, data); }

inline int
http_post_json (string& ret, string url,
		array<string> headers_attr, tree data) {
  return qt_http_post (ret, url, headers_attr, data); }

inline int
http_post_query (string& ret, string url, array<string> headers_attr,
		 array<string> attr) {
  return qt_http_post (ret, url, headers_attr, attr); }

inline bool
async_http_post (string url, array<string> headers_attr,
		 string data, object callback) {
  return qt_async_http_post (url, headers_attr, data, callback); }

inline bool
async_http_post_json (string url, array<string> headers_attr,
		      tree data, object callback) {
  return qt_async_http_post (url, headers_attr, data, callback); }

inline bool
async_http_post_query (string url, array<string> headers_attr,
		       array<string> attr, object callback) {
  return qt_async_http_post (url, headers_attr, attr, callback); }

#else

#include "convert.hpp"

inline tree
http_from_json (string s) {
  const int mode= JSON_NULL | JSON_BOOLEAN | JSON_NUMBER;
  return json_to_tree (s, mode); }

int http_post (string& ret, string url, array<string> headers_attr,
	       string data);
int http_post_json (string& ret, string url, array<string> headers_attr,
		    tree data);
int http_post_query (string& ret, string url, array<string> headers_attr,
		     array<string> attr);
bool async_http_post (string url, array<string> headers_attr,
		      string data, object callback);
bool async_http_post_json (string url, array<string> headers_attr,
			   tree data, object callback);
bool async_http_post_query (string url, array<string> headers_attr,
			    array<string> attr, object callback);
#endif

inline string
http_post (string url, array<string> headers_attr, string data) {
  string ret; 
  http_post (ret, url, headers_attr, data);
  return ret;
}

inline string
http_post_json (string url, array<string> headers_attr, tree data) {
  string ret; 
  http_post_json (ret, url, headers_attr, data);
  return ret;
}

inline string
http_post_query (string url, array<string> headers_attr, array<string> attr) {
  string ret; 
  http_post_query (ret, url, headers_attr, attr);
  return ret;
}

#endif // defined WEB_FILES_H
