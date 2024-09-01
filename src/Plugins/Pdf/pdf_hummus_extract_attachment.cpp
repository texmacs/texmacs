/******************************************************************************
 * MODULE     : pdf_hummus_extract_attachment.cpp
 * DESCRIPTION: Interface for extract attachment file in pdf
 * COPYRIGHT  : (C) 2023 Tangdouer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/
#include "pdf_hummus_extract_attachment.hpp"

#include "analyze.hpp"
#include "file.hpp"
#include "new_buffer.hpp"
#include "sys_utils.hpp"

#include "PDFWriter/InputFileStream.h"
#include "PDFWriter/OutputBufferedStream.h"
#include "PDFWriter/OutputFileStream.h"
#include "PDFWriter/OutputStreamTraits.h"
#include "PDFWriter/PDFArray.h"
#include "PDFWriter/PDFLiteralString.h"
#include "PDFWriter/PDFObjectCast.h"
#include "PDFWriter/PDFParser.h"
#include "PDFWriter/PDFStreamInput.h"
#include "PDFWriter/PDFWriter.h"
#include "PDFWriter/SafeBufferMacrosDefs.h"
#include "PDFWriter/Trace.h"
using namespace PDFHummus;
using namespace IOBasicTypes;

bool
extract_attachments_from_pdf (url pdf_path, list<url>& names) {
  EStatusCode status= PDFHummus::eSuccess;
  InputFile   pdfFile;
  PDFParser   parser;
  do {
    status= pdfFile.OpenFile (as_charp (as_string (pdf_path)));
    if (status != PDFHummus::eSuccess) {
      if (DEBUG_CONVERT)
        debug_convert << "fail to open " << as_string (pdf_path) << LF;
      break;
    }
    parser.StartPDFParsing (pdfFile.GetInputStream ());
    PDFObjectCastPtr<PDFDictionary> catalog (
        parser.QueryDictionaryObject (parser.GetTrailer (), "Root"));
    // return 0;
    if (!catalog) {
      if (DEBUG_CONVERT) debug_convert << "Can't find catalog. fail" << LF;
      status= PDFHummus::eFailure;
      break;
    }

    PDFObjectCastPtr<PDFDictionary> d_1 (catalog->QueryDirectObject ("Names"));
    if (!d_1) {
      if (DEBUG_CONVERT)
        debug_convert << "Can't find Names dictionary. fail" << LF;
      status= PDFHummus::eFailure;
      break;
    }

    PDFObjectCastPtr<PDFDictionary> d_2 (
        d_1->QueryDirectObject ("EmbeddedFiles"));
    if (!d_2) {
      if (DEBUG_CONVERT)
        debug_convert << "Can't find /Names/EmbeddedFiles dictionary. fail"
                      << LF;
      status= PDFHummus::eFailure;
      break;
    }

    PDFObjectCastPtr<PDFArray> arr (d_2->QueryDirectObject ("Names"));
    if (!arr) {
      if (DEBUG_CONVERT)
        debug_convert << "Can't find /Names/EmbeddedFiles/Names array. fail"
                      << LF;
      status= PDFHummus::eFailure;
      break;
    }

    unsigned long n= arr->GetLength ();
    // Every two elements in the array represent an attachment
    if (n == 0) {
      if (DEBUG_CONVERT) debug_convert << "arr->GetLength () is 0" << LF;
      status= PDFHummus::eFailure;
      break;
    }
    if (n & 1) {
      if (DEBUG_CONVERT) debug_convert << "arr->GetLength () is wrong" << LF;
      status= PDFHummus::eFailure;
      break;
    }
    for (unsigned long i= 0; i < n; i+= 2) {
      PDFObjectCastPtr<PDFLiteralString> name (arr->QueryObject (i));
      if (!name) {
        if (DEBUG_CONVERT)
          debug_convert << "Can't find arr->QueryObject (" << i << ")" << LF;
        status= PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFDictionary> arr_d1 (arr->QueryObject (i + 1));
      if (!arr_d1) {
        if (DEBUG_CONVERT)
          debug_convert << "Can't find arr->QueryObject (" << i + 1 << ")"
                        << LF;
        status= PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFDictionary> arr_d2 (arr_d1->QueryDirectObject ("EF"));
      if (!arr_d2) {
        if (DEBUG_CONVERT) debug_convert << "Can't find arr_d2" << LF;
        status= PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFStreamInput> stream (
          parser.QueryDictionaryObject (arr_d2.GetPtr (), "F"));
      if (!stream) {
        if (DEBUG_CONVERT) debug_convert << "Can't find stream" << LF;
        status= PDFHummus::eFailure;
        break;
      }
      PDFDictionary* dir= stream->QueryStreamDictionary (); (void) dir;

      IByteReader* streamReader=
          parser.CreateInputStreamReader (stream.GetPtr ());
      if (!streamReader) {
        if (DEBUG_CONVERT) debug_convert << "Can't find streamReader" << LF;
        status= PDFHummus::eFailure;
        break;
      }

      url attachment_path=
          relative (pdf_path, url (string (name->GetValue ().c_str ())));
      OutputFile attachment_file;
      status= attachment_file.OpenFile (
          std::string (as_charp (as_string (attachment_path))));
      if (status != PDFHummus::eSuccess) {
        if (DEBUG_CONVERT)
          debug_convert << "fail to open " << as_string (attachment_path) << LF;
        break;
      }
      pdfFile.GetInputStream ()->SetPosition (stream->GetStreamContentStart ());
      OutputStreamTraits copy_help (
          (IByteWriter*) attachment_file.GetOutputStream ());
      status= copy_help.CopyToOutputStream (streamReader);
      if (status != PDFHummus::eSuccess) {
        if (DEBUG_CONVERT) debug_convert << "Can't CopyToOutputStream" << LF;
        break;
      }
      status= attachment_file.CloseFile ();
      if (status != PDFHummus::eSuccess) {
        if (DEBUG_CONVERT)
          debug_convert << "fail to close " << as_string (attachment_path)
                        << LF;
        break;
      }

      names= names * attachment_path;
      delete streamReader;
    }
  } while (false);
  if (status == PDFHummus::eFailure) return false;
  else return true;
}

bool
scm_extract_attachments (url pdf_path) {
  list<url> attachments_paths;
  bool      ret= extract_attachments_from_pdf (pdf_path, attachments_paths);
  return ret;
}
static hashset<string> internal_styles;

static void
declare_style (url u) {
  if (is_or (u)) {
    declare_style (u[1]);
    declare_style (u[2]);
  }
  else if (is_concat (u)) {
    string dir= upcase_first (as_string (u[1]));
    if (dir == "CVS" || dir == ".svn")
      ;
    else declare_style (u[2]);
  }
  else if (is_atomic (u)) {
    string s= as_string (u);
    if (ends (s, ".ts") && !starts (s, "source")) {
      internal_styles->insert (s (0, N (s) - 3));
      if (starts (s, "old-")) internal_styles->insert (s (4, N (s) - 3));
      if (starts (s, "old2-")) internal_styles->insert (s (5, N (s) - 3));
    }
  }
}

// Determine whether the style is a internal style
static bool
is_internal_style (string style) {
  if (N (internal_styles) == 0) {
    url sty_u= descendance ("$TEXMACS_PATH/styles");
    declare_style (sty_u);
    sty_u= descendance ("$TEXMACS_PATH/packages");
    declare_style (sty_u);
  }
  return internal_styles->contains (style);
}

// Pass in a image or include tree.
// return all include or image file url
static url
get_url_image_or_include_tree (tree t, url path) {
  if (is_atomic (t[0])) {
    url pre_url= url (get_label (t[0]));
    if (!exists (pre_url)) {
      pre_url= relative (path, pre_url);
      if (!exists (pre_url)) {
        if (DEBUG_CONVERT) debug_convert << pre_url << " do not exist" << LF;
      }
    }
    return pre_url;
  }
  else {
    if ((DEBUG_CONVERT) && is_func (t, INCLUDE))
      debug_convert << t << " include tree format wrong" << LF;
  }
  return url_none ();
}

// Pass in a tree with style label.
// return a actual ts file url
static url
get_actual_style_url (string style_name, url path) {
  url style_file;
  if (!is_internal_style (style_name)) {
    style_file= glue (url (style_name), ".ts");
    if (!exists (style_file)) {
      style_file= relative (path, style_file);
      if (!exists (style_file)) {
        if (DEBUG_CONVERT) debug_convert << style_file << "do not exist" << LF;
        style_file= url_none ();
      }
    }
  }
  return style_file;
}

// Pass in a style tree.
// return all external ts file url
static array<url>
get_url_style_tree (tree t, url path) {
  array<url> style_file;
  if (N (t) == 0) return style_file;
  if (get_label (t[0]) == "tuple") {
    for (int i= 0; i < N (t[0]); i++) {
      url style_url= get_actual_style_url (get_label (t[0][i]), path);
      if (!is_none (style_url)) style_file << style_url;
    }
  }
  else {
    if (!is_atomic (t[0])) {
      if (DEBUG_CONVERT)
        debug_convert << get_label (t[0]) << "is not atomic tree" << LF;
      return style_file;
    }
    url style_url= get_actual_style_url (get_label (t[0]), path);
    if (!is_none (style_url)) style_file << style_url;
  }
  return style_file;
}

array<url>
get_linked_file_paths (tree t, url path) {
  array<url> tm_and_linked_file;
  string     label= get_label (t);
  if (label == "image" || label == "include") {
    url incl_url= get_url_image_or_include_tree (t, path);
    if (incl_url != url ()) tm_and_linked_file << incl_url;
    return tm_and_linked_file;
  }
  if (label == "style") return get_url_style_tree (t, path);
  if (!is_atomic (t))
    for (int i= 0; i < N (t); i++)
      tm_and_linked_file << get_linked_file_paths (t[i], path);
  return tm_and_linked_file;
}

// Pass in an image or include tree and a path.
// change the url in tree to a url with the same path as the path.
static tree
replace_url_image_or_include_tree (tree t, url path) {
  if (get_label (t) != "image" && get_label (t) != "include") {
    if (DEBUG_CONVERT)
      debug_convert << get_label (t) << " is not image or include" << LF;
    return t;
  }
  if (is_atomic (t)) {
    if (DEBUG_CONVERT) debug_convert << get_label (t) << " is atomic" << LF;
    return t;
  }
  if (is_atomic (t[0])) {
    url pre_url= url (get_label (t[0]));
    if (!exists (pre_url)) {
      pre_url= relative (path, pre_url);
      if (!exists (pre_url)) {
        if (DEBUG_CONVERT) debug_convert << pre_url << " do not exist" << LF;
      }
    }
    string name= as_string (tail (pre_url));
    if (path != url ()) {
      name= as_string (relative (path, name));
    }
    t[0]->label= string (name);
  }
  else {
    if ((DEBUG_CONVERT) && is_func (t, INCLUDE))
      debug_convert << t << " include tree format wrong" << LF;
  }
  return t;
}

// Pass in an tree with style label and a path.
// change the label to a url with the same path as the path.
static tree
repalce_url_style (tree t, url path) {
  if (!is_atomic (t)) {
    if (DEBUG_CONVERT) debug_convert << get_label (t) << " is not atomic" << LF;
    return t;
  }
  string style_name= get_label (t);
  if (!is_internal_style (style_name)) {
    url style_url= url (style_name);
    style_url    = glue (style_url, ".ts");
    if (!exists (style_url)) {
      style_url= relative (path, style_url);
      if (!exists (style_url)) {
        if (DEBUG_CONVERT) debug_convert << style_url << "do not exist" << LF;
      }
    }
    string name= basename (style_url);
    if (path != url ()) {
      name= as_string (relative (path, name));
    }
    t->label= name;
  }
  return t;
}

// Pass in an style tree and a path.
// change the urls in style tree to a url with the same path as the path.
static tree
replace_url_style_tree (tree t, url path) {
  if (get_label (t) != "style") {
    if (DEBUG_CONVERT) debug_convert << get_label (t) << " is not style" << LF;
    return t;
  }
  if (is_atomic (t)) {
    if (DEBUG_CONVERT) debug_convert << get_label (t) << " is atomic" << LF;
    return t;
  }
  if (is_tuple (t[0]))
    for (int i= 0; i < N (t[0]); i++)
      repalce_url_style (t[0][i], path);
  else repalce_url_style (t[0], path);
  return t;
}

tree
replace_with_relative_path (tree t, url path) {
  string label= get_label (t);
  if (label == "image" || label == "include") {
    replace_url_image_or_include_tree (t, path);
    return t;
  }
  if (label == "style") {
    replace_url_style_tree (t, path);
    return t;
  }
  if (!is_atomic (t))
    for (int i= 0; i < N (t); i++)
      replace_with_relative_path (t[i], path);
  return t;
}

url
get_main_tm (url pdf_path) {
  list<url> attachments_paths;
  bool      ret= extract_attachments_from_pdf (pdf_path, attachments_paths);
  (void) ret;
  return attachments_paths[0];
}
