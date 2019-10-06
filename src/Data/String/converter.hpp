
/******************************************************************************
* MODULE     : converter.hpp
* DESCRIPTION: Applies dictionaries to strings in an efficient manner.
* COPYRIGHT  : (C) 2002  Felix Breuer
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CONVERTER_H
#define CONVERTER_H
#include "resource.hpp"
#include "hashtree.hpp"
#include "file.hpp"

enum escape_type { NOESCAPES, BIT2BIT, UTF8, ENTITY_NAME, CHAR_ENTITY };

RESOURCE(converter);

/******************************************************************************
* The converter class applies a dictionary to a given string.
* It does so by iterating over a string, finding the longest matching key
* in the dictionary and replacing the matched substring with the translation.
* The hashtree and converter classes are used.
******************************************************************************/

struct converter_rep: rep<converter> {
  hashtree<char,string> ht;
  string output, nil_string, from, to;
  bool copy_unmatched;
  void match (string& str, int& index);
  void load ();

public:
  inline converter_rep(string from2, string to2) : 
    rep<converter>(from2*"-"*to2), ht(), output(), 
    nil_string(), from(from2), to(to2), copy_unmatched(true) { load(); }

  inline bool has_value(hashtree<char,string> node);

  friend struct converter;
  friend string flush (converter c);
  friend string apply (converter c, string str);
  friend void operator << (converter c, string str);
};
 
/******************************************************************************
* functions that operate on converters
******************************************************************************/
  
// takes a string str and returns its translation. strings contained in the
// converter are lost in this process. the converter is empty when this
// method returns.
string apply (converter c, string str);
  
// take a string and writes it into the converter. thus multiple strings can
// be translated and concatenated into one big string
void operator << (converter c, string str);
  
// concatenates and returns the contents of this converter. this converter is
// empty when this method returns.
string flush (converter c);  

/**************************************************************************
* convenience functions
**************************************************************************/

// check that the input string is valid according to the specified encoding
// converts the input string from one encoding to the other
// recognized encodings are "Cork", "UTF-8" and all those recognized by iconv.

bool check_encoding (string input, string encoding);
string convert (string input, string from, string to);
string convert_to_cork (string input, string from); 
string convert_from_cork (string input, string to); 
string utf8_to_cork (string input); 
string cork_to_utf8 (string input); 
string strict_cork_to_utf8 (string input); 
string cork_to_sourcecode (string input);
string sourcecode_to_cork (string input);
string convert_utf8_to_LaTeX (string input);
string convert_LaTeX_to_utf8 (string input);
string utf8_to_html (string input);
string utf8_to_t2a (string input);
string t2a_to_utf8 (string input);
string cyrillic_subset_in_t2a_to_code_point (string input);
string code_point_to_cyrillic_subset_in_t2a (string input);
string cork_to_ascii (string input);
bool check_using_iconv (string input, string encoding);
string convert_using_iconv (string input, string from, string to); 

/**************************************************************************
* Functions for hashtree handling
**************************************************************************/

// find_node("test",tree) means tree('t')('e')('s')('t')
// might modify hashtree!
hashtree<char,string> find_node (string key, hashtree<char,string> ht);

// finds a node and assigns it a value
void put_prefix_code (string key, string value, hashtree<char,string> ht);

// reads a dictionary from a file

void hashtree_from_dictionary (
  hashtree<char,string> dic, string file_name, escape_type key_escape=BIT2BIT,
  escape_type val_escape=UTF8, bool reverse=false);

/***************************************************************************
* Functions for UTF-8 handling
* These functions are helper functions to convert escape string a la "#23F7"
* and HTML/XML character entities to and from UTF-8 byte sequences.
***************************************************************************/

int hex_digit_to_int (unsigned char c);
string encode_as_utf8 (unsigned int code);
unsigned int decode_from_utf8 (string s, int& i);
string convert_escapes (string in, bool utf8);
string convert_char_entities (string s);
string convert_char_entity (string s, int& start, bool& success);
string utf8_to_hex_entities (string s);
string utf8_to_hex_string (string s);
string utf8_to_pdf_hex_string (string s);

#endif // CONVERTER_H
