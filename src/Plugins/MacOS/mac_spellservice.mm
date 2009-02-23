
/******************************************************************************
* MODULE     : mac_spellservice.mm
* DESCRIPTION: interface with the MacOSX standard spell service
* COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "MacOS/mac_spellservice.h"
#include "converter.hpp"

#include "Cocoa/mac_cocoa.h"

static string from_nsstring(NSString *s)
{
  const char *cstr = [s cStringUsingEncoding:NSUTF8StringEncoding];
  return utf8_to_cork(string((char*)cstr));
}


static NSString *to_nsstring_utf8(string s)
{
  s= cork_to_utf8 (s);
  char *p = as_charp(s);
  NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
  tm_delete_array (p);	
  return nss;
}


/******************************************************************************
* Dictionaries
******************************************************************************/

static hashmap<string,string> available_dicts ("");
static hashmap<string,string> the_dicts ("");
static string current_lang = "";

string
mac_init_dictionary (string lang) {
  if (N(the_dicts) == 0) {
    //FIXME: Complete dictionary list
    the_dicts("en") = "english";
    the_dicts("fr") = "french";
    the_dicts("it") = "italian";
    
    NSArray *arr = [[NSSpellChecker sharedSpellChecker] availableLanguages];
    NSEnumerator *enumerator = [arr objectEnumerator];
    NSString *nsdict;
    
    while ((nsdict = (NSString*)[enumerator nextObject])) {
      string dict = from_nsstring(nsdict);
      available_dicts(the_dicts(dict)) = dict;
    }
  }
  if (available_dicts->contains (lang)) {
    current_lang = lang;
    return "ok";
  }
  return "Error: no dictionary for#" * lang;
}


/******************************************************************************
* Spell checking interface
******************************************************************************/


string
mac_spell_start (string lan) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  string r = mac_init_dictionary(lan);
  [pool release];
  return r;
}

tree
mac_spell_check (string lan, string s) {
  tree t;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
 
  if (lan != current_lang) {
    if (!(available_dicts->contains (lan))) {
      [pool release];
      return "Error: dictionary not available";
    }
    current_lang = lan;
    [[NSSpellChecker sharedSpellChecker]  setLanguage: to_nsstring_utf8(available_dicts(lan))];
  }
  
  NSString *nss = to_nsstring_utf8(s);
  NSRange r =  [[NSSpellChecker sharedSpellChecker]  
                checkSpellingOfString: nss
                startingAt: 0];
  if (r.length == 0) t = "ok";
  else {
    NSArray *arr = [[NSSpellChecker sharedSpellChecker] guessesForWord: nss];
    if ([arr count] == 0) {
      t = tree (TUPLE, "0");
    } else {
      NSEnumerator *enumerator = [arr objectEnumerator];
      NSString *sugg;
      tree a (TUPLE);
      
      while ((sugg = (NSString*)[enumerator nextObject])) {
        a << from_nsstring(sugg);
      }
      t= tree (TUPLE, [arr count]);
      t << A (a);        
    }
  }
        
  return t;
}

void
mac_spell_accept (string lan, string s) {
//  ispell_send (lan, "@" * s);
}

void
mac_spell_insert (string lan, string s) {
//  ispell_send (lan, "*" * s);
}

void
mac_spell_done (string lan) {
//  ispell_send (lan, "#");
}
