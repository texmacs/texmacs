
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
#include "language.hpp"

#include "Cocoa/mac_cocoa.h"

static string 
from_nsstring (NSString *s) {
  const char *cstr = [s cStringUsingEncoding:NSUTF8StringEncoding];
  return utf8_to_cork(string((char*)cstr));
}


static NSString *
to_nsstring_utf8 (string s) {
  s= cork_to_utf8 (s);
  c_string p = c_string (s);
  NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
  return nss;
}


/******************************************************************************
* Dictionaries
******************************************************************************/

static hashmap<string,string> available_dicts ("");
static string current_lang = "";
static NSInteger current_tag = 0;

static bool mac_spelling_language(string lang)
{
  if (available_dicts->contains (lang)) {
    current_lang = lang;
    [[NSSpellChecker sharedSpellChecker]
	setLanguage: to_nsstring_utf8(available_dicts(lang)) ];
    [[NSSpellChecker sharedSpellChecker]
	setAutomaticallyIdentifiesLanguages:NO ];
    if (DEBUG_IO)
      debug_spell << "setting language: "  << lang << LF;
    return true;
  }
  return false;
}

void
mac_init_dictionary () {
  if (N(available_dicts) == 0) {
    array<string> languages= get_supported_languages ();
    NSArray *tmp = [[NSSpellChecker sharedSpellChecker] availableLanguages];
    NSEnumerator *enumerator = [tmp objectEnumerator];
    NSString *nsdict;
    array<string> arr;
    while ((nsdict = (NSString*)[enumerator nextObject])) {
      string dict = from_nsstring(nsdict);
      arr= append (dict, arr);
    }
    if (DEBUG_IO)
      debug_spell << "available dictionaries: " << arr << LF;
    for (int i= 0; i < N(languages); i++) {
      string lan= languages[i];
      string loc= language_to_locale (lan);
      if (contains (loc, arr))
	available_dicts (lan) = loc;
      else if (N(loc) > 2 && contains (loc (0, 2), arr))
	available_dicts (lan) = loc (0, 2);
    }
    if (DEBUG_IO)
      debug_spell << "selected dictionaries: " << available_dicts << LF;
  }
}


/******************************************************************************
* Spell checking interface
******************************************************************************/


string
mac_spell_start (string lan) {
  string r;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  // we need to be sure that the Cocoa application infrastructure is
  // initialized. (apparently Qt does not do this properly and the
  // NSSpellChecker instance returns null without the following instruction)
  NSApplication *NSApp=[NSApplication sharedApplication]; (void) NSApp;
  mac_init_dictionary ();
  if (mac_spelling_language (lan)) {
    // warning: we must be sure that the tag is relased by the appropriate
    // message to sharedSpellChecker
    current_tag = [NSSpellChecker uniqueSpellDocumentTag];
    r = "ok";
  }
  else r = "Error: no dictionary available for '" * lan * "'";
  [pool release];
  if (DEBUG_IO) debug_spell << r << LF;   
  return r;
}

tree
mac_spell_check (string lan, string s) {
  if (DEBUG_IO) debug_spell << "check " << lan << " :: " << s << LF;
  tree t;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  if ((lan != current_lang) && (! mac_spelling_language (lan)))
    t = "Error: no dictionary available for '" * lan * "'";
  else {
    NSString *nss = to_nsstring_utf8 (s);
    NSRange r =  [[NSSpellChecker sharedSpellChecker]  
                  checkSpellingOfString: nss
	          startingAt: 0
                  language: nil
                  wrap: NO
                  inSpellDocumentWithTag: current_tag
                  wordCount: NULL];
    if (r.length == 0) 
      t = "ok";
    else {
#if MAC_OS_X_VERSION_MIN_ALLOWED < MAC_OS_X_VERSION_10_6
      NSArray *arr = [[NSSpellChecker sharedSpellChecker]
		      guessesForWord: nss];
#else
      NSArray *arr = [[NSSpellChecker sharedSpellChecker]
		      guessesForWordRange: NSMakeRange(0, [nss length])
		      inString: nss
		      language: nil
		      inSpellDocumentWithTag: current_tag];
#endif
      if ([arr count] == 0)
        t = tree (TUPLE, "0");
      else {
        NSEnumerator *enumerator = [arr objectEnumerator];
        NSString *sugg;
        tree a (TUPLE);
        while ((sugg = (NSString*)[enumerator nextObject])) {
          a << from_nsstring (sugg);
        }
        t= tree (TUPLE, as_string((int)[arr count]));
        t << A (a);        
      }
    }
  }
  [pool release];

  if (DEBUG_IO) debug_spell << t << LF;     
  return t;
}

void
mac_spell_accept (string lan, string s) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  if ((lan != current_lang) && (! mac_spelling_language (lan))) {
    // do nothing
    ;
  } else {
    [[NSSpellChecker sharedSpellChecker]
     ignoreWord:to_nsstring_utf8 (s)
     inSpellDocumentWithTag:current_tag];
  }  
  //  ispell_send (lan, "@" * s);
  [pool release];
}

void
mac_spell_insert (string lan, string s) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  if ((lan != current_lang) && (! mac_spelling_language (lan))) {
    // do nothing
    ;
  } else {
    [[NSSpellChecker sharedSpellChecker]
     learnWord:to_nsstring_utf8 (s)];
  }  
  //  ispell_send (lan, "*" * s);
  [pool release];
}

void
mac_spell_done (string lan) {
  (void) lan;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  [[NSSpellChecker sharedSpellChecker]
   closeSpellDocumentWithTag:current_tag];
  current_tag = 0;
  [pool release];
}

