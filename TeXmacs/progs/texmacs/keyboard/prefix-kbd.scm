
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : prefix-kbd.scm
;; DESCRIPTION : prefixes for keyboard shortcuts
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs keyboard prefix-kbd)
  (:use (utils library cursor)
	(utils edit selections)
	(texmacs texmacs tm-server)
	(texmacs texmacs tm-files)
	(generic generic-edit)))

(set-variant-keys "tab" "S-tab")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs prefixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (like-emacs?)

(kbd-wildcards pre
  ("emacs" "C-" #t)
  ("emacs:contextual" "emacs c")
  ("emacs:prefix" "emacs x")
  ("emacs:meta" "M-" #t)
  ("std" "C-" #t)
  ("cmd" "A-")
  ("altcmd" "M-")
  ("special" "M-C-")
  ("extra" "M-A-")
  ("accent" "M-" #t)
  ("copyto" "M-W")
  ("cutto" "emacs W")
  ("pastefrom" "emacs Y")
  ("structured:cmd" "C-")
  ("structured:move" "M-A-")
  ("structured:insert" "A-")
  ("structured:geometry" "M-"))

(kbd-wildcards
  ("escape" "M-" #t)
  ("escape escape" "A-" #t)
  ("escape escape escape" "C-" #t)
  ("escape escape escape escape" "noop" #t)
  ("S-escape" "M-A-" #t)
  ("S-escape S-escape" "M-C-" #t)
  ("S-escape S-escape S-escape" "A-C-" #t)
  ("S-escape S-escape S-escape S-escape" "noop" #t))

(kbd-map
  ("emacs" "" "Emacs command")
  ("emacs:contextual" "" "Emacs mode specific prefix command")
  ("emacs:prefix" "" "Emacs prefix command")
  ("emacs:meta" "Emacs meta"))

) ;; end when (like-emacs?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnome prefixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (like-gnome?)

(kbd-wildcards pre
  ("gnome" "C-" #t)
  ("std" "C-" #t)
  ("cmd" "A-")
  ("altcmd" "M-")
  ("special" "A-C-")
  ("accent" "M-" #t)
  ("copyto" "std C")
  ("cutto" "std X")
  ("pastefrom" "std V")
  ("structured:cmd" "C-")
  ("structured:move" "M-A-")
  ("structured:insert" "M-")
  ("structured:geometry" "M-C-"))

(kbd-wildcards
  ("escape" "M-" #t)
  ("escape escape" "A-" #t)
  ("escape escape escape" "C-" #t)
  ("escape escape escape escape" "noop" #t)
  ("S-escape" "M-C-" #t)
  ("S-escape S-escape" "A-C-" #t)
  ("S-escape S-escape S-escape" "M-A-" #t)
  ("S-escape S-escape S-escape S-escape" "noop" #t))

(kbd-map
  ("gnome" "" "Gnome command")
  ("std" "" "Gnome command"))

) ;; end when (like-gnome?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KDE prefixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (like-kde?)

(kbd-wildcards pre
  ("kde" "C-" #t)
  ("std" "C-" #t)
  ("cmd" "A-")
  ("altcmd" "M-")
  ("special" "A-C-")
  ("accent" "M-" #t)
  ("copyto" "std C")
  ("cutto" "std X")
  ("pastefrom" "std V")
  ("structured:cmd" "C-")
  ("structured:move" "M-A-")
  ("structured:insert" "M-")
  ("structured:geometry" "M-C-"))

(kbd-wildcards
  ("escape" "M-" #t)
  ("escape escape" "A-" #t)
  ("escape escape escape" "C-" #t)
  ("escape escape escape escape" "noop" #t)
  ("S-escape" "M-C-" #t)
  ("S-escape S-escape" "A-C-" #t)
  ("S-escape S-escape S-escape" "M-A-" #t)
  ("S-escape S-escape S-escape S-escape" "noop" #t))

(kbd-map
  ("kde" "" "KDE command")
  ("std" "" "KDE command"))

) ;; end when (like-kde?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac OS prefixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (like-macos?)

(kbd-wildcards pre
  ("macos" "M-" #t)
  ("std" "M-" #t)
  ("cmd" "C-")
  ("altcmd" "M-C-")
  ("special" "A-C-")
  ("copyto" "std C")
  ("cutto" "std X")
  ("pastefrom" "std V")
  ("accent" "M-" #t)
  ("structured:cmd" "A-")
  ("structured:move" "M-C-")
  ("structured:insert" "C-")
  ("structured:geometry" "M-A-"))

(kbd-wildcards
  ("escape" "A-" #t)
  ("escape escape" "C-" #t)
  ("escape escape escape" "M-" #t)
  ("escape escape escape escape" "noop" #t)
  ("S-escape" "M-A-" #t)
  ("S-escape S-escape" "A-C-" #t)
  ("S-escape S-escape S-escape" "M-C-" #t)
  ("S-escape S-escape S-escape S-escape" "noop" #t))

(kbd-map
  ("macos" "" "MacOS command")
  ("std" "" "MacOS command")
  ("A-" "" "MacOS option"))

) ;; end when (like-macos?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows prefixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (like-windows?)

(kbd-wildcards pre
  ("windows" "C-" #t)
  ("std" "C-" #t)
  ("cmd" "A-")
  ("altcmd" "M-")
  ("special" "A-C-")
  ("accent" "M-" #t)
  ("copyto" "std C")
  ("cutto" "std X")
  ("pastefrom" "std V")
  ("structured:cmd" "C-")
  ("structured:move" "M-A-")
  ("structured:insert" "M-")
  ("structured:geometry" "M-C-"))

(kbd-wildcards
  ("escape" "M-" #t)
  ("escape escape" "A-" #t)
  ("escape escape escape" "C-" #t)
  ("escape escape escape escape" "noop" #t)
  ("S-escape" "M-C-" #t)
  ("S-escape S-escape" "A-C-" #t)
  ("S-escape S-escape S-escape" "M-A-" #t)
  ("S-escape S-escape S-escape S-escape" "noop" #t))

(kbd-map
  ("windows" "" "Windows command")
  ("std" "" "Windows command"))

) ;; end when (or (like-gnome?) (like-windows?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global prefixes for all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-wildcards pre
  ("S-C-" "C-S-" #t)
  ("S-A-" "A-S-" #t)
  ("S-M-" "M-S-" #t)
  ("C-A-" "A-C-" #t)
  ("C-M-" "M-C-" #t)
  ("A-M-" "M-A-" #t)

  ("var" "tab" #t)
  ("unvar" "S-tab" #t)  

  ("text" "cmd" #t)
  ("math" "cmd" #t)
  ("prog" "cmd" #t)

  ("font" "altcmd f")
  ("executable" "altcmd e")
  ("inactive" "altcmd i")
  ("link" "altcmd l")
  ("version" "altcmd #")
  ("table" "altcmd t")
  ("script" "altcmd *")

  ("accent:tilde" "accent ~")
  ("accent:hat" "accent ^")
  ("accent:umlaut" "accent \"")
  ("accent:acute" "accent '")
  ("accent:grave" "accent `")
  ("accent:cedilla" "accent C")
  ("accent:breve" "accent U")
  ("accent:invbreve" "accent A")
  ("accent:check" "accent V")
  ("accent:doubleacute" "accent H")
  ("accent:abovering" "accent O")
  ("accent:abovedot" "accent .")

  ("symbol" "S-F5" #t)
  ("text:symbol" "S-F5" #t)
  ("math:misc" "F5" #t)

  ("math:up" "F5" #t)
  ("math:bold" "F6" #t)
  ("math:cal" "F7" #t)
  ("math:frak" "F8" #t)
  ("math:symbol" "S-F5" #t)
  ("math:bbb" "S-F6" #t)
  ("math:greek" "S-F7" #t)

  ("math:bold:up" "math:bold math:up" #t)
  ("math:bold:cal" "math:bold math:cal" #t)
  ("math:bold:greek" "math:bold math:greek" #t)
  ("math:symbol:circled" "math:symbol @" #t)
  ("math:symbol:limits" "math:symbol L" #t)

  ("math:over" "math o" #t)
  ("math:under" "math u" #t)
  ("math:small" "math" #t)
  ("math:left" "math l" #t)
  ("math:middle" "math m" #t)
  ("math:right" "math r" #t)
  ("math:syntax" "math x" #t))

(kbd-wildcards
  ("tilde tilde" "tilde")
  ("hat hat" "hat")
  ("umlaut umlaut" "umlaut")
  ("acute acute" "acute")
  ("grave grave" "grave")
  ("cedilla cedilla" "cedilla")
  ("breve breve" "breve")
  ("invbreve invbreve" "invbreve")
  ("check check" "check")
  ("doubleacute doubleacute" "doubleacute")
  ("abovering abovering" "abovering")
  ("abovedot abovedot" "abovedot")
  ("ogonek ogonek" "ogonek")

  ("tilde" "accent ~")
  ("hat" "accent:deadhat") ;; needed for dead ^ in math mode
  ("umlaut" "accent \"")
  ("acute" "accent '")
  ("grave" "accent `")
  ("cedilla" "accent C")
  ("breve" "accent U")
  ("invbreve" "accent A")
  ("check" "accent V")
  ("doubleacute" "accent H")
  ("abovering" "accent O")
  ("abovedot" "accent .")
  ("ogonek" "accent G"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explain prefixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("cmd" "" "TeXmacs command")
  ("altcmd" "" "Alternate TeXmacs command")
  ("special" "" "Special command")
  ("structured:move" "" "Structured move")
  ("structured:geometry" "" "Structured position or resize")
  ("symbol" "" "Insert a TeXmacs symbol")
  ("copyto" "" "Copy to (1, 2, 3, *:other)")
  ("cutto" "" "Cut to (1, 2, 3, *:other)")
  ("pastefrom" "" "Paste from (1, 2, 3, *:other)")
  ("noop" (set-message "" ""))

  ("font" "" "Change font")
  ("executable" "" "Insert executable markup")
  ("inactive" "" "Insert inactive markup")
  ("script" "" "Evaluate function or insert evaluation tag")
  ("link" "" "Linking command")
  ("version" "" "Versioning command")
  ("table" "" "Table command"))

(kbd-map
  (:profile emacs)
  ("altcmd" "" "Alternate TeXmacs command"))

(kbd-map
  (:mode in-text?)
  ("text" "" "TeXmacs command")
  ("text:symbol" "" "Insert a special character"))

(kbd-map
  (:mode in-math?)
  ("math" "" "Insert mathematical markup")
  ("math:up" "" "Insert an upright character")
  ("math:greek" "" "Insert a Greek character")
  ("math:bold" "" "Insert a bold character")
  ("math:bold:up" "" "Insert an upright bold character")
  ("math:bold:greek" "" "Insert a bold Greek character")
  ("math:cal" "" "Insert a calligraphic character")
  ("math:bold:cal" "" "Insert a bold calligraphic character")
  ("math:frak" "" "Insert a fraktur character")
  ("math:bbb" "" "Insert a blackboard bold character")
  ("math:over" "" "Insert a wide symbol above")
  ("math:under" "" "Insert a wide symbol below")
  ("math:left" "" "Insert a large left delimiter or left subscript")
  ("math:middle" "" "Insert a large separator")
  ("math:right" "" "Insert a large right delimiter")
  ("math:symbol" "" "Insert a mathematical symbol")
  ("math:symbol:circled" "" "Insert a big circled operator")
  ("math:symbol:limits" "" "Insert a mathematical symbol with limits")
  ("math:syntax" "" "Specify an alternative semantics"))

(kbd-map
  (:mode in-prog?)
  ("prog" "" "TeXmacs command"))
