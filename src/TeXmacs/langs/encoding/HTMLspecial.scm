;; Special characters for HTML

;; Character entity set. Typical invocation:
;; <!ENTITY % HTMLspecial PUBLIC
;; "-//W3C//ENTITIES Special//EN//HTML">
;; %HTMLspecial; 

;; Portions (C) International Organization for Standardization 1986:
;; Permission to copy in any form is granted for use with
;; conforming SGML systems and applications as defined in
;; ISO 8879, provided this notice is included in all copies.

;; Relevant ISO entity set is given unless names are newly introduced.
;; New names (i.e., not in ISO 8879 list) do not clash with any
;; existing ISO 8879 entity names. ISO 10646 character numbers
;; are given for each character, in hex. CDATA values are decimal
;; conversions of the ISO 10646 values and refer to the document
;; character set. Names are ISO 10646 names.


;; C0 Controls and Basic Latin
("quot"         "&#x0022")      ; quotation mark = APL quote
("amp"          "&#x0026")      ; ampersand
("lt"           "&#x003C")      ; less-than sign
("gt"           "&#x003E")      ; greater-than sign

;; Latin Extended-A
("OElig"        "&#x0152")      ; latin capital ligature OE
("oelig"        "&#x0153")      ; latin small ligature oe
;; ligature is a misnomer, this is a separate character in some languages
("Scaron"       "&#x0160")      ; latin capital letter S with caron
("scaron"       "&#x0161")      ; latin small letter s with caron
("Yuml"         "&#x0178")      ; latin capital letter Y with diaeresis

;; Spacing Modifier Letters
("circ"         "&#x02C6")      ; modifier letter circumflex accent
("tilde"        "&#x02DC")      ; small tilde

;; General Punctuation
("ensp"         "&#x2002")      ; en space
("emsp"         "&#x2003")      ; em space
("thinsp"       "&#x2009")      ; thin space
("zwnj"         "&#x200C")      ; zero width non-joiner
("zwj"          "&#x200D")      ; zero width joiner
("lrm"          "&#x200E")      ; left-to-right mark
("rlm"          "&#x200F")      ; right-to-left mark
("ndash"        "&#x2013")      ; en dash
("mdash"        "&#x2014")      ; em dash
("lsquo"        "&#x2018")      ; left single quotation mark
("rsquo"        "&#x2019")      ; right single quotation mark
("sbquo"        "&#x201A")      ; single low-9 quotation mark
("ldquo"        "&#x201C")      ; left double quotation mark
("rdquo"        "&#x201D")      ; right double quotation mark
("bdquo"        "&#x201E")      ; double low-9 quotation mark
("dagger"       "&#x2020")      ; dagger
("Dagger"       "&#x2021")      ; double dagger
("permil"       "&#x2030")      ; per mille sign
("lsaquo"       "&#x2039")      ; single left-pointing angle quotation mark
;; lsaquo is proposed but not yet ISO standardized
("rsaquo"       "&#x203A")      ; single right-pointing angle quotation mark
;; rsaquo is proposed but not yet ISO standardized
("euro"         "&#x20AC")      ; euro sign
