;; Mathematical, Greek and Symbolic characters for HTML

;; Character entity set. Typical invocation:
;; <!ENTITY % HTMLsymbol PUBLIC
;; "-//W3C//ENTITIES Symbols//EN//HTML">
;; %HTMLsymbol; 

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


;; Latin Extended-B
("fnof"         "&#x0192")      ; latin small f with hook = function

;; Greek
("Alpha"        "&#x0391")      ; greek capital letter alpha
("Beta"         "&#x0392")      ; greek capital letter beta
("Gamma"        "&#x0393")      ; greek capital letter gamma
("Delta"        "&#x0394")      ; greek capital letter delta
("Epsilon"      "&#x0395")      ; greek capital letter epsilon
("Zeta"         "&#x0396")      ; greek capital letter zeta
("Eta"          "&#x0397")      ; greek capital letter eta
("Theta"        "&#x0398")      ; greek capital letter theta
("Iota"         "&#x0399")      ; greek capital letter iota
("Kappa"        "&#x039A")      ; greek capital letter kappa
("Lambda"       "&#x039B")      ; greek capital letter lambda
("Mu"           "&#x039C")      ; greek capital letter mu
("Nu"           "&#x039D")      ; greek capital letter nu
("Xi"           "&#x039E")      ; greek capital letter xi
("Omicron"      "&#x039F")      ; greek capital letter omicron
("Pi"           "&#x03A0")      ; greek capital letter pi
("Rho"          "&#x03A1")      ; greek capital letter rho
;; there is no Sigmaf, and no U+03A2 character either
("Sigma"        "&#x03A3")      ; greek capital letter sigma
("Tau"          "&#x03A4")      ; greek capital letter tau
("Upsilon"      "&#x03A5")      ; greek capital letter upsilon
("Phi"          "&#x03A6")      ; greek capital letter phi
("Chi"          "&#x03A7")      ; greek capital letter chi
("Psi"          "&#x03A8")      ; greek capital letter psi
("Omega"        "&#x03A9")      ; greek capital letter omega

("alpha"        "&#x03B1")      ; greek small letter alpha
("beta"         "&#x03B2")      ; greek small letter beta
("gamma"        "&#x03B3")      ; greek small letter gamma
("delta"        "&#x03B4")      ; greek small letter delta
("epsilon"      "&#x03B5")      ; greek small letter epsilon
("zeta"         "&#x03B6")      ; greek small letter zeta
("eta"          "&#x03B7")      ; greek small letter eta
("theta"        "&#x03B8")      ; greek small letter theta
("iota"         "&#x03B9")      ; greek small letter iota
("kappa"        "&#x03BA")      ; greek small letter kappa
("lambda"       "&#x03BB")      ; greek small letter lambda
("mu"           "&#x03BC")      ; greek small letter mu
("nu"           "&#x03BD")      ; greek small letter nu
("xi"           "&#x03BE")      ; greek small letter xi
("omicron"      "&#x03BF")      ; greek small letter omicron
("pi"           "&#x03C0")      ; greek small letter pi
("rho"          "&#x03C1")      ; greek small letter rho
("sigmaf"       "&#x03C2")      ; greek small letter final sigma
("sigma"        "&#x03C3")      ; greek small letter sigma
("tau"          "&#x03C4")      ; greek small letter tau
("upsilon"      "&#x03C5")      ; greek small letter upsilon
("phi"          "&#x03C6")      ; greek small letter phi
("chi"          "&#x03C7")      ; greek small letter chi
("psi"          "&#x03C8")      ; greek small letter psi
("omega"        "&#x03C9")      ; greek small letter omega
("thetasym"     "&#x03D1")      ; greek small letter theta symbol
("upsih"        "&#x03D2")      ; greek upsilon with hook symbol
("piv"          "&#x03D6")      ; greek pi symbol

;; General Punctuation
("bull"         "&#x2022")      ; bullet = black small circle
;; bullet is NOT the same as bullet operator, U+2219
("hellip"       "&#x2026")      ; horizontal ellipsis = three dot leader
("prime"        "&#x2032")      ; prime = minutes = feet
("Prime"        "&#x2033")      ; double prime = seconds = inches
("oline"        "&#x203E")      ; overline = spacing overscore
("frasl"        "&#x2044")      ; fraction slash

;; Letterlike Symbols
("weierp"       "&#x2118")      ; script capital P = power set
("image"        "&#x2111")      ; blackletter capital I = imaginary part
("real"         "&#x211C")      ; blackletter capital R = real part symbol
("trade"        "&#x2122")      ; trade mark sign
("alefsym"      "&#x2135")      ; alef symbol = first transfinite cardinal
;; alef symbol is NOT the same as hebrew letter alef,
;; U+05D0 although the same glyph could be used to depict both characters 

;; Arrows
("larr"         "&#x2190")      ; leftwards arrow
("uarr"         "&#x2191")      ; upwards arrow
("rarr"         "&#x2192")      ; rightwards arrow
("darr"         "&#x2193")      ; downwards arrow
("harr"         "&#x2194")      ; left right arrow
("crarr"        "&#x21B5")      ; downwards arrow with corner leftwards
("lArr"         "&#x21D0")      ; leftwards double arrow
;; ISO 10646 does not say that lArr is the same as the 'is implied by' arrow
;; but also does not have any other character for that function. So ? lArr can
;; be used for 'is implied by' as ISOtech suggests 
("uArr"         "&#x21D1")      ; upwards double arrow
("rArr"         "&#x21D2")      ; rightwards double arrow
;; ISO 10646 does not say this is the 'implies' character but does not have 
;; another character with this function so ?
;; rArr can be used for 'implies' as ISOtech suggests 
("dArr"         "&#x21D3")      ; downwards double arrow
("hArr"         "&#x21D4")      ; left right double arrow

;; Mathematical Operators
("forall"       "&#x2200")      ; for all
("part"         "&#x2202")      ; partial differential
("exist"        "&#x2203")      ; there exists
("empty"        "&#x2205")      ; empty set = null set = diameter
("nabla"        "&#x2207")      ; nabla = backward difference
("isin"         "&#x2208")      ; element of
("notin"        "&#x2209")      ; not an element of
("ni"           "&#x220B")      ; contains as member
;; should there be a more memorable name than 'ni'?
("prod"         "&#x220F")      ; n-ary product = product sign
;; prod is NOT the same character as U+03A0 'greek capital letter pi' though
;; the same glyph might be used for both 
("sum"          "&#x2211")      ; n-ary sumation
;; sum is NOT the same character as U+03A3 'greek capital letter sigma'
;; though the same glyph might be used for both 
("minus"        "&#x2212")      ; minus sign
("lowast"       "&#x2217")      ; asterisk operator
("radic"        "&#x221A")      ; square root = radical sign
("prop"         "&#x221D")      ; proportional to
("infin"        "&#x221E")      ; infinity
("ang"          "&#x2220")      ; angle
("and"          "&#x2227")      ; logical and = wedge
("or"           "&#x2228")      ; logical or = vee
("cap"          "&#x2229")      ; intersection = cap
("cup"          "&#x222A")      ; union = cup
("int"          "&#x222B")      ; integral
("there4"       "&#x2234")      ; therefore
("sim"          "&#x223C")      ; tilde operator = varies with = similar to
;; tilde operator is NOT the same character as the tilde, U+007E,
;; although the same glyph might be used to represent both  
("cong"         "&#x2245")      ; approximately equal to
("asymp"        "&#x2248")      ; almost equal to = asymptotic to
("ne"           "&#x2260")      ; not equal to
("equiv"        "&#x2261")      ; identical to
("le"           "&#x2264")      ; less-than or equal to
("ge"           "&#x2265")      ; greater-than or equal to
("sub"          "&#x2282")      ; subset of
("sup"          "&#x2283")      ; superset of
;; note that nsup, 'not a superset of, U+2283' is not covered by the Symbol 
;; font encoding and is not included. Should it be, for symmetry?
;; It is in ISOamsn  
("nsub"         "&#x2284")      ; not a subset of
("sube"         "&#x2286")      ; subset of or equal to
("supe"         "&#x2287")      ; superset of or equal to
("oplus"        "&#x2295")      ; circled plus = direct sum
("otimes"       "&#x2297")      ; circled times = vector product
("perp"         "&#x22A5")      ; up tack = orthogonal to = perpendicular
("sdot"         "&#x22C5")      ; dot operator
;; dot operator is NOT the same character as U+00B7 middle dot

;; Miscellaneous Technical
("lceil"        "&#x2308")      ; left ceiling = apl upstile
("rceil"        "&#x2309")      ; right ceiling
("lfloor"       "&#x230A")      ; left floor = apl downstile
("rfloor"       "&#x230B")      ; right floor
("lang"         "&#x2329")      ; left-pointing angle bracket = bra
;; lang is NOT the same character as U+003C 'less than' 
;; or U+2039 'single left-pointing angle quotation mark' 
("rang"         "&#x232A")      ; right-pointing angle bracket = ket
;; rang is NOT the same character as U+003E 'greater than' 
;; or U+203A 'single right-pointing angle quotation mark' 

;; Geometric Shapes
("loz"          "&#x25CA")      ; lozenge

;; Miscellaneous Symbols
("spades"       "&#x2660")      ; black spade suit
;; black here seems to mean filled as opposed to hollow
("clubs"        "&#x2663")      ; black club suit = shamrock
("hearts"       "&#x2665")      ; black heart suit = valentine
("diams"        "&#x2666")      ; black diamond suit
