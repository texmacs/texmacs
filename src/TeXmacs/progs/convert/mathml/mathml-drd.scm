
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathml-drd.scm
;; DESCRIPTION : DRD properties for MathML
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert mathml mathml-drd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordinary symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table tm->mathml-constant%
  ("<mathcatalan>" "C")
  ("<mathe>" "e")
  ("<matheuler>" "&eulergamma;")
  ("<mathi>" "&ImaginaryI;")
  ("<mathpi>" "&pi;"))

(logic-table mathml-constant->tm%
  ("&eulergamma;" "<matheuler>")
  ("&ImaginaryI;" "<mathi>")
  ("&ii;" "<mathi>")
  ("&pi;" "<mathpi>")
  ("&true;" "true")
  ("&false;" "false")
  ("&phiv;" "<varphi>")
  ("&ell;" "<ell>")
  ("&Copf;" "<bbb-C>")
  ("&Qopf;" "<bbb-Q>")
  ("&Zopf;" "<bbb-Z>")
  ("&Ropf;" "<bbb-R>"))

(logic-table tm->mathml-operator%
  ("&" "&amp;")
  ("<less>" "&lt;")
  ("*" "&InvisibleTimes;")
  (" " "&ApplyFunction;"))

(logic-table mathml-operator->tm%
  ("⁢" "*") ;U+2062 Invisible Times 
  ("*" "*")
  ("−" "-") ;U+2212 minus 
  ("⃛" "<dddot>") ;U+20DB
  ("˚" "<abovering>")
  ("´" "<acute>")
  ("`" "<grave>")
  ("¯" "¯" ) ;U+00AF
  ("‾" "¯") ; U+203E
  ("̲" "¯") ;U+0332 
  ("⃗" "<vect>")
  ("̑" "<invbreve>")
  ("ˇ" "<check>")
  ("˘" "<breve>")
  ("⏞" "<overbrace>") ;U+23DE
  ("︷" "<overbrace>") ;U+FE37
  ("⏟" "<underbrace>") ;U+23DE
  ("︸" "<underbrace>") ;U+FE38
  ("⎴" "<sqoverbrace>") ;U+23B4
  ("﹇" "<sqoverbrace>") ;U+FE47
  ("﹈" "<squnderbrace>") ;U+FE48
  ("⎵" "<squnderbrace>") ;U+23B5
  ("⏜" "<poverbrace>") ;U+23DC
  ("︵" "<poverbrace>") ;U+FE35
  ("⏝" "<punderbrace>") ;U+23DD
  ("︶" "<punderbrace>") ;U+FE36
  ("︿" "^") ;U+FE3F
  ("﹀" "<check>") ;U+FE40

  ("&InvisibleTimes;" "*")
  ("&PlusMinus;" "<pm>")
  ("&it;" "*")
  ("&leq;" "<leq>")
  ("&geq;" "<geq>")
  ("&RightArrow;" "<rightarrow>")
  ("&OverBar;" "¯")
  
;below a selection of html math entities (there are so many, and redundant ones)
;with unicode equivalent (data from the web), or equivalent texmacs code
;note that when Firefox saves an html file, it replaces &entities by utf8 characters
;can be a workaround for missing entities
  
  ("&DifferentialD;" "<mathd>")  ; was ⅆ U+02146
  ("&Sum;" "<sum>")  ; was ∑ U+02211
  ("&straightphi;" "<phi>")  ; was ϕ U+003D5
  ("&PartialD;" "<partial>")  ; was ∂ U+02202
  ("&UnderBrace;" "<underbrace>")  ; was ⏟ U+023DF
  ("&OverBrace;" "<overbrace>")  ; was ⏞ U+023DE
  ("&ee;" "<mathe>")  ; was ⅇ U+2147 Double-Struck Italic Small E
  ("&plus;" "+")  ; + U+002B Plus Sign
  ("&minus;" "-")  ; − U+2212 Minus Sign
  ("&times;" "<times>")  ; × U+00D7 Multiplication Sign
  ("&divide;" "<div>")  ; ÷ U+00F7 Division Sign
  ("&equals;" "=")  ; = U+003D Equals Sign
  ("&ne;" "<neq>")  ; ≠ U+2260 Not Equal To
  ("&plusmn;" "<pm>")  ; ± U+00B1 Plus minus symbol
  ("&not;" "<neg>")  ; ¬ U+00AC Not Sign
  ("&gt;" "<gtr>")  ; > U+003E Greater-Than Sign
  ("&deg;" "<#00B0>")  ; ° U+00B0 Degree Sign
  ("&bigcup;" (big "cup"))  ; ⋃ U+022C3 
  ("&bigcap;" (big "cap"))  ; ⋂ U+022C2
  ("&Integral;" (big "int"))  ; ∫ U+0222B
  ("&hamilt;" "<#210B>")  ; ℋ U+210B Script Capital H
  ("&planckh;" "<#210E>")  ; ℎ U+210E Planck Constant
  ("&planck;" "<hbar>")  ; ℏ U+210F Planck Constant Over Two Pi
  ("&image;" "<#2111>")  ; ℑ U+2111 Black-Letter Capital I
  ("&weierp;" "<#2118>")  ; ℘ U+2118 Script Capital P
  ("&real;" "<#211C>")  ; ℜ U+211C Black-Letter Capital R
  ("&mho;" "<#2127>")  ; ℧ U+2127 Inverted Ohm Sign
  ("&iiota;" "<#2129>")  ; ℩ U+2129 Turned Greek Small Letter Iota
  ("&bernou;" "<#212C>")  ; ℬ U+212C Script Capital B
  ("&alefsym;" "<#2135>")  ; ℵ U+2135 Alef Symbol
  ("&beth;" "<#2136>")  ; ℶ U+2136 Bet Symbol
  ("&gimel;" "<#2137>")  ; ℷ U+2137 Gimel Symbol
  ("&daleth;" "<#2138>")  ; ℸ U+2138 Dalet Symbol
  ("&DD;" "<bbb-D>")  ; ⅅ U+2145 Double-Struck Italic Capital D
  ("&dd;" "<bbb-d>")  ; ⅆ U+2146 Double-Struck Italic Small D
  ("&starf;" "<#2605>")  ; ★ U+2605 Black Star
  ("&star;" "<#2606>")  ; ☆ U+2606 White Star
  ("&loz;" "<#25CA>")  ; ◊ U+25CA Lozenge
  ("&sung;" "<#266A>")  ; ♪ U+266A Eighth Note
  ("&flat;" "<#266D>")  ; ♭ U+266D Music Flat Sign
  ("&natural;" "<#266E>")  ; ♮ U+266E Music Natural Sign
  ("&sharp;" "<#266F>")  ; ♯ U+266F Music Sharp Sign
  ("&check;" "<#2713>")  ; ✓ U+2713 Check Mark
  ("&cross;" "<#2717>")  ; ✗ U+2717 Ballot X
  ("&malt;" "<#2720>")  ; ✠ U+2720 Maltese Cross
  ("&sext;" "<#2736>")  ; ✶ U+2736 Six Pointed Black Star
  ("&VerticalSeparator;" "<#2758>")  ; ❘ U+2758 Light Vertical Bar
  ("&excl;" "<#0021>")  ; ! U+0021 Exclamation Mark
  ("&num;" "<#0023>")  ; # U+0023 Number Sign
  ("&percnt;" "<#0025>")  ; % U+0025 Percent Sign
  ("&lpar;" "<#0028>")  ; ( U+0028 Left Parenthesis
  ("&rpar;" "<#0029>")  ; ) U+0029 Right Parenthesis
  ("&ast;" "<#002A>")  ; * U+002A Asterisk
  ("&comma;" "<#002C>")  ; , U+002C Comma
  ("&period;" "<#002E>")  ; . U+002E Full Stop
  ("&sol;" "<#002F>")  ; / U+002F Solidus
  ("&colon;" "<#003A>")  ; : U+003A Colon
  ("&semi;" "<#003B>")  ; ; U+003B Semicolon
  ("&quest;" "<#003F>")  ; ? U+003F Question Mark
  ("&lbrack;" "<#005B>")  ; [ U+005B Left Square Bracket
  ("&bsol;" "<#005C>")  ; \ U+005C Reverse Solidus
  ("&rbrack;" "<#005D>")  ; ] U+005D Right Square Bracket
  ("&Hat;" "<#005E>")  ; ^ U+005E Circumflex Accent
  ("&lowbar;" "<#005F>")  ; _ U+005F Low Line
  ("&grave;" "<#0060>")  ; ` U+0060 Grave Accent
  ("&lbrace;" "<#007B>")  ; { U+007B Left Curly Bracket
  ("&vert;" "<#007C>")  ; | U+007C Vertical Line
  ("&rbrace;" "<#007D>")  ; } U+007D Right Curly Bracket
  ("&tilde;" "<#007E>")  ; ~ U+007E Tilde
  ("&circ;" "<#02C6>")  ; ˆ U+02C6 Modifier Letter Circumflex Accent
  ("&nbsp;" "<#00A0>")  ;   U+00A0 No-Break Space
  ("&ensp;" "<#2002>")  ;   U+2002 En Space
  ("&emsp;" "<#2003>")  ;   U+2003 Em Space
  ("&thinsp;" "<#2009>")  ;   U+2009 Thin Space
  ("&zwnj;" "<#200C>")  ; ‌ U+200C Zero Width Non-Joiner
  ("&zwj;" "<#200D>")  ; ‍ U+200D Zero Width Joiner
  ;("&lrm;" "<#200E>")  ; ‎ U+200E Left-To-Right Mark
  ("&iexcl;" "<#00A1>")  ; ¡ U+00A1 Inverted Exclamation Mark
  ("&brvbar;" "<#00A6>")  ; ¦ U+00A6 Broken Bar
  ("&sect;" "<#00A7>")  ; § U+00A7 Section Sign
  ("&uml;" "<#00A8>")  ; ¨ U+00A8 Diaeresis
  ("&ordf;" "<#00AA>")  ; ª U+00AA Feminine Ordinal Indicator
  ("&shy;" "<#00AD>")  ; ­ U+00AD Soft Hyphen
  ("&macr;" "<#00AF>")  ; ¯ U+00AF Macron
  ("&acute;" "<#00B4>")  ; ´ U+00B4 Acute Accent
  ("&micro;" "<#00B5>")  ; µ U+00B5 Micro Sign
  ("&para;" "<#00B6>")  ; ¶ U+00B6 Pilcrow Sign
  ("&middot;" "<#00B7>")  ; · U+00B7 Middle Dot
  ("&cedil;" "<#00B8>")  ; ¸ U+00B8 Cedilla
  ("&ordm;" "<#00BA>")  ; º U+00BA Masculine Ordinal Indicator
  ("&iquest;" "<#00BF>")  ; ¿ U+00BF Inverted Question Mark
  ("&hyphen;" "<#2010>")  ; ‐ U+2010 Hyphen
  ("&ndash;" "<#2013>")  ; – U+2013 En Dash
  ("&mdash;" "<#2014>")  ; — U+2014 Em Dash
  ("&horbar;" "<#2015>")  ; ― U+2015 Horizontal Bar
  ("&Vert;" "<#2016>")  ; ‖ U+2016 Double Vertical Line
  ("&dagger;" "<#2020>")  ; † U+2020 Dagger
  ("&Dagger;" "<#2021>")  ; ‡ U+2021 Double Dagger
  ("&bull;" "<#2022>")  ; • U+2022 Bullet
  ("&nldr;" "<#2025>")  ; ‥ U+2025 Two Dot Leader
  ("&hellip;" "<#2026>")  ; … U+2026 Horizontal Ellipsis
  ("&permil;" "<#2030>")  ; ‰ U+2030 Per Mille Sign
  ("&pertenk;" "<#2031>")  ; ‱ U+2031 Per Ten Thousand Sign
  ("&prime;" "<#2032>")  ; ′ U+2032 Prime
  ("&Prime;" "<#2033>")  ; ″ U+2033 Double Prime
  ("&tprime;" "<#2034>")  ; ‴ U+2034 Triple Prime
  ("&bprime;" "<#2035>")  ; ‵ U+2035 Reversed Prime
  ("&oline;" "¯")  ; ‾ U+203E Overline
  ("&caret;" "<#2041>")  ; ⁁ U+2041 Caret Insertion Point
  ("&hybull;" "<#2043>")  ; ⁃ U+2043 Hyphen Bullet
  ("&frasl;" "/")  ; ⁄ U+2044 Fraction Slash
  ("&sup1;" "<#00B9>")  ; ¹ U+00B9 Superscript One
  ("&sup2;" "<#00B2>")  ; ² U+00B2 Superscript Two
  ("&sup3;" "<#00B3>")  ; ³ U+00B3 Superscript Three
  ("&fnof;" "<#0192>")  ; ƒ U+0192 Latin Small Letter F with Hook
  ("&percnt;" "<#0025>")  ; % U+0025 Percent Sign
  ("&permil;" "<#2030>")  ; ‰ U+2030 Character Tabulation with Justification
  ("&pertenk;" "<#2031>")  ; ‱ U+2031 Per Ten Thousand Sign
  ("&forall;" "<#2200>")  ; ∀ U+2200 For All
  ("&comp;" "<#2201>")  ; ∁ U+2201 Complement
  ("&part;" "<partial>")  ; ∂ U+2202 Partial Differential
  ("&exist;" "<#2203>")  ; ∃ U+2203 There Exists
  ("&nexist;" "<#2204>")  ; ∄ U+2204 There Does Not Exist
  ("&empty;" "<#2205>")  ; ∅ U+2205 Empty Set
  ("&nabla;" "<#2207>")  ; ∇ U+2207 Nabla
  ("&isin;" "<#2208>")  ; ∈ U+2208 Element Of
  ("&notin;" "<#2209>")  ; ∉ U+2209 Not an Element Of
  ("&ni;" "<#220B>")  ; ∋ U+220B Contains As Member
  ("&notni;" "<#220C>")  ; ∌ U+220C Does Not Contain As Member
  ("&prod;" (big "prod"))  ; ∏ U+220F N-Ary Product
  ("&coprod;" (big "amalg"))  ; ∐ U+2210 N-Ary Coproduct
  ("&sum;" (big "sum"))  ; ∑ U+2211 N-Ary Summation
  ("&mnplus;" <mp>)  ; ∓ U+2213 Minus-or-Plus Sign
  ("&setminus;" "<#2216>")  ; ∖ U+2216 Set Minus
  ("&lowast;" "<#2217>")  ; ∗ U+2217 Asterisk Operator
  ("&compfn;" "<circ>")  ; ∘ U+2218 Ring Operator
  ("&radic;" "<#221A>")  ; √ U+221A Square Root
  ("&prop;" "<#221D>")  ; ∝ U+221D Proportional To
  ("&infin;" "<#221E>")  ; ∞ U+221E Infinity
  ("&angrt;" "<#221F>")  ; ∟ U+221F Right Angle
  ("&ang;" "<#2220>")  ; ∠ U+2220 Angle
  ("&mid;" "<#2223>")  ; ∣ U+2223 Divides
  ("&parallel;" "<#2225>")  ; ∥ U+2225 Parallel To
  ("&and;" "<#2227>")  ; ∧ U+2227 Logical And
  ("&or;" "<#2228>")  ; ∨ U+2228 Logical Or
  ("&cap;" "<#2229>")  ; ∩ U+2229 Intersection
  ("&cup;" "<#222A>")  ; ∪ U+222A Union
  ("&int;" "<#222B>")  ; ∫ U+222B Integral
  ("&Int;" "<#222C>")  ; ∬ U+222C Double Integral
  ("&iiint;" "<#222D>")  ; ∭ U+222D Triple Integral
  ("&conint;" "<#222E>")  ; ∮ U+222E Contour Integral 
  ("&Conint;" "<#222F>")  ; ∯ U+222F Surface Integral
  ("&Cconint;" "<#2230>")  ; ∰ U+2230 Volume Integral
  ("&there4;" "<#2234>")  ; ∴ U+2234 Therefore
  ("&because;" "<#2235>")  ; ∵ U+2235 Because
  ("&ratio;" "<#2236>")  ; ∶ U+2236 Ratio
  ("&Colon;" "<#2237>")  ; ∷ U+2237 Proportion
  ("&homtht;" "<#223B>")  ; ∻ U+223B Homothetic
  ("&sim;" "<#223C>")  ; ∼ U+223C Tilde Operator
  ("&bsim;" "<#223D>")  ; ∽ U+223D Reversed Tilde
  ("&ac;" "<#223E>")  ; ∾ U+223E Inverted Lazy S
  ("&acd;" "<#223F>")  ; ∿ U+223F Sine Wave
  ("&sime;" "<#2243>")  ; ≃ U+2243 Asymptotically Equal To
  ("&nsime;" "<#2244>")  ; ≄ U+2244 Not Asymptotically Equal To
  ("&cong;" "<#2245>")  ; ≅ U+2245 Approximately Equal To
  ("&simne;" "<#2246>")  ; ≆ U+2246 Approximately But Not Actually Equal To
  ("&ncong;" "<#2247>")  ; ≇ U+2247 Neither Approximately nor Actually Equal To
  ("&asymp;" "<#2248>")  ; ≈ U+2248 Almost Equal To
  ("&nap;" "<#2249>")  ; ≉ U+2249 Not Almost Equal To
  ("&approxeq;" "<#224A>")  ; ≊ U+224A Almost Equal or Equal To
  ("&asympeq;" "<#224D>")  ; ≍ U+224D Equivalent To
  ("&bump;" "<#224E>")  ; ≎ U+224E Geometrically Equivalent To
  ("&bumpe;" "<#224F>")  ; ≏ U+224F Difference Between
  ("&ecir;" "<#2256>")  ; ≖ U+2256 Ring In Equal To
  ("&cire;" "<#2257>")  ; ≗ U+2257 Ring Equal To
  ("&equiv;" "<#2261>")  ; ≡ U+2261 Identical To
  ("&nequiv;" "<#2262>")  ; ≢ U+2262 Not Identical To
  ("&le;" "<#2264>")  ; ≤ U+2264 Less-Than or Equal To
  ("&ge;" "<#2265>")  ; ≥ U+2265 Greater-Than or Equal To
  ("&lE;" "<#2266>")  ; ≦ U+2266 Less-Than Over Equal To
  ("&gE;" "<#2267>")  ; ≧ U+2267 Greater-Than Over Equal To
  ("&lnE;" "<#2268>")  ; ≨ U+2268 Less-Than But Not Equal To
  ("&gnE;" "<#2269>")  ; ≩ U+2269 Greater-Than But Not Equal To
  ("&Lt;" "<#226A>")  ; ≪ U+226A Much Less-Than
  ("&Gt;" "<#226B>")  ; ≫ U+226B Much Greater-Than
  ("&NotCupCap;" "<#226D>")  ; ≭ U+226D Not Equivalent To
  ("&nlt;" "<#226E>")  ; ≮ U+226E Not Less-Than
  ("&ngt;" "<#226F>")  ; ≯ U+226F Not Greater-Than
  ("&nle;" "<#2270>")  ; ≰ U+2270 Neither Less-Than nor Equal To
  ("&nge;" "<#2271>")  ; ≱ U+2271 Neither Greater-Than nor Equal To
  ("&lsim;" "<#2272>")  ; ≲ U+2272 Less-Than or Equivalent To
  ("&gsim;" "<#2273>")  ; ≳ U+2273 Greater-Than or Equivalent To
  ("&nlsim;" "<#2274>")  ; ≴ U+2274 Neither Less-Than nor Equivalent To
  ("&ngsim;" "<#2275>")  ; ≵ U+2275 Neither Greater-Than nor Equivalent To
  ("&pr;" "<#227A>")  ; ≺ U+227A Precedes
  ("&sc;" "<#227B>")  ; ≻ U+227B Succeeds
  ("&prcue;" "<#227C>")  ; ≼ U+227C Precedes or Equal To
  ("&sccue;" "<#227D>")  ; ≽ U+227D Succeeds or Equal To
  ("&prsim;" "<#227E>")  ; ≾ U+227E Precedes or Equivalent To
  ("&scsim;" "<#227F>")  ; ≿ U+227F Succeeds or Equivalent To
  ("&npr;" "<#2280>")  ; ⊀ U+2280 Does Not Precede
  ("&nsc;" "<#2281>")  ; ⊁ U+2281 Does Not Succeed
  ("&sub;" "<#2282>")  ; ⊂ U+2282 Subset Of
  ("&sup;" "<#2283>")  ; ⊃ U+2283 Superset Of
  ("&nsub;" "<#2284>")  ; ⊄ U+2284 Not a Subset Of
  ("&nsup;" "<#2285>")  ; ⊅ U+2285 Not a Superset Of
  ("&sube;" "<#2286>")  ; ⊆ U+2286 Subset of or Equal To
  ("&supe;" "<#2287>")  ; ⊇ U+2287 Superset of or Equal To
  ("&nsube;" "<#2288>")  ; ⊈ U+2288 Neither a Subset of nor Equal To
  ("&nsupe;" "<#2289>")  ; ⊉ U+2289 Neither a Superset of nor Equal To
  ("&subne;" "<#228A>")  ; ⊊ U+228A Subset of with Not Equal To
  ("&supne;" "<#228B>")  ; ⊋ U+228B Superset of with Not Equal To
  ("&uplus;" "<#228E>")  ; ⊎ U+228E Multiset Union
  ("&sqsub;" "<#228F>")  ; ⊏ U+228F Square Image Of
  ("&sqsup;" "<#2290>")  ; ⊐ U+2290 Square Original Of
  ("&sqsube;" "<#2291>")  ; ⊑ U+2291 Square Image of or Equal To
  ("&sqsupe;" "<#2292>")  ; ⊒ U+2292 Square Original of or Equal To
  ("&sqcap;" "<#2293>")  ; ⊓ U+2293 Square Cap
  ("&sqcup;" "<#2294>")  ; ⊔ U+2294 Square Cup
  ("&oplus;" "<#2295>")  ; ⊕ U+2295 Circled Plus
  ("&ominus;" "<#2296>")  ; ⊖ U+2296 Circled Minus
  ("&otimes;" "<#2297>")  ; ⊗ U+2297 Circled Times
  ("&osol;" "<#2298>")  ; ⊘ U+2298 Circled Division Slash
  ("&odot;" "<#2299>")  ; ⊙ U+2299 Circled Dot Operator
  ("&ocir;" "<#229A>")  ; ⊚ U+229A Circled Ring Operator
  ("&oast;" "<#229B>")  ; ⊛ U+229B Circled Asterisk Operator
  ("&odash;" "<#229D>")  ; ⊝ U+229D Circled Dash
  ("&plusb;" "<#229E>")  ; ⊞ U+229E Squared Plus
  ("&minusb;" "<#229F>")  ; ⊟ U+229F Squared Minus
  ("&timesb;" "<#22A0>")  ; ⊠ U+22A0 Squared Times
  ("&sdotb;" "<#22A1>")  ; ⊡ U+22A1 Squared Dot Operator
  ("&vdash;" "<#22A2>")  ; ⊢ U+22A2 Right Tack
  ("&dashv;" "<#22A3>")  ; ⊣ U+22A3 Left Tack
  ("&top;" "<#22A4>")  ; ⊤ U+22A4 Down Tack
  ("&perp;" "<#22A5>")  ; ⊥ U+22A5 Up Tack
  ("&models;" "<#22A7>")  ; ⊧ U+22A7 Models
  ("&vDash;" "<#22A8>")  ; ⊨ U+22A8 True
  ("&Vdash;" "<#22A9>")  ; ⊩ U+22A9 Forces
  ("&Vvdash;" "<#22AA>")  ; ⊪ U+22AA Triple Vertical Bar Right Turnstile
  ("&prurel;" "<#22B0>")  ; ⊰ U+22B0 Precedes Under Relation
  ("&vltri;" "<#22B2>")  ; ⊲ U+22B2 Normal Subgroup Of
  ("&vrtri;" "<#22B3>")  ; ⊳ U+22B3 Contains As Normal Subgroup
  ("&ltrie;" "<#22B4>")  ; ⊴ U+22B4 Normal Subgroup of or Equal To
  ("&rtrie;" "<#22B5>")  ; ⊵ U+22B5 Contains As Normal Subgroup or Equal To
  ("&hercon;" "<#22B9>")  ; ⊹ U+22B9 Hermitian Conjugate Matrix
  ("&xwedge;" "<#22C0>")  ; ⋀ U+22C0 N-Ary Logical And
  ("&xvee;" "<#22C1>")  ; ⋁ U+22C1 N-Ary Logical Or
  ("&xcap;" "<#22C2>")  ; ⋂ U+22C2 N-Ary Intersection
  ("&xcup;" "<#22C3>")  ; ⋃ U+22C3 N-Ary Union
  ("&diamond;" "<#22C4>")  ; ⋄ U+22C4 Diamond Operator
  ("&sdot;" "<#22C5>")  ; ⋅ U+22C5 Dot Operator
  ("&Star;" "<#22C6>")  ; ⋆ U+22C6 Star Operator
  ("&ltimes;" "<#22C9>")  ; ⋉ U+22C9 Left Normal Factor Semidirect Product
  ("&rtimes;" "<#22CA>")  ; ⋊ U+22CA Right Normal Factor Semidirect Product
  ("&lthree;" "<#22CB>")  ; ⋋ U+22CB Left Semidirect Product
  ("&rthree;" "<#22CC>")  ; ⋌ U+22CC Right Semidirect Product
  ("&bsime;" "<#22CD>")  ; ⋍ U+22CD Reversed Tilde Equals
  ("&cuvee;" "<#22CE>")  ; ⋎ U+22CE Curly Logical Or
  ("&cuwed;" "<#22CF>")  ; ⋏ U+22CF Curly Logical And
  ("&Sub;" "<#22D0>")  ; ⋐ U+22D0 Double Subset
  ("&Sup;" "<#22D1>")  ; ⋑ U+22D1 Double Superset
  ("&Cap;" "<#22D2>")  ; ⋒ U+22D2 Double Intersection
  ("&Cup;" "<#22D3>")  ; ⋓ U+22D3 Double Union
  ("&Ll;" "<#22D8>")  ; ⋘ U+22D8 Very Much Less-Than
  ("&Gg;" "<#22D9>")  ; ⋙ U+22D9 Very Much Greater-Than
  ("&cuepr;" "<#22DE>")  ; ⋞ U+22DE Equal To or Precedes
  ("&cuesc;" "<#22DF>")  ; ⋟ U+22DF Equal To or Succeeds
  ("&nprcue;" "<#22E0>")  ; ⋠ U+22E0 Does Not Precede or Equal
  ("&nsccue;" "<#22E1>")  ; ⋡ U+22E1 Does Not Succeed or Equal
  ("&nsqsube;" "<#22E2>")  ; ⋢ U+22E2 Not Square Image of or Equal To
  ("&nsqsupe;" "<#22E3>")  ; ⋣ U+22E3 Not Square Original of or Equal To
  ("&lnsim;" "<#22E6>")  ; ⋦ U+22E6 Less-Than But Not Equivalent To
  ("&gnsim;" "<#22E7>")  ; ⋧ U+22E7 Greater-Than But Not Equivalent To
  ("&prnsim;" "<#22E8>")  ; ⋨ U+22E8 Precedes But Not Equivalent To
  ("&scnsim;" "<#22E9>")  ; ⋩ U+22E9 Succeeds But Not Equivalent To
  ("&nltri;" "<#22EA>")  ; ⋪ U+22EA Not Normal Subgroup Of
  ("&nrtri;" "<#22EB>")  ; ⋫ U+22EB Does Not Contain As Normal Subgroup
  ("&nltrie;" "<#22EC>")  ; ⋬ U+22EC Not Normal Subgroup of or Equal To
  ("&nrtrie;" "<#22ED>")  ; ⋭ U+22ED Does Not Contain As Normal Subgroup or Equal
  ("&vellip;" "<#22EE>")  ; ⋮ U+22EE Vertical Ellipsis
  ("&ctdot;" "<#22EF>")  ; ⋯ U+22EF Midline Horizontal Ellipsis
  ("&utdot;" "<#22F0>")  ; ⋰ U+22F0 Up Right Diagonal Ellipsis
  ("&dtdot;" "<#22F1>")  ; ⋱ U+22F1 Down Right Diagonal Ellipsis
  ("&disin;" "<#22F2>")  ; ⋲ U+22F2 Element of with Long Horizontal Stroke
  ("&lceil;" "<#2308>")  ; ⌈ U+2308 Left Ceiling
  ("&rceil;" "<#2309>")  ; ⌉ U+2309 Right Ceiling
  ("&lfloor;" "<#230A>")  ; ⌊ U+230A Left Floor
  ("&rfloor;" "<#230B>")  ; ⌋ U+230B Right Floor
  ("&lang;" "<#2329>")  ; 〈 U+2329 Left-Pointing Angle Bracket
  ("&rang;" "<#232A>")  ; 〉 U+232A Right-Pointing Angle Bracket
  ("&Alpha;" "<#0391>")  ; Α U+0391 Greek Capital Letter Alpha
  ("&Beta;" "<#0392>")  ; Β U+0392 Greek Capital Letter Beta
  ("&Gamma;" "<#0393>")  ; Γ U+0393 Greek Capital Letter Gamma
  ("&Delta;" "<#0394>")  ; Δ U+0394 Greek Capital Letter Delta
  ("&Epsilon;" "<#0395>")  ; Ε U+0395 Greek Capital Letter Epsilon
  ("&Zeta;" "<#0396>")  ; Ζ U+0396 Greek Capital Letter Zeta
  ("&Eta;" "<#0397>")  ; Η U+0397 Greek Capital Letter Eta
  ("&Theta;" "<#0398>")  ; Θ U+0398 Greek Capital Letter Theta
  ("&Iota;" "<#0399>")  ; Ι U+0399 Greek Capital Letter Iota
  ("&Kappa;" "<#039A>")  ; Κ U+039A Greek Capital Letter Kappa
  ("&Lambda;" "<#039B>")  ; Λ U+039B Greek Capital Letter Lamda
  ("&Mu;" "<#039C>")  ; Μ U+039C Greek Capital Letter Mu
  ("&Nu;" "<#039D>")  ; Ν U+039D Greek Capital Letter Nu
  ("&Xi;" "<#039E>")  ; Ξ U+039E Greek Capital Letter Xi
  ("&Omicron;" "<#039F>")  ; Ο U+039F Greek Capital Letter Omicron
  ("&Pi;" "<#03A0>")  ; Π U+03A0 Greek Capital Letter Pi
  ("&Rho;" "<#03A1>")  ; Ρ U+03A1 Greek Capital Letter Rho
  ("&Sigma;" "<#03A3>")  ; Σ U+03A3 Greek Capital Letter Sigma
  ("&Tau;" "<#03A4>")  ; Τ U+03A4 Greek Capital Letter Tau
  ("&Upsilon;" "<#03A5>")  ; Υ U+03A5 Greek Capital Letter Upsilon
  ("&Phi;" "<#03A6>")  ; Φ U+03A6 Greek Capital Letter Phi
  ("&Chi;" "<#03A7>")  ; Χ U+03A7 Greek Capital Letter Chi
  ("&Psi;" "<#03A8>")  ; Ψ U+03A8 Greek Capital Letter Psi
  ("&Omega;" "<#03A9>")  ; Ω U+03A9 Greek Capital Letter Omega
  ("&alpha;" "<#03B1>")  ; α U+03B1 Greek Small Letter Alpha
  ("&beta;" "<#03B2>")  ; β U+03B2 Greek Small Letter Beta
  ("&gamma;" "<#03B3>")  ; γ U+03B3 Greek Small Letter Gamma
  ("&delta;" "<#03B4>")  ; δ U+03B4 Greek Small Letter Delta
  ("&epsilon;" "<#03B5>")  ; ε U+03B5 Greek Small Letter Epsilon
  ("&zeta;" "<#03B6>")  ; ζ U+03B6 Greek Small Letter Zeta
  ("&eta;" "<#03B7>")  ; η U+03B7 Greek Small Letter Eta
  ("&theta;" "<#03B8>")  ; θ U+03B8 Greek Small Letter Theta
  ("&iota;" "<#03B9>")  ; ι U+03B9 Greek Small Letter Iota
  ("&kappa;" "<#03BA>")  ; κ U+03BA Greek Small Letter Kappa
  ("&lambda;" "<#03BB>")  ; λ U+03BB Greek Small Letter Lamda
  ("&mu;" "<#03BC>")  ; μ U+03BC Greek Small Letter Mu
  ("&nu;" "<#03BD>")  ; ν U+03BD Greek Small Letter Nu
  ("&xi;" "<#03BE>")  ; ξ U+03BE Greek Small Letter Xi
  ("&omicron;" "<#03BF>")  ; ο U+03BF Greek Small Letter Omicron
  ("&rho;" "<#03C1>")  ; ρ U+03C1 Greek Small Letter Rho
  ("&sigmaf;" "<#03C2>")  ; ς U+03C2 Greek Small Letter Final Sigma
  ("&sigma;" "<#03C3>")  ; σ U+03C3 Greek Small Letter Sigma
  ("&tau;" "<#03C4>")  ; τ U+03C4 Greek Small Letter Tau
  ("&upsilon;" "<#03C5>")  ; υ U+03C5 Greek Small Letter Upsilon
  ("&phi;" "<#03C6>")  ; φ U+03C6 Greek Small Letter Phi
  ("&chi;" "<#03C7>")  ; χ U+03C7 Greek Small Letter Chi
  ("&psi;" "<#03C8>")  ; ψ U+03C8 Greek Small Letter Psi
  ("&omega;" "<#03C9>")  ; ω U+03C9 Greek Small Letter Omega
  ("&thetasym;" "<#03D1>")  ; ϑ U+03D1 Greek Theta Symbol
  ("&upsih;" "<#03D2>")  ; ϒ U+03D2 Greek Upsilon with Hook Symbol
  ("&piv;" "<#03D6>")  ; ϖ U+03D6 Greek Pi Symbol
  ("&olarr;" "<#21BA>")  ; ↺ U+21BA Anticlockwise Open Circle Arrow
  ("&orarr;" "<#21BB>")  ; ↻ U+21BB Clockwise Open Circle Arrow
  ("&lharu;" "<#21BC>")  ; ↼ U+21BC Leftwards Harpoon with Barb Upwards
  ("&lhard;" "<#21BD>")  ; ↽ U+21BD Leftwards Harpoon with Barb Downwards
  ("&uharr;" "<#21BE>")  ; ↾ U+21BE Upwards Harpoon with Barb Rightwards
  ("&uharl;" "<#21BF>")  ; ↿ U+21BF Upwards Harpoon with Barb Leftwards
  ("&rharu;" "<#21C0>")  ; ⇀ U+21C0 Rightwards Harpoon with Barb Upwards
  ("&rhard;" "<#21C1>")  ; ⇁ U+21C1 Rightwards Harpoon with Barb Downwards
  ("&dharr;" "<#21C2>")  ; ⇂ U+21C2 Downwards Harpoon with Barb Rightwards
  ("&dharl;" "<#21C3>")  ; ⇃ U+21C3 Downwards Harpoon with Barb Leftwards
  ("&rlarr;" "<#21C4>")  ; ⇄ U+21C4 Rightwards Arrow Over Leftwards Arrow
  ("&udarr;" "<#21C5>")  ; ⇅ U+21C5 Upwards Arrow Leftwards of Downwards Arrow
  ("&lrarr;" "<#21C6>")  ; ⇆ U+21C6 Leftwards Arrow Over Rightwards Arrow
  ("&llarr;" "<#21C7>")  ; ⇇ U+21C7 Leftwards Paired Arrows
  ("&uuarr;" "<#21C8>")  ; ⇈ U+21C8 Upwards Paired Arrows
  ("&rrarr;" "<#21C9>")  ; ⇉ U+21C9 Rightwards Paired Arrows
  ("&ddarr;" "<#21CA>")  ; ⇊ U+21CA Downwards Paired Arrows
  ("&lrhar;" "<#21CB>")  ; ⇋ U+21CB Leftwards Harpoon Over Rightwards Harpoon
  ("&rlhar;" "<#21CC>")  ; ⇌ U+21CC Rightwards Harpoon Over Leftwards Harpoon
  ("&lArr;" "<#21D0>")  ; ⇐ U+21D0 Leftwards Double Arrow
  ("&uArr;" "<#21D1>")  ; ⇑ U+21D1 Upwards Double Arrow
  ("&rArr;" "<#21D2>")  ; ⇒ U+21D2 Rightwards Double Arrow
  ("&dArr;" "<#21D3>")  ; ⇓ U+21D3 Downwards Double Arrow
  ("&hArr;" "<#21D4>")  ; ⇔ U+21D4 Left Right Double Arrow
  ("&vArr;" "<#21D5>")  ; ⇕ U+21D5 Up Down Double Arrow
  ("&nwArr;" "<#21D6>")  ; ⇖ U+21D6 North West Double Arrow
  ("&neArr;" "<#21D7>")  ; ⇗ U+21D7 North East Double Arrow
  ("&seArr;" "<#21D8>")  ; ⇘ U+21D8 South East Double Arrow
  ("&swArr;" "<#21D9>")  ; ⇙ U+21D9 South West Double Arrow
  ("&lAarr;" "<#21DA>")  ; ⇚ U+21DA Leftwards Triple Arrow
  ("&rAarr;" "<#21DB>")  ; ⇛ U+21DB Rightwards Triple Arrow
  ("&ziglarr;" "<#21DC>")  ; ⇜ U+21DC Leftwards Squiggle Arrow
  ("&zigrarr;" "<#21DD>")  ; ⇝ U+21DD Rightwards Squiggle Arrow
  ("&larrb;" "<#21E4>")  ; ⇤ U+21E4 Leftwards Arrow To Bar
  ("&rarrb;" "<#21E5>")  ; ⇥ U+21E5 Rightwards Arrow To Bar
  ("&duarr;" "<#21F5>")  ; ⇵ U+21F5 Downwards Arrow Leftwards of Upwards Arrow
  ("&xlarr;" "<#27F5>")  ; ⟵ U+27F5 Long Leftwards Arrow
  ("&xrarr;" "<#27F6>")  ; ⟶ U+27F6 Long Rightwards Arrow
  ("&xharr;" "<#27F7>")  ; ⟷ U+27F7 Long Left Right Arrow
  ("&xlArr;" "<#27F8>")  ; ⟸ U+27F8 Long Leftwards Double Arrow
  ("&xrArr;" "<#27F9>")  ; ⟹ U+27F9 Long Rightwards Double Arrow
  ("&xhArr;" "<#27FA>")  ; ⟺ U+27FA Long Left Right Double Arrow
  ("&dzigrarr;" "<#27FF>")  ; ⟿ U+27FF Long Rightwards Squiggle Arrow
  ("&xmap;" "<#27FC>")  ; ⟼ U+27FC Long Rightwards Arrow from Bar
  ("&lurdshar;" "<#294A>")  ; ⥊ U+294A Left Barb Up Right Barb Down Harpoon
  ("&ldrushar;" "<#294B>")  ; ⥋ U+294B Left Barb Down Right Barb Up Harpoon
  ("&RightUpDownVector;" "<#294F>")  ; ⥏ U+294F Up Barb Right Down Barb Right Harpoon
  ("&DownLeftRightVector;" "<#2950>")  ; ⥐ U+2950 Left Barb Down Right Barb Down Harpoon
  ("&LeftUpDownVector;" "<#2951>")  ; ⥑ U+2951 Up Barb Left Down Barb Left Harpoon
  ("&udhar;" "<#296E>")  ; ⥮ U+296E Upwards Harpoon with Barb Left Beside Downwards Harpoon with Barb Right
  ("&duhar;" "<#296F>")  ; ⥯ U+296F Downwards Harpoon with Barb Left Beside Upwards Harpoon with Barb Right
  ("&eth;" "<#00F0>")  ; ð U+00F0 Latin Small Letter Eth
  ("&thorn;" "<#00FE>")  ; þ U+00FE Latin Small Letter Thorn
  ("&Dstrok;" "<#0110>")  ; Đ U+0110 Latin Capital Letter D with Stroke
  ("&dstrok;" "<#0111>")  ; đ U+0111 Latin Small Letter D with Stroke
  ("&Hstrok;" "<#0126>")  ; Ħ U+0126 Latin Capital Letter H with Stroke
  ("&hstrok;" "<#0127>")  ; ħ U+0127 Latin Small Letter H with Stroke
  ("&imath;" "<#0131>")  ; ı U+0131 Latin Small Letter Dotless I
  ("&kgreen;" "<#0138>")  ; ĸ U+0138 Latin Small Letter Kra
  ("&Lstrok;" "<#0141>")  ; Ł U+0141 Latin Capital Letter L with Stroke
  ("&lstrok;" "<#0142>")  ; ł U+0142 Latin Small Letter L with Stroke
  ("&ENG;" "<#014A>")  ; Ŋ U+014A Latin Capital Letter Eng
  ("&eng;" "<#014B>")  ; ŋ U+014B Latin Small Letter Eng
  ("&Tstrok;" "<#0166>")  ; Ŧ U+0166 Latin Capital Letter T with Stroke
  ("&tstrok;" "<#0167>")  ; ŧ U+0167 Latin Small Letter T with Stroke
  ("&DownBreve;" "<#0311>")  ; ̑ U+0311 Combining Inverted Breve
  ("&female;" "<#02640>")  ; ♀ U+02640 female
  ("&male;" "<#02642>")  ; ♂ U+02642 male
  ("&Longleftrightarrow;" "<#027FA>")  ; ⟺ U+027FA
  ("&Implies;" "<#021D2>")  ; ⇒ U+021D2
  ("&longrightarrow;" "<#027F6>")  ; ⟶ U+027F6
  ("&aleph;" "<#02135>")  ; ℵ U+02135
  ("&uparrow;" "<#02191>")  ; ↑ U+02191
  ("&Product;" "<#0220F>")  ; ∏ U+0220F
  ("&exponentiale;" "<mathe>")  ; ⅇ U+02147

  )

(logic-table mathml-symbol->tm%
  ("&mldr;" "<cdots>")
  ("&ThinSpace;" (hspace "0.1666667em"))
  ("&MediumSpace;" (hspace "0.2222222em"))
  ("&ThickSpace;" (hspace "0.2777778em"))
  (" " (hspace "1em"))
  (" " (hspace "1en")))
  
; when in mover or munder, the decorations in the 3 logic-tables below, 
; automatically become wide, irrespective of the "stretchy" attribute  
(logic-table mathml-above->tm%
  ("<overbrace>" "<wide-overbrace>")
  ("<underbrace>" "<wide-underbrace>")
  ("<sqoverbrace>" "<wide-sqoverbrace>")
  ("<squnderbrace>" "<wide-squnderbrace>")
  ("<poverbrace>" "<wide-poverbrace>")
  ("<punderbrace>" "<wide-punderbrace>"))

(logic-table mathml-below->tm%
  ("<overbrace>" "<wide-overbrace*>")
  ("<underbrace>" "<wide-underbrace*>")
  ("<sqoverbrace>" "<wide-sqoverbrace*>")
  ("<squnderbrace>" "<wide-squnderbrace*>")
  ("<poverbrace>" "<wide-poverbrace*>")
  ("<punderbrace>" "<wide-punderbrace*>"))

(logic-table mathml-above-below->tm%
  ("<vect>" "<vect>")
  ("<breve>" "<breve>")
  ("<check>" "<check>")
  ("^" "^")
  ("~" "~")
  ("¯"  "<wide-bar>")
  ("<invbreve>" "<invbreve>")
  ("<acute>" "<acute>")
  ("<grave>" "<grave>")
  ("<abovering>" "<abovering>")
  ("<dddot>" "<dddot>")
  )



(logic-rules
  ((mathml-operator->tm% 'x 'y) (tm->mathml-operator% 'y 'x))
  ((mathml-symbol->tm% 'x 'y) (mathml-constant->tm% 'x 'y))
  ((mathml-symbol->tm% 'x 'y) (mathml-operator->tm% 'x 'y))
  ((mathml-below->tm% 'x 'y) (mathml-above-below->tm% 'x 'y))
  ((mathml-above->tm% 'x 'y) (mathml-above-below->tm% 'x 'y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table tm->mathml-left%
  ("(" "(")
  ("[" "[")
  ("{" "{")
  ("langle" "&LeftAngleBracket;")
  ("lfloor" "&LeftFloor;")
  ("lceil" "&LeftCeiling;")
  ("llbracket" "&LeftDoubleBracket;")
  ("/" "/"))

(logic-table tmtm-left%
  ;; For HTML entities &lfloor;, &lceil, etc.
  ("<lfloor>" "lfloor")
  ("<lceil>" "lceil")
  ("<langle>" "langle"))

(logic-table tm->mathml-right%
  (")" ")")
  ("]" "]")
  ("}" "}")
  ("rangle" "&RightAngleBracket;")
  ("rfloor" "&RightFloor;")
  ("rceil" "&RightCeiling;")
  ("rrbracket" "&RightDoubleBracket;")
  ("\\\\" "&Backslash;"))

(logic-table tmtm-right%
  ;; For HTML entities &rfloor;, &rceil, etc.
  ("<rfloor>" "rfloor")
  ("<rceil>" "rceil")
  ("<rangle>" "rangle"))

(logic-table tm->mathml-big%
  ("sum" "&Sum;")
  ("prod" "&Product;")
  ("int" "&Integral;")
  ("oint" "&ContourIntegral;")
  ("amalg" "&Coproduct;")
  ("cap" "&Intersection;")
  ("cup" "&Union;")
  ("wedge" "&Wedge;")
  ("vee" "&Vee;")
  ("odot" "&CircleDot;")
  ("oplus" "&CirclePlus;")
  ("otimes" "&CircleTimes;")
  ("sqcap" "&SquareIntersection;")     ;; FIXME: displayed too small
  ("sqcup" "&SquareUnion;")            ;; FIXME: displayed too small
  ;;("curlywedge" "&CurlyWedge;")
  ;;("curlyvee" "&CurlyVee;")
  ;;("triangleup" "&TriangleUp;")
  ;;("triangledown" "&TriangleDown;")
  ;;("box" "&Box;")
  ("pluscup" "&UnionPlus;")
  ;;("parallel" "&Parallel;")
  ;;("interleave" "&Interleave;")
)

(logic-table tmtm-big%
  ;; For HTML entities &sum;, &prod;, etc.
  ("<sum>" "sum")
  ("<prod>" "prod")
  ("<int>" "int"))

(logic-table tm->mathml-above-below%
  ("^" "&Hat;")
  ("~" "&Tilde;")
  ("<bar>" "&OverBar;")
  ("<vect>" "&RightVector;")
  ("<check>" "&Hacek;")
  ("<breve>" "&Breve;")
  ("<invbreve>" "&#x311;") ; combining inverted breve
  ("<acute>" "&DiacriticalAcute;")
  ("<grave>" "&DiacriticalGrave;")
  ("<dot>" "&DiacriticalDot;")
  ("<ddot>" "&DoubleDot;")
;;  ("<abovering>" "&AboveRing;")
  ("<wide-varrightarrow>" "&RightArrow;")
  ("<wide-varleftarrow>" "&LeftArrow;"))

(logic-table tm->mathml-only-above%
  ("<wide-overbrace>" "&OverBrace;")
  ("<wide-underbrace*>" "&UnderBrace;")
  ("<wide-poverbrace>" "&OverParenthesis;")
  ("<wide-punderbrace*>" "&UnderParenthesis;")
  ("<wide-sqoverbrace>" "&OverBracket;")
  ("<wide-squnderbrace*>" "&UnderBracket;"))

(logic-table tm->mathml-only-below%
  ("<wide-overbrace*>" "&OverBrace;")
  ("<wide-underbrace>" "&UnderBrace;")
  ("<wide-poverbrace*>" "&OverParenthesis;")
  ("<wide-punderbrace>" "&UnderParenthesis;")
  ("<wide-sqoverbrace*>" "&OverBracket;")
  ("<wide-squnderbrace>" "&UnderBracket;"))

(logic-rules
  ((tm->mathml-large% 'x 'y) (tm->mathml-left% 'x 'y))
  ((tm->mathml-large% 'x 'y) (tm->mathml-right% 'x 'y))
  ((mathml-left->tm% 'x 'y) (tm->mathml-left% 'y 'x))
  ((mathml-right->tm% 'x 'y) (tm->mathml-right% 'y 'x))
  ((mathml-large->tm% 'x 'y) (mathml-left->tm% 'x 'y))
  ((mathml-large->tm% 'x 'y) (mathml-right->tm% 'x 'y))
  ((mathml-big->tm% 'x 'y) (tm->mathml-big% 'y 'x))
  ((tm->mathml-above% 'x 'y) (tm->mathml-only-above% 'x 'y))
  ((tm->mathml-above% 'x 'y) (tm->mathml-above-below% 'x 'y))
  ((tm->mathml-below% 'x 'y) (tm->mathml-only-below% 'x 'y))
  ((tm->mathml-below% 'x 'y) (tm->mathml-above-below% 'x 'y))
  ((tm->mathml-wide*% 'x 'y) (tm->mathml-only-below% 'x 'y))
  ((tm->mathml-wide*% 'x 'y) (tm->mathml-only-above% 'x 'y))
  ((tm->mathml-wide*% 'x 'y) (tm->mathml-above-below% 'x 'y))
  ((tm->mathml-wide% 'x 'y) (tm->mathml-wide*% 'x 'y))
  ((tm->mathml-wide% "<wide-bar>" "&OverBar;"))
  ((mathml-above->tm% 'x 'y) (tm->mathml-above% 'y 'x))
  ((mathml-below->tm% 'x 'y) (tm->mathml-below% 'y 'x))
  ((mathml-wide->tm% 'x 'y) (tm->mathml-wide*% 'y 'x)))
