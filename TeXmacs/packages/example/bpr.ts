<TeXmacs|1.0.4.7>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|bpr|1.0>

    <\src-purpose>
      Style package for the book ``Algorithms in Real Algebraic Geometry'' by
      Basu, Pollack and Roy. With the aid of <TeXmacs>, this book should
      become interactive.
    </src-purpose>

    <src-copyright|1998--2004|Marie-Françoise Roy and Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <active*|<\src-comment>
    Global environment variables.
  </src-comment>>

  <assign|page-screen-left|1.5cm>

  <assign|page-screen-right|1.5cm>

  <active*|<\src-comment>
    Abbreviations.
  </src-comment>>

  <assign|A|<macro|<with|mode|text|font-family|rm|A>>>

  <assign|addots|<macro|<mathinner|<mkern>1mu
  <raise>1pt<vbox|<kern>7pt<hbox|.>> <mkern>2mu<raise>4pt<hbox|.><mkern>2mu
  <raise>7pt<hbox|.><mkern>1mu>>>

  <assign|Ada|<macro|Ada>>

  <assign|Adj|<macro|Adj>>

  <assign|alg|<macro|alg>>

  <assign|App|<macro|App>>

  <assign|BB|<macro|<with|mode|text|font-family|rm|B>>>

  <assign|B|<macro|Bor>>

  <assign|be|<macro|b>>

  <assign|ba|<macro|ba>>

  <assign|Bern|<macro|Bern>>

  <assign|Bez|<macro|Bez>>

  <assign|BezElim|<macro|BezElim>>

  <assign|BElim|<macro|BElim>>

  <assign|C|<macro|<with|mode|text|font-family|rm|C>>>

  <assign|CF|<macro|CF>>

  <assign|cf|<macro|cof>>

  <assign|Ch|<macro|Ch>>

  <assign|chiep|<macro|\<chi\>>>

  <assign|charpoly|<macro|CharPol>>

  <assign|Co|<macro|Co>>

  <assign|complex|<macro|<Complex|>>>

  <assign|Complex|<macro|1|<with|mode|text|mode|math|<with|math-font|Bbb|C><rsup|<arg|1>>>>>

  <assign|cont|<macro|cont>>

  <assign|Cr|<macro|Cr>>

  <assign|CS|<macro|CS>>

  <assign|CSign|<macro|CSIGN>>

  <assign|der|<macro|<with|mode|text|font-family|rm|d>>>

  <assign|D|<macro|<with|mode|text|font-family|rm|D>>>

  <assign|decide|<macro|Decide>>

  <assign|Def|<macro|Def>>\ 

  <assign|deg|<macro|Deg>>\ 

  <assign|Der|<macro|Der>>

  <assign|diam|<macro|diam>>

  <assign|Disc|<macro|Disc>>

  <assign|dist|<macro|dist>>

  <assign|epc|<macro|EPC>>

  <assign|Ext|<macro|Ext>>

  <assign|Extend|<macro|Ex>>

  <assign|Elim|<macro|Elim>>

  <assign|eps|<macro|\<varepsilon\>>>

  <assign|EQ|<macro|EuQ>>

  <assign|F|<macro|<with|mode|text|font-family|rm|F>>>

  <assign|False|<macro|False>>

  <assign|Free|<macro|Free>>

  <assign|gau|<macro|Gau>>

  <assign|Grad|<macro|Grad>>

  <assign|grlex|<macro|grlex>>

  <assign|Hom|<macro|Hom>>

  <assign|Han|<macro|Han>>

  <assign|han|<macro|han>>

  <assign|Her|<macro|Her>>

  <assign|Hes|<macro|Hes>>

  <assign|her|<macro|her>>

  <assign|Ho|<macro|Ho>>

  <assign|Hor|<macro|Hor>>

  <assign|Ideal|<macro|Ideal>>

  <assign|Id|<macro|Id>>

  <assign|Im|<macro|Im>>

  <assign|Ind|<macro|Ind>>

  <assign|ini|<macro|In>>

  <assign|K|<macro|<with|mode|text|font-family|rm|K>>>

  <assign|Ker|<macro|Ker>>

  <assign|la|<macro|\<langle\>>>

  <assign|lc|<macro|lcof>>

  <assign|lcm|<macro|lcm>>

  <assign|ldp|<macro|ldp>>

  <assign|Le|<macro|Len>>

  <assign|lex|<macro|lex>>

  <assign|LL|<macro|<with|font-family|rm|L>>>

  <assign|lmon|<macro|lmon>>

  <assign|lt|<macro|lt>>

  <assign|M|<macro|Mea>>

  <assign|Mat|<macro|Mat>>

  <assign|modul|<macro|mod>>

  <assign|Mon|<macro|Mon>>

  <assign|MUS|<macro|MonUnS>>

  <assign|N|<macro|<with|math-font|Bbb|N>>>

  <assign|n|<macro|n>>

  <assign|NCom|<macro|NCom>>

  <assign|Newton|<macro|Newt>>

  <assign|num|<macro|num>>

  <assign|NF|<macro|NF>>

  <assign|NN|<macro|<with|font-family|rm|N>>>

  <assign|pagcd|<macro|posgcd>>

  <assign|paremseq|<macro|TRems>>

  <assign|parem|<macro|paRem>>

  <assign|pdet|<macro|pdet>>

  <assign|PMC|<macro|PmV>>

  <assign|Po|<macro|Pol>>

  <assign|pos|<macro|pos>>

  <assign|posgcd|<macro|posgcd>>

  <assign|posquo|<macro|posQuo>>

  <assign|PP|<macro|<with|math-font|Bbb|P>>>

  <assign|pquo|<macro|PQuo>>

  <assign|pr|<macro|proj>>

  <assign|prem|<macro|PRem>>

  <assign|Proj|<macro|Proj>>

  <assign|Q|<macro|<with|math-font|Bbb|Q>>>

  <assign|Qu|<macro|Qu>>

  <assign|quo|<macro|Quo>>

  <assign|R|<macro|<with|mode|text|font-family|rm|font-shape|right|R>>>

  <assign|ra|<macro|\<rangle\>>>

  <assign|rad|<macro|rad>>

  <assign|Rad|<macro|Rad>>

  <assign|Rank|<macro|Rank>>

  <assign|re|<macro|<Real|>>>

  <assign|Real|<macro|1|<with|mode|text|mode|math|<with|math-font|Bbb|R><rsup|<arg|1>>>>>

  <assign|Rec|<macro|Rec>>

  <assign|Red|<macro|Red>>

  <assign|RElim|<macro|RElim>>

  <assign|Rem|<macro|Remo>>

  <assign|rem|<macro|Rem>>

  <assign|Res|<macro|Res>>

  <assign|RM|<macro|RM>>

  <assign|RR|<macro|Reali>>

  <assign|SB|<macro|SB>>

  <assign|SDisc|<macro|sDisc>>

  <assign|sep|<macro|sep>>

  <assign|SH|<macro|SyHa>>

  <assign|SHPol|<macro|SyHaPol>>

  <assign|SI|<macro|SIGN<rsub|R>>>

  <assign|SIGN|<macro|SIGN>>

  <assign|Sign|<macro|Sign>>

  <assign|sign|<macro|sign>>

  <assign|SSign|<macro|SSIGN>>

  <assign|SSQ|<macro|SRemS>>

  <assign|SSU|<macro|SRemU>>

  <assign|SSV|<macro|SRemV>>

  <assign|SQ|<macro|StQ>>

  <assign|SR|<macro|sResP>>

  <assign|sr|<macro|sRes>>

  <assign|SU|<macro|sResU>>

  <assign|SV|<macro|sResV>>

  <assign|Sy|<macro|<with|mode|text|font-family|rm|S>>>

  <assign|Syl|<macro|Syl>>

  <assign|Sym|<macro|Sym>>

  <assign|T|<macro|<with|mode|text|font-family|rm|T>>>

  <assign|tdeg|<macro|tDeg>>

  <assign|Thom|<macro|Thom>>

  <assign|To|<macro|To>>

  <assign|Tr|<macro|Tr>>

  <assign|Tru|<macro|Tru>>

  <assign|True|<macro|True>>

  <assign|UR|<macro|UR>>

  <assign|URM|<macro|URM>>

  <assign|V|<macro|Var>>

  <assign|W|<macro|MVar>>

  <assign|Z|<macro|Z>>

  <assign|Z|<macro|<with|math-font|Bbb|Z>>>

  <assign|ZZ|<macro|Zer>>

  <active*|<\src-comment>
    Environments for folding.
  </src-comment>>

  <assign|hide|<macro|x|<flag|hide|red|x>>>

  <assign|existproof|<macro|x|<flag|existproof|red|x>>>

  <assign|existpart|<macro|x|<flag|existpart|red|x>>>

  <assign|hidediagram|<macro|x|<flag|hide|red|x>>>

  <assign|hideproof|<\macro|x>
    <arg|x>
  </macro>>

  <assign|hidepart|<\macro|x>
    <arg|x>
  </macro>>

  <assign|hidealg|<\macro|x>
    <arg|x>
  </macro>>

  <assign|compressed|<macro|x|<flag|compressed|red|x>>>

  \;

  <assign|orig-padded-normal|<value|padded-normal>>

  <assign|fold-padded-normal|<macro|a|b|body|<\surround|<vspace*|<arg|a>>|<htab|0fn|first><vspace|<arg|b>>>
    <\with|padded-normal|<value|orig-padded-normal>>
      <arg|body>
    </with>
  </surround>>>

  <assign|fold-bpr|<macro|x|y|<style-with|src-compact|none|<\surround|<with|par-first|-1.5fn|<style-with|src-compact|none|<yes-indent><action|<resize|<specific|screen|<active*|<with|mode|math|<op|\<circ\>>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>>>|<right-flush>>
    <\with|padded-normal|<value|fold-padded-normal>>
      <arg|x>
    </with>
  </surround>>>>

  <assign|unfold-bpr|<\macro|x|y>
    <style-with|src-compact|none|<\surround|<with|par-first|-1.5fn|<style-with|src-compact|none|<yes-indent><action|<resize|<specific|screen|<active*|<with|mode|math|\<bullet\>>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>>>|<right-flush>>
      <\with|padded-normal|<value|fold-padded-normal>>
        <arg|y>
      </with>
    </surround>>
  </macro>>

  <assign|fold-text|<value|fold-bpr>>

  <assign|unfold-text|<value|unfold-bpr>>

  <assign|fold-proof|<value|fold-bpr>>

  <assign|unfold-proof|<value|unfold-bpr>>

  <assign|fold-algorithm|<value|fold-bpr>>

  <assign|unfold-algorithm|<value|unfold-bpr>>

  <assign|fold-exercise|<value|fold-bpr>>

  <assign|unfold-exercise|<value|unfold-bpr>>

  <active*|<\src-comment>
    Miscellaneous.
  </src-comment>>

  <assign|coucou|<macro|1| <with|mode|math|\<langle\>><with|font-shape|small-caps|<arg|1>><with|mode|math|\<rangle\>>>>

  <assign|w|<macro|1|<coucou|<underline|WARNING>: <arg|1>>>>

  <active*|<\src-comment>
    Customization of standard environments.
  </src-comment>>

  <assign|render-proof|<\macro|which|body>
    <\surround||<space|0.5fn><active*|<with|mode|math|\<box\>>>>
      <padded-normal|1fn|1fn|<surround|<theorem-name|<arg|which>>||<arg|body>>>
    </surround>
  </macro>>

  <assign|old-notation|<value|notation>>

  <assign|notation|<\macro|body>
    <old-notation|<surround||<right-flush><active*|<with|mode|math|\<box\>>>|<with|font-shape|right|<arg|body>>>>
  </macro>>

  <assign|old-remark|<value|remark>>

  <assign|remark|<\macro|body>
    <old-remark|<surround||<right-flush><active*|<with|mode|math|\<box\>>>|<with|font-shape|right|<arg|body>>>>
  </macro>>

  <assign|old-definition|<value|definition>>

  <assign|definition|<\macro|body>
    <old-definition|<surround||<right-flush><active*|<with|mode|math|\<box\>>>|<with|font-shape|right|<arg|body>>>>
  </macro>>

  <assign|old-example|<value|example>>

  <assign|example|<\macro|body>
    <old-example|<surround||<right-flush><active*|<with|mode|math|\<box\>>>|<arg|body>>>
  </macro>>

  \;

  <add-to-counter-group|algorithm|std-env>

  <assign|algorithm|<macro|body|<surround|<next-algorithm>||<render-remark|<localize|Algorithm>
  <the-algorithm>|<arg|body>>>>>

  \;

  <assign|cases|<value|choice>>

  <assign|namenot|<macro|content|<with|font-series|bold|<with|font-shape|right|[<arg|content>]>>>>

  <assign|nametheo|<macro|content|<with|font-series|bold|<with|font-shape|right|[<arg|content>]>>>>

  <assign|namealgo|<macro|content|<with|font-series|bold|<with|font-shape|right|[<arg|content>]>>>>

  <active*|<\src-comment>
    Automatically generated content.
  </src-comment>>

  Suppress paragraphs and subparagraphs from table of contents\ 

  <assign|toc-small-1|<macro|what|>>

  <assign|toc-small-2|<macro|what|>>

  \;

  <assign|transform-bibitem|<macro|x|<arg|x>. >>

  <assign|bib-nr|0>

  <assign|bibitem|<macro|text|<style-with|src-compact|none|<assign|bib-nr|<plus|<value|bib-nr>|1>><bibitem*|<value|bib-nr>><label|<merge|bib-|<arg|text>>>>>>

  \;

  <assign|the-glossary|<\macro|body>
    <\small>
      <\margin-first-other|0em|2.3em>
        <arg|body>
      </margin-first-other>
    </small>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|french>
    <associate|preamble|true>
  </collection>
</initial>