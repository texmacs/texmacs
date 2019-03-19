<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|vdh|1.0>

    <\src-purpose>
      An example style package used by Joris van der Hoeven.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <style-only|<\with|mode|math>
    <assign|A|\<cal-A\>>

    <assign|B|\<cal-B\>>

    <assign|D|\<cal-D\>>

    <assign|L|\<cal-L\>>

    <assign|o|<with|magnification|<times|0.7|<value|magnification>>|\<cal-O\>>>

    <assign|O|\<cal-O\>>

    <assign|R|\<cal-R\>>

    <assign|T|\<cal-T\>>

    <assign|FS|<with|math-font|cal*|F>>

    <assign|GS|<with|math-font|cal*|G>>

    <assign|PS|<with|math-font|cal*|P>>

    <assign|Lin|<with|math-font|cal*|L>>

    <assign|Gb|<with|math-font|cal*|G>>

    <assign|Mlin|<with|math-font|cal*|M>>

    <assign|Mlt|<with|math-font|cal*|T>>

    <assign|Set|<with|math-font|cal*|S>>

    \;

    <assign|I|<with|math-display|false|<op|<big|int>>>>

    <drd-props|I|syntax|\<int\>>

    <drd-props|\<uparrow\>|syntax|\<postup\>>

    <drd-props|\<downarrow\>|syntax|\<postdown\>>

    <drd-props|\<updownarrow\>|syntax|\<postupdown\>>

    <drd-props|\<mapsup\>|syntax|\<postmapsup\>>

    <drd-props|\<mapsdown\>|syntax|\<postmapsdown\>>

    \;

    <assign|rsA|<with|math-font|cal*|A>>

    <assign|rsB|<with|math-font|cal*|B>>

    <assign|rsC|<with|math-font|cal*|C>>

    <assign|rsD|<with|math-font|cal*|D>>

    <assign|rsE|<with|math-font|cal*|E>>

    <assign|rsF|<with|math-font|cal*|F>>

    <assign|rsG|<with|math-font|cal*|G>>

    <assign|rsH|<with|math-font|cal*|H>>

    <assign|rsI|<with|math-font|cal*|I>>

    <assign|rsJ|<with|math-font|cal*|J>>

    <assign|rsK|<with|math-font|cal*|K>>

    <assign|rsL|<with|math-font|cal*|L>>

    <assign|rsM|<with|math-font|cal*|M>>

    <assign|rsN|<with|math-font|cal*|N>>

    <assign|rsO|<with|math-font|cal*|O>>

    <assign|rsP|<with|math-font|cal*|P>>

    <assign|rsQ|<with|math-font|cal*|Q>>

    <assign|rsR|<with|math-font|cal*|R>>

    <assign|rsS|<with|math-font|cal*|S>>

    <assign|rsT|<with|math-font|cal*|T>>

    <assign|rsU|<with|math-font|cal*|U>>

    <assign|rsV|<with|math-font|cal*|V>>

    <assign|rsW|<with|math-font|cal*|W>>

    <assign|rsX|<with|math-font|cal*|X>>

    <assign|rsY|<with|math-font|cal*|Y>>

    <assign|rsZ|<with|math-font|cal*|Z>>

    <assign|rso|<very-small|<with|math-font|cal*|O>>>

    \;

    <assign|ssA|<math|<math-ss|A>>>

    <assign|ssAlg|<math|<math-ss|Alg>>>

    <assign|ssB|<math|<math-ss|B>>>

    <assign|ssBall|<math|<math-ss|Ball>>>

    <assign|ssC|<math|<math-ss|C>>>

    <assign|ssD|<math|<math-ss|D>>>

    <assign|ssE|<math|<math-ss|E>>>

    <assign|ssF|<math|<math-ss|F>>>

    <assign|ssG|<math|<math-ss|G>>>

    <assign|ssGal|<math|<math-ss|Gal>>>

    <assign|ssGL|<math|<math-ss|GL>>>

    <assign|ssH|<math|<math-ss|H>>>

    <assign|ssI|<math|<math-ss|I>>>

    <assign|ssIM|<math|<math-ss|IM>>>

    <assign|ssJ|<math|<math-ss|J>>>

    <assign|ssK|<math|<math-ss|K>>>

    <assign|ssL|<math|<math-ss|L>>>

    <assign|ssLin|<math|<math-ss|Lin>>>

    <assign|ssLM|<math|<math-ss|LM>>>

    <assign|ssM|<math|<math-ss|M>>>

    <assign|ssMat|<math|<math-ss|Mat>>>

    <assign|ssMM|<math|<math-ss|MM>>>

    <assign|ssMV|<math|<math-ss|MV>>>

    <assign|ssN|<math|<math-ss|N>>>

    <assign|ssO|<math|<math-ss|O>>>

    <assign|ssP|<math|<math-ss|P>>>

    <assign|ssQ|<math|<math-ss|Q>>>

    <assign|ssR|<math|<math-ss|R>>>

    <assign|ssRM|<math|<math-ss|RM>>>

    <assign|ssS|<math|<math-ss|S>>>

    <assign|ssT|<math|<math-ss|T>>>

    <assign|ssU|<math|<math-ss|U>>>

    <assign|ssV|<inactive|<math|<math-ss|V>>>>

    <assign|ssVect|<math|<math-ss|Vect>>>

    <assign|ssW|<math|<math-ss|W>>>

    <assign|ssX|<math|<math-ss|X>>>

    <assign|ssY|<math|<math-ss|Y>>>

    <assign|ssZ|<math|<math-ss|Z>>>

    \;

    <assign|bpartial|<with|font-series|bold|math-font-series|bold|\<partial\>>>

    <assign|xor|<space|0.6spc>\<triangledown\><space|0.6spc>>

    <assign|fcut|#>

    <assign|iseg|<macro|x|in(<arg|x>)>>

    <assign|fseg|<macro|x|fin(<arg|x>)>>

    <assign|Iseg|<macro|x|In(<arg|x>)>>

    <assign|Fseg|<macro|x|Fin(<arg|x>)>>

    <assign|bl|<macro|<left|(|-1em|1em>>>

    <assign|br|<macro|<right|)|-1em|1em>>>

    \;

    <assign|gb|<macro|x|<with|math-font-family|mt|[<space|-0.6spc>[><arg|x><with|math-font-family|mt|]<space|-0.6spc>]>>>

    <assign|cgb|<macro|x|<with|math-font-family|mt|{<space|-0.6spc>{><arg|x><with|math-font-family|mt|}<space|-0.6spc>}>>>

    <assign|gbt|<macro|x|<with|math-font-family|mt|[<space|-0.6spc>[<space|-0.6spc>[><arg|x><with|math-font-family|mt|]<space|-0.6spc>]<space|-0.6spc>]>>>

    <assign|cgbt|<macro|x|<with|math-font-family|mt|{<space|-0.6spc>{<space|-0.6spc>{><arg|x><with|math-font-family|mt|}<space|-0.6spc>}<space|-0.6spc>}>>>

    \;

    <assign|head|<macro|x|<arg|x><rsup|\<sharp\>>>>

    <assign|tail|<macro|x|<arg|x><rsup|\<flat\>>>>

    <assign|hm|<macro|i|x|\<sharp\><rsub|<arg|i>> <arg|x>>>

    <assign|tm|<macro|i|x|\<flat\><rsub|<arg|i>> <arg|x>>>

    <assign|lcut|<macro|x|<wide|<arg|x>|\<wide-varleftarrow\>>>>

    <assign|rcut|<macro|x|<wide|<arg|x>|\<wide-varrightarrow\>>>>

    \;

    <assign|rth|<macro|f|r<rsub|<arg|f>>>>

    <assign|reff|<macro|f|<resize|<wide*|<resize|r||0ex|0.3em|1ex>|\<wide-bar\>>||0.5ex|0.4em|1ex><rsub|<arg|f>>>>

    <assign|Bth|<macro|f|<around|\<\|\|\>|<arg|f>|\<\|\|\>>>>

    <assign|Beff|<macro|f|\<lceil\><space|-0.75spc>\<lceil\><arg|f>\<rceil\><space|-0.75spc>\<rceil\>>>
  </with>>

  \;

  <assign|big-imbr|<macro|ind|str|<math-ordinary|<rigid|-<space|-0.5spc><around*|<left|\<langle\>|1>||<right|.>><rsub|<shift|<arg|ind>||-0.2em>>><space|0.6spc><arg|str>>>>

  <assign|Big-imbr|<macro|ind|str|<math-ordinary|<below|<rigid|<shift|<with|magnification|<times|1.297|<value|magnification>>|-<space|-0.5spc><around*|<left|\<langle\>|1>||<right|.>>>||-0.1em>>|<shift|<arg|ind>||-0.1em>>
  <arg|str>>>>

  \;

  <assign|bi-index|<macro|key|secondary|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><subindex|<arg|key>|<arg|secondary>><subindex|<arg|secondary>|<arg|key>>>>>

  <assign|bi-index*|<macro|key|secondary|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><index|<arg|key> <arg|secondary>><subindex|<arg|secondary>|<arg|key>>>>>

  <assign|sub-bi-index|<macro|key|secondary|tertiary|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><subsubindex|<arg|key>|<arg|secondary>|<arg|tertiary>><subsubindex|<arg|secondary>|<arg|key>|<arg|tertiary>>>>>

  <assign|sub-bi-index*|<macro|key|secondary|tertiary|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><subindex|<arg|key> <arg|secondary>|<arg|tertiary>><subsubindex|<arg|secondary>|<arg|key>|<arg|tertiary>>>>>

  <assign|subsub-bi-index|<macro|key|secondary|tertiary|quaternary|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><subsubsubindex|<arg|key>|<arg|secondary>|<arg|tertiary>|<arg|quaternary>><subsubsubindex|<arg|secondary>|<arg|key>|<arg|tertiary>|<arg|quaternary>>>>>

  <assign|recall|<macro|key|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
  red>|<table|<row|<cell|<arg|key>>>>>>>>

  <assign|fold|<macro|x|y|<surround||<right-flush><action|<active*|<with|color|blue|<with|mode|math|<rigid|\<Rightarrow\>>>>>|mouse-unfold|<arg|x>>|<arg|x>>>>

  <assign|unfold|<\macro|x|y>
    <arg|x>

    <surround||<right-flush><action|<active*|<with|color|blue|<with|mode|math|<rigid|\<Leftarrow\>>>>>|mouse-fold|<arg|x>>|<arg|y>>
  </macro>>

  <assign|solution*|<\macro|body>
    <render-exercise|<localize|Solution>|<arg|body>>
  </macro>>

  <assign|annote|<macro|body|<with|font-family|ss|<active*|<with|color|brown|[>><with|color|dark
  blue|<arg|body>><active*|<with|color|brown|]>>>>>

  <assign|correct|<macro|old|new|<active*|<with|color|brown|[>><with|color|dark
  blue|<arg|old>><active*|<with|color|brown|<with|mode|math|\<rightarrow\>>>><with|color|dark
  blue|<arg|new>><active*|<with|color|brown|]>>>>

  <assign|smart-qed|<macro|<htab|0.5fn|0><qed>>>

  \;

  <assign|joris-title|<macro|title|<block*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-lsep|2spc>|<cwith|1|1|1|1|cell-rsep|2spc>|<cwith|1|1|1|1|cell-bsep|2spc>|<cwith|1|1|1|1|cell-tsep|2spc>|<cwith|2|2|1|1|cell-lsep|1spc>|<cwith|2|2|1|1|cell-rsep|1spc>|<cwith|2|2|1|1|cell-bsep|1spc>|<cwith|2|2|1|1|cell-tsep|1spc>|<cwith|1|1|1|1|cell-background|#d0f0ff>|<cwith|2|2|1|1|cell-background|#e8f8ff>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|2|2|1|1|cell-lborder|1ln>|<cwith|2|2|1|1|cell-rborder|1ln>|<cwith|2|2|1|1|cell-bborder|1ln>|<cwith|2|2|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-block|no>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<cell|<with|par-mode|center|<very-large|<strong|<name|<arg|title>>>><hidden-title|<arg|title>>>>>|<row|<cell|<small|<strong|[
  <hlink|Homepage|../main/joris.tm> \| <hlink|Publications|../main/publs.tm>
  \| <hlink|Talks|../main/talks.tm> \| <hlink|<TeXmacs>|http://www.texmacs.org>
  \| <hlink|Mathemagix|http://www.mathemagix.org> ]>>>>>>>>>

  <assign|joris-title*|<macro|title|<block*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-lsep|2spc>|<cwith|1|1|1|1|cell-rsep|2spc>|<cwith|1|1|1|1|cell-bsep|2spc>|<cwith|1|1|1|1|cell-tsep|2spc>|<cwith|2|2|1|1|cell-lsep|1spc>|<cwith|2|2|1|1|cell-rsep|1spc>|<cwith|2|2|1|1|cell-bsep|1spc>|<cwith|2|2|1|1|cell-tsep|1spc>|<cwith|1|1|1|1|cell-background|#d0f0ff>|<cwith|2|2|1|1|cell-background|#e8f8ff>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|2|2|1|1|cell-lborder|1ln>|<cwith|2|2|1|1|cell-rborder|1ln>|<cwith|2|2|1|1|cell-bborder|1ln>|<cwith|2|2|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-block|no>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<cell|<with|par-mode|center|<very-large|<strong|<name|<arg|title>>>><hidden-title|<arg|title>>>>>|<row|<cell|<small|<strong|[
  <hlink|Homepage|../../main/joris.tm> \|
  <hlink|Publications|../../main/publs.tm> \|
  <hlink|Talks|../../main/talks.tm> \| <hlink|<TeXmacs>|http://www.texmacs.org>
  \| <hlink|Mathemagix|http://www.mathemagix.org> ]>>>>>>>>>

  <assign|joris-versions*|<macro|name|<hlink|Pdf|<merge|<arg|name>|.pdf>>,
  <hlink|Bib<TeX>|<merge|<arg|name>|.bib>>>>

  <assign|joris-versions**|<macro|name|<hlink|Gzipped
  Postscript|<merge|<arg|name>|.ps.gz>>, <hlink|Bib<TeX>|<merge|<arg|name>|.bib>>>>

  <assign|joris-versions|<macro|name|<hlink|Html|<merge|<arg|name>|.tm>>,
  <hlink|<TeXmacs>|<merge|<arg|name>|.texmacs>>,
  <joris-versions*|<arg|name>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>