<TeXmacs|1.0.7.2>

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

    <assign|I|<with|math-display|false|<op|<big|int>>>>

    <assign|L|\<cal-L\>>

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

    \;

    <assign|ssA|<math|<with|math-font-family|tss|A>>>

    <assign|ssAlg|<math|<with|math-font-family|tss|Alg>>>

    <assign|ssB|<math|<with|math-font-family|tss|B>>>

    <assign|ssBall|<math|<with|math-font-family|tss|Ball>>>

    <assign|ssC|<math|<with|math-font-family|tss|C>>>

    <assign|ssD|<math|<with|math-font-family|tss|D>>>

    <assign|ssE|<math|<with|math-font-family|tss|E>>>

    <assign|ssF|<math|<with|math-font-family|tss|F>>>

    <assign|ssG|<math|<with|math-font-family|tss|G>>>

    <assign|ssGal|<math|<with|math-font-family|tss|Gal>>>

    <assign|ssGL|<math|<with|math-font-family|tss|GL>>>

    <assign|ssH|<math|<with|math-font-family|tss|H>>>

    <assign|ssI|<math|<with|math-font-family|tss|I>>>

    <assign|ssIM|<math|<with|math-font-family|tss|IM>>>

    <assign|ssJ|<math|<with|math-font-family|tss|J>>>

    <assign|ssK|<math|<with|math-font-family|tss|K>>>

    <assign|ssL|<math|<with|math-font-family|tss|L>>>

    <assign|ssLin|<math|<with|math-font-family|tss|Lin>>>

    <assign|ssLM|<math|<with|math-font-family|tss|LM>>>

    <assign|ssM|<math|<with|math-font-family|tss|M>>>

    <assign|ssMat|<math|<with|math-font-family|tss|Mat>>>

    <assign|ssMM|<math|<with|math-font-family|tss|MM>>>

    <assign|ssMV|<math|<with|math-font-family|tss|MV>>>

    <assign|ssN|<math|<with|math-font-family|tss|N>>>

    <assign|ssO|<math|<with|math-font-family|tss|O>>>

    <assign|ssP|<math|<with|math-font-family|tss|P>>>

    <assign|ssQ|<math|<with|math-font-family|tss|Q>>>

    <assign|ssR|<math|<with|math-font-family|tss|R>>>

    <assign|ssRM|<math|<with|math-font-family|tss|RM>>>

    <assign|ssS|<math|<with|math-font-family|tss|S>>>

    <assign|ssT|<math|<with|math-font-family|tss|T>>>

    <assign|ssU|<math|<with|math-font-family|tss|U>>>

    <assign|ssV|<inactive|<math|<with|math-font-family|tss|V>>>>

    <assign|ssVect|<math|<with|math-font-family|tss|Vect>>>

    <assign|ssW|<math|<with|math-font-family|tss|W>>>

    <assign|ssX|<math|<with|math-font-family|tss|X>>>

    <assign|ssY|<math|<with|math-font-family|tss|Y>>>

    <assign|ssZ|<math|<with|math-font-family|tss|Z>>>

    \;

    <assign|bpartial|<with|math-font-series|bold|\<partial\>>>

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

    <assign|Bth|<macro|f|\<\|\|\><arg|f>\<\|\|\>>>

    <assign|Beff|<macro|f|\<lceil\><space|-0.75spc>\<lceil\><arg|f>\<rceil\><space|-0.75spc>\<rceil\>>>
  </with>>

  \;

  <assign|bi-index|<macro|x|y|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|x><subindex|<arg|x>|<arg|y>><subindex|<arg|y>|<arg|x>>>>>

  <assign|bi-index*|<macro|x|y|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|x><index|<arg|x> <arg|y>><subindex|<arg|y>|<arg|x>>>>>

  <assign|sub-bi-index|<macro|x|y|z|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|x><subsubindex|<arg|x>|<arg|y>|<arg|z>><subsubindex|<arg|y>|<arg|x>|<arg|z>>>>>

  <assign|sub-bi-index*|<macro|x|y|z|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|x><subindex|<arg|x> <arg|y>|<arg|z>><subsubindex|<arg|y>|<arg|x>|<arg|z>>>>>

  <assign|subsub-bi-index|<macro|x|y|z|a|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|x><subsubsubindex|<arg|x>|<arg|y>|<arg|z>|<arg|a>><subsubsubindex|<arg|y>|<arg|x>|<arg|z>|<arg|a>>>>>

  <assign|recall|<macro|x|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
  red>|<table|<row|<cell|<arg|x>>>>>>>>

  <assign|fold|<macro|x|y|<surround||<right-flush><action|<active*|<with|color|blue|<with|mode|math|<group|\<Rightarrow\>>>>>|(mouse-unfold)|<arg|x>>|<arg|x>>>>

  <assign|unfold|<\macro|x|y>
    <arg|x>

    <surround||<right-flush><action|<active*|<with|color|blue|<with|mode|math|<group|\<Leftarrow\>>>>>|(mouse-fold)|<arg|x>>|<arg|y>>
  </macro>>

  <assign|solution|<\macro|x>
    <render-exercise|<localize|Solution>|<arg|x>>
  </macro>>

  <assign|annote|<macro|i|<with|font-family|ss|<active*|<with|color|brown|[>><with|color|dark
  blue|<arg|i>><active*|<with|color|brown|]>>>>>

  <assign|correct|<macro|old|new|<active*|<with|color|brown|[>><with|color|dark
  blue|<arg|old>><active*|<with|color|brown|<with|mode|math|\<rightarrow\>>>><with|color|dark
  blue|<arg|new>><active*|<with|color|brown|]>>>>

  \;

  <assign|joris-title|<macro|x|<block*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-lsep|2spc>|<cwith|1|1|1|1|cell-rsep|2spc>|<cwith|1|1|1|1|cell-bsep|2spc>|<cwith|1|1|1|1|cell-tsep|2spc>|<cwith|2|2|1|1|cell-lsep|1spc>|<cwith|2|2|1|1|cell-rsep|1spc>|<cwith|2|2|1|1|cell-bsep|1spc>|<cwith|2|2|1|1|cell-tsep|1spc>|<cwith|1|1|1|1|cell-background|#d0f0ff>|<cwith|2|2|1|1|cell-background|#e8f8ff>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|2|2|1|1|cell-lborder|1ln>|<cwith|2|2|1|1|cell-rborder|1ln>|<cwith|2|2|1|1|cell-bborder|1ln>|<cwith|2|2|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-block|no>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<cell|<with|par-mode|center|<very-large|<strong|<name|<arg|x>>>><hidden-title|<arg|x>>>>>|<row|<cell|<small|<strong|[
  <hlink|Homepage|../main/joris.tm> \| <hlink|Publications|../main/publs.tm>
  \| <hlink|<TeXmacs>|http://www.texmacs.org> \|
  <hlink|Mathemagix|http://www.mathemagix.org> ]>>>>>>>>>

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