<TeXmacs|1.0.6.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|vdh|1.0>

    <\src-purpose>
      An example style package used by Joris van der Hoeven.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
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

    <assign|rsA|<with|math-font|cal*|A>>

    <assign|rsB|<with|math-font|cal*|B>>

    <assign|rsC|<with|math-font|cal*|C>>

    <assign|rsD|<with|math-font|cal*|D>>

    <assign|rsF|<with|math-font|cal*|F>>

    <assign|rsG|<with|math-font|cal*|G>>

    <assign|rsI|<with|math-font|cal*|I>>

    <assign|rsL|<with|math-font|cal*|L>>

    <assign|rsM|<with|math-font|cal*|M>>

    <assign|rsO|<with|math-font|cal*|O>>

    <assign|rsP|<with|math-font|cal*|P>>

    <assign|rsQ|<with|math-font|cal*|Q>>

    <assign|rsS|<with|math-font|cal*|S>>

    <assign|rsT|<with|math-font|cal*|T>>

    <assign|ssA|<math|<with|math-font-family|tss|A>>>

    <assign|ssAlg|<math|<with|math-font-family|tss|Alg>>>

    <assign|ssGal|<math|<with|math-font-family|tss|Gal>>>

    <assign|ssGL|<math|<with|math-font-family|tss|GL>>>

    <assign|ssL|<math|<with|math-font-family|tss|L>>>

    <assign|ssLin|<math|<with|math-font-family|tss|Lin>>>

    <assign|ssM|<math|<with|math-font-family|tss|M>>>

    <assign|ssMat|<math|<with|math-font-family|tss|Mat>>>

    <assign|ssMM|<math|<with|math-font-family|tss|MM>>>

    <assign|ssR|<math|<with|math-font-family|tss|R>>>

    <assign|ssRM|<math|<with|math-font-family|tss|RM>>>

    <assign|ssT|<math|<with|math-font-family|tss|T>>>

    <assign|ssVect|<math|<with|math-font-family|tss|Vect>>>

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
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>