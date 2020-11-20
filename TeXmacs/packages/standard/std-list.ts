<TeXmacs|1.99.15>

<style|<tuple|source|std|english>>

<\body>
  <active*|<\src-title>
    <src-package|std-list|1.0>

    <\src-purpose>
      Standard list environments
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Customizable ways to render items.
    </src-comment>
  </active*>

  <assign|item-hsep|<macro|3fn>>

  <assign|item-vsep|<macro|0.5fn>>

  <assign|aligned-item|<macro|name|<style-with|src-compact|none|<vspace*|<item-vsep>><with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<arg|name>|<minus|1r|<minus|<item-hsep>|0.5fn>>||<plus|1r|0.5fn>|>>>>

  <assign|compact-item|<macro|name|<style-with|src-compact|none|<vspace*|<item-vsep>><with|par-first|<minus|<over|<item-hsep>|2>>|<yes-indent>><resize|<arg|name>|||<maximum|1r|<over|<item-hsep>|2>>|>>>>

  <\active*>
    <\src-comment>
      Further variants for rendering items which should not be customized.
    </src-comment>
  </active*>

  <assign|item-spc|<macro|>>

  <assign|item-strong|<macro|name|<with|font-series|bold|math-font-series|bold|<arg|name>>>>

  <assign|item-long|<macro|name|<arg|name><next-line>>>

  <assign|aligned-space-item|<macro|name|<aligned-item|<arg|name> \ >>>

  <assign|aligned-dot-item|<macro|name|<aligned-item|<arg|name>.<item-spc>>>>

  <assign|aligned-bracket-item|<macro|name|<aligned-item|<arg|name><with|font-shape|right|)><item-spc>>>>

  <assign|aligned-strong-dot-item|<macro|name|<aligned-item|<item-strong|<arg|name>.<item-spc>>>>>

  <assign|compact-space-item|<macro|name|<compact-item|<arg|name> \ >>>

  <assign|compact-strong-space-item|<macro|name|<compact-item|<item-strong|<arg|name>
  \ >>>>

  <assign|compact-strong-dot-item|<macro|name|<compact-item|<item-strong|<arg|name>.
  >>>>

  <assign|compact-strong-dot-item*|<macro|name|<no-indent><move|<item-strong|<arg|name>.>|-1.5fn|0fn>>>

  <assign|compact-strong-dash-item|<macro|name|<compact-item|<item-strong|<arg|name>>
  <emdash> >>>

  <assign|long-compact-strong-dot-item|<macro|name|<item-long|<no-indent><move|<item-strong|<arg|name>.>|-1.5fn|0fn>>>>

  <\active*>
    <\src-comment>
      The main item tags; <verbatim|current-item> and
      <verbatim|transform-item> are locally changed inside lists.
    </src-comment>
  </active*>

  <new-counter|item>

  <assign|last-item|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>

  <assign|last-item-nr|0>

  <assign|current-item|<value|aligned-space-item>>

  <assign|transform-item|<macro|name|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <assign|the-item|<macro|<transform-item|<value|<counter-item>>>>>

  <assign|render-item|<macro|name|<assign|last-item|<arg|name>><current-item|<arg|name>>>>

  <assign|item*|<macro|name|<render-item|<arg|name>><with|index-enabled|false|<set-binding|<arg|name>>>>>

  <assign|item|<macro|<next-item><assign|last-item-nr|<value|item-nr>><render-item|<the-item>>>>

  <\active*>
    <\src-comment>
      Rendering of list environments.
    </src-comment>
  </active*>

  <assign|render-list|<\macro|body>
    <\padded-normal|<item-vsep>|<item-vsep>>
      <\indent-left|<item-hsep>>
        <surround|<no-page-break*>|<no-indent*>|<arg|body>>
      </indent-left>
    </padded-normal>
  </macro>>

  <assign|list|<\macro|item-render|item-transform|body>
    <\with|current-item|<arg|item-render>|transform-item|<arg|item-transform>|item-nr|0>
      <render-list|<arg|body>>
    </with>
  </macro>>

  <assign|list*|<\macro|item-render|item-transform|body>
    <\with|current-item|<arg|item-render>|transform-item|<quasiquote|<macro|name|<unquote|<value|last-item>>.<compound|<unquote|<arg|item-transform>>|<arg|name>>>>|item-nr|0|last-item-nr|0>
      <render-list|<arg|body>>
    </with>
  </macro>>

  <assign|list-continued|<\macro|item-render|item-transform|body>
    <\with|current-item|<arg|item-render>|transform-item|<arg|item-transform>|item-nr|<value|last-item-nr>>
      <render-list|<arg|body>>
    </with>
  </macro>>

  <assign|new-list|<macro|name|item-render|item-transform|<quasi|<style-with|src-compact|none|<assign|<merge|prefix-|<arg|name>>|false><assign|<arg|name>|<\macro|body>
    <compound|<if|<and|<value|<merge|prefix-|<unquote|<arg|name>>>>|<unequal|<value|last-item-nr>|0>>|list*|list>|<unquote|<arg|item-render>>|<unquote|<arg|item-transform>>|<arg|body>>
  </macro>><assign|<merge|<arg|name>|*>|<\macro|body>
    <compound|<if|<and|<value|<merge|prefix-|<unquote|<arg|name>>>>|<unequal|<value|last-item-nr>|0>>|list|list*>|<unquote|<arg|item-render>>|<unquote|<arg|item-transform>>|<arg|body>>
  </macro>><assign|<merge|<arg|name>|-continued>|<\macro|body>
    <list-continued|<unquote|<arg|item-render>>|<unquote|<arg|item-transform>>|<arg|body>>
  </macro>>>>>>

  <\active*>
    <\src-comment>
      The standard itemize environment with three levels.
    </src-comment>
  </active*>

  <assign|itemize-level|0>

  <assign|itemize-levels|3>

  <new-list|itemize-1|<value|aligned-space-item>|<macro|name|<active*|<with|mode|math|\<bullet\>>>>>

  <new-list|itemize-2|<value|aligned-space-item>|<macro|name|<active*|<with|mode|math|<rigid|\<circ\>>>>>>

  <new-list|itemize-3|<value|aligned-space-item>|<macro|name|<active*|<with|mode|math|<rigid|->>>>>

  <new-list|itemize-4|<value|aligned-space-item>|<macro|name|<active*|<with|mode|math|<rigid|.>>>>>

  <assign|itemize-reduce|<macro|nr|<plus|<mod|<minus|<arg|nr>|1>|<minimum|<value|itemize-levels>|4>>|1>>>

  <assign|itemize|<\macro|body>
    <\with|itemize-level|<plus|<value|itemize-level>|1>>
      <compound|<merge|itemize-|<itemize-reduce|<value|itemize-level>>>|<arg|body>>
    </with>
  </macro>>

  <assign|itemize*|<\macro|body>
    <\with|itemize-level|<plus|<value|itemize-level>|1>>
      <compound|<merge|itemize-|<itemize-reduce|<value|itemize-level>>|*>|<arg|body>>
    </with>
  </macro>>

  <assign|itemize-continued|<\macro|body>
    <\with|itemize-level|<plus|<value|itemize-level>|1>>
      <compound|<merge|itemize-|<itemize-reduce|<value|itemize-level>>|-continued>|<arg|body>>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      The standard enumerate environment with three levels.
    </src-comment>
  </active*>

  <assign|enumerate-level|0>

  <assign|enumerate-levels|4>

  <new-list|enumerate-1|<value|aligned-dot-item>|<value|identity>>

  <new-list|enumerate-2|<value|aligned-dot-item>|<macro|name|<number|<arg|name>|alpha>>>

  <new-list|enumerate-3|<value|aligned-dot-item>|<macro|name|<number|<arg|name>|roman>>>

  <new-list|enumerate-4|<value|aligned-dot-item>|<macro|name|<number|<arg|name>|Alpha>>>

  <assign|enumerate-reduce|<macro|nr|<plus|<mod|<minus|<arg|nr>|1>|<minimum|<value|enumerate-levels>|4>>|1>>>

  <assign|enumerate|<\macro|body>
    <\with|enumerate-level|<plus|<value|enumerate-level>|1>>
      <compound|<merge|enumerate-|<enumerate-reduce|<value|enumerate-level>>>|<arg|body>>
    </with>
  </macro>>

  <assign|enumerate*|<\macro|body>
    <\with|enumerate-level|<plus|<value|enumerate-level>|1>>
      <compound|<merge|enumerate-|<enumerate-reduce|<value|enumerate-level>>|*>|<arg|body>>
    </with>
  </macro>>

  <assign|enumerate-continued|<\macro|body>
    <\with|enumerate-level|<plus|<value|enumerate-level>|1>>
      <compound|<merge|enumerate-|<enumerate-reduce|<value|enumerate-level>>|-continued>|<arg|body>>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Descriptions which use one paragraph for each item with description
    </src-comment>
  </active*>

  <assign|item-paragraph|<macro|name|<paragraph-unnumbered-title|<arg|name>>>>

  <assign|description-paragraphs|<\macro|body>
    <\with|item*|<value|item-paragraph>>
      <\padded-normal|<item-vsep>|<item-vsep>>
        <surround|<no-page-break*>|<no-indent*>|<arg|body>>
      </padded-normal>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Further standard list environments
    </src-comment>
  </active*>

  <new-list|itemize-minus|<value|aligned-space-item>|<macro|name|<active*|<with|mode|math|<rigid|->>>>>

  <new-list|itemize-dot|<value|aligned-space-item>|<macro|name|<active*|<with|mode|math|\<bullet\>>>>>

  <new-list|itemize-arrow|<value|aligned-space-item>|<macro|name|<active*|<with|mode|math|<rigid|\<rightarrow\>>>>>>

  <new-list|enumerate-numeric|<value|aligned-dot-item>|<value|identity>>

  <new-list|enumerate-roman|<value|aligned-dot-item>|<macro|name|<number|<arg|name>|roman>>>

  <new-list|enumerate-Roman|<value|aligned-dot-item>|<macro|name|<number|<arg|name>|Roman>>>

  <new-list|enumerate-alpha|<value|aligned-bracket-item>|<macro|name|<number|<arg|name>|alpha>>>

  <new-list|enumerate-Alpha|<value|aligned-bracket-item>|<macro|name|<number|<arg|name>|Alpha>>>

  <new-list|description-compact|<value|compact-strong-dot-item>|<macro|name|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-aligned|<value|aligned-strong-dot-item>|<macro|name|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-dash|<value|compact-strong-dash-item>|<macro|name|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-long|<value|long-compact-strong-dot-item>|<macro|name|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description|<value|compact-strong-dot-item>|<macro|name|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  \;
</body>

<initial|<\collection>
</collection>>