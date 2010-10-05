<TeXmacs|1.0.7.7>

<style|<tuple|source|std>>

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

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-3fn|<yes-indent>><resize|<arg|x>|r-2.5fn||r+0.5fn|>>>>

  <assign|compact-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-1.5fn|<yes-indent>><resize|<arg|x>|||r]1.5fn|>>>>

  <\active*>
    <\src-comment>
      Further variants for rendering items which should not be customized.
    </src-comment>
  </active*>

  <assign|item-strong|<macro|name|<with|font-series|bold|math-font-series|bold|<arg|name>>>>

  <assign|item-long|<macro|name|<arg|name><next-line>>>

  <assign|aligned-space-item|<macro|x|<aligned-item|<arg|x> \ >>>

  <assign|aligned-dot-item|<macro|x|<aligned-item|<arg|x>.>>>

  <assign|aligned-bracket-item|<macro|x|<aligned-item|<arg|x><with|font-shape|right|)>>>>

  <assign|aligned-strong-dot-item|<macro|x|<aligned-item|<item-strong|<arg|x>.>>>>

  <assign|compact-space-item|<macro|x|<compact-item|<arg|x> \ >>>

  <assign|compact-strong-space-item|<macro|x|<compact-item|<item-strong|<arg|x>
  \ >>>>

  <assign|compact-strong-dot-item|<macro|x|<compact-item|<item-strong|<arg|x>.
  >>>>

  <assign|compact-strong-dash-item|<macro|x|<compact-item|<item-strong|<arg|x>>
  <emdash> >>>

  <assign|long-compact-strong-dot-item|<macro|x|<item-long|<no-indent><move|<item-strong|<arg|x>.>|-1.5fn|0fn>>>>

  <\active*>
    <\src-comment>
      The main item tags; <verbatim|current-item> and
      <verbatim|transform-item> are locally changed inside lists.
    </src-comment>
  </active*>

  <new-counter|item>

  <assign|last-item|<active*|<with|mode|math|||\<cdot\>||<with|math-font-series|bold|<rigid|\<ast\>>>>>>

  <assign|current-item|<value|aligned-space-item>>

  <assign|transform-item|<macro|x|<active*|<with|mode|math|||\<cdot\>||<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <assign|the-item|<macro|<transform-item|<value|<counter-item>>>>>

  <assign|render-item|<macro|x|<assign|last-item|<arg|x>><current-item|<arg|x>>>>

  <assign|item*|<macro|x|<set-binding|<arg|x>><render-item|<arg|x>>>>

  <assign|item|<macro|<next-item><render-item|<the-item>>>>

  <\active*>
    <\src-comment>
      Rendering of list environments.
    </src-comment>
  </active*>

  <assign|render-list|<\macro|body>
    <\padded-normal|0.5fn|0.5fn>
      <\indent-left|3fn>
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
    <style-with|src-compact|none|<list|<arg|item-render>|<quasiquote|<macro|x|<unquote|<value|last-item>>.<compound|<unquote|<arg|item-transform>>|<arg|x>>>>|<arg|body>>>
  </macro>>

  <assign|new-list|<macro|name|item-render|item-transform|<quasi|<style-with|src-compact|none|<assign|<arg|name>|<\macro|body>
    <list|<unquote|<arg|item-render>>|<unquote|<arg|item-transform>>|<arg|body>>
  </macro>><assign|<merge|<arg|name>|*>|<\macro|body>
    <list*|<unquote|<arg|item-render>>|<unquote|<arg|item-transform>>|<arg|body>>
  </macro>>>>>>

  <\active*>
    <\src-comment>
      The standard itemize environment with three levels.
    </src-comment>
  </active*>

  <assign|itemize-level|0>

  <new-list|itemize-1|<value|aligned-space-item>|<macro|x|<active*|<with|mode|math|\<bullet\>>>>>

  <new-list|itemize-2|<value|aligned-space-item>|<macro|x|<active*|<with|mode|math|<rigid|\<circ\>>>>>>

  <new-list|itemize-3|<value|aligned-space-item>|<macro|x|<active*|<with|mode|math|<rigid|->>>>>

  <assign|itemize|<\macro|body>
    <\with|itemize-level|<plus|<mod|<value|itemize-level>|3>|1>>
      <compound|<merge|itemize-|<value|itemize-level>>|<arg|body>>
    </with>
  </macro>>

  <assign|itemize*|<\macro|body>
    <\with|itemize-level|<plus|<mod|<value|itemize-level>|3>|1>>
      <compound|<merge|itemize-|<value|itemize-level>|*>|<arg|body>>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      The standard enumerate environment with three levels.
    </src-comment>
  </active*>

  <assign|enumerate-level|0>

  <new-list|enumerate-1|<value|aligned-dot-item>|<value|identity>>

  <new-list|enumerate-2|<value|aligned-dot-item>|<macro|x|<number|<arg|x>|alpha>>>

  <new-list|enumerate-3|<value|aligned-dot-item>|<macro|x|<number|<arg|x>|roman>>>

  <assign|enumerate|<\macro|body>
    <\with|enumerate-level|<plus|<mod|<value|enumerate-level>|3>|1>>
      <compound|<merge|enumerate-|<value|enumerate-level>>|<arg|body>>
    </with>
  </macro>>

  <assign|enumerate*|<\macro|body>
    <\with|enumerate-level|<plus|<mod|<value|enumerate-level>|3>|1>>
      <compound|<merge|enumerate-|<value|enumerate-level>|*>|<arg|body>>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Further standard list environments
    </src-comment>
  </active*>

  <new-list|itemize-minus|<value|aligned-space-item>|<macro|x|<active*|<with|mode|math|<rigid|->>>>>

  <new-list|itemize-dot|<value|aligned-space-item>|<macro|x|<active*|<with|mode|math|\<bullet\>>>>>

  <new-list|itemize-arrow|<value|aligned-space-item>|<macro|x|<active*|<with|mode|math|<rigid|\<rightarrow\>>>>>>

  <new-list|enumerate-numeric|<value|aligned-dot-item>|<value|identity>>

  <new-list|enumerate-roman|<value|aligned-dot-item>|<macro|x|<number|<arg|x>|roman>>>

  <new-list|enumerate-Roman|<value|aligned-dot-item>|<macro|x|<number|<arg|x>|Roman>>>

  <new-list|enumerate-alpha|<value|aligned-bracket-item>|<macro|x|<number|<arg|x>|alpha>>>

  <new-list|enumerate-Alpha|<value|aligned-bracket-item>|<macro|x|<number|<arg|x>|Alpha>>>

  <new-list|description-compact|<value|compact-strong-dot-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-aligned|<value|aligned-strong-dot-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-dash|<value|compact-strong-dash-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-long|<value|long-compact-strong-dot-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description|<value|compact-strong-dot-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>