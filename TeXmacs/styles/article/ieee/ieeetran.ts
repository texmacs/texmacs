<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|ieeetran|0.1>

      <\src-purpose>
        The IEEEtran style.
      </src-purpose>

      <\src-copyright|2013>
        François Poulain, Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|article|std-latex|two-columns|termes-font>

  <active*|<src-comment|Global style parameters>>

  <assign|font|math=roman,termes>

  <assign|par-sep|0.2fn>

  <assign|par-ver-sep|0.1fn>

  <active*|<src-comment|Global layout parameters>>

  <assign|tex-text-width|<macro|43pc>>

  <assign|tex-head-height|<macro|12pt>>

  <assign|tex-foot-skip|<macro|0.4in>>

  <assign|tex-column-sep|<macro|1pc>>

  <assign|tex-margin-par-width|<macro|20pt>>

  <assign|tex-above-display-skip|<macro|<tex-len|1.5ex|3pt|1pt>>>

  <assign|tex-below-display-skip|<macro|<tex-len|1.5ex|3pt|1pt>>>

  <assign|tex-above-display-short-skip|<macro|<tex-len|1.5ex|3pt|1pt>>>

  <assign|tex-below-display-short-skip|<macro|<tex-len|0pt|3pt|0pt>>>

  <assign|par-first|<macro|1.0em>>

  <\active*>
    <\src-comment>
      Title.
    </src-comment>
  </active*>

  <assign|doc-title-name|<macro|x|<arg|x>>>

  <assign|doc-title|<macro|x|<\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|2|<doc-title-name|<arg|x>>>>
  </surround>>>

  <assign|doc-make-title|<\macro|body>
    <\with|par-columns|1>
      <\surround||<right-flush>>
        <doc-title-block|<arg|body>>

        \;

        \;
      </surround>
    </with>
  </macro>>

  <assign|by-text|<macro|>>

  <assign|author-name|<macro|author|<doc-author-block|<author-by|<arg|author>>>>>

  <assign|author-email|<macro|email|<doc-author-block|<with|font-shape|italic|<email-text><localize|:>
  > <arg|email>>>>

  <assign|author-email-note|<macro|sym|id|email|<doc-author-block|<doc-note-text|<arg|sym>|<arg|id>|<with|font-shape|italic|<email-text><localize|:>
  ><arg|email>>>>>

  <\active*>
    <\src-comment>
      Abstract.
    </src-comment>
  </active*>

  <assign|render-abstract|<\macro|body>
    <\flat-size>
      <\with|font-series|bold>
        <surround|<em|<abstract-text>>\V||<arg|body>>
      </with>
    </flat-size>
  </macro>>

  <assign|render-classify|<macro|scheme|text|<em|<arg|scheme>>\V<arg|text>>>

  <assign|keywords-text|<macro|<localize|Index terms>>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|header-title|<macro|name|<assign|page-odd-header|><assign|page-even-header|>>>

  <assign|header-author|<macro|name|<assign|page-odd-header|><assign|page-even-header|>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Sections
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  <assign|sectional-post-sep|<macro|<space|2spc>>>

  <assign|sectional-no-indent|<macro|>>

  <assign|the-section|<macro|<number|<section-nr>|Roman>>>

  <assign|the-subsection|<macro|<number|<subsection-nr>|Alpha>>>

  <assign|the-subsubsection|<macro|<number|<subsection-nr>|roman>>>

  <assign|the-paragraph|<macro|<number|<subsection-nr>|alpha>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-centered|<vspace*|<tex-len|1.5ex|1.5ex|0.5ex>><with|font-shape|small-caps|<arg|name>><vspace|<tex-len|0.7ex|1ex|0ex>>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tex-len|1.5ex|1.5ex|0.5ex>><arg|name><vspace|<tex-len|0.7ex|0.5ex|0ex>>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tex-len|0ex|0.1ex|0.1ex>><arg|name>>>>>

  <assign|paragraph-title|<value|subsubsection-title>>

  <assign|subparagraph-title|<value|subsubsection-title>>

  <active*|<src-comment|Lists>>

  <assign|item-1|<macro|<active*|<with|mode|math|\<bullet\>>>>>

  <assign|item-2|<macro|<active*|<with|mode|math|<rigid|->>>>>

  <assign|item-3|<macro|<math|\<ast\>>>>

  <assign|item-4|<macro|<math|\<cdot\>>>>

  <assign|itemize-reduce|<macro|nr|<minimum|<arg|nr>|4>>>

  <assign|enumerate-reduce|<macro|nr|<minimum|<arg|nr>|4>>>

  <assign|transform-bibitem|<macro|body|[<arg|body>] >>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>