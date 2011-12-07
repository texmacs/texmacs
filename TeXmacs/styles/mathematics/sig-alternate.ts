<TeXmacs|1.0.7.14>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|sig-alternate|1.0>

      <\src-purpose>
        The sig-alternate style.
      </src-purpose>

      <\src-copyright|2011>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env|title-generic|header-article|section-article|two-columns|std-latex>

  <active*|<\src-comment>
    TeX-like style parameters.
  </src-comment>>

  <assign|page-type|letter>

  <assign|tex-odd-side-margin|<macro|<minus|4.5pc|1in>>>

  <assign|tex-even-side-margin|<macro|<minus|4.5pc|1in>>>

  <assign|tex-text-width|<macro|42pc>>

  \;

  <assign|tex-voffset|<macro|0pt>>

  <assign|tex-top-margin|<macro|<minus|4.5pc|1in>>>

  <assign|tex-head-height|<macro|0pt>>

  <assign|tex-head-sep|<macro|0pt>>

  <assign|xxx-tex-top-skip|<macro|10pt>>

  <assign|tex-text-height|<macro|666pt>>

  <assign|xxx-tex-foot-height-heuristic|<macro|1em>>

  <assign|tex-foot-skip|<macro|30pt>>

  \;

  <assign|tex-footnote-sep|<macro|5.6pt>>

  <assign|tex-footnote-tm-barlen|<macro|0.25par>>

  <assign|tex-column-sep|<macro|2pc>>

  <assign|tex-float-sep|<macro|<tmlen|11pt|2pt|2pt>>>

  <assign|tex-margin-par-width|<macro|0pt>>

  <assign|tex-margin-par-sep|<macro|11pt>>

  <active*|<\src-comment>
    Global layout.
  </src-comment>>

  <assign|font-base-size|9>

  <assign|par-first|9pt>

  <assign|par-sep|1.5pt>

  \;

  <assign|tex-jot|<macro|2pt>>

  <assign|tex-above-display-skip|<macro|<tex-len|6pt|2pt|1pt>>>

  <assign|tex-below-display-skip|<macro|<tex-len|6pt|2pt|1pt>>>

  <assign|tex-above-display-short-skip|<macro|<tex-len|6pt|0pt|3pt>>>

  <assign|tex-below-display-short-skip|<macro|<tex-len|6pt|0pt|3pt>>>

  <active*|<\src-comment>
    Sizes.
  </src-comment>>

  <assign|tiny|<macro|x|<with|font-base-size|5|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-base-size|6|par-sep|1pt|<arg|x>>>>

  <assign|smaller|<macro|x|<with|font-base-size|7|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<with|font-base-size|7|par-sep|2pt|<arg|x>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-base-size|9|par-sep|1.5pt|tex-above-display-skip|<macro|<tex-len|6pt|2pt|1pt>>|tex-below-display-skip|<macro|<tex-len|6pt|2pt|1pt>>|tex-above-display-short-skip|<macro|<tex-len|6pt|0pt|3pt>>|tex-below-display-short-skip|<macro|<tex-len|6pt|0pt|3pt>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-base-size|12|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-base-size|14|par-sep|4pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-base-size|17|par-sep|3pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-base-size|20|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-base-size|25|par-sep|5pt|<arg|x>>>>

  <active*|<\src-comment>
    Sectional macros.
  </src-comment>>

  <assign|sectional-sep|<macro|<space|1em>>>

  <assign|section-sep|<macro|.<space|1em>>>

  <assign|section-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-bold|<style-with|src-compact|none|<vspace*|<tex-len|10pt|4pt|2pt>><large|<with|font|times|<change-case|<arg|name>|UPCASE>>><vspace|<tex-len|4pt|0pt|0pt>>>>>>>>

  <assign|subsection-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-bold|<style-with|src-compact|none|<vspace*|<tex-len|8pt|2pt|1pt>><large|<with|font|times|<arg|name>>><vspace|<tex-len|4pt|0pt|0pt>>>>>>>>

  <assign|subsubsection-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-italic|<style-with|src-compact|none|<vspace*|<tex-len|8pt|2pt|1pt>><large|<arg|name>><vspace|<tex-len|4pt|0pt|0pt>>>>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<tex-len|6pt|1pt|1pt>><normal-size|<arg|name>><space|5.5pt>>>>>

  <assign|bibliography-text|<macro|<localize|References>>>

  <assign|render-bibliography|<\macro|name|body>
    <principal-section|<arg|name>>

    <with|par-first|0fn|par-par-sep|0fn|<arg|body>>
  </macro>>

  <assign|transform-bibitem|<macro|body|[<arg|body>] \ >>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|doc-render-title|<\macro|x>
    <with|font-family|ss|<\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
      <doc-title-block|<font-magnify|2|<with|math-font-series|bold|font-series|bold|<arg|x>>>>
    </surround>>
  </macro>>

  <assign|author-render-name|<macro|author|<surround|<vspace*|0.25fn>|<vspace|0.25fn>|<doc-author-block|<with|font|ms-arial|font-base-size|12|<arg|author>>>>>>

  <assign|author-address|<\macro|address>
    <surround|<vspace*|0.25fn>|<vspace|0.25fn>|<doc-author-block|<with|font|ms-arial|font-base-size|10|<arg|address>>>>
  </macro>>

  <assign|author-email|<macro|email|<doc-author-block|<with|font|ms-arial|font-base-size|12|<arg|email>>>>>

  <assign|author-homepage|<macro|homepage|<doc-author-block|<with|font|ms-arial|font-base-size|12|<arg|homepage>>>>>

  <assign|doc-abstract|<\macro|body>
    <section*|<abstract-text>>

    <surround|<no-indent>||<arg|body>>
  </macro>>

  <assign|doc-keywords|<\xmacro|args>
    <subsection*|<keywords-text>>

    <no-indent><concat-tuple|<copy|<quote-arg|args>>|, >
  </xmacro>>

  <assign|doc-AMS-class|<\xmacro|args>
    <subsection*|<AMS-class-text>>

    <no-indent><concat-tuple|<copy|<map|msc-ref|<quote-arg|args>>>|, >
  </xmacro>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|page-odd-header|>

  <assign|page-even-header|>

  <assign|page-odd-footer|<htab|5mm><quote|<page-the-page>><htab|5mm>>

  <assign|page-even-footer|<htab|5mm><quote|<page-the-page>><htab|5mm>>

  \;

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <active*|<\src-comment>
    Theorem-like environemments rendering.
  </src-comment>>

  <assign|theorem-name|<macro|name|<with|font-shape|small-caps|<arg|name>>>>

  <assign|theorem-sep|<macro|. >>

  <assign|remark-name|<macro|name|<with|font-shape|italic|<arg|name>>>>

  <assign|remark-sep|<macro|. >>

  <assign|exercise-name|<macro|name|<with|font-shape|small-caps|<arg|name>>>>

  <assign|exercise-sep|<macro|. >>

  \;

  <assign|render-remark|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<yes-indent><remark-name|<arg|which><remark-sep>>||<arg|body>>>
  </macro>>

  <assign|render-theorem|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<yes-indent><theorem-name|<arg|which><theorem-sep>>||<with|font-shape|italic|<arg|body>>>>
  </macro>>

  <assign|render-exercise|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<yes-indent><exercise-name|<arg|which><exercise-sep>>||<arg|body>>>
  </macro>>

  \;

  <assign|proof-text|<macro|<localize|Proof>>>

  <assign|dueto|<macro|name|<with|font-shape|right|<theorem-name|(<arg|name>)
  >>>>

  <assign|render-proof|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<yes-indent><theorem-name|<arg|which><theorem-sep>>|<space|1em><active*|<with|mode|math|\<box\>>>|<arg|body>>>
  </macro>>

  <assign|proof|<\macro|body>
    <render-proof|<proof-text>|<arg|body>>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>