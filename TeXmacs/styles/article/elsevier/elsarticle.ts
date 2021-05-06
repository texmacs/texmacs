<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|elsarticle|1.0>

      <\src-purpose>
        The elsarticle style.
      </src-purpose>

      <\src-copyright|2002--2004>
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

  <use-package|std|env|title-generic|header-article|section-article|std-latex|cite-author-year|html-font-size>

  <active*|<\src-comment>
    TeX-like style parameters.
  </src-comment>>

  <assign|tex-odd-side-margin|<macro|<if|<equal|<value|par-columns>|1>|20pt|20pt>>>

  <assign|tex-even-side-margin|<macro|<if|<equal|<value|par-columns>|1>|20pt|20pt>>>

  <assign|tex-text-width|<macro|<if|<equal|<value|par-columns>|1>|33pc|33pc>>>

  \;

  <assign|tex-top-margin|<macro|0pt>>

  <assign|tex-head-height|<macro|0pt>>

  <assign|tex-head-sep|<macro|10pt>>

  <assign|tex-top-skip|<macro|12pt>>

  <assign|tex-text-height|<macro|<if|<equal|<value|par-columns>|1>|651pt|651pt>>>

  <assign|tex-foot-height-heuristic|<macro|1em>>

  <assign|tex-foot-skip|<macro|40pt>>

  \;

  <assign|tex-footnote-sep|<macro|8.4pt>>

  <assign|tex-footnote-tm-barlen|<macro|3pc>>

  <assign|tex-column-sep|<macro|2pc>>

  <assign|tex-float-sep|<macro|<tmlen|6pt|8pt|12pt>>>

  <assign|tex-margin-par-width|<macro|<if|<equal|<value|par-columns>|1>|10pt|2pc>>>

  <assign|tex-margin-par-sep|<macro|20pt>>

  <active*|<\src-comment>
    Global layout.
  </src-comment>>

  <assign|font-base-size|12>

  <assign|par-par-sep|14.5pt>

  <assign|par-first|0fn>

  <assign|padded-par-par-sep|14.5pt>

  <assign|indent-par-first|1.5fn>

  \;

  <assign|tex-jot|<macro|2pt>>

  <assign|tex-math-indent|<macro|<if|<equal|<value|par-columns>|1>|2em|0em>>>

  <assign|tex-above-display-skip|<macro|<tex-len|12pt|2pt|2pt>>>

  <assign|tex-below-display-skip|<macro|<tex-len|12pt|2pt|2pt>>>

  <assign|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>>

  <assign|tex-below-display-short-skip|<macro|<tex-len|3.5pt|2pt|2pt>>>

  <active*|<\src-comment>
    Headers and footers.
  </src-comment>>

  <set-header|>

  <set-footer|<htab|5mm><page-number><htab|5mm>>

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <active*|<\src-comment>
    Sizes.
  </src-comment>>

  <assign|tiny|<macro|x|<with|font-size|<over|6|12>|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-size|<over|8|12>|par-sep|1.5pt|<arg|x>>>>

  <assign|small|<macro|x|<style-with|src-compact|none|<with|font-size|<over|11|12>|par-sep|2.6pt|tex-above-display-skip|<macro|<tex-len|11pt|3pt|6pt>>|tex-below-display-skip|<macro|<tex-len|11pt|3pt|6pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|3pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|6.5pt|3.5pt|3pt>>|<arg|x>>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-size|<over|12|12>|par-sep|0.2em|tex-above-display-skip|<macro|<tex-len|12pt|2pt|2pt>>|tex-below-display-skip|<macro|<tex-len|12pt|2pt|2pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|texs-below-display-short-skip|<macro|<tex-len|3.5pt|2pt|2pt>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-size|<over|14|12>|par-sep|4pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-size|<over|17|12>|par-sep|5pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-size|<over|20|12>|par-sep|2pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-size|<over|25|12>|par-sep|2pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<huge|<arg|x>>>>

  <active*|<\src-comment>
    Sectional macros.
  </src-comment>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|<tmlen|1.7bls|1.6bls|2.1bls>><normal-size|<arg|name>><vspace|1bls>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.9bls|1bls|1.3bls>><normal-size|<arg|name>><vspace|1bls>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|1bls|1bls|1.2bls>><normal-size|<arg|name>>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|<tmlen|3.05ex|3.25ex|5.25ex>><normal-size|<arg|name>><space|1em>>>>>

  <active*|<\src-comment>
    Section and environment numbering.
  </src-comment>>

  <assign|sectional-sep|<macro|<space|1.1fn>>>

  <assign|subparagraph-title|<macro|name|<paragraph-title|<arg|name>>>>

  <assign|section-clean|<macro|<reset-subsection>>>

  <assign|display-std-env|<macro|nr|<arg|nr>>>

  <assign|paragraph-display-numbers|<macro|true>>

  <assign|subparagraph-display-numbers|<macro|true>>

  <active*|<\src-comment>
    Theorem-like environments.
  </src-comment>>

  <assign|enunciation-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|enunciation-sep|<space|1em>>

  <assign|remark-name|<macro|name|<with|font-shape|italic|<arg|name>>>>

  <new-theorem|claim|<localize|Claim>>

  <new-theorem|fact|<localize|Fact>>

  <new-theorem|hypothesis|<localize|Hypothesis>>

  <new-theorem|assumption|<localize|Assumption>>

  <new-theorem|criterion|<localize|Criterion>>

  <new-theorem|principle|<localize|Principle>>

  <active*|<\src-comment>
    Customization of other environments.
  </src-comment>>

  <assign|figure-text|<macro|<localize|Fig.>>>

  <assign|bibliography-text|<macro|<localize|References>>>

  <assign|table-of-contents-text|<macro|<localize|Table of Contents>>>

  <assign|list-of-figures-text|<macro|<localize|List of Figures>>>

  <assign|list-of-tables-text|<macro|<localize|List of Tables>>>

  <\active*>
    <\src-comment>
      List environments.
    </src-comment>
  </active*>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.0fn><with|par-first|-2.5em|<yes-indent>><resize|<arg|x>|<minus|1r|2.2em>|||><hspace|0.3em>>>>

  <assign|render-list|<macro|body|<surround|<no-page-break*>|<right-flush><no-indent*>|<with|par-left|<plus|<value|par-left>|1.5em>|par-sep|0.2fn|par-par-sep|0fn|<arg|body>>>>>

  \;

  <assign|item-1|<macro|<active*|<with|mode|math|\<bullet\>>>>>

  <assign|item-2|<macro|<active*|<with|mode|math|<rigid|\<cdot\>>>>>>

  <assign|item-3|<macro|>>

  <\active*>
    <\src-comment>
      Bibliographies.
    </src-comment>
  </active*>

  <assign|bibitem-width|2em>

  <assign|transform-bibitem|<macro|body|[<arg|body>] >>

  <assign|elsart-bibitem*|<macro|text|<style-with|src-compact|none|<assign|bibitem-nr|<plus|<value|bibitem-nr>|1>><render-bibitem|<transform-bibitem|<value|bibitem-nr>>><set-binding|<value|bibitem-nr>>>>>

  <assign|render-bibliography|<\macro|name|body>
    <principal-section*|<arg|name>><yes-indent*>

    <with|par-first|0fn|par-par-sep|0fn|font-size|0.84|<arg|body>>
  </macro>>

  <assign|bib-list|<\macro|largest|body>
    <\description>
      <\with|par-left|2em|par-first|0em|font-size|1|bibitem-nr|0|xbibitem*|<value|elsart-bibitem*>>
        <arg|body>
      </with>
    </description>
  </macro>>

  <active*|<\src-comment>
    Title rendering.
  </src-comment>>

  <assign|doc-title|<macro|x|<\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
    <doc-title-block|<with|font-size|1.414|<arg|x>>>
  </surround>>>

  <assign|by-text|<macro|>>

  <assign|author-name|<macro|author|<doc-author-block|<author-by|<arg|author>>>>>

  <assign|author-affiliation|<\macro|address>
    <surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<smaller|<em|<arg|address>>>>>
  </macro>>

  <assign|author-email|<macro|email|<doc-author-block|<smaller|<with|font-shape|italic|<email-text><localize|:>
  > <verbatim|<arg|email>>>>>>

  <assign|author-email-note|<macro|sym|id|email|<doc-author-block|<smaller|<doc-note-text|<arg|sym>|<arg|id>|<with|font-shape|italic|<email-text><localize|:>
  ><verbatim|<arg|email>>>>>>>

  <active*|<\src-comment>
    Abstract.
  </src-comment>>

  <assign|render-abstract|<macro|body|<\surround|<vspace*|2fn>|<right-flush><vspace|1fn>>
    <\with|par-first|10pt|par-par-sep|5pt|font-size|<over|9|12>>
      <\surround|<hrule><next-line>|<next-line><hrule><yes-indent>>
        <no-indent><with|font-series|bold|<abstract-text>><vspace|5pt><no-page-break>

        <surround|<no-indent>||<arg|body>>
      </surround>
    </with>
  </surround>>>

  <assign|render-classify|<macro|scheme|text|<no-indent><em|<arg|scheme><localize|:>>
  <arg|text>>>

  <active*|<\src-comment>
    Specific macros for Elsevier styles.
  </src-comment>>

  <style-only|<\with|mode|math>
    <assign|reduce|<macro|x|<resize|<arg|x>|<plus|1l|0.1fn>|<plus|1b|0.15fn>|<minus|1r|0.1fn>|<minus|1t|0.15fn>>>>

    <assign|smfrac|<macro|x|y|<with|math-display|false|<style-with|src-compact|none|<frac|<move|<reduce|<arg|x>>||-0.175fn>|<move|<reduce|<arg|y>>||0.125fn>>>>>>

    \;

    <assign|half|<smfrac|1|2>>

    <assign|threehalf|<smfrac|3|2>>

    <assign|quart|<smfrac|1|4>>

    \;

    <assign|d|\<mathd\>>

    <assign|e|\<mathe\>>

    \;

    <assign|Nset|\<bbb-N\>>

    <assign|Zset|\<bbb-Z\>>

    <assign|Qset|\<bbb-Q\>>

    <assign|Rset|\<bbb-R\>>

    <assign|Cset|\<bbb-C\>>

    <assign|Hset|\<bbb-H\>>

    \;

    <assign|nuc|<macro|nr|el|<lsup|<arg|nr>><with|mode|text|<arg|el>>>>

    <assign|vec|<macro|sym|<wide|<arg|sym>|\<vect\>>>>

    <assign|pol|<macro|sym|<wide|<arg|sym>|\<vect\>>>>

    <assign|FMslash|<macro|sym|<neg|<arg|sym>>>>

    <assign|FMSlash|<macro|sym|<neg|<resize|<arg|sym>|<minus|1l|0.1fn>|<minus|1b|0.1fn>|<plus|1r|0.1fn>|<plus|1t|0.1fn>>>>>
  </with>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|7>
  </collection>
</initial>