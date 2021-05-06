<TeXmacs|1.99.20>

<style|<tuple|source|doc>>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|sig-alternate|1.1>

      <\src-purpose>
        The sig-alternate style.
      </src-purpose>

      <\src-copyright|2011>
        Joris van der Hoeven, 2015 by Grégoire Lecerf
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env|title-generic|header-article|section-article|std-latex|two-columns|html-font-size>

  <active*|<\src-comment>
    Conference information and copyrights
  </src-comment>>

  <assign|conference-boilerplate|Permission to make digital or hard copies of
  all or part of this work for personal or classroom use is granted without
  fee provided that copies are not made or distributed for profit or
  commercial advantage and that copies bear this notice and the full citation
  on the first page. \ To copy otherwise, to republish, to post on servers or
  to redistribute to lists, requires prior specific permission and/or a fee.>

  <assign|conference-name|(Please declare
  <with|font-shape|right|<src-macro|conferenceinfo>>,
  <with|font-shape|right|<src-macro|CopyrightYear>>, and
  <with|font-shape|right|<src-macro|crdata>> in your preamble, following ACM
  guidelines:<next-line><hlink|http://www.acm.org/sigs/publications/proceedings-templates|http://www.acm.org/sigs/publications/proceedings-templates>)>

  <assign|conference-info|>

  <assign|conference-copyright-year|20XX>

  <assign|conference-cr-data|XXX-X-XXXX-XXXX-X/XX/XX>

  <assign|conference-price|$15.00>

  <assign|conferenceinfo|<macro|name|infos|<assign|conference-name|<arg|name>><assign|conference-info|<arg|infos>>>>

  <assign|CopyrightYear|<macro|year|<assign|conference-copyright-year|<arg|year>>>>

  <assign|crdata|<macro|data|<assign|conference-cr-data|<arg|data>>>>

  <assign|permission|<macro|data|<assign|conference-boilerplate|<arg|data>>>>

  <assign|conference-copyrightetc|Copyright <value|conference-copyright-year>
  ACM <value|conference-cr-data> ...<value|conference-price>.>

  <assign|copyrightetc|<macro|data|<assign|conference-copyrightetc|<arg|data>>>>

  <assign|conference-permission-par-line-sep|0pt>

  <assign|conference-permission-font-base-size|8pt>

  <assign|make-conference-permissions|<macro|<with|font|TeX Gyre
  Termes|par-par-sep|0pt|par-line-sep|<value|conference-permission-par-line-sep>|par-sep|1pt|font-base-size|<value|conference-permission-font-base-size>|<no-indent><value|conference-boilerplate><new-line><no-indent><with|font-shape|italic|<value|conference-name>>,
  <value|conference-info><new-line><no-indent><value|conference-copyrightetc>>>>

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

  <assign|tex-footnote-sep|<macro|2.5pc>>

  <assign|tex-footnote-tm-barlen|<macro|0par>>

  <assign|tex-column-sep|<macro|2pc>>

  <assign|tex-float-sep|<macro|<tmlen|11pt|2pt|2pt>>>

  <assign|tex-margin-par-width|<macro|0pt>>

  <assign|tex-margin-par-sep|<macro|11pt>>

  <active*|<\src-comment>
    Global layout.
  </src-comment>>

  <assign|font-base-size|9>

  <assign|par-line-sep|0pt>

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

  <assign|tiny|<macro|x|<with|font-size|<over|5|9>|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-size|<over|6|9>|par-sep|1pt|<arg|x>>>>

  <assign|smaller|<macro|x|<with|font-size|<over|7|9>|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<with|font-size|<over|7|9>|par-sep|2pt|<arg|x>>>>

  <assign|xxsmall|<macro|x|<style-with|src-compact|none|<with|font-size|<over|8|9>|par-sep|1pt|tex-above-display-skip|<macro|<tex-len|6.6pt|3pt|4pt>>|tex-below-display-skip|<macro|<tex-len|6.6pt|3pt|4pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|3.6pt|2pt|2pt>>|<arg|x>>>>>

  <assign|xsmall|<macro|x|<style-with|src-compact|none|<with|font-size|<over|9|9>|par-sep|1pt|tex-above-display-skip|<macro|<tex-len|7.6pt|3pt|4pt>>|tex-below-display-skip|<macro|<tex-len|7.6pt|3pt|4pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|3.6pt|2pt|2pt>>|<arg|x>>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-size|<over|9|9>|par-sep|1.5pt|tex-above-display-skip|<macro|<tex-len|6pt|2pt|1pt>>|tex-below-display-skip|<macro|<tex-len|6pt|2pt|1pt>>|tex-above-display-short-skip|<macro|<tex-len|6pt|0pt|3pt>>|tex-below-display-short-skip|<macro|<tex-len|6pt|0pt|3pt>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-size|<over|12|9>|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-size|<over|14|9>|par-sep|4pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-size|<over|17|9>|par-sep|3pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-size|<over|20|9>|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-size|<over|25|9>|par-sep|5pt|<arg|x>>>>

  <active*|<\src-comment>
    Sectional macros.
  </src-comment>>

  <assign|sectional-sep|<macro|<space|1em>>>

  <assign|section-sep|<macro|.<space|1em>>>

  <assign|section-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-bold|<style-with|src-compact|none|<vspace*|<tex-len|10pt|4pt|2pt>><large|<with|font|TeX
  Gyre Termes|<change-case|<arg|name>|UPCASE>>><vspace|<tex-len|4pt|0pt|0pt>>>>>>>>

  <assign|subsection-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-bold|<style-with|src-compact|none|<vspace*|<tex-len|8pt|2pt|1pt>><large|<with|font|TeX
  Gyre Termes|<arg|name>>><vspace|<tex-len|4pt|0pt|0pt>>>>>>>>

  <assign|subsubsection-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-italic|<style-with|src-compact|none|<vspace*|<tex-len|8pt|2pt|1pt>><large|<arg|name>><vspace|<tex-len|4pt|0pt|0pt>>>>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<tex-len|6pt|1pt|1pt>><normal-size|<arg|name>><space|5.5pt>>>>>

  <assign|bibliography-text|<macro|<localize|References>>>

  <assign|render-bibliography|<\macro|name|body>
    <principal-section|<arg|name>>

    <with|par-first|0fn|par-par-sep|0fn|par-mode|left|<arg|body>>
  </macro>>

  <assign|transform-bibitem|<macro|body|[<arg|body>] \ >>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|title-bold-hv|<macro|x|<with|font|TeX Gyre
  Heros|font-size|<over|18|9>|font-series|bold|math-font-series|bold|<arg|x>>>>

  <assign|title-hv|<macro|x|<with|font|TeX Gyre
  Heros|font-size|<over|12|9>|<arg|x>>>>

  <assign|doc-title|<\macro|x>
    <\surround|<new-line><vspace*|<minus|2em|10pt>>|<vspace|1.5em>>
      <doc-title-block|<title-bold-hv|<arg|x>>>
    </surround>
  </macro>>

  <assign|doc-authors|<\xmacro|data>
    <\style-with|src-compact|none>
      <\quasi>
        <\doc-author>
          <\with|doc-author|<value|doc-author*>>
            <space|0spc><unquote*|<map|padded-author|<quote-arg|data>>>
          </with>
        </doc-author>
      </quasi>
    </style-with>
  </xmacro>>

  <assign|doc-author-block|<\macro|body>
    <style-with|src-compact|none|<space|0pt><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|-1|1|-1|cell-bsep|-3pt>|<table|<row|<\cell>
      <\with|par-mode|center>
        <arg|body>
      </with>
    </cell>>>>>>
  </macro>>

  <assign|author-name|<macro|author|<surround|<vspace*|0.25fn>|<vspace|0.25fn>|<doc-author-block|<compound|title-hv|<arg|author>>>>>>

  <assign|author-affiliation|<\macro|address>
    <surround|<vspace*|0.25fn>|<vspace|0.25fn>|<doc-author-block|<title-hv|<with|font-size|<over|10|9>|<arg|address>>>>>
  </macro>>

  <assign|author-email|<macro|email|<doc-author-block|<title-hv|<arg|email>>>>>

  <assign|author-homepage|<macro|homepage|<doc-author-block|<title-hv|<arg|homepage>>>>>

  <assign|render-abstract|<\macro|body>
    <section*|<abstract-text>>

    <surround|<no-indent>|<float|footnote|bf|<smaller|<make-conference-permissions>>>|<arg|body>>
  </macro>>

  <assign|abstract-category-item|<macro|ind|cat|sub|det|<arg|ind><if|<unequal|<arg|cat>|<uninit>>|
  [<with|font-series|bold|<arg|cat>>]><if|<unequal|<arg|sub>|<uninit>>|:
  <arg|sub>><if|<unequal|<arg|det>|<uninit>>|\V<with|font-shape|italic|<arg|det>>>>>

  <assign|abstract-category|<\xmacro|args>
    <subsection*|Categories and Subject Descriptors>

    <no-indent><concat-tuple|<copy|<quote-arg|args>>|; >
  </xmacro>>

  <assign|abstract-terms|<\xmacro|args>
    <subsection*|General Terms>

    <no-indent><concat-tuple|<copy|<quote-arg|args>>|, >
  </xmacro>>

  <assign|abstract-keywords|<\xmacro|args>
    <subsection*|<keywords-text>>

    <no-indent><concat-tuple|<copy|<quote-arg|args>>|; >
  </xmacro>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|page-odd-header|>

  <assign|page-even-header|>

  <assign|page-odd-footer|>

  <assign|page-even-footer|>

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

  <assign|aligned-item-bis|<macro|name|<style-with|src-compact|none|<vspace*|<item-vsep>><with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<arg|name>|<minus|1r|<minus|<item-hsep>|0fn>>||<plus|1r|0fn>|>>>>

  <assign|render-bibitem|<macro|text|<with|item-hsep|<macro|1fn>|<aligned-item-bis|<arg|text>>>>>

  <\active*>
    <\src-comment>
      Big figures.
    </src-comment>
  </active*>

  <assign|render-big-figure-2col|<value|render-big-figure>>

  <assign|render-big-figure|<\macro|type|name|fig|cap>
    <\with|par-columns|1>
      <render-big-figure-2col|<arg|type>|<arg|name>|<arg|fig>|<arg|cap>>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>