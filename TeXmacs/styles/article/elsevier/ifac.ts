<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|ifac|1.0>

      <\src-purpose>
        The ifac style.
      </src-purpose>

      <\src-copyright|2002--2004>
        Adrien Bourdet
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <active*|><use-package|elsarticle|env-math|two-columns>

  <active*|<\src-comment>
    LaTeX-like page layout parameters.
  </src-comment>>

  <assign|tex-odd-side-margin|<macro|-11mm>>

  <assign|tex-even-side-margin|<macro|-11mm>>

  <assign|tex-text-width|<macro|180mm>>

  \;

  <assign|tex-voffset|<macro|-12.5mm>>

  <assign|tex-top-margin|<macro|2mm>>

  <assign|tex-head-height|<macro|10pt>>

  <assign|tex-head-sep|<macro|20pt>>

  <assign|tex-top-skip|<macro|10pt>>

  <assign|tex-text-height|<macro|245mm>>

  <assign|tex-foot-height-heuristic|<macro|1em>>

  <assign|tex-foot-skip|<macro|10pt>>

  \;

  <assign|tex-footnote-sep|<macro|6.65pt>>

  <assign|tex-footnote-tm-barlen|<macro|2in>>

  <assign|tex-column-sep|<macro|5mm>>

  <assign|tex-float-sep|<macro|<tmlen|8pt|4pt|2pt>>>

  <assign|tex-margin-par-width|<macro|1pc>>

  <assign|tex-margin-par-sep|<macro|10pt>>

  <active*|<\src-comment>
    Global layout.
  </src-comment>>

  <assign|font-base-size|10>

  <assign|par-columns|2>

  <assign|par-par-sep|5pt>

  <assign|par-sep|1pt>

  <assign|par-first|0fn>

  <assign|padded-par-par-sep|5pt>

  <assign|indent-par-first|1.5fn>

  \;

  <assign|tex-jot|2pt>

  <assign|tex-above-display-skip|<macro|<tex-len|3pt|1pt|1pt>>>

  <assign|tex-below-display-skip|<macro|<tex-len|3pt|1pt|1pt>>>

  <assign|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>>

  <assign|tex-below-display-short-skip|<macro|<tex-len|2pt|1pt|1pt>>>

  <active*|<\src-comment>
    Sizes.
  </src-comment>>

  <assign|tiny|<macro|x|<with|font-size|0.6|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-size|0.7|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<style-with|src-compact|none|<with|font-size|0.8|par-sep|2pt|tex-above-display-skip|<macro|<tex-len|7pt|2pt|4pt>>|tex-below-display-skip|<macro|<tex-len|7pt|2pt|4pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|1pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|3pt|1pt|2pt>>|<arg|x>>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-size|1.0|par-sep|0.2em|tex-above-display-skip|<macro|<tex-len|5.75pt|2pt|2pt>>|tex-below-display-skip|<macro|<tex-len|5.75pt|2pt|2pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|3.5pt|2pt|2pt>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-size|1.3|par-sep|1pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-size|1.7|par-sep|3pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-size|1.8|par-sep|4pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-size|2.0|par-sep|2pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-size|2.5|par-sep|2pt|<arg|x>>>>

  <active*|<\src-comment>
    Sectional macros.
  </src-comment>>

  <assign|display-part|<macro|nr|<change-case|<localize|Part>|UPCASE>
  <number|<arg|nr>|Roman>>>

  <assign|part-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<vspace*|<tmlen|0.8bls|0.4bls|0bls>><normal-size|<change-case|<arg|name>|UPCASE>><vspace|1bls>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-centered|<vspace*|<tmlen|0.5bls|0.2bls|0.1bls>><normal-size|<change-case|<arg|name>|UPCASE>><vspace|0.5bls>>>>>

  <assign|section-numbered-title|<macro|name|<section-title|<sectional-prefixed|<the-section><section-sep>|<change-case|<arg|name>|UPCASE><section-post-sep>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.3bls|0.2bls|0.1bls>><normal-size|<arg|name>><vspace|0.3bls>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.3bls|0.1bls|0bls>><normal-size|<arg|name>><vspace|0.001bls>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|3.25ex|2ex|0.2ex><normal-size|<arg|name>><space|1em>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|3.25ex|2ex|0.2ex><space|1em><normal-size|<arg|name>><space|1em>>>>>

  <active*|<\src-comment>
    Section and environment numbering.
  </src-comment>>

  <assign|section-sep|<macro|.<space|0.6fn>>>

  <assign|part-sep|<macro|.<space|0.6fn>>>

  <assign|paragraph-display-numbers|<macro|true>>

  <assign|subparagraph-display-numbers|<macro|true>>

  <active*|<\src-comment>
    Theorem-like environments.
  </src-comment>>

  <assign|enunciation-name|<macro|name|<with|font-shape|italic|<arg|name>>>>

  <assign|enunciation-sep|. >

  <assign|render-theorem|<\macro|which|body>
    <render-enunciation|<theorem-name|<arg|which><theorem-sep>>|<arg|body>>
  </macro>>

  <assign|render-proof|<macro|which|body|<surround|<with|font-series|bold|font-shape|right|<arg|which>><space|1em>|<tab-qed>|<arg|body>>>>

  <active*|<\src-comment>
    Rendering of floating objects.
  </src-comment>>

  <assign|render-big-figure|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<style-with|src-compact|none|<tabular*|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-valign|b>|<table|<row|<\cell>
      <\with|par-left|<plus|<value|par-left>|4em>>
        \;

        <\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
          <arg|cap>
        </surround>
      </with>
    </cell>>|<row|<cell|>>|<row|<cell|<arg|fig>>>>>>>>
  </macro>>

  <assign|render-footnote|<macro|nr|body|<style-with|src-compact|none|<float|footnote||<with|font-size|0.84|par-mode|justify|par-left|0cm|par-right|0cm|<style-with|src-compact|none|<surround|<arg|nr><footnote-sep><label|<merge|footnote-|<arg|nr>>>|<right-flush>|<style-with|src-compact|none|<with|font-shape|right|<arg|body>>>>>>>>>>

  <active*|<\src-comment>
    Title rendering.
  </src-comment>>

  <assign|doc-title|<\macro|x>
    \;

    <\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
      <doc-title-block|<with|font-size|1.414|<strong|<arg|x>>>>
    </surround>
  </macro>>

  <assign|author-name|<macro|author|<doc-author-block|<strong|<author-by|<arg|author>>>>>>

  <assign|author-affiliation|<\macro|address>
    <surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<em|<arg|address>>>>
  </macro>>

  <assign|author-email|<macro|email|<doc-author-block|<with|font-shape|italic|<email-text><localize|:>
  > <verbatim|<arg|email>>>>>

  <assign|author-email-note|<macro|sym|id|email|<doc-author-block|<doc-note-text|<arg|sym>|<arg|id>|<with|font-shape|italic|<email-text><localize|:>
  ><verbatim|<arg|email>>>>>>

  <active*|<\src-comment>
    Abstract.
  </src-comment>>

  <assign|render-abstract|<\macro|body>
    <\with|par-columns|1|par-left|17mm|par-right|17mm|overlined-sep|1spc|underlined-sep|1spc>
      <\wide-std-bothlined>
        <surround|<abstract-text><localize|:> |<right-flush>|<arg|body>>
      </wide-std-bothlined>

      \;
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <set-header|>

  <set-footer|>

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|7>
  </collection>
</initial>