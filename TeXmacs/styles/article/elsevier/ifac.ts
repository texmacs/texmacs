<TeXmacs|1.99.6>

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

  <active*|><use-package|elsart|two-columns>

  <active*|<\src-comment>
    LaTeX-like page layout parameters.
  </src-comment>>

  <assign|tex-odd-side-margin|<macro|<if|<equal|<value|par-columns>|1>|0pt|0pt>>>

  <assign|tex-even-side-margin|<macro|<if|<equal|<value|par-columns>|1>|0pt|0pt>>>

  <assign|tex-text-width|<macro|<if|<equal|<value|par-columns>|1>|38pc|38pc>>>

  \;

  <assign|tex-voffset|<macro|-10.5mm>>

  <assign|tex-top-margin|<macro|0pt>>

  <assign|tex-head-height|<macro|10pt>>

  <assign|tex-head-sep|<macro|11pt>>

  <assign|tex-top-skip|<macro|10pt>>

  <assign|tex-text-height|<macro|<if|<equal|<value|par-columns>|1>|252mm|252mm>>>

  <assign|tex-foot-height-heuristic|<macro|1em>>

  <assign|tex-foot-skip|<macro|23pt>>

  \;

  <assign|tex-footnote-sep|<macro|6.65pt>>

  <assign|tex-footnote-tm-barlen|<macro|2in>>

  <assign|tex-column-sep|<macro|0.3in>>

  <assign|tex-float-sep|<macro|<tmlen|6pt|8pt|12pt>>>

  <assign|tex-margin-par-width|<macro|2pc>>

  <assign|tex-margin-par-sep|<macro|20pt>>

  <active*|<\src-comment>
    Global layout.
  </src-comment>>

  <assign|font-base-size|10>

  <assign|par-par-sep|5pt>

  <assign|par-first|0fn>

  <assign|par-columns|2>

  \;

  <assign|tex-jot|2pt>

  <assign|tex-above-display-skip|<macro|<tex-len|5.75pt|2pt|2pt>>>

  <assign|tex-below-display-skip|<macro|<tex-len|5.75pt|2pt|2pt>>>

  <assign|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>>

  <assign|tex-below-display-short-skip|<macro|<tex-len|3.5pt|2pt|2pt>>>

  <active*|<\src-comment>
    Sizes.
  </src-comment>>

  <assign|tiny|<macro|x|<with|font-base-size|6|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-base-size|7|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<style-with|src-compact|none|<with|font-base-size|8|par-sep|2pt|tex-above-display-skip|<macro|<tex-len|7pt|2pt|4pt>>|tex-below-display-skip|<macro|<tex-len|7pt|2pt|4pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|1pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|3pt|1pt|2pt>>|<arg|x>>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-base-size|10|par-sep|0.2em|tex-above-display-skip|<macro|<tex-len|5.75pt|2pt|2pt>>|tex-below-display-skip|<macro|<tex-len|5.75pt|2pt|2pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|3.5pt|2pt|2pt>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-base-size|13|par-sep|1pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-base-size|17|par-sep|3pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-base-size|18|par-sep|4pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-base-size|20|par-sep|2pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-base-size|25|par-sep|2pt|<arg|x>>>>

  <active*|<\src-comment>
    Sectional macros.
  </src-comment>>

  <assign|display-part|<macro|nr|<change-case|<localize|Part>|UPCASE>
  <number|<arg|nr>|Roman>>>

  <assign|part-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<vspace*|2bls|2bls|2.4bls><normal-size|<change-case|<arg|name>|UPCASE>><vspace|1bls>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-centered|<vspace*|<plus|18pt|<tmlen|-4pt|0bls|1bls>>><normal-size|<change-case|<arg|name>|UPCASE>><vspace*|5pt|6pt|8pt>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<plus|18pt|<tmlen|-4pt|0bls|1bls>>><normal-size|<arg|name>><vspace*|5pt|6pt|8pt>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|1bls|1bls|1.2bls><normal-size|<arg|name>><space|1em>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|3.05ex|3.25ex|5.25ex><normal-size|<arg|name>><space|1em>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|3.05ex|3.25ex|5.25ex><space|1em><normal-size|<arg|name>><space|1em>>>>>

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

  <assign|theorem-sep|. >

  <assign|theorem-name|<macro|name|<arg|name>>>

  <assign|render-theorem|<\macro|which|body>
    <render-remark|<with|font-shape|italic|<arg|which>>|<arg|body>>
  </macro>>

  <assign|proof-text|<with|font-series|bold|<change-case|<localize|Proof>|UPCASE>>>

  <assign|render-proof|<macro|which|body|<render-remark|<arg|which>|<arg|body>>>>

  <active*|<\src-comment>
    List environments.
  </src-comment>>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-3fn|<yes-indent>><resize|<arg|x>|<minus|1r|2.3fn>||<minus|1r|0.2fn>|>>>>

  <assign|render-list|<macro|body|<padded-normal|0.0fn|1em|<with|par-sep|-0.8em|<arg|body>>>>>

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

  <assign|doc-abstract|<\macro|body>
    <\with|par-columns|1|par-left|17mm|par-right|17mm>
      <surround|<abstract-text><localize|:> |<right-flush>|<arg|body>>
    </with>
  </macro>>

  <assign|keywords|<macro|x|<vspace*|0.5fn><no-indent><keywords-text>
  <arg|x><right-flush>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|7>
  </collection>
</initial>