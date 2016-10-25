<TeXmacs|1.99.5>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|acmsmall|1.0>

      <\src-purpose>
        The acmsmall style.
      </src-purpose>

      <\src-copyright|2016>
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

  <use-package|std|env|title-generic|header-article|section-base|std-latex>

  <active*|<src-comment|Global layout parameters>>

  <assign|tex-odd-side-margin|<macro|0.625in>>

  <assign|tex-even-side-margin|<macro|<plus|6.75in|-0.625in|-33pc>>>

  <assign|tex-text-width|<macro|33pc>>

  <assign|tex-top-margin|<macro|36pt>>

  <assign|tex-head-height|<macro|6.5pt>>

  <assign|tex-top-skip|<macro|6.2pt>>

  <assign|tex-text-height|<macro|48pc>>

  <assign|tex-text-height|<macro|-7.3pt>>

  <assign|tex-foot-skip|<macro|28pt>>

  <assign|tex-footnote-sep|<macro|7pt>>

  <assign|tex-column-sep|<macro|12pt>>

  <assign|tex-margin-par-width|<macro|.5in>>

  <assign|par-first|<macro|10pt>>

  <active*|<src-comment|Font sizes>>

  <assign|tiny|<macro|x|<with|font-base-size|5|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-base-size|7|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<style-with|src-compact|none|<with|font-base-size|9|par-sep|1pt|tex-above-display-skip|<macro|<tex-len|5pt|2pt|1pt>>|tex-below-display-skip|<macro|<tex-len|5pt|2pt|1pt>>|tex-above-display-short-skip|<macro|<tex-len|5pt|0pt|2pt>>|tex-below-display-short-skip|<macro|<tex-len|5pt|0pt|2pt>>|<arg|x>>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-base-size|10|par-sep|1pt|tex-above-display-skip|<macro|<tex-len|5.5pt|2pt|1pt>>|tex-below-display-skip|<macro|<tex-len|5.5pt|2pt|1pt>>|tex-above-display-short-skip|<macro|<tex-len|6pt|0pt|3pt>>|texs-below-display-short-skip|<macro|<tex-len|6pt|0pt|3pt>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-base-size|12|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-base-size|14|par-sep|4pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-base-size|17|par-sep|5pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-base-size|20|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-base-size|25|par-sep|5pt|<arg|x>>>>

  <active*|<src-comment|Sectional macros>>

  <assign|section-font|<macro|name|<normal-size|<with|font-base-size|9|font-family|ss|<change-case|<arg|name>|UPCASE>>>>>

  <assign|subsection-font|<macro|name|<normal-size|<with|font-base-size|9|font-family|ss|<arg|name>>>>>

  <assign|subsubsection-font|<macro|name|<normal-size|<with|font-base-size|9|font-family|ss|<arg|name>>>>>

  <assign|paragraph-font|<macro|name|<normal-size|<arg|name>>>>

  <assign|subparagraph-font|<macro|name|<normal-size|<arg|name>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|<tmlen|0.75bls|0.55bls|0.95bls>><section-font|<arg|name>><vspace|0.25bls>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.75bls|0.55bls|0.55bls>><subsection-font|<arg|name>><vspace|0.25bls>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.5bls|0.3bls|0.7bls>><subsubsection-font|<arg|name>><vspace|3.5pt>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.5bls|0.3bls|0.7bls>><paragraph-font|<arg|name>><space|1em>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.25bls|0.15bls|0.35bls>><subparagraph-font|<arg|name>><space|1em>>>>>

  <\active*>
    <\src-comment>
      Rendering of theorem-like environments and exercises.
    </src-comment>
  </active*>

  <assign|render-enunciation|<\macro|which|body>
    <\padded*>
      <surround|<yes-indent><arg|which>|<yes-indent*>|<arg|body>>
    </padded*>
  </macro>>

  <assign|render-proof|<\macro|which|body>
    <\render-enunciation|<theorem-name|<arg|which>><remark-sep>>
      <surround||<hspace|1em><qed>|<arg|body>>
    </render-enunciation>
  </macro>>

  <assign|theorem-name|<macro|name|<with|font-shape|small-caps|<arg|name>>>>

  <assign|remark-name|<macro|name|<with|font-shape|italic|<arg|name>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>