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

  <assign|section-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-bold|<style-with|src-compact|none|<vspace*|<tex-len|10pt|4pt|2pt>><large|<with|font|times|<change-case|<arg|name>|UPCASE>>><vspace|<tex-len|4pt|0pt|0pt>>>>>>>>

  <assign|subsection-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-bold|<style-with|src-compact|none|<vspace*|<tex-len|8pt|2pt|1pt>><large|<with|font|times|<arg|name>>><vspace|<tex-len|4pt|0pt|0pt>>>>>>>>

  <assign|subsubsection-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-italic|<style-with|src-compact|none|<vspace*|<tex-len|8pt|2pt|1pt>><large|<arg|name>><vspace|<tex-len|4pt|0pt|0pt>>>>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<tex-len|6pt|1pt|1pt>><normal-size|<arg|name>><space|5.5pt>>>>>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|doc-abstract|<\macro|body>
    <section*|<abstract-text>>

    <surround|<no-indent>||<arg|body>>
  </macro>>

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

  <\active*>
    <\src-comment>
      Other customizations.
    </src-comment>
  </active*>

  <assign|xaligned-item|<macro|name|<style-with|src-compact|none|<vspace*|<item-vsep>><with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<with|math-font-series|bold|font-series|bold|<arg|name>>
  |<minus|1r|<item-hsep>>||1r|>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>