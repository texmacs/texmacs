<TeXmacs|1.0.7.16>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|jsc|1.0>

      <\src-purpose>
        The jsc style (for the Journal of Symbolic Computation).
      </src-purpose>

      <\src-copyright|2002--2004>
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

  <use-package|elsart>

  <active*|<\src-comment>
    Global layout.
  </src-comment>>

  <assign|tex-odd-side-margin|<macro|<if|<equal|<value|par-columns>|1>|20pt|20pt>>>

  <assign|tex-even-side-margin|<macro|<if|<equal|<value|par-columns>|1>|20pt|20pt>>>

  <assign|tex-text-width|<macro|<if|<equal|<value|par-columns>|1>|32pc|32pc>>>

  <assign|tex-text-height|<macro|<if|<equal|<value|par-columns>|1>|47pc|47pc>>>

  \;

  <assign|font-base-size|10>

  <assign|par-sep|<tex-len|2pt|1pt|0.5pt>>

  <assign|par-par-sep|0pt>

  <assign|par-first|1pc>

  \;

  <assign|tex-above-display-skip|<macro|<tex-len|6pt|1pt|1pt>>>

  <assign|tex-below-display-skip|<macro|<tex-len|6pt|1pt|1pt>>>

  <assign|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>>

  <assign|tex-below-display-short-skip|<macro|<tex-len|2pt|1pt|1pt>>>

  <active*|<\src-comment>
    Sizes.
  </src-comment>>

  <assign|tiny|<macro|x|<with|font-base-size|6|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-base-size|8|par-sep|2pt|<arg|x>>>>

  <assign|small|<macro|x|<style-with|src-compact|none|<with|font-base-size|9|par-sep|2pt|tex-above-display-skip|<macro|<tex-len|5pt|2pt|1pt>>|tex-below-display-skip|<macro|<tex-len|5pt|2pt|1pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|2pt|1pt|1pt>>|<arg|x>>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-base-size|10|par-sep|<tex-len|2pt|1pt|0.5pt>|tex-above-display-skip|<macro|<tex-len|6pt|1pt|1pt>>|tex-below-display-skip|<macro|<tex-len|6pt|1pt|1pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|texs-below-display-short-skip|<macro|<tex-len|2pt|1pt|1pt>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-base-size|14|par-sep|4pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-base-size|17|par-sep|5pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-base-size|20|par-sep|2pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-base-size|25|par-sep|2pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<huge|<arg|x>>>>

  <active*|<\src-comment>
    Sectional macros.
  </src-comment>>

  <assign|section-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|<tex-len|19pt|3pt|2pt>><normal-size|<arg|name>><vspace|<tex-len|8pt|3pt|2pt>>>>>>>

  <assign|subsection-title|<macro|name|<surround||<yes-indent*>|<sectional-normal-italic|<vspace*|<tex-len|9pt|3pt|2pt>><normal-size|<arg|name>><vspace|<tex-len|8pt|3pt|2pt>>>>>>

  <assign|subsubsection-title|<macro|name|<surround||<yes-indent*>|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tex-len|9pt|3pt|2pt>><normal-size|<arg|name>><vspace|<tex-len|1pt|2pt|1pt>>>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<tex-len|9pt|3pt|2pt>><normal-size|<arg|name>><space|1em>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<tex-len|0pt|2pt|0pt>><normal-size|<arg|name>><space|1em>>>>>

  <\active*>
    <\src-comment>
      List environments.
    </src-comment>
  </active*>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.0fn><with|par-first|-25pt|<yes-indent>><resize|<arg|x>|<minus|1r|20pt>|||><hspace|5pt>>>>

  <assign|compact-item|<macro|x|<style-with|src-compact|none|<vspace*|0.0fn><with|par-first|-12.5pt|<yes-indent>><resize|<arg|x>|||<maximum|1r|12.5pt>|>>>>

  <assign|render-list|<macro|body|<surround|<no-page-break*>|<right-flush><no-indent*>|<with|par-left|<plus|<value|par-left>|25pt>|par-par-sep|<tex-len|4.5pt|2pt|1pt>|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Hack for citations.
    </src-comment>
  </active*>

  <assign|cite-raw-1|<macro|x|<cite-link|<arg|x>|<cite-author|<arg|x>>>,
  <cite-link|<arg|x>|<cite-year|<arg|x>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|7>
  </collection>
</initial>