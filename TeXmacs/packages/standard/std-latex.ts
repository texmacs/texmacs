<TeXmacs|1.99.6>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-latex|1.0>

    <\src-purpose>
      Macros which make it easier to port TeX/LaTeX style files to TeXmacs
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|std-latex-base>

  <active*|<\src-comment>
    Default values of TeX page layout parameters.
  </src-comment>>

  <assign|tex-hoffset|<macro|0cm>>

  <assign|tex-odd-side-margin|<macro|<if|<equal|<value|par-columns>|1>|0pt|-30pt>>>

  <assign|tex-even-side-margin|<macro|<if|<equal|<value|par-columns>|1>|0pt|-30pt>>>

  <assign|tex-text-width|<macro|<if|<equal|<value|par-columns>|1>|25.5cc|17.5cm>>>

  <assign|tex-line-width|<value|tex-text-width>>

  <assign|tex-column-width|<value|tex-text-width>>

  \;

  <assign|tex-voffset|<macro|0cm>>

  <assign|tex-top-margin|<macro|-10pt>>

  <assign|tex-head-height|<macro|12pt>>

  <assign|tex-head-sep|<macro|16.74pt>>

  <assign|tex-top-skip|<macro|10pt>>

  <assign|tex-text-height|<macro|<if|<equal|<value|par-columns>|1>|517.5dd|640dd>>>

  <assign|tex-foot-height-heuristic|<macro|1em>>

  <assign|tex-foot-skip|<macro|30pt>>

  \;

  <assign|tex-footnote-sep|<macro|8pt>>

  <assign|tex-footnote-tm-barlen|<macro|0.4par>>

  <assign|tex-column-sep|<macro|1.5cc>>

  <assign|tex-float-sep|<macro|<tmlen|10pt|12pt|14pt>>>

  <assign|tex-margin-par-width|<macro|48pt>>

  <assign|tex-margin-par-sep|<macro|10pt>>

  <active*|<\src-comment>
    Computing the page layout.
  </src-comment>>

  <assign|page-width-margin|tex>

  <assign|page-odd|<macro|<plus|<tex-hoffset>|<tex-odd-side-margin>|1in>>>

  <assign|page-even|<macro|<plus|<tex-hoffset>|<tex-even-side-margin>|1in>>>

  <assign|par-width|<macro|<tex-text-width>>>

  \;

  <assign|page-height-margin|tex>

  <assign|page-head-sep|<macro|<tex-head-sep>>>

  <assign|page-top|<macro|<minus|1in|<tex-voffset>|<tex-top-margin>|<tex-head-height>|<page-head-sep>|<tex-top-skip>|0.75em>>>

  <assign|page-user-height|<macro|<tex-text-height>>>

  <assign|page-foot-sep|<macro|<minus|<tex-foot-skip>|<tex-foot-height-heuristic>>>>

  \;

  <assign|page-fnote-sep|<macro|<tex-footnote-sep>>>

  <assign|page-fnote-barlen|<macro|<tex-footnote-tm-barlen>>>

  <assign|par-columns-sep|<macro|<tex-column-sep>>>

  <assign|page-float-sep|<style-with|src-compact|all|><macro|<tex-float-sep>>>

  <assign|page-mnote-width|<macro|<tex-margin-par-width>>>

  <assign|page-mnote-sep|<macro|<tex-margin-par-sep>>>

  <active*|<\src-comment>
    Default TeX values for equation layout and analogues in TeXmacs.
  </src-comment>>

  <assign|tex-jot|<macro|0.25fn>>

  <assign|tex-math-indent|<macro|1.5fn>>

  <assign|tex-above-display-skip|<macro|0.75fn>>

  <assign|tex-below-display-skip|<macro|0.75fn>>

  <assign|tex-above-display-short-skip|<macro|0.15fn>>

  <assign|tex-below-display-short-skip|<macro|0.15fn>>

  \;

  <assign|eqn-left-indent|<macro|<tex-math-indent>>>

  <assign|eqn-short-above|<macro|<plus|<value|par-sep>|<tex-above-display-short-skip>>>>

  <assign|eqn-short-below|<macro|<plus|<value|par-sep>|<tex-below-display-short-skip>>>>

  <assign|eqn-long-above|<macro|<minus|<tex-above-display-skip>|<tex-jot>>>>

  <assign|eqn-long-below|<macro|<minus|<tex-below-display-skip>|<tex-jot>>>>

  <assign|eqn-row-sep|<macro|<plus|<value|par-sep>|<tex-jot>>>>

  <assign|eqn-ver-sep|<macro|<style-with|src-compact|none|<over|<style-with|src-compact|none|<minus|<tex-above-display-skip>|<tex-below-display-skip>|<plus|<tex-above-display-short-skip>|<tex-below-display-short-skip>>>>|2>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|7>
  </collection>
</initial>