<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|acmtog|1.0>

      <\src-purpose>
        The acmtog style.
      </src-purpose>

      <\src-copyright|2018>
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

  <use-package|acmart|two-columns>

  <active*|<src-comment|Global layout parameters>>

  <assign|page-width|8.5in>

  <assign|page-height|11in>

  <assign|page-type|user>

  <assign|font-base-size|9>

  \;

  <assign|page-odd|52pt>

  <assign|page-even|52pt>

  <assign|page-right|52pt>

  <assign|page-top|<plus|57pt|13pt|12pt>>

  <assign|page-bot|<plus|73pt|30pt|-5pt>>

  <assign|page-head-sep|<plus|13pt|6pt>>

  <assign|page-foot-sep|30pt>

  \;

  <assign|par-columns-sep|24pt>

  <assign|marginal-note-width|2pc>

  <assign|marginal-note-sep|11pt>

  <active*|<\src-comment>
    Sizes.
  </src-comment>>

  <assign|tiny|<macro|x|<with|font-size|<over|5|9>|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-size|<over|6|9>|par-sep|1pt|<arg|x>>>>

  <assign|smaller|<macro|x|<with|font-size|<over|7|9>|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<with|font-size|<over|7|9>|par-sep|2pt|<arg|x>>>>

  <assign|flat-size|<macro|x|<with|font-size|<over|8|9>|par-sep|1.5pt|<arg|x>>>>

  <assign|normal-size|<macro|x|<with|font-size|<over|9|9>|par-sep|1.5pt|<arg|x>>>>

  <assign|sharp-size|<macro|x|<with|font-size|<over|10|9>|par-sep|1.5pt|<arg|x>>>>

  <assign|large|<macro|x|<with|font-size|<over|12|9>|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-size|<over|14|9>|par-sep|4pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-size|<over|17|9>|par-sep|3pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-size|<over|20|9>|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-size|<over|25|9>|par-sep|5pt|<arg|x>>>>

  <active*|<src-comment|Sectional macros>>

  <assign|section-font|<macro|name|<sharp-size|<with|font-family|ss|<change-case|<arg|name>|UPCASE>>>>>

  <assign|subsection-font|<macro|name|<sharp-size|<with|font-family|ss|<arg|name>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<vspace*|<tmlen|0.75bls|0.55bls|0.95bls>><section-font|<arg|name>><vspace|0.25bls>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<vspace*|<tmlen|0.75bls|0.55bls|0.55bls>><subsection-font|<arg|name>><vspace|0.25bls>>>>>

  <active*|<src-comment|Title information>>

  <assign|doc-title|<macro|x|<\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|1.9|<with|font-family|ss|<arg|x>>>>
  </surround>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>