<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|sigchi|1.0>

      <\src-purpose>
        The ACM sigchi style.
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

  <use-package|sigconf>

  <active*|<\src-comment>
    Global style parameters.
  </src-comment>>

  <assign|font-base-size|10>

  \;

  <assign|page-odd|54pt>

  <assign|page-even|54pt>

  <assign|page-right|54pt>

  <assign|page-top|<plus|66pt|13pt|12pt>>

  <assign|page-bot|<plus|73pt|30pt|-24pt>>

  <assign|page-head-sep|13pt>

  <assign|page-foot-sep|30pt>

  \;

  <assign|par-columns-sep|2pc>

  <assign|marginal-note-width|2pc>

  <assign|marginal-note-sep|11pt>

  <active*|<src-comment|Font sizes>>

  <assign|tiny|<macro|x|<with|font-size|0.5|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-size|0.7|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<with|font-size|0.9|par-sep|1pt|<arg|x>>>>

  <assign|flat-size|<macro|x|<with|font-size|0.9|par-sep|1pt|<arg|x>>>>

  <assign|normal-size|<macro|x|<with|font-size|1.0|par-sep|1pt|<arg|x>>>>

  <assign|sharp-size|<macro|x|<with|font-size|1.1|par-sep|1.5pt|<arg|x>>>>

  <assign|large|<macro|x|<with|font-size|1.2|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-size|1.4|par-sep|4pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-size|1.7|par-sep|5pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-size|2.0|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-size|2.5|par-sep|5pt|<arg|x>>>>

  <active*|<src-comment|Sectional macros>>

  <assign|section-font|<macro|name|<with|font-family|ss|<change-case|<arg|name>|UPCASE>>>>

  <assign|subsection-font|<macro|name|<with|font-family|ss|<arg|name>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>