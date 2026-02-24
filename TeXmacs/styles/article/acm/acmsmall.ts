<TeXmacs|2.1.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|acmsmall|1.0>

      <\src-purpose>
        The acmsmall style.
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

  <use-package|acmart>

  <active*|<src-comment|Global layout parameters>>

  <assign|page-width|6.75in>

  <assign|page-height|10in>

  <assign|page-type|user>

  \;

  <assign|page-odd|46pt>

  <assign|page-even|46pt>

  <assign|page-right|46pt>

  <assign|page-top|<macro|<plus|58pt|13pt|16pt>>>

  <assign|page-bot|<macro|<plus|44pt|2pc|-5pt>>>

  <assign|page-head-sep|<macro|<plus|13pt|4pt>>>

  <assign|page-foot-sep|2pc>

  \;

  <assign|marginal-note-width|2pc>

  <assign|marginal-note-sep|11pt>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>