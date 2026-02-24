<TeXmacs|2.1.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|acmlarge|1.0>

      <\src-purpose>
        The acmlarge style.
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

  <assign|page-width|8.5in>

  <assign|page-height|11in>

  <assign|page-type|user>

  \;

  <assign|page-odd|81pt>

  <assign|page-even|81pt>

  <assign|page-right|81pt>

  <assign|page-top|<macro|<plus|78pt|13pt|16pt>>>

  <assign|page-bot|<macro|<plus|114pt|2pc|0pt>>>

  <assign|page-head-sep|<macro|<plus|13pt|4pt>>>

  <assign|page-foot-sep|2pc>

  \;

  <assign|marginal-note-width|4pc>

  <assign|marginal-note-sep|11pt>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>