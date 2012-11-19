<TeXmacs|1.0.7.17>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|email|1.0>

      <\src-purpose>
        Style for emails
      </src-purpose>

      <\src-copyright|2012>
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

  <use-package|generic>

  <assign|par-line-sep|0.025fns>

  <assign|par-par-sep|0.025fns>

  <assign|email-header|<\macro|body>
    <\surround||<right-flush>>
      <\with|font-family|rm|par-mode|left>
        <arg|body>
      </with>
    </surround>
  </macro>>

  <assign|email-header-table|<macro|body|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|2|2|cell-rsep|0spc>|<cwith|1|-1|1|1|cell-halign|r>|<twith|table-min-cols|2>|<twith|table-max-cols|2>|<twith|table-bsep|1em>|<arg|body>>>>

  <assign|verbatim-message|<\macro|body>
    <\surround||<right-flush>>
      <\verbatim>
        <arg|body>
      </verbatim>
    </surround>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>