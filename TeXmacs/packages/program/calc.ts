<TeXmacs|1.0.7.14>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|calc|1.0>

    <\src-purpose>
      Macros for spreadsheets
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Macros for spreadsheet elements
    </src-comment>
  </active*>

  <assign|calc-input|<macro|ref|in|<locus|<id|<arg|ref>>|<arg|in>>>>

  <assign|calc-output|<macro|ref|out|formula|<locus|<id|<arg|ref>>|<arg|out>>>>

  <assign|calc-formula|<macro|ref|out|formula|<locus|<id|<arg|ref>>|<arg|formula>>>>

  <assign|calc-ref|<macro|ref|<with|color|dark magenta|<arg|ref>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>