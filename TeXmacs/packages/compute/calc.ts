<TeXmacs|1.99.11>

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
      Style parameters
    </src-comment>
  </active*>

  <assign|calc-ref-color|dark magenta>

  <assign|calc-default-cell-width|6em>

  <\active*>
    <\src-comment>
      Macros for spreadsheet elements
    </src-comment>
  </active*>

  <assign|calc-table|<macro|ref|body|<locus|<id|<arg|ref>>|<arg|body>>>>

  <assign|calc-inert|<macro|ref|in|<locus|<id|<arg|ref>>|<arg|in>>>>

  <assign|calc-input|<macro|ref|in|out|<locus|<id|<arg|ref>>|<arg|in>>>>

  <assign|calc-output|<macro|ref|in|out|<locus|<id|<arg|ref>>|<arg|out>>>>

  <assign|calc-ref|<macro|ref|<with|color|<value|calc-ref-color>|<arg|ref>>>>

  \;

  <assign|calc-table|<macro|ref|body|<arg|body>>>

  <assign|calc-inert|<macro|ref|in|<flag|<arg|ref>|dark magenta><arg|in>>>

  <assign|calc-input|<macro|ref|in|out|<arg|in>>>

  <assign|calc-output|<macro|ref|in|out|<arg|out>>>

  <assign|calc-ref|<macro|ref|<with|color|<value|calc-ref-color>|<arg|ref>>>>

  <drd-props|calc-table|arity|2|identifier|0|regular|1|accessible|1>

  <drd-props|calc-inert|arity|2|identifier|0|regular|1|accessible|1>

  <drd-props|calc-input|arity|3|identifier|0|regular|1|regular|2|accessible|1>

  <drd-props|calc-output|arity|3|identifier|0|regular|1|regular|2|accessible|2>

  \;

  <assign|cell-inert|<macro|ref|body|<with|calc-inert|<macro|ref|in|<arg|in>>|<calc-inert|<arg|ref>|<arg|body>>>>>

  <assign|cell-input|<macro|ref|in|out|<calc-input|<arg|ref>|<arg|in>|<arg|out>>>>

  <assign|cell-output|<macro|ref|in|out|<calc-output|<arg|ref>|<arg|in>|<arg|out>>>>

  <assign|cell-ref|<macro|ref|<calc-ref|<arg|ref>>>>

  <assign|cell-range|<macro|start|end|<arg|start>:<arg|end>>>

  <assign|cell-sum|<macro|start|end|<arg|start><cell-plusses><arg|end>>>

  <drd-props|cell-inert|arity|2|identifier|0|regular|1|border|no|accessible|1>

  <drd-props|cell-input|arity|3|identifier|0|regular|1|regular|2|border|no|accessible|1>

  <drd-props|cell-output|arity|3|identifier|0|regular|1|regular|2|border|no|accessible|2>

  \;

  <assign|cell-commas|<macro|<math|,\<ldots\>,>>>

  <assign|cell-plusses|<macro|<math|+\<cdots\>+>>>

  <\active*>
    <\src-comment>
      Extra tables
    </src-comment>
  </active*>

  <assign|textual-table|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-width|<value|calc-default-cell-width>>|<cwith|1|-1|1|-1|cell-hmode|max>|<arg|body>>>>

  <assign|numeric-dot-table|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-width|<value|calc-default-cell-width>>|<cwith|1|-1|1|-1|cell-hmode|max>|<cwith|1|-1|1|-1|cell-halign|R.>|<arg|body>>>>

  <assign|numeric-comma-table|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-width|<value|calc-default-cell-width>>|<cwith|1|-1|1|-1|cell-hmode|max>|<cwith|1|-1|1|-1|cell-halign|R,>|<arg|body>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>