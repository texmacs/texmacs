<TeXmacs|1.0.3.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|title-generic|1.0|header-title|1.0>

    <\src-purpose>
      Titles for the generic style.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <use-package|title-base>

  \;

  <assign|make-title|<macro|body|<surround|<start-page|>|<vspace|2fn>|<with|par-mode|center|<arg|body>>>>>

  <assign|abstract|<macro|body|<surround|<vspace*|2fn>|<right-flush><vspace|1fn>|<\with|par-left|15mm|par-right|15mm|font-size|0.84>
    <no-indent><left-flush><with|font-series|bold|<translate|Abstract|english|<language>>><right-flush><vspace|0.5fn><no-page-break>

    <arg|body>
  </with>>>>

  <assign|keywords|<macro|x|<vspace*|0.5fn><no-indent><theorem-name|<translate|Keywords:|english|<language>>
  ><arg|x>>>

  <assign|AMS-class|<macro|x|<no-indent><theorem-name|<translate|A.M.S.
  subject classification:|english|<language>> ><arg|x>>>

  \;

  <assign|title*|<macro|body|<with|math-font-series|bold|font-series|bold|font-size|1.68|<arg|body>>>>

  <assign|title|<macro|body|<title*|<arg|body>><header-title|<arg|body>>>>

  <assign|author*|<macro|body|<with|font-shape|small-caps|<translate|by|english|<language>>
  <arg|body>>>>

  <assign|author|<macro|body|<vspace*|1fn><author*|<arg|body>><header-author|<arg|body>>>>

  <assign|address*|<macro|body|<with|par-par-sep|0fn|<arg|body>>>>

  <assign|address|<macro|body|<surround|<vspace*|1fn>||<address*|<arg|body>>>>>

  <assign|address-block|<macro|x|<tformat|<cwith|1|-1|1|1|cell-lsep|1.5fn>|<cwith|1|-1|-1|-1|cell-rsep|1.5fn>|<twith|table-valign|T>|<arg|x>>>>

  <assign|title-email*|<macro|body|<with|font-shape|small-caps|<translate|Email:|english|<language>>
  ><verbatim|<arg|body>>>>

  <assign|title-email|<macro|body|<vspace*|1fn><title-email*|<arg|body>>>>

  <assign|title-date*|<macro|body|<with|font-shape|italic|<arg|body>>>>

  <assign|title-date|<macro|body|<vspace*|1.5fn><left-flush><title-date*|<arg|body>><right-flush>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>