<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|title-seminar|1.0|header-title|1.0>

    <\src-purpose>
      Titles for the seminar style.
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

  <assign|make-title|<\macro|body>
    <no-indent><assign|page-this-header|><assign|page-this-footer|><vspace|0.1pag>

    <\with|par-mode|center>
      <arg|body>
    </with>

    <surround||<right-flush>|<new-page>>
  </macro>>

  <assign|abstract|<\macro|body>
    <surround|<no-indent>||<section*|<translate|Abstract|english|<language>>>>

    <surround||<right-flush>|<arg|body>>
  </macro>>

  <assign|keywords|<macro|x|<vspace*|0.5fn><no-indent><theorem-name|<translate|Keywords:|english|<language>>
  ><arg|x>>>

  <assign|AMS-class|<macro|x|<no-indent><theorem-name|<translate|A.M.S.
  subject classification:|english|<language>> ><arg|x>>>

  \;

  <assign|title*|<macro|name|<with|math-font-series|bold|font-series|bold|font-size|2|color|red|<arg|name>>>>

  <assign|title|<macro|body|<title*|<arg|body>><header-title|<arg|body>>>>

  <assign|author*|<macro|body|<with|font-shape|small-caps|font-size|1.19|<translate|by|english|<language>>
  <arg|body>>>>

  <assign|author|<macro|body|<vspace*|0.1pag><author*|<arg|body>><header-author|<arg|body>>>>

  <assign|address*|<macro|body|<with|par-par-sep|0fn|<arg|body>>>>

  <assign|address|<macro|body|<surround|<vspace*|0.1pag>||<address*|<arg|body>>>>>

  <assign|address-block|<macro|x|<tformat|<cwith|1|-1|1|1|cell-lsep|1.5fn>|<cwith|1|-1|-1|-1|cell-rsep|1.5fn>|<twith|table-valign|T>|<arg|x>>>>

  <assign|title-email*|<macro|body|<with|font-shape|small-caps|<translate|Email:|english|<language>>
  ><verbatim|<arg|body>>>>

  <assign|title-email|<macro|body|<vspace*|0.1pag><title-email*|<arg|body>>>>

  <assign|title-date*|<macro|body|<with|font-shape|italic|<arg|body>>>>

  <assign|title-date|<macro|body|<vspace*|0.1pag><left-flush><title-date*|<arg|body>><right-flush>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>