<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|header-letter|1.0|header-letter|1.0>

    <\src-purpose>
      Headers for the generic style.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public
      license, version 3 or later|$TEXMACS_PATH/LICENSE>.
      It comes WITHOUT ANY WARRANTY WHATSOEVER.
      You should have received a copy of the license which the software.
      If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|header-generic>

  \;

  <assign|address*|<\macro|body>
    <arg|body>
  </macro>>

  <assign|address|<macro|body|<surround|<vspace*|0.5fn><left-flush>||<tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|n>|<twith|table-valign|T>|<table|<row|<\cell>
    <address*|<arg|body>>
  </cell>>>>>>>>

  <assign|destination|<macro|body|<surround|<vspace*|1.5fn>|<right-flush>|<tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|n>|<twith|table-valign|T>|<table|<row|<\cell>
    <address*|<arg|body>>
  </cell>>>>>>>>

  <assign|letter-date*|<macro|body|<arg|body>>>

  <assign|letter-date|<macro|body|<surround|<vspace*|0.5fn><left-flush>||<letter-date*|<arg|body>>>>>

  <assign|letter-header|<macro|body|<surround||<vspace|5fn>|<arg|body>>>>

  \;

  <assign|opening*|<macro|body|<arg|body>>>

  <assign|opening|<macro|body|<surround||<right-flush><vspace|1fn>|<opening*|<arg|body>>>>>

  <assign|closing*|<macro|body|<arg|body>>>

  <assign|closing|<macro|body|<surround||<right-flush><vspace|1fn>|<closing*|<arg|body>>>>>

  <assign|signature*|<macro|body|<arg|body>>>

  <assign|signature|<macro|body|<surround|<vspace*|5fn><left-flush>||<signature*|<arg|body>>>>>

  <assign|cc*|<\macro|body>
    <arg|body>
  </macro>>

  <assign|cc|<macro|body|<style-with|src-compact|none|<vspace*|0.5fn><style-with|src-compact|all|<localize|Cc>:
  ><tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|n>|<twith|table-valign|T>|<table|<row|<\cell>
    <cc*|<arg|body>>
  </cell>>>>><right-flush>>>>

  <assign|encl*|<\macro|body>
    <arg|body>
  </macro>>

  <assign|encl|<macro|body|<style-with|src-compact|none|<vspace*|0.5fn><style-with|src-compact|all|<localize|Encl>:
  ><tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|n>|<twith|table-valign|T>|<table|<row|<\cell>
    <encl*|<arg|body>>
  </cell>>>>><right-flush>>>>

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