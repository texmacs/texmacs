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
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <use-package|header-generic>

  \;

  <assign|address*|<\macro|body>
    <arg|body>
  </macro>>

  <assign|address|<macro|body|<surround|<vspace*|0.5fn><leftflush>||<tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|n>|<twith|table-valign|T>|<table|<row|<\cell>
    <address*|<arg|body>>
  </cell>>>>>>>>

  <assign|destination|<macro|body|<surround|<vspace*|1.5fn>|<rightflush>|<tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|n>|<twith|table-valign|T>|<table|<row|<\cell>
    <address*|<arg|body>>
  </cell>>>>>>>>

  <assign|letter-date*|<macro|body|<arg|body>>>

  <assign|letter-date|<macro|body|<surround|<vspace*|0.5fn><leftflush>||<letter-date*|<arg|body>>>>>

  <assign|letter-header|<macro|body|<surround||<vspace|5fn>|<arg|body>>>>

  \;

  <assign|opening*|<macro|body|<arg|body>>>

  <assign|opening|<macro|body|<surround||<rightflush><vspace|1fn>|<opening*|<arg|body>>>>>

  <assign|closing*|<macro|body|<arg|body>>>

  <assign|closing|<macro|body|<surround||<rightflush><vspace|1fn>|<closing*|<arg|body>>>>>

  <assign|signature*|<macro|body|<arg|body>>>

  <assign|signature|<macro|body|<surround|<vspace*|5fn><leftflush>||<signature*|<arg|body>>>>>

  <assign|cc*|<\macro|body>
    <arg|body>
  </macro>>

  <assign|cc|<macro|body|<style-with|src-compact|none|<vspace*|0.5fn><style-with|src-compact|all|<localize|Cc>:
  ><tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|n>|<twith|table-valign|T>|<table|<row|<\cell>
    <cc*|<arg|body>>
  </cell>>>>><rightflush>>>>

  <assign|encl*|<\macro|body>
    <arg|body>
  </macro>>

  <assign|encl|<macro|body|<style-with|src-compact|none|<vspace*|0.5fn><style-with|src-compact|all|<localize|Encl>:
  ><tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|n>|<twith|table-valign|T>|<table|<row|<\cell>
    <encl*|<arg|body>>
  </cell>>>>><rightflush>>>>

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