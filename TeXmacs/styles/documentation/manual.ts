<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|manual|1.0>

      <\src-purpose>
        The old manual style.
      </src-purpose>

      <\src-copyright|1998--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This <TeXmacs> style file falls under the <hlink|GNU general public
        license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
        WHATSOEVER. If you don't have this file, then write to the Free
        Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
        02111-1307, USA.
      </src-license>
    </src-title>
  </active*>

  <use-package|book>

  \;

  <assign|par-hyphen|professional>

  \;

  <assign|scheme|<with|font-shape|small-caps|Scheme>>

  <assign|pari|<with|font-shape|small-caps|Pari>>

  <assign|tmat|@>

  <assign|tmunsc|<with|font-family|tt|_>>

  \;

  <assign|tmdef|<macro|concept|<with|font-shape|italic|<concept>>>>

  <assign|tmref|<macro|concept|index|extra|<arg|concept>>>

  <assign|key|<macro|which|<block*|<tformat|<table|<row|<cell|<arg|which>>>>>>>>

  <assign|menu|<macro|name|<with|font-family|ss|<arg|name>>>>

  <assign|submenu|<macro|name|sub|<with|font-family|ss|<style-with|src-compact|none|<arg|name><with|mode|math|\<rightarrow\>><arg|sub>>>>>

  <assign|subsubmenu|<macro|name|sub|subsub|<with|font-family|ss|<style-with|src-compact|none|<arg|name><with|mode|math|\<rightarrow\>><arg|sub><with|mode|math|\<rightarrow\>><arg|subsub>>>>>

  \;

  <assign|verbatim|<macro|body|<style-with|src-compact|none|<surround|<vspace*|0.5fn><no-page-break*>|<vspace|0.5fn><no-indent*>|<with|font-family|tt|language|verbatim|<arg|body>>>>>>

  <assign|big-figure|<macro|fig|cap|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<vspace*|1fn><no-indent><assign|figurenr|<plus|<figurenr>|1>><assign|thelabel|<*prefix><figurenr>>>|<style-with|src-compact|none|<vspace|1fn><no-indent*>>|<tabular*|<tformat|<twith|table-width|1par>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
    <with|font-size|0.84|<surround|<with|font-series|bold|<localize|Figure>
    <*prefix><figurenr>. >||<arg|cap>>>
  </cell>>>>>>>>>
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