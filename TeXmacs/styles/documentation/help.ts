<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|help|1.0>

      <\src-purpose>
        The old help style.
      </src-purpose>

      <\src-copyright|1998--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public
        license, version 3 or later|$TEXMACS_PATH/LICENSE>.
        It comes WITHOUT ANY WARRANTY WHATSOEVER.
        You should have received a copy of the license which the software.
        If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|generic>

  \;

  <assign|scheme|<with|font-shape|small-caps|Scheme>>

  <assign|pari|<with|font-shape|small-caps|Pari>>

  <assign|tmat|@>

  <assign|tmunsc|<with|font-family|tt|_>>

  \;

  <assign|tmdef|<macro|concept|<with|font-shape|italic|<arg|concept>>>>

  <assign|tmref|<macro|concept|index|extra|<arg|concept>>>

  <assign|key|<macro|which|<block*|<tformat|<table|<row|<cell|<arg|which>>>>>>>>

  <assign|skey|<macro|x|<key|S-<arg|x>>>>

  <assign|ckey|<macro|x|<key|ctrl-<arg|x>>>>

  <assign|akey|<macro|x|<key|alt-<arg|x>>>>

  <assign|mkey|<macro|x|<key|meta-<arg|x>>>>

  <assign|hkey|<macro|x|<key|hyper-<arg|x>>>>

  <assign|menu-sub|<macro|what|<style-with|src-compact|none|<with|font-family|ss|<translate|<look-up|<arg|what>|0>|english|<language>>><style-with|src-compact|none|<if|<is-tuple|<look-up|<arg|what>|1>>|<with|mode|math|\<rightarrow\>><menu-sub|<look-up|<arg|what>|1>>>>>>>

  <assign|menu|<macro|what*|<if|<is-tuple|<value|what>>|<menu-sub|<value|what>>>>>

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