<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-title-line|Package|env-manage-1.0 <with|font-shape|italic|(package
    and dtd assigned below)>>

    <\src-purpose>
      Managing groups of environments.
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

  <assign|env-manage-package|1.0>

  <if|<equal|<value|env-manage-dtd>|<uninit>>|<assign|init-document|<merge|<macro|<init-stdenv>>|<value|init-document>>>>

  <assign|env-manage-dtd|1.0>

  \;

  <assign|list-add|<macro|class|env|Name|render|<style-with|src-compact|none|<assign|<merge|list-|<arg|class>>|<tuple|<compound|<merge|list-|<arg|class>>>|<arg|class>|<arg|env>|<arg|Name>|<arg|render>>>>>>

  <assign|list-theorem|>

  <assign|list-exercise|>

  <assign|list-figure|>

  <assign|newtheorem|<macro|env|Name|<list-add|theorem|<arg|env>|<arg|Name>|theorem*>>>

  <assign|newremark|<macro|env|Name|<list-add|theorem|<arg|env>|<arg|Name>|remark*>>>

  <assign|newexercise|<macro|env|Name|<list-add|exercise|<arg|env>|<arg|Name>|exercise*>>>

  <assign|newfigure|<macro|env|Name|<list-add|figure|<arg|env>|<arg|Name>|figure*>>>

  \;

  <assign|init-stdenv|<macro|>>

  <assign|resetstdenv|<macro|<assign|footnotenr|0>>>

  <assign|thefootnote|<macro|<footnotenr>>>

  <assign|newstdenv-counter|<macro|which|<style-with|src-compact|none|<assign|<merge|the|<arg|which>>|<quasiquote|<macro|<style-with|src-compact|none|<theprefix><compound|<unquote|<merge|<arg|which>|nr>>>>>>><assign|resetstdenv|<style-with|src-compact|none|<merge|<value|resetstdenv>|<quasiquote|<macro|<assign|<unquote|<merge|<arg|which>|nr>>|0>>>>>>>>>

  <assign|newstdenv|<macro|class|env|Name|render|<assign|<arg|env>|<quasiquote|<macro|body|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|class>|nr>>|<plus|<compound|<unquote|<merge|<arg|class>|nr>>>|1>>><assign|thelabel|<compound|<unquote|<merge|the|<arg|class>>>>>>||<style-with|src-compact|none|<compound|<unquote|<arg|render>>|<translate|<unquote|<arg|Name>>|english|<language>>
  <compound|<unquote|<merge|the|<arg|class>>>>|<arg|body>>>>>>>>>>

  <assign|newsmallfigure|<macro|env|Name|<style-with|src-compact|none|<assign|<merge|small-|<arg|env>>|<quasiquote|<macro|body|caption|<style-with|src-compact|none|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|env>|nr>>|<plus|<value|<unquote|<merge|<arg|env>|nr>>>|1>>><assign|thelabel|<compound|<unquote|<merge|the|<arg|env>>>>><style-with|src-compact|none|<small-figure*|<unquote|<arg|env>>|<localize|<unquote|<arg|Name>>>
  <compound|<unquote|<merge|the|<arg|env>>>>|<arg|body>|<arg|caption>>>>>>>>>>

  <assign|newbigfigure|<macro|env|Name|<assign|<merge|big-|<arg|env>>|<quasiquote|<macro|body|caption|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|env>|nr>>|<plus|<value|<unquote|<merge|<arg|env>|nr>>>|1>>><assign|thelabel|<compound|<unquote|<merge|the|<arg|env>>>>>>||<style-with|src-compact|none|<big-figure*|<unquote|<arg|env>>|<localize|<unquote|<arg|Name>>>
  <compound|<unquote|<merge|the|<arg|env>>>>|<arg|body>|<arg|caption>>>>>>>>>>

  <assign|newstdfigure|<macro|env|Name|<newsmallfigure|<arg|env>|<arg|Name>><newbigfigure|<arg|env>|<arg|Name>>>>

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
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>