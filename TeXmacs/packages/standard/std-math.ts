<TeXmacs|1.0.3.4>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-math|1.0>

    <\src-purpose>
      This package defines additional macros for mathematics.
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

  <\active*>
    <\src-comment>
      Mathematical content tags.
    </src-comment>
  </active*>

  <assign|math-ord|<macro|x|<with|math-condensed|true|<arg|x>>>>

  <assign|math-open|<value|op>>

  <assign|math-close|<value|op>>

  <assign|math-punct|<value|op>>

  <assign|math-bin|<macro|x|<space|0.5spc><with|math-condensed|true|<arg|x>><space|0.5spc>>>

  <assign|math-rel|<value|math-bin>>

  <assign|math-op|<macro|x|<with|math-condensed|true|<arg|x>><space|1spc>>>

  <\active*>
    <\src-comment>
      Additional mathematical macros.
    </src-comment>
  </active*>

  <assign|shrink-inline|<macro|x|<style-with|src-compact|none|<if|<equal|<value|math-display>|false>|<with|math-level|<plus|<value|math-level>|1>|<arg|x>>|<with|math-display|false|<arg|x>>>>>>

  <drd-props|shrink-inline|arity|1|accessible|all>

  <assign|binom|<macro|x|y|<style-only*|<with|mode|math|<shrink-inline|<left|(><resize|<tabular*|<tformat|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0spc>|<table|<row|<cell|<inactive*|<arg|x>>>>|<row|<cell|<inactive*|<arg|y>>>>>>>||b+2.5sep||t-2.5sep|><right|)>>>>>>

  <assign|choose|<value|binom>>

  <\active*>
    <\src-comment>
      Mathematical tabular structures.
    </src-comment>
  </active*>

  <assign|matrix|<macro|x|<shrink-inline|<style-with|src-compact|none|<left|(><tformat|<cwith|1|-1|1|-1|cell-halign|c>|<arg|x>><right|)>>>>>

  <assign|det|<macro|x|<shrink-inline|<style-with|src-compact|none|<left|\|><tformat|<cwith|1|-1|1|-1|cell-halign|c>|<arg|x>><right|\|>>>>>

  <assign|choice|<macro|x|<shrink-inline|<style-with|src-compact|none|<left|{><tformat|<arg|x>><right|.>>>>>

  <assign|stack|<macro|x|<tformat|<twith|table-valign|C>|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0.5sep>|<cwith|1|-1|1|-1|cell-tsep|0.5sep>|<cwith|1|1|1|-1|cell-tsep|0sep>|<cwith|-1|-1|1|-1|cell-bsep|0sep>|<arg|x>>>>

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