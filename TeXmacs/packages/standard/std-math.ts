<TeXmacs|1.0.7.2>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-math|1.0>

    <\src-purpose>
      This package defines additional macros for mathematics.
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

  <assign|ontop|<macro|x|y|<style-only*|<with|mode|math|<shrink-inline|<resize|<tabular*|<tformat|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0spc>|<table|<row|<cell|<inactive*|<arg|x>>>>|<row|<cell|<inactive*|<arg|y>>>>>>>||b+2.5sep||t-2.5sep|>>>>>>

  <assign|choose|<value|binom>>

  <assign|cfrac|<macro|x|y|<style-only*|<with|mode|math|<with|math-display|true|<frac|<arg|x>|<arg|y>>>>>>>

  <assign|tfrac|<macro|x|y|<style-only*|<with|mode|math|<with|math-display|true|<frac|<arg|x>|<arg|y>>>>>>>

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
    <associate|preamble|true>
  </collection>
</initial>