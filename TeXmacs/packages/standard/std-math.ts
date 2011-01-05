<TeXmacs|1.0.7.9>

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
      Textual operators
    </src-comment>
  </active*>

  <assign|math-up|<macro|x|<rigid|<text|<with|font-family|rm|font-shape|right|<arg|x>>>>>>

  <assign|math-ss|<macro|x|<rigid|<text|<with|font-family|ss|font-shape|right|<arg|x>>>>>>

  <assign|math-tt|<macro|x|<rigid|<text|<with|font-family|tt|font-shape|right|<arg|x>>>>>>

  <assign|math-bf|<macro|x|<rigid|<text|<with|font-family|rm|font-series|bold|font-shape|right|<arg|x>>>>>>

  <assign|math-it|<macro|x|<rigid|<text|<with|font-family|rm|font-shape|italic|<arg|x>>>>>>

  <assign|math-sl|<macro|x|<rigid|<text|<with|font-family|rm|font-shape|slanted|<arg|x>>>>>>

  <drd-props|math-up|syntax|<macro|x|x>>

  <drd-props|math-ss|syntax|<macro|x|x>>

  <drd-props|math-tt|syntax|<macro|x|x>>

  <drd-props|math-bf|syntax|<macro|x|x>>

  <drd-props|math-it|syntax|<macro|x|x>>

  <drd-props|math-sl|syntax|<macro|x|x>>

  <\active*>
    <\src-comment>
      Mathematical content tags.
    </src-comment>
  </active*>

  <assign|math-separator|<macro|x|<syntax|<arg|x>|,>>>

  <assign|math-quantifier|<macro|x|<syntax|<arg|x>|\<forall\>>>>

  <assign|math-imply|<macro|x|<syntax|<arg|x>|\<Rightarrow\>>>>

  <assign|math-or|<macro|x|<syntax|<arg|x>|\<cap\>>>>

  <assign|math-and|<macro|x|<syntax|<arg|x>|\<cup\>>>>

  <assign|math-not|<macro|x|<syntax|<arg|x>|\<neg\>>>>

  <assign|math-relation|<macro|x|<syntax|<arg|x>|\<less\>>>>

  <assign|math-union|<macro|x|<syntax|<arg|x>|\<cup\>>>>

  <assign|math-intersection|<macro|x|<syntax|<arg|x>|\<cap\>>>>

  <assign|math-exclude|<macro|x|<syntax|<arg|x>|\<setminus\>>>>

  <assign|math-plus|<macro|x|<syntax|<arg|x>|+>>>

  <assign|math-minus|<macro|x|<syntax|<arg|x>|->>>

  <assign|math-times|<macro|x|<syntax|<arg|x>|*>>>

  <assign|math-over|<macro|x|<syntax|<arg|x>|/>>>

  <assign|math-big|<macro|x|<syntax|<arg|x>|<big|sum>>>>

  <assign|math-prefix|<macro|x|<syntax|<arg|x>|#>>>

  <assign|math-postfix|<macro|x|<syntax|<arg|x>|!>>>

  <assign|math-open|<macro|x|<syntax|<arg|x>|(>>>

  <assign|math-close|<macro|x|<syntax|<arg|x>|)>>>

  <assign|math-ordinary|<macro|x|<syntax|<arg|x>|x>>>

  <assign|math-ignore|<macro|x|<syntax|<arg|x>|>>>

  <\active*>
    <\src-comment>
      Additional mathematical macros.
    </src-comment>
  </active*>

  <assign|shrink-inline|<macro|x|<style-with|src-compact|none|<if|<equal|<value|math-display>|false>|<with|math-level|<plus|<value|math-level>|1>|<arg|x>>|<with|math-display|false|<arg|x>>>>>>

  <drd-props|shrink-inline|arity|1|accessible|all|syntax|<macro|x|<arg|x>>>

  <assign|binom|<macro|x|y|<style-only*|<with|mode|math|<shrink-inline|<left|(><resize|<tabular*|<tformat|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0spc>|<table|<row|<cell|<inactive*|<arg|x>>>>|<row|<cell|<inactive*|<arg|y>>>>>>>||<plus|1b|2.5sep>||<minus|1t|2.5sep>><right|)>>>>>>

  <assign|ontop|<macro|x|y|<style-only*|<with|mode|math|<shrink-inline|<resize|<tabular*|<tformat|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0spc>|<table|<row|<cell|<inactive*|<arg|x>>>>|<row|<cell|<inactive*|<arg|y>>>>>>>||<plus|1b|2.5sep>||<minus|1t|2.5sep>>>>>>>

  <assign|choose|<value|binom>>

  <assign|cfrac|<macro|x|y|<style-only*|<with|mode|math|<with|math-display|true|<frac|<arg|x>|<arg|y>>>>>>>

  <assign|tfrac|<macro|x|y|<with|mode|math|<with|math-display|false|<frac|<arg|x>|<arg|y>>>>>>

  <assign|dfrac|<macro|x|y|<with|mode|math|<with|math-display|true|<frac|<arg|x>|<arg|y>>>>>>

  <assign|frac*|<macro|x|y|<move|<lsup|<arg|x>><resize|/|<plus|1l|0.15em>|<plus|1b|0.5em>|<minus|1r|0.15em>|<minus|1t|0.5em>><rsub|<arg|y>>||0.05em>>>

  <drd-props|frac*|arity|2|syntax|<macro|x|y|<arg|x>/<arg|y>>>

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