<TeXmacs|1.99.6>

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
      Extra style parameters
    </src-comment>
  </active*>

  <assign|math-swell|0.6666ex>

  <assign|table-math-swell|0.9ex>

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

  <assign|math-or|<macro|x|<syntax|<arg|x>|\<vee\>>>>

  <assign|math-and|<macro|x|<syntax|<arg|x>|\<wedge\>>>>

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

  <assign|math-choice|<macro|disp|level-0|level-1|level-2|<if|<equal|<value|math-display>|true>|<arg|disp>|<case|<equal|<value|math-level>|0>|<arg|level-0>|<equal|<value|math-level>|1>|<arg|level-1>|<arg|level-2>>>>>

  <assign|binom|<macro|x|y|<style-only*|<with|mode|math|<shrink-inline|<left|(><resize|<tabular*|<tformat|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0spc>|<table|<row|<cell|<inactive*|<arg|x>>>>|<row|<cell|<inactive*|<arg|y>>>>>>>||<plus|1b|2.5sep>||<minus|1t|2.5sep>><right|)>>>>>>

  <assign|tbinom|<macro|x|y|<with|mode|math|<with|math-display|false|<binom|<arg|x>|<arg|y>>>>>>

  <assign|dbinom|<macro|x|y|<with|mode|math|<with|math-display|true|<binom|<arg|x>|<arg|y>>>>>>

  <assign|modulo|<macro|x|<space|0.2spc>mod <arg|x>>>

  <assign|bmod|<macro|<space|0.2spc>mod >>

  <assign|pmod|<macro|x|<around*|(|mod <arg|x>|)>>>

  <assign|pod|<macro|x|<around*|(|<arg|x>|)>>>

  <assign|ontop|<macro|x|y|<style-only*|<with|mode|math|<shrink-inline|<resize|<tabular*|<tformat|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0spc>|<table|<row|<cell|<inactive*|<arg|x>>>>|<row|<cell|<inactive*|<arg|y>>>>>>>||<plus|1b|2.5sep>||<minus|1t|2.5sep>>>>>>>

  <assign|choose|<value|binom>>

  <assign|tfrac|<macro|x|y|<with|mode|math|<with|math-display|false|<frac|<arg|x>|<arg|y>>>>>>

  <assign|dfrac|<macro|x|y|<with|mode|math|<with|math-display|true|<frac|<arg|x>|<arg|y>>>>>>

  <assign|cfrac|<macro|x|y|<with|mode|math|<dfrac|<arg|x>|<resize|<arg|y>|||<plus|1r|-1sep>|>>>>>

  <assign|frac*|<macro|x|y|<move|<lsup|<arg|x>><resize|/|<plus|1l|0.15em>|<plus|1b|0.5em>|<minus|1r|0.15em>|<minus|1t|0.5em>><rsub|<arg|y>>||0.05em>>>

  <drd-props|frac*|arity|2|syntax|<macro|x|y|<arg|x>/<arg|y>>>

  <assign|separating-space|<macro|len|<space|<arg|len>>>>

  <assign|application-space|<macro|len|<space|<arg|len>>>>

  <drd-props|separating-space|syntax|<macro|len|,>>

  <drd-props|application-space|syntax|<macro|len| >>

  <assign|genfrac|<macro|left|right|sep|disp|x|y|<style-only*|<with|mode|math|<shrink-inline|<around*|<inactive*|<arg|left>>|<resize|<tabular*|<tformat|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0spc>|<cwith|1|1|1|-1|cell-bborder|sep>|<table|<row|<cell|<inactive*|<arg|x>>>>|<row|<cell|<inactive*|<arg|y>>>>>>>||<plus|1b|2.5sep>||<minus|1t|2.5sep>>|<inactive*|<arg|right>>>>>>>>

  <assign|left-script|<macro|body|s|<active*|<tabular|<tformat|<cwith|1|1|1|-1|cell-lsep|0spc>|<cwith|1|1|1|-1|cell-rsep|0spc>|<cwith|1|1|1|-1|cell-bsep|0spc>|<cwith|1|1|1|-1|cell-tsep|0spc>|<cwith|1|1|1|-1|cell-vcorrect|n>|<cwith|1|1|1|-1|cell-valign|c>|<cwith|1|1|1|1|cell-halign|r>|<cwith|1|1|2|2|cell-halign|c>|<cwith|1|1|3|3|cell-halign|l>|<table|<row|<cell|<with|math-level|<plus|<value|math-level>|1>|<resize|<arg|s>|<minus|1r|1.5ex>||<minus|1r|0.5ex>|>>>|<cell|<arg|body>>|<cell|<with|math-level|<plus|<value|math-level>|1>|<resize||0ex||1ex|>>>>>>>>>>

  <assign|right-script|<macro|body|s|<active*|<tabular|<tformat|<cwith|1|1|1|-1|cell-lsep|0spc>|<cwith|1|1|1|-1|cell-rsep|0spc>|<cwith|1|1|1|-1|cell-bsep|0spc>|<cwith|1|1|1|-1|cell-tsep|0spc>|<cwith|1|1|1|-1|cell-vcorrect|n>|<cwith|1|1|1|-1|cell-valign|c>|<cwith|1|1|2|2|cell-halign|c>|<table|<row|<cell|<with|math-level|<plus|<value|math-level>|1>|<resize||0ex||1ex|>>>|<cell|<arg|body>>|<cell|<with|math-level|<plus|<value|math-level>|1>|<resize|<arg|s>|<plus|1l|0.5ex>||<plus|1l|1.5ex>|>>>>>>>>>>

  <assign|suppressed|<macro|x|<with|color|#e0b080|<arg|x>>>>

  <drd-props|suppressed|arity|1|accessible|none>

  <assign|tiny-box|<macro|<tiny|<with|font|roman|math-font|roman|<move|\<Box\>||0.05ex>>>>>

  <assign|explicit-space|<macro|<syntax|\<short-underscore\>| >>>

  <assign|old-spacing|<macro|body|<with|spacing-policy|old|<arg|body>>>>

  <assign|normal-spacing|<macro|body|<with|spacing-policy|default|<arg|body>>>>

  <assign|wide-spacing|<macro|body|<with|spacing-policy|wide|<arg|body>>>>

  <\active*>
    <\src-comment>
      Mathematical tabular structures.
    </src-comment>
  </active*>

  <assign|math-table-base|<macro|x|<style-with|src-compact|none|<if|<equal|<value|math-display>|false>|<with|math-level|<plus|<value|math-level>|1>|math-top-swell-start|1.6ex|math-top-swell-end|2.5ex|math-bot-swell-start|-0.6ex|math-bot-swell-end|-1.5ex|<arg|x>>|<with|math-display|false|math-top-swell-start|1.6ex|math-top-swell-end|2.5ex|math-bot-swell-start|-0.6ex|math-bot-swell-end|-1.5ex|<arg|x>>>>>>

  <drd-props|math-table-base|arity|1|accessible|all|syntax|<macro|x|<arg|x>>>

  <assign|matrix*|<macro|x|<math-table-base|<style-with|src-compact|none|<tformat|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|-1|cell-swell|<value|table-math-swell>>|<arg|x>>>>>>

  <assign|matrix|<macro|x|<math-table-base|<style-with|src-compact|none|||||<left|(><tformat|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|-1|cell-swell|<value|table-math-swell>>|<arg|x>><right|)>>>>>

  <assign|bmatrix|<macro|x|<math-table-base|<style-with|src-compact|none|<left|[><tformat|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|-1|cell-swell|<value|table-math-swell>>|<arg|x>><right|]>>>>>

  <assign|det|<macro|x|<math-table-base|<style-with|src-compact|none|<left|\|><tformat|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|-1|cell-swell|<value|table-math-swell>>|<arg|x>><right|\|>>>>>

  <assign|choice|<macro|x|<math-table-base|<style-with|src-compact|none|<left|{><tformat|<cwith|1|-1|1|-1|cell-swell|<value|table-math-swell>>|<arg|x>><right|.>>>>>

  <assign|stack|<macro|x|<tformat|<twith|table-valign|C>|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0.5sep>|<cwith|1|-1|1|-1|cell-tsep|0.5sep>|<cwith|1|1|1|-1|cell-tsep|0sep>|<cwith|-1|-1|1|-1|cell-bsep|0sep>|<arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>