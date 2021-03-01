<TeXmacs|1.99.18>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|smart-ref|1.0>

    <\src-purpose>
      Smart references
    </src-purpose>

    <src-copyright|2020|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(link ref-markup)>

  <\active*>
    <\src-comment>
      Main ``smart-ref'' macro.
    </src-comment>
  </active*>

  <assign|unknown-ref|<xmacro|args|<extern|ext-typed-ref||<quote-arg|args>>>>

  <assign|smart-ref|<xmacro|args|<extern|ext-smart-ref|<quote-arg|args>>>>

  <drd-props|smart-ref|arity|<tuple|repeat|1|1>|accessible|none|identifier|all>

  <\active*>
    <\src-comment>
      Enunciations.
    </src-comment>
  </active*>

  <assign|t-ref|<xmacro|args|<extern|ext-typed-ref|Theorem|<quote-arg|args>>>>

  <assign|th-ref|<xmacro|args|<extern|ext-typed-ref|Theorem|<quote-arg|args>>>>

  <assign|thm-ref|<xmacro|args|<extern|ext-typed-ref|Theorem|<quote-arg|args>>>>

  <assign|theorem-ref|<xmacro|args|<extern|ext-typed-ref|Theorem|<quote-arg|args>>>>

  <assign|p-ref|<xmacro|args|<extern|ext-typed-ref|Proposition|<quote-arg|args>>>>

  <assign|pr-ref|<xmacro|args|<extern|ext-typed-ref|Proposition|<quote-arg|args>>>>

  <assign|prop-ref|<xmacro|args|<extern|ext-typed-ref|Proposition|<quote-arg|args>>>>

  <assign|proposition-ref|<xmacro|args|<extern|ext-typed-ref|Proposition|<quote-arg|args>>>>

  <assign|l-ref|<xmacro|args|<extern|ext-typed-ref|Lemma|<quote-arg|args>>>>

  <assign|lm-ref|<xmacro|args|<extern|ext-typed-ref|Lemma|<quote-arg|args>>>>

  <assign|lem-ref|<xmacro|args|<extern|ext-typed-ref|Lemma|<quote-arg|args>>>>

  <assign|lemma-ref|<xmacro|args|<extern|ext-typed-ref|Lemma|<quote-arg|args>>>>

  <assign|co-ref|<xmacro|args|<extern|ext-typed-ref|Corollary|<quote-arg|args>>>>

  <assign|cor-ref|<xmacro|args|<extern|ext-typed-ref|Corollary|<quote-arg|args>>>>

  <assign|corr-ref|<xmacro|args|<extern|ext-typed-ref|Corollary|<quote-arg|args>>>>

  <assign|corrolary-ref|<xmacro|args|<extern|ext-typed-ref|Corollary|<quote-arg|args>>>>

  <assign|def-ref|<xmacro|args|<extern|ext-typed-ref|Definition|<quote-arg|args>>>>

  <assign|dfn-ref|<xmacro|args|<extern|ext-typed-ref|Definition|<quote-arg|args>>>>

  <assign|defn-ref|<xmacro|args|<extern|ext-typed-ref|Definition|<quote-arg|args>>>>

  <assign|definition-ref|<xmacro|args|<extern|ext-typed-ref|Definition|<quote-arg|args>>>>

  <assign|not-ref|<xmacro|args|<extern|ext-typed-ref|Notation|<quote-arg|args>>>>

  <assign|nota-ref|<xmacro|args|<extern|ext-typed-ref|Notation|<quote-arg|args>>>>

  <assign|notation-ref|<xmacro|args|<extern|ext-typed-ref|Notation|<quote-arg|args>>>>

  <assign|ax-ref|<xmacro|args|<extern|ext-typed-ref|Axiom|<quote-arg|args>>>>

  <assign|axiom-ref|<xmacro|args|<extern|ext-typed-ref|Axiom|<quote-arg|args>>>>

  <assign|conv-ref|<xmacro|args|<extern|ext-typed-ref|Convention|<quote-arg|args>>>>

  <assign|convention-ref|<xmacro|args|<extern|ext-typed-ref|Convention|<quote-arg|args>>>>

  <assign|conj-ref|<xmacro|args|<extern|ext-typed-ref|Conjecture|<quote-arg|args>>>>

  <assign|conjecture-ref|<xmacro|args|<extern|ext-typed-ref|Conjecture|<quote-arg|args>>>>

  <assign|rem-ref|<xmacro|args|<extern|ext-typed-ref|Remark|<quote-arg|args>>>>

  <assign|remark-ref|<xmacro|args|<extern|ext-typed-ref|Remark|<quote-arg|args>>>>

  <assign|note-ref|<xmacro|args|<extern|ext-typed-ref|Note|<quote-arg|args>>>>

  <assign|war-ref|<xmacro|args|<extern|ext-typed-ref|Warning|<quote-arg|args>>>>

  <assign|warning-ref|<xmacro|args|<extern|ext-typed-ref|Warning|<quote-arg|args>>>>

  <assign|ex-ref|<xmacro|args|<extern|ext-typed-ref|Example|<quote-arg|args>>>>

  <assign|example-ref|<xmacro|args|<extern|ext-typed-ref|Example|<quote-arg|args>>>>

  <assign|exc-ref|<xmacro|args|<extern|ext-typed-ref|Exercise|<quote-arg|args>>>>

  <assign|exe-ref|<xmacro|args|<extern|ext-typed-ref|Exercise|<quote-arg|args>>>>

  <assign|exer-ref|<xmacro|args|<extern|ext-typed-ref|Exercise|<quote-arg|args>>>>

  <assign|exercise-ref|<xmacro|args|<extern|ext-typed-ref|Exercise|<quote-arg|args>>>>

  <assign|prb-ref|<xmacro|args|<extern|ext-typed-ref|Problem|<quote-arg|args>>>>

  <assign|prob-ref|<xmacro|args|<extern|ext-typed-ref|Problem|<quote-arg|args>>>>

  <assign|problem-ref|<xmacro|args|<extern|ext-typed-ref|Problem|<quote-arg|args>>>>

  <assign|sol-ref|<xmacro|args|<extern|ext-typed-ref|Solution|<quote-arg|args>>>>

  <assign|solution-ref|<xmacro|args|<extern|ext-typed-ref|Solution|<quote-arg|args>>>>

  <\active*>
    <\src-comment>
      Sectional.
    </src-comment>
  </active*>

  <assign|part-ref|<xmacro|args|<extern|ext-typed-ref|Part|<quote-arg|args>>>>

  <assign|c-ref|<xmacro|args|<extern|ext-typed-ref|Chapter|<quote-arg|args>>>>

  <assign|ch-ref|<xmacro|args|<extern|ext-typed-ref|Chapter|<quote-arg|args>>>>

  <assign|chp-ref|<xmacro|args|<extern|ext-typed-ref|Chapter|<quote-arg|args>>>>

  <assign|chap-ref|<xmacro|args|<extern|ext-typed-ref|Chapter|<quote-arg|args>>>>

  <assign|chapter-ref|<xmacro|args|<extern|ext-typed-ref|Chapter|<quote-arg|args>>>>

  <assign|s-ref|<xmacro|args|<extern|ext-typed-ref|Section|<quote-arg|args>>>>

  <assign|sec-ref|<xmacro|args|<extern|ext-typed-ref|Section|<quote-arg|args>>>>

  <assign|section-ref|<xmacro|args|<extern|ext-typed-ref|Section|<quote-arg|args>>>>

  <assign|ss-ref|<xmacro|args|<extern|ext-typed-ref|Subsection|<quote-arg|args>>>>

  <assign|ssec-ref|<xmacro|args|<extern|ext-typed-ref|Subsection|<quote-arg|args>>>>

  <assign|subs-ref|<xmacro|args|<extern|ext-typed-ref|Subsection|<quote-arg|args>>>>

  <assign|subsec-ref|<xmacro|args|<extern|ext-typed-ref|Subsection|<quote-arg|args>>>>

  <assign|subsection-ref|<xmacro|args|<extern|ext-typed-ref|Section|<quote-arg|args>>>>

  <assign|par-ref|<xmacro|args|<extern|ext-typed-ref|Paragraph|<quote-arg|args>>>>

  <assign|para-ref|<xmacro|args|<extern|ext-typed-ref|Paragraph|<quote-arg|args>>>>

  <assign|paragraph-ref|<xmacro|args|<extern|ext-typed-ref|Paragraph|<quote-arg|args>>>>

  <\active*>
    <\src-comment>
      Equations.
    </src-comment>
  </active*>

  <assign|add-brackets|<macro|body|(<arg|body>)>>

  <assign|e-ref|<xmacro|args|<extern|ext-typed-ref*||add-brackets|<quote-arg|args>>>>

  <assign|eq-ref|<xmacro|args|<extern|ext-typed-ref*||add-brackets|<quote-arg|args>>>>

  <assign|eqn-ref|<xmacro|args|<extern|ext-typed-ref*||add-brackets|<quote-arg|args>>>>

  <assign|equa-ref|<xmacro|args|<extern|ext-typed-ref*||add-brackets|<quote-arg|args>>>>

  <assign|equation-ref|<xmacro|args|<extern|ext-typed-ref*||add-brackets|<quote-arg|args>>>>

  <\active*>
    <\src-comment>
      Figures, tables, and algorithms.
    </src-comment>
  </active*>

  <assign|fig-ref|<xmacro|args|<extern|ext-typed-ref|Figure|<quote-arg|args>>>>

  <assign|figure-ref|<xmacro|args|<extern|ext-typed-ref|Figure|<quote-arg|args>>>>

  <assign|tab-ref|<xmacro|args|<extern|ext-typed-ref|Table|<quote-arg|args>>>>

  <assign|table-ref|<xmacro|args|<extern|ext-typed-ref|Table|<quote-arg|args>>>>

  <assign|alg-ref|<xmacro|args|<extern|ext-typed-ref|Algorithm|<quote-arg|args>>>>

  <assign|algo-ref|<xmacro|args|<extern|ext-typed-ref|Algorithm|<quote-arg|args>>>>

  <assign|algorithm-ref|<xmacro|args|<extern|ext-typed-ref|Algorithm|<quote-arg|args>>>>

  <assign|step-ref|<xmacro|args|<extern|ext-typed-ref|Step|<quote-arg|args>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>