<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-default|1.0>

    <\src-purpose>
      Default environments.
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

  <new-theorem|theorem|Theorem>

  <new-theorem|proposition|Proposition>

  <new-theorem|lemma|Lemma>

  <new-theorem|corollary|Corollary>

  <new-theorem|axiom|Axiom>

  <new-theorem|definition|Definition>

  <new-theorem|notation|Notation>

  <new-theorem|conjecture|Conjecture>

  \;

  <new-remark|remark|Remark>

  <new-remark|example|Example>

  <new-remark|note|Note>

  <new-remark|warning|Warning>

  <new-remark|convention|Convention>

  \;

  <new-exercise|exercise|Exercise>

  <new-exercise|problem|Problem>

  \;

  <new-figure|figure|Figure>

  <new-figure|table|Table>

  \;

  <assign|keywords|<macro|x|<vspace*|0.5fn><no-indent><theorem-name|<translate|Keywords:|english|<language>>
  ><arg|x>>>

  <assign|AMS-class|<macro|x|<no-indent><theorem-name|<translate|A.M.S.
  subject classification:|english|<language>> ><arg|x>>>

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