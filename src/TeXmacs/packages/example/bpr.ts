<TeXmacs|1.0.3.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|bpr|1.0>

    <\src-purpose>
      Style package for the book ``Algorithms in Real Algebraic Geometry'' by
      Basu, Pollack and Roy. With the aid of <TeXmacs>, this book should
      become interactive.
    </src-purpose>

    <src-copyright|1998--2004|David Allouche>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|A|<macro|<with|mode|text|font-family|rm|A>>>

  <assign|addots|<macro|<mathinner|<mkern>1mu
  <raise>1pt<vbox|<kern>7pt<hbox|.>> <mkern>2mu<raise>4pt<hbox|.><mkern>2mu
  <raise>7pt<hbox|.><mkern>1mu>>>

  <assign|Adj|<macro|<with|font-family|rm|Adj>>>

  <assign|alg|<macro|<with|font-family|rm|alg>>>

  <assign|App|<macro|<with|font-family|rm|App>>>

  <assign|B|<macro|<with|mode|text|font-family|rm|B>>>

  <assign|b|<macro|<with|font-family|rm|b>>>

  <assign|ba|<macro|<with|font-family|rm|ba>>>

  <assign|Bez|<macro|<with|font-family|rm|Bez>>>

  <assign|BezElim|<macro|<with|font-family|rm|BezElim>>>

  <assign|BElim|<macro|<with|font-family|rm|BElim>>>

  <assign|C|<macro|<with|mode|text|font-family|rm|C>>>

  <assign|CF|<macro|<with|font-family|rm|CF>>>

  <assign|cf|<macro|<with|mode|text|font-family|rm|cof>>>

  <assign|Ch|<macro|<with|font-family|rm|Ch>>>

  <assign|Co|<macro|<with|mode|text|font-family|rm|C>>>

  <assign|complex|<macro|<Complex|>>>

  <assign|Complex|<macro|1|<with|mode|text|mode|math|<with|math-font|Bbb|C><rsup|<arg|1>>>>>

  <assign|cont|<macro|<with|font-family|rm|cont>>>

  <assign|Cr|<macro|<with|mode|text|font-family|rm|Cr>>>

  <assign|CS|<macro|<with|font-family|rm|CS>>>

  <assign|CSign|<macro|<with|font-family|rm|CSIGN>>>

  <assign|D|<macro|<with|mode|text|font-family|rm|D>>>

  <assign|d|<macro|<with|font-family|rm|d>>>

  <assign|decide|<macro|<with|font-family|rm|Decide>>>

  <assign|Def|<macro|<with|mode|text|font-family|rm|Def>>>

  <assign|Der|<macro|<with|font-family|rm|Der>>>

  <assign|diam|<macro|<with|font-family|rm|diam>>>

  <assign|Disc|<macro|<with|font-family|rm|Disc>>>

  <assign|dist|<macro|<with|font-family|rm|dist>>>

  <assign|epc|<macro|<with|mode|text|font-family|rm|EPC>>>

  <assign|E|<macro|<with|font-family|rm|Ext>>>

  <assign|Elim|<macro|<with|font-family|rm|Elim>>>

  <assign|eps|<macro|\<varepsilon\>>>

  <assign|EQ|<macro|<with|mode|text|font-family|rm|EQ>>>

  <assign|F|<macro|<with|mode|text|font-family|rm|F>>>

  <assign|False|<macro|<with|font-family|rm|False>>>

  <assign|Free|<macro|<with|font-family|rm|Free>>>

  <assign|gau|<macro|<with|font-family|rm|Gau>>>

  <assign|Grad|<macro|<with|font-family|rm|Grad>>>

  <assign|GRem|<macro|<with|mode|text|font-family|rm|GRem>>>

  <assign|grlex|<macro|<with|font-family|rm|grlex>>>

  <assign|H|<macro|<with|font-family|rm|H>>>

  <assign|Han|<macro|<with|font-family|rm|Han>>>

  <assign|han|<macro|<with|font-family|rm|han>>>

  <assign|Her|<macro|<with|font-family|rm|Her>>>

  <assign|her|<macro|<with|font-family|rm|her>>>

  <assign|Ho|<macro|<with|mode|text|font-family|rm|H>>>

  <assign|I|<macro|<with|mode|text|font-family|rm|I>>>

  <assign|Id|<macro|<with|mode|text|font-family|rm|Id>>>

  <assign|Im|<macro|<with|font-family|rm|Im>>>

  <assign|Ind|<macro|<with|font-family|rm|Ind>>>

  <assign|ini|<macro|<with|font-family|rm|In>>>

  <assign|K|<macro|<with|mode|text|font-family|rm|K>>>

  <assign|Ker|<macro|<with|mode|text|font-family|rm|Ker>>>

  <assign|la|<macro|\<langle\>>>

  <assign|lc|<macro|<with|mode|text|font-family|rm|lcof>>>

  <assign|lcm|<macro|<with|mode|text|font-family|rm|lcm>>>

  <assign|ldp|<macro|<with|mode|text|font-family|rm|ldp>>>

  <assign|Le|<macro|<with|mode|text|font-family|rm|L>>>

  <assign|lex|<macro|<with|font-family|rm|lex>>>

  <assign|LL|<macro|<with|font-family|rm|L>>>

  <assign|lmon|<macro|<with|mode|text|font-family|rm|lmon>>>

  <assign|lt|<macro|<with|mode|text|font-family|rm|lt>>>

  <assign|M|<macro|<with|mode|text|font-family|rm|M>>>

  <assign|Mat|<macro|<with|font-family|rm|Mat>>>

  <assign|mod|<macro|<with|font-family|rm|mod>>>

  <assign|MUS|<macro|<with|mode|text|font-family|rm|U>>>

  <assign|N|<macro|<with|math-font|Bbb|N>>>

  <assign|NF|<macro|<with|mode|text|font-family|rm|NF>>>

  <assign|NN|<macro|<with|font-family|rm|N>>>

  <assign|pagcd|<macro|<with|mode|text|font-family|rm|Posgcd>>>

  <assign|paremseq|<macro|<with|font-family|rm|TRems>>>

  <assign|parem|<macro|<with|mode|text|font-family|rm|Parem>>>

  <assign|pdet|<macro|<with|mode|text|font-family|rm|pdet>>>

  <assign|PMC|<macro|<with|font-family|rm|n>>>

  <assign|Po|<macro|<with|mode|text|font-family|rm|Pol>>>

  <assign|pos|<macro|<with|font-family|rm|pos>>>

  <assign|posgcd|<macro|<with|mode|text|font-family|rm|Posgcd>>>

  <assign|posquo|<macro|<with|mode|text|font-family|rm|Posquo>>>

  <assign|PP|<macro|<with|math-font|Bbb|P>>>

  <assign|pquo|<macro|<with|mode|text|font-family|rm|Pquo>>>

  <assign|pr|<macro|<with|mode|text|font-family|rm|Proj>>>

  <assign|prem|<macro|<with|mode|text|font-family|rm|Prem>>>

  <assign|Proj|<macro|<with|font-family|rm|Proj>>>

  <assign|Q|<macro|<with|math-font|Bbb|Q>>>

  <assign|Qu|<macro|<with|font-family|rm|Q>>>

  <assign|quo|<macro|<with|mode|text|font-family|rm|Quo>>>

  <assign|R|<macro|<with|mode|text|font-family|rm|R>>>

  <assign|ra|<macro|\<rangle\>>>

  <assign|rad|<macro|<with|font-family|rm|rad>>>

  <assign|Rad|<macro|<with|font-family|rm|Rad>>>

  <assign|Rank|<macro|<with|font-family|rm|Rank>>>

  <assign|re|<macro|<Real|>>>

  <assign|Real|<macro|1|<with|mode|text|mode|math|<with|math-font|Bbb|R><rsup|<arg|1>>>>>

  <assign|Rec|<macro|<with|font-family|rm|Rec>>>

  <assign|Red|<macro|<with|mode|text|font-family|rm|Red>>>

  <assign|Rem|<macro|<with|font-family|rm|Exp>>>

  <assign|rem|<macro|<with|mode|text|font-family|rm|Rem>>>

  <assign|Res|<macro|<with|mode|text|font-family|rm|Res>>>

  <assign|RM|<macro|<with|mode|text|font-family|rm|RM>>>

  <assign|RR|<macro|\<cal-R\>>>

  <assign|s|<macro|<with|mode|text|font-family|rm|sign>>>

  <assign|SB|<macro|<with|mode|text|font-family|rm|SB>>>

  <assign|sep|<macro|<with|font-family|rm|sep>>>

  <assign|SH|<macro|<with|mode|text|math-font|cal|SH>>>

  <assign|SI|<macro|<with|mode|text|font-family|rm|SIGN><rsub|R>>>

  <assign|SIGN|<macro|<with|font-family|rm|SIGN>>>

  <assign|Sign|<macro|<with|font-family|rm|Sign>>>

  <assign|sign|<macro|<with|font-family|rm|sign>>>

  <assign|SS|<macro|<with|mode|text|font-family|rm|S>>>

  <assign|SSign|<macro|<with|font-family|rm|SSIGN>>>

  <assign|SSQ|<macro|<with|mode|text|font-family|rm|S>>>

  <assign|SQ|<macro|<with|font-family|rm|SQ>>>

  <assign|SR|<macro|<with|mode|text|font-family|rm|SR>>>

  <assign|sr|<macro|<with|mode|text|font-family|rm|sr>>>

  <assign|SU|<macro|<with|mode|text|font-family|rm|SU>>>

  <assign|SV|<macro|<with|mode|text|font-family|rm|SV>>>

  <assign|Sy|<macro|<with|mode|text|font-family|rm|S>>>

  <assign|Syl|<macro|<with|mode|text|font-family|rm|Syl>>>

  <assign|T|<macro|<with|mode|text|font-family|rm|T>>>

  <assign|tdeg|<macro|<with|font-family|rm|tdeg>>>

  <assign|Thom|<macro|<with|font-family|rm|Thom>>>

  <assign|To|<macro|<with|font-family|rm|To>>>

  <assign|Tr|<macro|<with|font-family|rm|Tr>>>

  <assign|Tru|<macro|<with|font-family|rm|Tru>>>

  <assign|True|<macro|<with|font-family|rm|True>>>

  <assign|UR|<macro|<with|math-font|cal|UR>>>

  <assign|URM|<macro|<with|mode|text|font-family|rm|URM>>>

  <assign|V|<macro|<with|font-family|rm|V>>>

  <assign|W|<macro|<with|mode|text|font-family|rm|W>>>

  <assign|Z|<macro|<with|math-font|Bbb|Z>>>

  <assign|ZZ|<macro|<with|font-family|rm|Z>>>

  \;

  <assign|coucou|<macro|1|<ifvmode><else><marginpar*|<htab|1fn><with|mode|math|\<rhd\>>|<with|mode|math|\<lhd\>>><fi>
  <with|mode|math|\<langle\>><with|font-shape|small-caps|<arg|1>><with|mode|math|\<rangle\>>>>

  <assign|w|<macro|1|<coucou|<underline|WARNING>: <arg|1>>>>

  <assign|hide|<macro|x|<flag|hide|red|x>>>

  <assign|hideproof|<\macro|x>
    <arg|x>
  </macro>>

  <assign|hidepart|<\macro|x>
    <arg|x>
  </macro>>

  <assign|hidealg|<\macro|x>
    <arg|x>
  </macro>>

  <assign|compressed|<macro|x|<flag|compressed|red|x>>>

  \;

  <add-to-counter-group|algorithm|std-env>

  <assign|algorithm|<macro|body|<surround|<next-algorithm>||<render-remark|<localize|Algorithm>
  <the-algorithm>|<arg|body>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|french>
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
  </collection>
</initial>