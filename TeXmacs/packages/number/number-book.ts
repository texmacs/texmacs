<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-title-line|Package|number-book-1.0 <with|font-shape|italic|(package
    and dtd assigned below)>>

    <\src-purpose>
      Numbering books.
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

  <assign|number-book-package|1.0>

  <if|<equal|<value|number-section-dtd>|<uninit>>|<assign|init-document|<merge|<value|init-document>|<macro|<resettop>>>>>

  <assign|number-section-dtd|1.0>

  \;

  <assign|resettop|<macro|<style-with|src-compact|none|<assign|chapternr|0><assign|appendixnr|0><assign|sectionnr|0><resetstdenv>>>>

  <assign|resetchapter|<macro|<assign|sectionnr|0><resetstdenv>>>

  <assign|resetsection|<macro|<assign|subsectionnr|0>>>

  <assign|resetsubsection|<macro|<assign|subsubsectionnr|0>>>

  <assign|resetsubsubsection|<macro|>>

  \;

  <assign|thechapter|<macro|<value|chapternr>>>

  <assign|thesection|<macro|<thechapter>.<sectionnr>>>

  <assign|thesubsection|<macro|<thesection>.<subsectionnr>>>

  <assign|thesubsubsection|<macro|<thesubsection>.<subsubsectionnr>>>

  <assign|theprefix|<macro|<thechapter>.>>

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