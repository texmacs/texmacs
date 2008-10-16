<TeXmacs|1.0.6.15>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|program|1.0>

    <\src-purpose>
      Some environments for typesetting algorithms. This package is very
      simplistic and should be replaced by something more elaborate in the
      future.
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

  <assign|algorithm|<macro|name|body|<surround|<vspace*|1fn>|<vspace|1fn><right-flush>|<with|par-first|0cm|<surround|<with|font-series|bold|<translate|Algorithm|english|<language>>
  <with|font-family|tt|<arg|name>>>||<with|item*|<macro|what|<with|font-series|bold|math-font-series|bold|<arg|what>
  >>|<arg|body>>>>>>>

  <assign|body|<macro|body|<surround|<vspace*|0.5fn>||<with|item*|<macro|name|<vspace*|0.5fn><with|font-series|bold|math-font-series|bold|<arg|name>
  >>|<arg|body>>>>>

  <assign|indent|<macro|body|<with|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>

  <assign|pile|<macro|x|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0.5sep>|<cwith|1|-1|1|-1|cell-tsep|0.5sep>|<cwith|1|1|1|-1|cell-tsep|0sep>|<cwith|-1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>