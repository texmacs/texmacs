<TeXmacs|1.0.3.4>

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

  <assign|indent|<macro|body|<with|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>

  <assign|algorithm|<macro|name|body|<surround|<vspace*|1fn>|<vspace|1fn>|<with|par-first|0cm|<surround|<with|font-series|bold|<translate|Algorithm|english|<language>>
  <with|font-family|tt|<arg|name>>>||<with|item*|<macro|what|<with|font-series|bold|math-font-series|bold|<arg|what>
  >>|<arg|body>>>>>>>

  <assign|body|<macro|body|<surround|<vspace*|0.5fn>||<with|item*|<macro|name|<vspace*|0.5fn><with|font-series|bold|math-font-series|bold|<arg|name>
  >>|<arg|body>>>>>

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