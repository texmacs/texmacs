<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|number-europe|1.0|number-env|1.0>

    <\src-purpose>
      European-style numbering.
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

  <assign|newliststdenv|<macro|l|<if|<is-tuple|<arg|l>>|<newliststdenv|<look-up|<arg|l>|0>><newstdenv|<look-up|<arg|l>|2>|<look-up|<arg|l>|2>|<look-up|<arg|l>|3>|<look-up|<arg|l>|4>>>>>

  <assign|newlistfigure|<macro|l|<if|<is-tuple|<arg|l>>|<newlistfigure|<look-up|<arg|l>|0>><newstdfigure|<look-up|<arg|l>|2>|<look-up|<arg|l>|3>>>>>

  <assign|newliststdenv-counter|<macro|l|<if|<is-tuple|<arg|l>>|<newliststdenv-counter|<look-up|<arg|l>|0>><newstdenv-counter|<look-up|<arg|l>|2>>>>>

  <assign|init-stdenv|<macro|<style-with|src-compact|none|<newstdenv-counter|equation><newliststdenv-counter|<value|list-theorem>><newliststdenv-counter|<value|list-exercise>><newliststdenv-counter|<value|list-figure>><newliststdenv|<value|list-theorem>><newliststdenv|<value|list-exercise>><newlistfigure|<value|list-figure>>>>>

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