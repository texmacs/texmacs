<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|header-amsart|1.0>

    <\src-purpose>
      Headers for the amsart style.
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

  <use-package|header-article>

  <assign|header-article-package|1.0>

  \;

  <assign|odd-page-text|<macro|s|<assign|page-odd-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><htab|0mm><with|font-shape|small-caps|<arg|s>><htab|5mm><quote|<page-the-page>>>>>>>

  <assign|even-page-text|<macro|s|<assign|page-even-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><quote|<page-the-page>><htab|5mm><with|font-shape|small-caps|<arg|s>><htab|0mm>>>>>>

  \;

  <assign|header-title|<macro|name|<even-page-text|<arg|name>>>>

  <assign|header-author|<macro|name|<odd-page-text|<arg|name>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  \;

  <assign|title*|<macro|body|<with|math-font-series|bold|font-series|bold|font-shape|small-caps|font-size|1.19|<arg|body>>>>

  <assign|author*|<macro|body|<with|font-shape|small-caps|<arg|body>>>>

  \;

  <assign|abstract|<macro|body|<surround|<vspace*|2fn>|<vspace|1fn>|<with|par-left|15mm|par-right|15mm|font-size|0.84|<style-with|src-compact|none|<surround|<no-indent><with|font-shape|small-caps|<abstract-text>>.
  |<right-flush>|<arg|body>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>