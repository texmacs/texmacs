<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|tmarticle|1.0>

      <\src-purpose>
        A future style for <TeXmacs> articles.
      </src-purpose>

      <\src-copyright|1998--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This <TeXmacs> style file falls under the <hlink|GNU general public
        license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
        WHATSOEVER. If you do not have a copy of the license, then write to
        the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
        Boston, MA 02111-1307, USA.
      </src-license>
    </src-title>
  </active*>

  <use-package|article>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|odd-page-text|<macro|s|<assign|page-odd-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><tabular|<tformat|<cwith|1|-1|1|-1|cell-bborder|1ln>|<twith|table-width|1par>|<cwith|1|1|2|2|cell-halign|r>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-halign|l>|<table|<row|<cell|<with|font-shape|small-caps|<arg|s>>>|<cell|<quote|<page-the-page>>>>>>>>>>>>

  <assign|even-page-text|<macro|s|<assign|page-even-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><tabular|<tformat|<cwith|1|-1|1|-1|cell-bborder|1ln>|<twith|table-width|1par>|<cwith|1|1|2|2|cell-halign|r>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-halign|l>|<table|<row|<cell|<quote|<page-the-page>>>|<cell|<with|font-shape|small-caps|<arg|s>>>>>>>>>>>>

  \;

  <assign|header-title|<macro|name|<even-page-text|<arg|name>>>>

  <assign|header-author|<macro|name|<odd-page-text|<arg|name>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  \;

  <assign|title*|<macro|body|<with|font-series|bold|math-font-series|bold|font-shape|small-caps|font-size|1.54|<arg|body>>>>

  <assign|author*|<macro|body|<with|font-shape|italic|font-size|1.19|<arg|body>>>>

  <assign|abstract|<\macro|body>
    <\with|par-left|15mm|par-right|15mm|font-size|0.84>
      <value|hrule>

      <surround|<no-indent>||<arg|body>>

      <value|hrule>

      \;
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Sectional tags.
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<new-line><no-indent><vspace*|2fn><with|math-font-series|bold|font-series|bold|font-shape|small-caps|font-size|1.54|<htab|0fn><arg|name><htab|0fn>><vspace|2fn><no-page-break><no-indent*>>>>

  <assign|chapter-numbered-title|<macro|name|<style-with|src-compact|none|<chapter*|<style-with|src-compact|none|<with|font-size|1.83|<localize|Chapter>
  <the-chapter>><htab|0fn><vspace|1.5fn><new-line><htab|0fn><arg|name>>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|2fn><with|math-font-series|bold|font-series|bold|font-size|1.30|font-shape|small-caps|<style-with|src-compact|none|<htab|0fn><arg|name><htab|0fn>>><vspace|1fn><no-page-break><no-indent*>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1.5fn><with|math-font-series|bold|font-series|bold|font-size|1.19|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<no-indent><vspace*|1fn><with|math-font-series|bold|font-series|bold|<arg|name>><vspace|0.5fn><no-page-break><no-indent*>>>>

  <\active*>
    <\src-comment>
      Other customization.
    </src-comment>
  </active*>

  <assign|theorem-name|<macro|name|<with|font-shape|small-caps|<arg|name>>>>

  <assign|toc-main-2|<macro|what|<style-with|src-compact|none|<assign|toc-nr|<plus|<value|toc-nr>|1>><label|<the-toc>><style-with|src-compact|none|<write|toc|<style-with|src-compact|none|<vspace*|1fn><with|font-series|bold|math-font-series|bold|font-shape|small-caps|<arg|what>><quote|<value|toc-dots>><pageref|<the-toc>><vspace|0.5fn>>>>>>>

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