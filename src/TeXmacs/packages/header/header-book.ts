<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|header-book|1.0|header|1.0>

    <\src-purpose>
      Headers for books.
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

  <assign|odd-page-text|<macro|s|<assign|page-odd-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><with|font-shape|small-caps|<arg|s>><htab|5mm><quote|<page-the-page>>>>>>>

  <assign|even-page-text|<macro|s|<assign|page-even-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><quote|<page-the-page>><htab|5mm><with|math-font-shape|small-caps|<arg|s>>>>>>>

  \;

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|<style-with|src-compact|none|<odd-page-text|<arg|name>><even-page-text|<arg|name>><simple-page>>>>

  <assign|header-secondary|<macro|name|nr|what|<odd-page-text|<arg|nr><space|2spc><arg|name>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>