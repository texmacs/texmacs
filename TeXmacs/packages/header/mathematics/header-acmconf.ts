<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|header-acmconf|1.0>

    <\src-purpose>
      Headers for the acmconf style.
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

  <assign|header-article-2col-package|1.0>

  \;

  <assign|page-odd-header|>

  <assign|page-even-header|>

  <assign|page-odd-footer|<htab|5mm><quote|<page-the-page>><htab|5mm>>

  <assign|page-even-footer|<htab|5mm><quote|<page-the-page>><htab|5mm>>

  \;

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  \;

  <assign|make-title|<macro|body|<surround||<vspace|2fn>|<with|par-columns|1|par-mode|center|<arg|body>>>>>

  \;

  <assign|abstract|<\macro|body>
    <\padded-normal|1.5fn|1.5fn>
      <with|font-series|bold|<abstract-text>><vspace|1.5fn><no-page-break>

      <surround|<no-indent>||<arg|body>>
    </padded-normal>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>