<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|section-automatic-long|1.0|section-automatic|1.0>

    <\src-purpose>
      Environments for automatically generated content for long works like
      books.
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

  <\active*>
    <\src-comment>
      Rendering.
    </src-comment>
  </active*>

  <assign|automatic-chapter|<macro|name|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<assign|the-chapter|<macro|*>><resetchapter><assign|the-label|<the-chapter>><header-primary|<arg|name>|*|<arg|name>>>||<chapter*|<arg|name>>>>>>

  <assign|bibliography*|<\macro|aux|style|file-name|name|body>
    <style-with|src-compact|none|<automatic-chapter|<localize|<arg|name>>><toc-main-2|<localize|<arg|name>>>>

    <with|par-par-sep|0fn|font-size|0.84|<description|<arg|body>>>
  </macro>>

  <assign|table-of-contents*|<\macro|aux|name|body>
    <automatic-chapter|<localize|<arg|name>>>

    <with|par-first|0fn|par-par-sep|0fn|<arg|body>>
  </macro>>

  <assign|the-index*|<\macro|aux|name|body>
    <style-with|src-compact|none|<automatic-chapter|<localize|<arg|name>>><toc-main-2|<localize|<arg|name>>>>

    <with|par-first|0fn|par-par-sep|0fn|font-size|0.84|par-columns|2|<arg|body>>
  </macro>>

  <assign|the-glossary*|<\macro|aux|name|body>
    <style-with|src-compact|none|<automatic-chapter|<localize|<arg|name>>><toc-main-2|<localize|<arg|name>>>>

    <with|par-first|0cm|par-par-sep|0fn|font-size|0.84|<arg|body>>
  </macro>>

  <\active*>
    <\src-comment>
      The environments.
    </src-comment>
  </active*>

  <assign|bibliography|<\macro|aux|style|file-name|body>
    <bibliography*|<arg|aux>|<arg|style>|<arg|file-name>|Bibliography|<arg|body>>
  </macro>>

  <assign|table-of-contents|<\macro|aux|body>
    <table-of-contents*|<arg|aux>|Table of contents|<arg|body>>
  </macro>>

  <assign|the-index|<\macro|aux|body>
    <the-index*|<arg|aux>|Index|<arg|body>>
  </macro>>

  <assign|the-glossary|<\macro|aux|body>
    <the-glossary*|<arg|aux>|Glossary|<arg|body>>
  </macro>>

  <assign|thebibliography|<\macro|dummy|body>
    <style-with|src-compact|none|<automatic-chapter|<localize|Bibliography>><toc-main-2|<localize|Bibliography>>>

    <with|par-par-sep|0fn|font-size|0.84|<description|<arg|body>>>
  </macro>>

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