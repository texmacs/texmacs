<TeXmacs|1.0.7.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|compact-list|1.0>

    <\src-purpose>
      Compact lists
    </src-purpose>

    <src-copyright|2008--2010|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|item-hsep|<macro|1.5fn>>

  <assign|item-vsep|<macro|0.25fn>>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|<item-vsep>><with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<arg|x>|<minus|1r|<minus|<item-hsep>|0.25fn>>||<plus|1r|0.25fn>|>>>>

  <assign|compact-item|<macro|x|<style-with|src-compact|none|<vspace*|<item-vsep>><with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<arg|x>|||<maximum|1r|0fn>|>>>>

  <assign|render-bibitem|<macro|text|<aligned-item|<transform-bibitem|<arg|text>>>>>

  <assign|render-list|<\macro|body>
    <\padded-normal|<item-vsep>|<item-vsep>>
      <\indent-left|<item-hsep>>
        <surround|<no-page-break*>|<no-indent*>|<arg|body>>
      </indent-left>
    </padded-normal>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|sfactor|7>
  </collection>
</initial>