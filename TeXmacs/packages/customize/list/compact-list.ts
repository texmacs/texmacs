<TeXmacs|1.99.6>

<style|<tuple|source|english>>

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
      Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
      02110-1301, USA.
    </src-license>
  </src-title>>

  <assign|item-spc|<macro| >>

  <assign|item-hsep|<macro|1tab>>

  <assign|item-vsep|<macro|0.25fn>>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|<item-vsep>><with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<arg|x>|<minus|1r|<minus|<item-hsep>|0fn>>||<plus|1r|0fn>|>>>>

  <assign|compact-item|<macro|x|<style-with|src-compact|none|<vspace*|<item-vsep>><with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<arg|x>|||<maximum|1r|0fn>|>>>>

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
    <associate|sfactor|7>
  </collection>
</initial>