<TeXmacs|1.0.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|tmdoc|1.0>

      <\src-purpose>
        Style for the <TeXmacs> manual(s).
      </src-purpose>

      <\src-copyright|2002--2004>
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

  <use-package|tmbook|tmdoc-markup|tmdoc-keyboard|tmdoc-traversal>

  \;

  <assign|par-hyphen|professional>

  <assign|par-par-sep|0.5fn>

  <assign|par-first|0fn>

  <assign|font-base-size|11>

  \;

  <assign|doc-make-title|<\macro|name>
    <assign|page-this-header|><assign|page-this-footer|><vspace|0.33pag>

    <with|math-font-series|bold|font-series|bold|font-shape|small-caps|<style-with|src-compact|none|<really-huge|<doc-title-block|<arg|name>>>>>

    <new-page>

    <assign|page-this-header|><assign|page-this-footer|><vspace|0.33pag>

    <new-page>
  </macro>>

  <assign|title|<macro|name|<doc-make-title|<arg|name>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>