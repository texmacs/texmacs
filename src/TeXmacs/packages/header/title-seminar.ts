<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|title-seminar|1.0|header-title|1.0>

    <\src-purpose>
      Titles for the seminar style.
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

  <use-package|title-base>

  <\active*>
    <\src-comment>
      Containers.
    </src-comment>
  </active*>

  <assign|doc-make-title|<\macro|body>
    <no-indent><assign|page-this-header|><assign|page-this-footer|><vspace|0.1pag>

    <doc-title-block|<arg|body>>

    <vspace|0.1pag>

    <new-page><right-flush>
  </macro>>

  <assign|doc-abstract|<\macro|body>
    <section*|<abstract-text>>

    <\wide-normal>
      <arg|body>
    </wide-normal>
  </macro>>

  <\active*>
    <\src-comment>
      Title elements.
    </src-comment>
  </active*>

  <assign|doc-render-title|<macro|x|<surround||<vspace|0.1pag>|<style-with|src-compact|none|<doc-title-block|<with|math-font-series|bold|font-series|bold|font-size|2|color|red|<arg|x>>>>>>>

  <assign|doc-author|<macro|body|<\surround|<vspace*|0.1pag>|<vspace|0.1pag>>
    <\with|par-par-sep|0fn>
      <doc-title-block|<arg|body>>
    </with>
  </surround>>>

  <assign|doc-date|<macro|x|<surround|<vspace*|0.1pag>||<doc-author-block|<with|font-shape|italic|<arg|x>>>>>>

  <\active*>
    <\src-comment>
      Author elements.
    </src-comment>
  </active*>

  <assign|author-render-name|<macro|x|<surround|<vspace*|1fn>||<doc-author-block|<with|font-shape|small-caps|<large|<arg|x>>>>>>>

  <assign|author-address|<\macro|x>
    <surround|<vspace*|2fn>|<vspace|2fn>|<doc-author-block|<arg|x>>>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>