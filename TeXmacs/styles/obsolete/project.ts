<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|project|1.0>

      <\src-purpose>
        The (obsolete) project style.
      </src-purpose>

      <\src-copyright|1998--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This <TeXmacs> style file falls under the <hlink|GNU general public
        license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
        WHATSOEVER. If you don't have this file, then write to the Free
        Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
        02111-1307, USA.
      </src-license>
    </src-title>
  </active*>

  <assign|project-initialize|<macro|body|<surround|<with|font-series|bold|font-size|1.19|Project
  initialization program><vspace|0.5fn><new-line><no-page-break*>|<vspace|1.5fn>|<with|par-left|<plus|<value|par-left>|3fn>|par-first|0fn|font-family|tt|<arg|body>>>>>

  <assign|project-preamble|<macro|body|<surround|<with|font-series|bold|font-size|1.19|Project
  body><vspace|0.5fn><new-line><no-page-break*>|<vspace|1.5fn>|<with|par-left|<plus|<value|par-left>|3fn>|par-first|0fn|<arg|body>>>>>

  <assign|project-body|<macro|body|<surround|<with|font-series|bold|font-size|1.19|Project
  body><vspace|0.5fn><new-line><no-page-break*>|<vspace|1.5fn>|<with|par-left|<plus|<value|par-left>|3fn>|par-first|0fn|<arg|body>>>>>

  \;

  <assign|include-document|<macro|name|<with|font-shape|small-caps|Include
  document ><arg|name>>>

  <assign|include-project|<macro|name|<with|font-shape|small-caps|Include
  project ><arg|name>>>

  <assign|globalize-variable|<macro|var|<with|font-shape|small-caps|Globalize
  ><arg|var>>>

  <assign|localize-variable|<macro|var|<with|font-shape|small-caps|Localize
  ><arg|var>>>

  <assign|assign-variable|<macro|var|with|<with|font-shape|small-caps|Assign
  ><arg|var><with|mode|math|<space|0.5spc>\<assign\><space|0.5spc>><arg|with>>>

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
  </collection>
</initial>