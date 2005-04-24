<TeXmacs|1.0.4.7>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|mmxdoc|1.0>

      <\src-purpose>
        Style for the documentation of Mathemagix.
      </src-purpose>

      <\src-copyright|2003--2004>
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

  <use-package|tmdoc|tmdoc-web>

  \;

  <assign|mml|<with|font-shape|small-caps|Mmxlib>>

  <assign|mmx|<with|font-shape|small-caps|Mathemagix>>

  \;

  <assign|mmxweb-main-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|Welcome|welcome>|<tmweb-link|Download|download>|<tmweb-link|Progress|progress>|<tmweb-link|Mailing
  lists|ml>|<tmweb-link|Contact|contact>>>>>

  <assign|aldor-fragment|<value|scheme-fragment>>

  <assign|caml-fragment|<value|scheme-fragment>>

  <assign|cpp-fragment|<value|scheme-fragment>>

  <assign|mmx-fragment|<value|scheme-fragment>>

  <assign|type|<macro|x|<with|mode|text|font-family|ss|<arg|x>>>>

  <assign|andt|<active*|<with|mode|math|<space|0.6spc><wide*|\<wedge\><rsub|>|\<wide-bar\>><space|0.6spc>>>>

  <assign|ort|<active*|<with|mode|math|<space|0.6spc><wide*|\<vee\><rsub|>|\<wide-bar\>><space|0.6spc>>>>

  <assign|implt|<active*|<with|mode|math|<space|0.6spc><wide*|\<Rightarrow\><rsub|>|\<wide-bar\>><space|0.6spc>>>>

  <assign|forallt|<active*|<with|mode|math|<wide*|\<forall\><rsub|>|\<wide-bar\>><space|0.2spc>>>>

  <assign|existst|<active*|<with|mode|math|<wide*|\<exists\><rsub|>|\<wide-bar\>><space|0.2spc>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>