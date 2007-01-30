<TeXmacs|1.0.6.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|gui-utils|1.0>

    <\src-purpose>
      Standard GUI utilities
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <use-package|gui-utils>

  <assign|form-prefix|>

  <assign|form|<macro|name|body|<style-with|src-compact|none|<with|form-prefix|<merge|<value|form-prefix>|<arg|name>|->|<wide-normal|<arg|body>>>>>>

  <assign|form-id|<macro|name|<id|<merge|<value|form-prefix>|<arg|name>>>>>

  \;

  <assign|form-button|<macro|body|cmd|<gui-small-raise|<action|<arg|body>|<arg|cmd>>>>>

  <assign|form-small-input|<macro|name|val|<gui-small-input|<locus|<form-id|<arg|name>>|<arg|val>>>>>

  <assign|form-line-input|<macro|name|val|<gui-line-input|<locus|<form-id|<arg|name>>|<arg|val>>>>>

  <assign|form-big-input|<\macro|name|val>
    <\gui-big-input>
      <\locus|<form-id|<arg|name>>>
        <arg|val>
      </locus>
    </gui-big-input>
  </macro>>

  <assign|form-circ-toggle|<macro|name|val|<style-with|src-compact|none|<action|<gui-circ-button|pastel
  blue|<gui-toggle-checked|<arg|val>>>|(form-toggle)|<arg|val>><hidden|<locus|<form-id|<arg|name>>|<arg|val>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>

<\references>
  <\collection>
    <associate||<tuple|<error|argument body>|?>>
  </collection>
</references>