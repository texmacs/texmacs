<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|base-frame|1.0|base-frame|1.0>

    <\src-purpose>
      Common base for frames.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Global parameters for all themes
    </src-comment>
  </active*>

  <assign|frame-thickness|1>

  <assign|frame-recolor|>

  <assign|frame-hpadding|0.5tab>

  <assign|frame-vpadding|0.5tab>

  <\active*>
    <\src-comment>
      Defaults
    </src-comment>
  </active*>

  <assign|frame-picture|tmfs://artwork/pictures/frames/carved-wood-frame.jpg>

  <assign|frame-effect|<eff-make-transparent|0|white>>

  <assign|frame-lcrop|0.1056751>

  <assign|frame-rcrop|0.8943249>

  <assign|frame-bcrop|0.1324405>

  <assign|frame-tcrop|0.8650794>

  <assign|frame-lrepeat|>

  <assign|frame-rrepeat|>

  <assign|frame-brepeat|>

  <assign|frame-trepeat|>

  <assign|frame-lborder|1.5tab>

  <assign|frame-rborder|1.5tab>

  <assign|frame-bborder|1.5tab>

  <assign|frame-tborder|1.5tab>

  <assign|frame-lpadding|0tab>

  <assign|frame-rpadding|0tab>

  <assign|frame-bpadding|0tab>

  <assign|frame-tpadding|0tab>

  <\active*>
    <\src-comment>
      Defaults
    </src-comment>
  </active*>

  <assign|art-frame|<macro|body|<art-box|<arg|body>|<tuple|frame|<value|frame-picture>|effect|<if|<equal|<value|frame-recolor>|>|<value|frame-effect>|<eff-recolor|<value|frame-effect>|<value|frame-recolor>>>|lcrop|<value|frame-lcrop>|rcrop|<value|frame-rcrop>|bcrop|<value|frame-bcrop>|tcrop|<value|frame-tcrop>|lrepeat|<value|frame-lrepeat>|rrepeat|<value|frame-rrepeat>|brepeat|<value|frame-brepeat>|trepeat|<value|frame-trepeat>|lwidth|<times|<value|frame-thickness>|<value|frame-lborder>>|rwidth|<times|<value|frame-thickness>|<value|frame-rborder>>|bheight|<times|<value|frame-thickness>|<value|frame-bborder>>|theight|<times|<value|frame-thickness>|<value|frame-tborder>>>|<tuple|text|normal|lpadding|<plus|<times|<value|frame-thickness>|<plus|<value|frame-lborder>|<value|frame-lpadding>>>|<value|frame-hpadding>>|rpadding|<plus|<times|<value|frame-thickness>|<plus|<value|frame-rborder>|<value|frame-rpadding>>>|<value|frame-hpadding>>|bpadding|<plus|<times|<value|frame-thickness>|<plus|<value|frame-bborder>|<value|frame-bpadding>>>|<value|frame-vpadding>>|tpadding|<plus|<times|<value|frame-thickness>|<plus|<value|frame-tborder>|<value|frame-tpadding>>>|<value|frame-vpadding>>>>>>

  <\active*>
    <\src-comment>
      Frame theme parameters
    </src-comment>
  </active*>

  <new-theme|frame|frame-picture|frame-effect|frame-lcrop|frame-rcrop|frame-bcrop|frame-tcrop|frame-lrepeat|frame-rrepeat|frame-brepeat|frame-trepeat|frame-lborder|frame-rborder|frame-bborder|frame-tborder|frame-lpadding|frame-rpadding|frame-bpadding|frame-tpadding>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>