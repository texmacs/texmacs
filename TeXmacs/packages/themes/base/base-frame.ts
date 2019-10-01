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
      Defaults
    </src-comment>
  </active*>

  <assign|frame-picture|tmfs://artwork/pictures/frames/carved-wood-frame.jpg>

  <assign|frame-effect|<eff-make-transparent|0|white>>

  <assign|frame-lcrop|0.1056751>

  <assign|frame-rcrop|0.8943249>

  <assign|frame-bcrop|0.1324405>

  <assign|frame-tcrop|0.8650794>

  <assign|frame-lborder|1tab>

  <assign|frame-rborder|1tab>

  <assign|frame-bborder|1tab>

  <assign|frame-tborder|1tab>

  <assign|frame-lpadding|1tab>

  <assign|frame-rpadding|1tab>

  <assign|frame-bpadding|1tab>

  <assign|frame-tpadding|1tab>

  <\active*>
    <\src-comment>
      Defaults
    </src-comment>
  </active*>

  <assign|art-frame|<macro|body|<art-box|<arg|body>|<tuple|frame|<value|frame-picture>|effect|<value|frame-effect>|lcrop|<value|frame-lcrop>|rcrop|<value|frame-rcrop>|bcrop|<value|frame-bcrop>|tcrop|<value|frame-tcrop>|lwidth|<value|frame-lborder>|rwidth|<value|frame-rborder>|bheight|<value|frame-bborder>|theight|<value|frame-tborder>>|<tuple|text|normal|lpadding|<plus|<value|frame-lborder>|<value|frame-lpadding>>|rpadding|<plus|<value|frame-rborder>|<value|frame-rpadding>>|bpadding|<plus|<value|frame-bborder>|<value|frame-bpadding>>|tpadding|<plus|<value|frame-tborder>|<value|frame-tpadding>>>>>>

  <\active*>
    <\src-comment>
      Frame theme parameters
    </src-comment>
  </active*>

  <new-theme|frame|frame-picture|frame-effect|frame-lcrop|frame-rcrop|frame-bcrop|frame-tcrop|frame-lborder|frame-rborder|frame-bborder|frame-tborder|frame-lpadding|frame-rpadding|frame-bpadding|frame-tpadding>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>