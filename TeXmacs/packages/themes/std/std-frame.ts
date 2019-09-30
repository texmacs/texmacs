<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|std-frame|1.0|std-frame|1.0>

    <\src-purpose>
      Standard artistic frames
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|base-frame>

  <\active*>
    <\src-comment>
      Carved wood
    </src-comment>
  </active*>

  <copy-theme|carved-wood|frame>

  <assign|carved-wood-frame-picture|$TEXMACS_PATH/misc/pictures/frames/thumbnail-carved-wood-frame.jpg>

  <assign|carved-wood-frame-effect|<eff-make-transparent|0|white>>

  <assign|carved-wood-frame-lcrop|0.1056751>

  <assign|carved-wood-frame-rcrop|0.8943249>

  <assign|carved-wood-frame-bcrop|0.1324405>

  <assign|carved-wood-frame-tcrop|0.8650794>

  <assign|carved-wood-frame|<macro|body|<with-carved-wood|<art-frame|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Decorated wood
    </src-comment>
  </active*>

  <copy-theme|decorated-wood|frame>

  <assign|decorated-wood-frame-picture|$TEXMACS_PATH/misc/pictures/frames/thumbnail-decorated-wood-frame.jpg>

  <assign|decorated-wood-frame-effect|<eff-make-transparent|0|white>>

  <assign|decorated-wood-frame-lcrop|0.1401443>

  <assign|decorated-wood-frame-rcrop|0.8575769>

  <assign|decorated-wood-frame-bcrop|0.1866029>

  <assign|decorated-wood-frame-tcrop|0.8149081>

  <assign|decorated-wood-frame|<macro|body|<with-decorated-wood|<art-frame|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Floral 1
    </src-comment>
  </active*>

  <copy-theme|black-floral1|frame>

  <assign|black-floral1-frame-picture|$TEXMACS_PATH/misc/pictures/frames/thumbnail-black-floral1-frame.png>

  <assign|black-floral1-frame-effect|<eff-make-transparent|0|white>>

  <assign|black-floral1-frame-lcrop|0.3333333>

  <assign|black-floral1-frame-rcrop|0.6666666>

  <assign|black-floral1-frame-bcrop|0.3333333>

  <assign|black-floral1-frame-tcrop|0.6666666>

  <assign|black-floral1-frame|<macro|body|<with-black-floral1|<art-frame|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Floral 2
    </src-comment>
  </active*>

  <copy-theme|black-floral2|frame>

  <assign|black-floral2-frame-picture|$TEXMACS_PATH/misc/pictures/frames/thumbnail-black-floral2-frame.png>

  <assign|black-floral2-frame-effect|<eff-make-transparent|0|white>>

  <assign|black-floral2-frame-lcrop|0.3333333>

  <assign|black-floral2-frame-rcrop|0.6666666>

  <assign|black-floral2-frame-bcrop|0.3333333>

  <assign|black-floral2-frame-tcrop|0.6666666>

  <assign|black-floral2-frame|<macro|body|<with-black-floral2|<art-frame|<arg|body>>>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>