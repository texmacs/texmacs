<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|std-shadow|1.0|std-shadow|1.0>

    <\src-purpose>
      Standard artistic shadows
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|base-shadow>

  <\active*>
    <\src-comment>
      Rough shadow. FIXME: these low resolution shadows are not well exported
      to Pdf
    </src-comment>
  </active*>

  <copy-theme|rough-shadow|shadow>

  <assign|rough-shadow-shadow-picture|$TEXMACS_PATH/misc/pictures/shadows/thumbnail-drop-shadow.png>

  <assign|rough-shadow|<macro|body|<with-rough-shadow|<art-shadow|<arg|body>>>>>

  \;

  <\active*>
    <\src-comment>
      Drop contours
    </src-comment>
  </active*>

  <copy-theme|drop-contour|shadow>

  <assign|drop-contour|<macro|body|<with-drop-contour|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Bend-in contours
    </src-comment>
  </active*>

  <copy-theme|bend-in-contour|drop-contour>

  <assign|bend-in-contour-shadow-picture|tmfs://artwork/pictures/shadows/bend-in-shadow.png>

  <assign|bend-in-contour-shadow-lcrop|0.05>

  <assign|bend-in-contour-shadow-rcrop|0.95>

  <assign|bend-in-contour-shadow-bcrop|0.05>

  <assign|bend-in-contour-shadow-tcrop|0.95>

  <assign|bend-in-contour-shadow-lborder|0.4em>

  <assign|bend-in-contour-shadow-rborder|0.4em>

  <assign|bend-in-contour-shadow-bborder|0.4em>

  <assign|bend-in-contour-shadow-tborder|0.4em>

  <assign|bend-in-contour|<macro|body|<with-bend-in-contour|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Bend-out contours
    </src-comment>
  </active*>

  <copy-theme|bend-out-contour|bend-in-contour>

  <assign|bend-out-contour-shadow-picture|tmfs://artwork/pictures/shadows/bend-out-shadow.png>

  <assign|bend-out-contour|<macro|body|<with-bend-out-contour|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Diagonal bend-in contours
    </src-comment>
  </active*>

  <copy-theme|diagonal-bend-in-contour|bend-in-contour>

  <assign|diagonal-bend-in-contour-shadow-picture|tmfs://artwork/pictures/shadows/bend-diag-in-shadow.png>

  <assign|diagonal-bend-in-contour-shadow-lcrop|0.04>

  <assign|diagonal-bend-in-contour-shadow-rcrop|0.96>

  <assign|diagonal-bend-in-contour-shadow-bcrop|0.04>

  <assign|diagonal-bend-in-contour-shadow-tcrop|0.96>

  <assign|diagonal-bend-in-contour|<macro|body|<with-diagonal-bend-in-contour|<art-shadow|<arg|body>>>>>

  \;

  <\active*>
    <\src-comment>
      Drop shadows
    </src-comment>
  </active*>

  <copy-theme|drop-shadow|shadow>

  <assign|drop-shadow-shadow-picture|tmfs://artwork/pictures/shadows/drop-shadow.png>

  <assign|drop-shadow-shadow-align|south east>

  <assign|drop-shadow-shadow-format|..x ..x xxx>

  <assign|drop-shadow|<macro|body|<with-drop-shadow|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Bend-in shadows
    </src-comment>
  </active*>

  <copy-theme|bend-in-shadow|drop-shadow>

  <assign|bend-in-shadow-shadow-picture|tmfs://artwork/pictures/shadows/bend-in-shadow.png>

  <assign|bend-in-shadow-shadow-lcrop|0.05>

  <assign|bend-in-shadow-shadow-rcrop|0.95>

  <assign|bend-in-shadow-shadow-bcrop|0.05>

  <assign|bend-in-shadow-shadow-tcrop|0.95>

  <assign|bend-in-shadow-shadow-lborder|0.4em>

  <assign|bend-in-shadow-shadow-rborder|0.4em>

  <assign|bend-in-shadow-shadow-bborder|0.4em>

  <assign|bend-in-shadow-shadow-tborder|0.4em>

  <assign|bend-in-shadow|<macro|body|<with-bend-in-shadow|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Half bend-in shadows
    </src-comment>
  </active*>

  <copy-theme|half-bend-in-shadow|bend-in-shadow>

  <assign|half-bend-in-shadow-shadow-rcrop|0.96>

  <assign|half-bend-in-shadow-shadow-tcrop|0.5>

  <assign|half-bend-in-shadow-shadow-rborder|0.4em>

  <assign|half-bend-in-shadow-shadow-tborder|0em>

  <assign|half-bend-in-shadow|<macro|body|<with-half-bend-in-shadow|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Quarter bend-in shadows
    </src-comment>
  </active*>

  <copy-theme|quarter-bend-in-shadow|half-bend-in-shadow>

  <assign|quarter-bend-in-shadow-shadow-lcrop|0.5>

  <assign|quarter-bend-in-shadow-shadow-bcrop|0.04>

  <assign|quarter-bend-in-shadow-shadow-lborder|0em>

  <assign|quarter-bend-in-shadow-shadow-bborder|0.4em>

  <assign|quarter-bend-in-shadow|<macro|body|<with-quarter-bend-in-shadow|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Bend-out shadows
    </src-comment>
  </active*>

  <copy-theme|bend-out-shadow|bend-in-shadow>

  <assign|bend-out-shadow-shadow-picture|tmfs://artwork/pictures/shadows/bend-out-shadow.png>

  <assign|bend-out-shadow|<macro|body|<with-bend-out-shadow|<art-shadow|<arg|body>>>>>

  \;

  <\active*>
    <\src-comment>
      Drop down shadows
    </src-comment>
  </active*>

  <copy-theme|drop-down-shadow|drop-shadow>

  <assign|drop-down-shadow-shadow-align|south west east>

  <assign|drop-down-shadow-shadow-format|x.x x.x xxx>

  <assign|drop-down-shadow-shadow-lcrop|0.03>

  <assign|drop-down-shadow-shadow-rcrop|0.97>

  <assign|drop-down-shadow-shadow-lborder|0.3em>

  <assign|drop-down-shadow-shadow-rborder|0.3em>

  <assign|drop-down-shadow|<macro|body|<with-drop-down-shadow|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Bend down shadows
    </src-comment>
  </active*>

  <copy-theme|bend-down-shadow|drop-down-shadow>

  <assign|bend-down-shadow-shadow-picture|tmfs://artwork/pictures/shadows/bend-in-shadow.png>

  <assign|bend-down-shadow-shadow-bcrop|0.05>

  <assign|bend-down-shadow-shadow-tcrop|0.5>

  <assign|bend-down-shadow-shadow-tborder|0em>

  <assign|bend-down-shadow|<macro|body|<with-bend-down-shadow|<art-shadow|<arg|body>>>>>

  \;

  <\active*>
    <\src-comment>
      Classic shadows
    </src-comment>
  </active*>

  <copy-theme|classic-shadow|shadow>

  <assign|classic-shadow-shadow-picture|tmfs://artwork/pictures/shadows/drop-shadow.png>

  <assign|classic-shadow|<macro|body|<with-classic-shadow|<deco-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Rounded shadows
    </src-comment>
  </active*>

  <copy-theme|rounded-shadow|shadow>

  <assign|rounded-shadow-shadow-picture|tmfs://artwork/pictures/shadows/round-shadow.png>

  <assign|rounded-shadow|<macro|body|<with-rounded-shadow|<deco-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Angular shadows
    </src-comment>
  </active*>

  <copy-theme|angular-shadow|shadow>

  <assign|angular-shadow-shadow-picture|tmfs://artwork/pictures/shadows/angular-shadow.png>

  <assign|angular-shadow|<macro|body|<with-angular-shadow|<deco-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Angular shadows
    </src-comment>
  </active*>

  <copy-theme|cartoon-shadow|shadow>

  <assign|cartoon-shadow-shadow-picture|tmfs://artwork/pictures/shadows/cartoon-shadow.png>

  <assign|cartoon-shadow|<macro|body|<with-cartoon-shadow|<deco-shadow|<arg|body>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>