<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard-Konstrukte für die Schnittstelle zu Fremdprogrammen>

  Die <tmdtd|session> <abbr|D.T.D.> erzeugt Kontexte für die Benutzung von
  <TeXmacs> als interaktive Oberfläche für andere Programme, Computer Algebra
  (CAS): Sitzung, session.\ 

  <\explain|<explain-macro|session|body>>
    Erzeugt den Kontext für eine Sitzung.\ 
  </explain>

  Alle folgenden Makros sind nur innerhalb eines Sitzungs-Kontextes zulässig:

  <\explain|<explain-macro|input|prompt|body>>
    Ein Eingabefeld mit einer Eingabeaufforderung <src-arg|prompt> und der
    wirklichen Eingabe <with|color|brown|<em|body>>.
  </explain>

  <\explain|<explain-macro|output|body>>
    Ein Ausgabefeld.
  </explain>

  <\explain|<explain-macro|textput|body>>
    Feld mit normalem Text, als Kommentar oder Erläuterung geeignet.
  </explain>

  <\explain|<explain-macro|errput|body>>
    Dieses Makro wird innerhalb eines Ausgabefeldes benutzt, um
    Fehlermeldungen auszugeben.
  </explain>

  Diese Konstrukte basieren auf <markup|<em|lan>-session>,
  <markup|<em|lan>-input>, <markup|<em|lan>-output>,
  <markup|<em|lan>-textput> und <markup|<em|lan>-errput> für jede
  Programm-Sprache <markup|<em|lan>>.

  Wenn sprach-spezifische Konstrukte nicht existieren, werden die
  <TeXmacs>-Vorgeben <markup|generic-session>, <markup|generic-input>,
  <markup|generic-output>, <markup|generic-textput> und
  <markup|generic-errput> an ihrer Stelle benutzt. Wir empfehlen,
  sprach-spezifische Konstrukte auf Basis dieser Konstrukte zu
  implementieren. Sie können je nach \ Stil (<tmstyle|varsession> Paket in
  \ <menu|Document|Add package|Session>) unterschiedlich implementiert sein.
  Dafür dient das Konstrukt <markup|generic-output*>, \ das
  <markup|generic-output> entspricht, bei dem aber die Ränder aber
  unverändert erhalten bleiben.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|german>
  </collection>
</initial>