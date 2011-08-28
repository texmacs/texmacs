<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Titel und Zusammenfassungen einfügen>
  
   Die <tmdtd|header-title>
  <abbr|D.T.D.> stellt Befehle für Titel bereit. Die folgenden Befehle können
  nur innerhalb eines <markup|make-title>-Kontexts benutzt werden:

  <\explain|<explain-macro|title|title>>
    Spezifiziert den <src-arg|title> eines Dokuments.
  </explain>

  <\explain|<explain-macro|author|author>>
    Spezifiziert einen oder mehrere Autoren.
  </explain>

  <\explain|<explain-macro|author|address>>
    Spezifiziert die Adresse des Autors.
  </explain>

  <\explain|<explain-macro|author-block|address>>
    Spezifiziert die Adresse(n) eines Autors mit mehreren Adressen.
  </explain>

  <\explain|<explain-macro|title-email|email>>
    Spezifiziert die Email-Adresse eines Autors.
  </explain>

  <\explain|<explain-macro|title-date|email>>
    Spezifiziert das Erstellungsdatum des Dokuments, oft heute
    <inactive*|<date>>.
  </explain>

  <markup|title> und <markup|author> benutzen die Befehle
  <markup|header-title> und <markup|header-author>, um die fortlaufenden
  Titel und Kopfzeilen zu spezifizieren. Sie können das ändern, indem Sie
  <markup|header-title> <abbr|bzw.> <markup|header-author> umdefinieren.

  Die <tmdtd|header-title> <abbr|D.T.D.> definiert auch den Befehl
  <markup|abstract> für Zusammenfassungen. Innerhalb von Zusammenfassungen
  (abstracts) können Sie den Befehl <markup|keywords> \ benutzen, um
  Schlüsselworte für Ihre Veröffentlichung zu definieren sowie den Befehl
  <markup|AMS-class>, um eine \R<abbr|A.M.S.>-subject-classification''
  durchzuführen.

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
    <associate|preamble|false>
  </collection>
</initial>