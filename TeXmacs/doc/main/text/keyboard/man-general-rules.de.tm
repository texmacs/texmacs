<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Allgemeine Regeln für Tastatur-Kurzbefehle>

  Weil es eine groÿe Anzahl von Tastatur-Kurzbefehlen in <TeXmacs> gibt,
  existieren allgemeine Klassifizierungs-Regeln, die das Erlernen und
  Behalten dieser Befehle erleichtern sollen. Generell beginnen
  Tastaturbefehle, die zu einer bestimmten Kategorie gehören, mit der
  gleichen Modifiziertaste. Die wichtigsten dieser Tasten sind die folgenden:\ 

  <\description>
    <item*|<prefix|C->>Dies ist die Steuerungstaste
    <key|<with|mode|math|Strg>> auf manchen (englischen) Tastaturen auch mit
    <key|<with|mode|math|Ctrl>> beschriftet. Kurzbefehle auf Basis der
    Steuerungstaste werden für häufig benutzte Editorbefehle benutzt. Es
    hängt von den Einstellungen im Menü <menu|Edit|Preferences|Look and feel>
    ab, welche Kurzbefehle in einzelnen zur Verfügung stehen. Wenn Sie
    beispielsweise <em|Emacs> gewählt haben, dann entsprechen Kurzbefehle der
    Form <prefix|C-> <name|Emacs>-Kurzbefehlen, z.B. <key|C-y>
    dem Befehl <em|Text einfügen>.

    <item*|<prefix|A->>Die <key|Alt>-Taste wird für Befehle
    verwendet, die davon abhängen, in welchem Modus Sie sich gerade befinden.
    Beispielsweise erzeugt <key|text s> <strong|fetten> Text im Textmodus
    aber das Quadratwurzel-Zeichen im Mathematik-Modus. Beachten Sie bitte,
    dass zweimaliges Tippen der <key|Esc>-Taste, also <key|escape
    escape> die gleiche Funktion wie <prefix|A-> hat.

    <item*|<prefix|M->>Die Meta-Taste wird für allgemeine
    <TeXmacs>-Befehle genutzt, die in allen möglichen Moden verwendet werden
    können. Beispielsweise erzeugt <shortcut|(make-label)> ein Kennzeichen (label). Diese
    Taste wird auch für zusätzliche Editierbefehle genutzt, wenn die
    Einstellungen in <menu|Edit|Preferences|Look and feel> dies vorsehen.
    Beachten Sie bitte, dass einmaliges Tippen der <key|Esc>-Taste, also
    <key|escape>, \ gleichbedeutend ist mit <prefix|M->.

    <item*|<prefix|M-A->>Diese Modifiziertaste wird zur
    Erzeugung spezieller Symbole, wie z.B. griechischer Buchstaben, benutzt.
    Sie können Ihre Tastatur so konfigurieren, dass die Hochstelltaste die
    Rolle der Hypertaste spielt (<menu|Edit|Preferences|Keyboard>). Die Taste
    <prefix|math:greek> ist der Taste \ <prefix|M-A-> äquivalent.
  </description>

  Es sei daran erinnert, dass die Modifiziertasten, die zur Erzeugung von
  <prefix|M-> und <prefix|M-A-> benutzt werden, im Menü
  <menu|Edit|Preferences|Keyboard> <hyper-link|konfiguriert|../../config/man-config-kbd-modkeys.en.tm>
  werden können.

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