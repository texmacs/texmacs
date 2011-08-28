<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Sitzungen editieren>

  Innerhalb der Eingabefelder von Sitzungen haben die Cursor-Tasten eine
  besondere Bedeutung. Die Tasten <key|up> und
  <key|down> führen zu den vorgehenden bzw.
  nachfolgenden Feldern. Die Tasten <key|left> und
  <key|right> verlassen nie das Eingabefeld. Dazu
  sollten Sie die Maus benutzen.

  Die Menüs <menu|Session|Insert fields> und <menu|Session|Remove fields>,
  die zugänglich werden, wenn Sie sich in einer Sitzung befinden, stellen
  einige Optionen zum Editieren bereit. \ Die meisten Operationen wirken auf
  zugehörige Ein- bzw. Ausgabe-Felder. Mit dem Befehl <menu|Session|Insert
  fields|Insert text field> kann zu einem Eingabefeld ein erklärendes
  Textfeld eingefügt werden. Kurzbefehle zum Einfügen von Eingabefeldern sind
  <shortcut|(structured-insert-up)>, ( <shortcut|(structured-insert-up)>,
  <menu|Session|Insert fields|Insert field above>) und <shortcut|(structured-insert-down)>, (
  <shortcut|(structured-insert-down)>, <menu|Session|Insert fields|Insert
  field below>). Mit dem Kurzbefehl <shortcut|(structured-remove-right)>, ( <key|Entf>,
  <menu|Session|Remove fields|Remove inputfield> ) \ wird das aktuelle
  Eingabefeld entfernt. <shortcut|(structured-remove-left)>, (
  <key|A-<with|mode|math|<with|mode|math|\<longleftarrow\>>>>,
  \ <menu|Session|Remove fields|Remove inputfield above> ) entfernt das
  vorgehende Eingabefeld. Mit <menu|Session|Remove fields|Remove all
  outputfields> werden alle Ausgabefelder entfernt.

  Mit <menu|Session|Insert fields|Fold input field> oder <shortcut|(structured-insert-right)>
  ( <key|right> ) können Sie eine \RUnter-Sitzung''
  starten. Die aktuellen Eingabe-, Ausgabe- und Text-Felder werden zum Rumpf
  einer neuen nicht verborgenen \ \RUnter-Sitzung'' . Diese bestehen aus
  einem erklärenden Text und einer Folge von Text-, Eingabe- und
  Ausgabe-Feldern. Man kann verborgene Unter-Sitzungen mit
  \ <key|M-A-up> sichtbar machen und mit
  <key|M-A-down> verbergen. Unter-Sitzungen werden
  besonders gut dargestellt, wenn man das <tmpackage|varsession>-Paket
  benutzt: <menu|Document|Add package|Program|Varsession>.

  Nützlich ist auch der bereits erwähnte Befehl <menu|Session|Remove
  fields|Remove all output fields> und zwar besonders für
  Demonstrationszwecke, wenn man eine Sitzung erzeugt, die erst später
  ausgeführt werden soll. Ein anderer nützlicher Befehl ist
  <menu|Session|Split session>, mit dem eine Sitzung geteilt werden kann.
  Meist wird das benötigt, um die einzelnen Teile in Veröffentlichungen
  einzufügen.

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