<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Tabellen erzeugen>

  Um Tabellen zu erzeugen kann man Befehle aus dem <menu|Insert|Table>
  verwenden. Dabei erzeugen <menu|Insert|Table|Small table> und
  <menu|Insert|Table|Big table> nur Umgebungen mit Beschriftung und Zählern
  für die eigenständige Tabellen. In diese müssen die eigentlichen Tabellen
  noch, wie im Folgenden beschrieben, eingefügt werden. Dazu dienen Befehle
  aus dem <menu|Insert|Table> oder die folgenden Kurzbefehle. Wenn der Cursor
  sich in einer solchen Tabelle befindet, sind Sie im Tabellen-Modus. Sofern
  Sie <menu|Edit|Preferences|View|Context dependent icons> aktiviert haben,
  sehen Sie eine gröÿere Anzahl von Icons, die Ihnen Zugang zu den vielen
  Tabellen-Optionen verschaffen.

  <underline|Befehle zur Erzeugung von Tabellen:>

  <\description>
    <item*|<key|table N t>>Eine normale Tabelle erzeugen. Zellen sind
    linksbündig. Keine sichtbaren Ränder. Menü-Befehl:
    <menu|Insert|Table|Plain tabular>.

    <item*|<key|table N T>>Eine normale Tabelle mit zentrierten Zellen.
    Menü-Befehl: <menu|Insert|Table|Centered tabular>.

    <item*|<key|table N b>>Eine Tabelle mit sichtbaren Umrandungen. Zellen
    linksbündig. Menü-Befehl: <menu|Insert|Table|Plain block>.

    <item*|<key|table N B>>Eine Tabelle mit sichtbaren Umrandungen. Zellen
    zentriert. Menü-Befehl: <menu|Insert|Table|Centered block>.
  </description>

  <underline|Im Mathematik-Modus, in Kontexten für eigenständige Formeln,
  können noch einige weitere tabellenartige Strukturen erzeugt werden:>

  <\description>
    <item*|<key|table N m>>Erzeuge eine Matrix:
    <with|mode|math|<matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>>\ 

    <item*|<key|table N d>>Erzeuge eine Determinante:
    <with|mode|math|<det|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>>

    <item*|<key|table N c>>Erzeuge eine Auswahlliste:
    <with|mode|math|<choice|<tformat|<table|<row|<cell|x\<geqslant\> 0, f=
    0>>|<row|<cell|x=0, \ f= 1>>|<row|<cell|x\<leqslant\>0, f = 0>>>>>> \ 
  </description>

  Der Kontext <markup|eqnarray*> ist eine spezielle tabellenartige Struktur,
  die sich über eine ganze Zeile erstreckt. Man erzeugt diese mit dem
  Menübefehl <menu|Insert|Mathematics|Equations>.

  Wenn man eine neue Tabelle erzeugt hat, hat dies die Minimal-Gröÿe und
  enthält normalerweise nur eine einzige Zelle, die leer ist. Neue Zeilen und
  Spalten können mit den Kurzbefehlen <shortcut|(structured-insert-left)>,
  <shortcut|(structured-insert-right)>, <shortcut|(structured-insert-up)> und <shortcut|(structured-insert-down)> erzeugt
  werden. Beispielsweise erzeugt <shortcut|(structured-insert-right)> eine neue Spalte rechts
  von der Cursorposition. Neue Zeilen unterhalb kann man auch mit
  <shortcut|(kbd-return)> erzeugen. Man verlässt die Tabellen mit den Pfeiltasten
  oder der Maus.

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