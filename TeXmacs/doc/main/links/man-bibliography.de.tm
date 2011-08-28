<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Literaturverzeichnis erstellen>

  Im Moment benutzt <TeXmacs> <verbatim|bibtex>, um Literaturverzeichnisse zu
  erstellen. Deshalb sind folgende Schritte dazu notwendig:

  <\itemize>
    <item>Zunächst muss eine <verbatim|.bib>-Datei mit allen erforderlichen
    Zitaten erstellen. Diese Datei sollte das Standard <LaTeX>-Zitate-Format
    haben.\ 

    <item>Mit Befehlen aus dem Menü \ <menu|Insert|Link|Citation> fügen Sie
    dann ein Zitat mit einem Label, ein das einer Referenz in der
    \ <verbatim|.bib>-Datei entspricht und aktivieren mit der
    <key|enter>-Taste.\ 

    <item>An der Stelle, an der das Literaturverzeichnis eingefügt werden
    soll, klicken Sie auf \ <menu|Insert|Automatic-generated-lists|Bibliography>.
    An der Eingabeaufforderung geben Sie einen <verbatim|bibtex>-Stil ein und
    ihre <verbatim|.bib>-Datei. Es gibt sehr viele <verbatim|bibtex>-Stile.
    Standard-Stile sind z.B.: <strong|plain> (Die Einträge sind alphabetisch
    sortiert und mit Zahlen gekennzeichnet), <strong|unsr>t (wie plain, nur
    erscheinen die Einträge in der Reihenfolge, in der sie erstmalig im Text
    auftreten), <strong|alpha> (wie plain, nur wird mit einem
    Buchstaben-Nummern-Code gekennzeichnet, der aus den Autoren-Namen und dem
    Jahr der Publikation gebildet wird), <strong|abbrv> ( wie plain, die
    Vornamen der Autoren, Monate, Name der Zeitschrift usw. werden
    abgekürzt.).

    <item>Benutzen Sie <menu|Document|Update|Bibliography>, um das
    Literaturverzeichnis zu erstellen. Wiederholen Sie bitte den Vorgang
    mehrmals, bis sich nichts mehr ändert.\ 
  </itemize>

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