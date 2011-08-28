<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Eine Anwendung als \Rdynamic library'' einbinden.>

  Die Kommunikation zwischen <TeXmacs> und einer Anwendung kann auÿer über
  eine \RPipeline`` auch dadurch erfolgen, dass die Anwendung als
  \Rdynamically linked library'', DLL, eingebunden wird. \ Pipelines sind
  meist leichter zu implementieren, oft sehr robust und flexibel in Bezug auf
  Daten, die nur nach und nach erzeugt werden. Aber DLLs sind schneller.

  <section|Kommunikation über DLLs.>

  Die folgenden Schritte müssen getan werden, um eine Anwendung als DLL in
  <TeXmacs> \ einzubinden:

  <\enumerate>
    <item>Die Anwendung Muÿe so umgestaltet werden, dass das Hauptprogramm
    als DLL eingebunden werden kann. Damit erhalten Sie ein sehr kleines
    Binär-Programm, das die wörtliche Ein- und Ausgabe steuert und das erst
    zur Laufzeit mit den DLLs der Anwendung gelinkt wird.

    <item>Kopieren die Datei \RTeXmacs.h`` aus dem <TeXmacs>-Unterverzeichnis
    <verbatim|include> in das Include-Verzeichnis Ihrer Anwendung und
    schreiben Sie die laut <TeXmacs>-Kommunikations-Protokoll benötigten
    Eingabe/Ausgabe-Routinen wie weiter unten beschrieben:\ 

    <item>Schreiben Sie eine Zeile der Form

    <\verbatim>
      \ \ \ \ (package-declare "myplugin" "libmyplugin.so" "get_name_package"
      "init")
    </verbatim>

    in die Datei <verbatim|init-myplugin.scm>, die für den Fall der
    Kommunikation über Pipelines bereits beschrieben wurde. Hier ist
    <verbatim|libmyplugin.so> die entsprechende DLL,
    <verbatim|get_name_package> die Funktion, die <TeXmacs> aufruft, um Ihre
    Anwendung mit <TeXmacs> zu linken und <verbatim|init> eine
    Initialisierungs-Zeichenkette, die Ihre Anwendung vielleicht benötigt.

    <item>Weiter geht es analog zum Pipeline-Fall.
  </enumerate>

  <section|Das <TeXmacs> Kommunikations-Protokoll.>

  Das <TeXmacs> Kommunikations-Protokoll dient dazu, DLLs dynamisch in
  <TeXmacs> einzubinden. Die Datei <verbatim|include/TeXmacs.h> enthält die
  Definition aller Daten-Strukturen und Funktionen, die von dem Protokoll
  benutzt werden. In Zukunft erwarten wir eine Folge von unterschiedlichen
  Protokollen, die alle die abstrakten Daten-Strukturen
  <verbatim|TeXmacs_exports> und <verbatim|package_exports> gemein haben, in
  denen die Informationen über die Protokoll-, <TeXmacs>- und
  Anwendungs-Version, enthalten ist.

  Die <with|mode|math|n>-te konkrete Version des Kommunikations-Protokolls
  sollte zwei Datenstrukturen <verbatim|TeXmacs_exports_n> und
  <verbatim|package_exports_n> bereitstellen. Die erste davon enthält alle
  Routinen und Daten von <TeXmacs>, die in der Anwendung benötigt werden
  könnten. Die zweite enthält alle \ Routinen und Daten der Anwendung, die
  innerhalb <TeXmacs> gebraucht werden könnten.

  Um die Anwendung mit <TeXmacs> zu linken, müssen Sie die Funktion

  <\verbatim>
    \ \ \ \ package_exports* get_my_package (int version);
  </verbatim>

  implementieren. Die Funktion übernimmt das oberste <TeXmacs>
  Kommunikations-Protokoll, das von Ihrer <TeXmacs>-Version unterstützt wird.
  Es sollte einen Zeiger zurückgeben, der auf eine Instanz einer konkreten
  Struktur <verbatim|package_exports_n> zeigt mit n kleiner oder gleich
  \R<verbatim|version>''.

  <section|Version 1 des <TeXmacs> Kommunikations-Protokolls.>

  In der ersten Version des <TeXmacs> Kommunikations-Protokolls sollte Ihre
  Anwendung eine Instanz der folgenden Daten-Struktur exportieren:

  <\verbatim>
    \ \ \ \ typedef struct package_exports_1 {<next-line> \ \ \ \ \ char*
    version_protocol; /* "TeXmacs communication protocol 1" */<next-line>
    \ \ \ \ \ char* version_package;<next-line> \ \ \ \ \ char* (*install)
    (TeXmacs_exports_1* TM, char* options, char** errors);<next-line>
    \ \ \ \ \ char* (*evaluate) (char* what, char* session, char**
    errors);<next-line> \ \ \ \ \ char* (*execute) (char* what, char*
    session, char** errors);<next-line> \ \ \ } package_exports_1;
  </verbatim>

  Die Zeichenkette <verbatim|version_protocol> sollte <verbatim|\RTeXmacs
  communication protocol 1"> und die Zeichenkette <verbatim|version_package>
  die Version Ihrer Anwendung enthalten.

  Die Funktion <verbatim|install> wird einmal von <TeXmacs> mit den Optionen
  <verbatim|options> aufgerufen, um Ihre Anwendung zu initialisieren. Es
  überträgt die Routinen, die von <TeXmacs> exportiert werden, als
  <verbatim|TM>. Diese Routine sollte, sofern erfolgreich, eine Statusmeldung
  zurückgeben, z.B.:

  <\verbatim>
    \ \ \ \ "Ihre CAS-Version wurde erfolgreich in TeXmacs eingebunden"
  </verbatim>

  Wenn die Installation ohne Erfolg blieb, sollten Sie <verbatim|NULL>
  zurückgeben und <verbatim|*errors> sollte eine erklärende Meldung
  enthalten. Sowohl <verbatim|what> als auch die zurückgegebene Zeichenkette
  haben ein spezielles Format, in dem beliebige <TeXmacs>-Dokumente kodiert
  werden können. Dieses Format wird im nächsten Abschnitt beschrieben.

  Die <verbatim|evaluate>-Funktion dient zur Evaluierung von <verbatim|what>
  in einer <TeXmacs>-Sitzung mit dem Namen <verbatim|session>. Sie sollte das
  Ergebnis der Evaluierung von <verbatim|what> zurückgeben, falls
  erfolgreich, oder <verbatim|NULL>, wenn ein Fehler auftrat.
  <verbatim|*errors> kann, je nach Verlauf der Evaluierung, eine oder mehrere
  Warnungen enthalten oder eine Fehlermeldung, wenn die Evaluierung versagte.
  Der Befehl

  <\verbatim>
    \ \ \ \ (package-format "yourcas" "input-format" "output-format")
  </verbatim>

  wird analog zur Verwendung von Pipelines zur Spezifikation der Ein- und
  Ausgabe-Formate benutzt.

  Die Routine <verbatim|execute> ähnelt <verbatim|evaluate> dient aber nicht
  zur Evaluierung innerhalb einer <TeXmacs>-Sitzung sondern zur Kommunikation
  zwischen <TeXmacs> und Ihrer Anwendung.

  <\remark>
    Alle Zeichenketten, die von den Routinen <verbatim|install>,
    <verbatim|evaluate> und <verbatim|execute>, sowie alle Warn- und
    Fehlermeldungen müssen Speicherplatz mit <verbatim|malloc> reservieren.
    Der Speicherplatz wird von <TeXmacs> mit <verbatim|free> wieder
    freigegeben..
  </remark>

  In der ersten Version des <TeXmacs> Kommunikations-Protokoll muss <TeXmacs>
  eine Instanz folgender Daten-Struktur exportieren:

  <\verbatim>
    \ \ \ \ typedef struct TeXmacs_exports_1 {<next-line> \ \ \ \ \ char*
    version_protocol; /* "TeXmacs communication protocol 1" */<next-line>
    \ \ \ \ \ char* version_TeXmacs;<next-line> \ \ \ } TeXmacs_exports_1;
  </verbatim>

  Die Zeichenkette <verbatim|version_protocol> enthält <verbatim|"TeXmacs
  communication protocol 1"> und <verbatim|version_TeXmacs> die vorliegende
  Version von <TeXmacs>.

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