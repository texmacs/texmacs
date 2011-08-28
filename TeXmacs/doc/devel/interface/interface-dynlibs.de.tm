<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Dynamisch ladbare Bibliotheken>

  Die Kommunikation zwischen <TeXmacs> und einer Anwendung kann außer über
  eine \RPipeline`` auch dadurch erfolgen, dass die Anwendung als
  \Rdynamically linked library'', DLL, eingebunden wird. \ Pipelines sind
  meist leichter zu implementieren, oft sehr robust und flexibel in Bezug auf
  Daten, die nur nach und nach erzeugt werden. Aber DLLs sind schneller.

  Um eine Anwendung dynamisch mit <TeXmacs> zu linken, sollten Sie dem
  <TeXmacs> Kommunikations-Protokoll folgen, das in folgenden Datei

  <\verbatim>
    \ \ <hlink|include/TeXmacs.h|../../../../../include/TeXmacs.h> \ 
  </verbatim>

  definiert ist.

  Darin ist spezifiziert, dass die Anwendung folgende Datenstruktur
  exportieren soll:

  <\cpp-fragment>
    typedef struct package_exports_1 {

    \ \ char* version_protocol; /* "TeXmacs communication protocol 1" */

    \ \ char* version_package;

    \ \ char* (*install) (TeXmacs_exports_1* TeXmacs,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ char* options, char** errors);

    \ \ char* (*evaluate) (char* what, char* session, char** errors);

    } package_exports_1;
  </cpp-fragment>

  Sie enthält sowohl eine Installationsroutine für die Anwendung als auch
  eine Evaluierungsroutinen für zusätzliche Eingabe. (Mehr Informationen
  finden Sie in der Datei \ <hlink|include/TeXmacs.h|../../../../../include/TeXmacs.h>).
  <TeXmacs> exportiert folgende Struktur:

  <\cpp-fragment>
    typedef struct TeXmacs_exports_1 {

    \ \ char* version_protocol; /* "TeXmacs communication protocol 1" */

    \ \ char* version_TeXmacs;

    } TeXmacs_exports_1;
  </cpp-fragment>

  Jedes Programm hat für sein Speicher-Management selbst zu sorgen.
  Zeichenketten, die \ <TeXmacs> erzeugt, müssen von <TeXmacs> gelöscht
  werden. Das gilt entsprechend für die Anwendung.

  Die Zeichenkette <verbatim|version_protocol> sollte <verbatim|\RTeXmacs
  communication protocol 1"> und die Zeichenkette <verbatim|version_package>
  die Version Ihrer Anwendung enthalten.

  Die Funktion <verbatim|install> wird ein einziges Mal von <TeXmacs> mit den
  Optionen <verbatim|options> aufgerufen, um Ihre Anwendung zu
  initialisieren. Es überträgt die Routinen, die von <TeXmacs> exportiert
  werden, als <verbatim|TM>. Diese Routine sollte, sofern erfolgreich, eine
  Statusmeldung zurückgeben, z.B.:

  <\verbatim>
    \ \ \ \ "Ihre CAS-Version wurde erfolgreich in TeXmacs eingebunden"
  </verbatim>

  Wenn die Installation ohne Erfolg blieb, sollten Sie <verbatim|NULL>
  zurückgeben und <verbatim|*errors> sollte eine erklärende Meldung
  enthalten.\ 

  Die <verbatim|evaluate>-Funktion dient zur Evaluierung von <verbatim|what>
  in einer <TeXmacs>-Sitzung mit dem Namen <verbatim|session>. Sie sollte das
  Ergebnis der Evaluierung von <verbatim|what> zurückgeben, falls
  erfolgreich, oder <verbatim|NULL>, wenn ein Fehler auftrat.
  <verbatim|*errors> kann, je nach Verlauf der Evaluierung, eine oder mehrere
  Warnungen enthalten oder eine Fehlermeldung, wenn die Evaluierung versagte.
  Die Formate sind die gleichen wie im Fall der Kommunikation über Pipelines.

  Schlieÿlich sollte die Konfigurations-Datei des Plugins noch so etwas wie
  das folgende enthalten:

  <\scheme-fragment>
    (plugin-configure <em|myplugin>

    \ \ (:require (url-exists? (url "$LD_LIBRARY_PATH"
    "lib<em|myplugin>.so")))

    \ \ (:link "lib<em|myplugin>.so" "<em|myplugin>_exports" "")

    \ \ <em|further-configuration>)
  </scheme-fragment>

  Hier ist <verbatim|<em|myplugin>_exports> ein Zeiger auf eine Struktur vom
  Typ \ <cpp-code|package_exports_1>.

  <\remark>
    Es ist möglich, dass das Kommunikations-Protokoll sich ändert. In diesem
    Fall werden die Daten-Strukturen <verbatim|TeXmacs_exports_1> und
    <verbatim|package_exports_1> durch <verbatim|TeXmacs_exports_n> und
    <verbatim|package_exports_n> ersetzt, worin n die Version des Protokolls
    ist. Diese Strukturen werden immer die abstrakten Strukturen
    <verbatim|TeXmacs_exports> und <verbatim|package_exports> behalten, in
    denen die Informationen über die Protokoll-, <TeXmacs>- und
    Anwendungs-Version, enthalten sind.
  </remark>

  <paragraph*|Das <verbatim|dynlink> plugin>

  Das Beispiel Plugin <verbatim|dynlink> zeigt, wie dynamisch ladbare
  Bibliotheken geschrieben und benutzt werden. Es besteht aus den Dateien:

  <\verbatim>
    \ \ \ \ <example-plugin-link|dynlink/Makefile>

    \ \ \ \ <example-plugin-link|dynlink/progs/init-dynlink.scm>

    \ \ \ \ <example-plugin-link|dynlink/src/dynlink.cpp>
  </verbatim>

  Das <verbatim|Makefile> enthält die Zeilen

  <\quotation>
    <\framed-fragment>
      <\with|par-par-sep|0fn>
        <\with|font-family|tt>
          tmsrc = /home/vdhoeven/texmacs/src/TeXmacs

          CXX = g++

          LD \ = g++

          \;

          lib/libtmdynlink.so: src/dynlink.cpp

          \ \ \ \ \ \ \ \ $(CXX) -I$(tmsrc)/include -c src/dynlink.cpp -o
          src/dynlink.o

          \ \ \ \ \ \ \ \ $(LD) -shared -o lib/libtmdynlink.so src/dynlink.o
        </with>
      </with>
    </framed-fragment>
  </quotation>

  so dass <verbatim|dynlink.cpp> zu einer dynamisch linkbaren Datei (DLL)
  <verbatim|dynlink/lib/libdynlink.so> kompiliert wird. Die <verbatim|tmsrc>
  Variable sollte so gesetzt sein, dass die include-Datei
  <verbatim|TeXmacs.h> gefunden wird. Die Konfigurations-Datei
  <verbatim|init-dynlink.scm> enthält die folgenden Zeilen

  <\scheme-fragment>
    (plugin-configure dynlink

    \ \ (:require (url-exists? (url "$LD_LIBRARY_PATH"

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "libtmdynlink.so")))

    \ \ (:link "libtmdynlink.so" "dynlink_exports" "")

    \ \ (:session "Dynlink"))
  </scheme-fragment>

  Die <value|cpp> Datei <verbatim|dynlink.cpp> enthält die Zeichenkette

  <\cpp-fragment>
    static char* output= NULL;
  </cpp-fragment>

  mit der letzten Ausgabe, die Initialisierungs-Routine

  <\cpp-fragment>
    char*

    dynlink_install (TeXmacs_exports_1* TM, char* opts, char** errs) {

    \ \ output= (char*) malloc (50);

    \ \ strcpy (output, "\\2verbatim:Started dynamic link\\5");

    \ \ return output;

    }
  </cpp-fragment>

  die Evaluierungs-Routine

  <\cpp-fragment>
    char*

    dynlink_eval (char* what, char* session, char** errors) {

    \ \ free (output);

    \ \ output= (char*) malloc (50 + strlen (what));

    \ \ strcpy (output, "\\2verbatim:You typed ");

    \ \ strcat (output, what);

    \ \ strcat (output, "\\5");

    \ \ return output;

    }
  </cpp-fragment>

  und die Daten-Struktur den Exporten

  <\cpp-fragment>
    package_exports_1 dynlink_exports= {

    \ \ "TeXmacs communication protocol 1",

    \ \ "Dynlink 1",

    \ \ dynlink_install,

    \ \ dynlink_eval

    };
  </cpp-fragment>

  Beachten Sie bitte, dass bei der Ausgabe die Anwendung für die Reservierung
  und die Freigabe von Speicher zu sorgen hat.

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