<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Ausgabe-Kanäle, Eingabe-Aufforderungen und Eingabe-Vorgaben>

  Neben Blöcken der Form

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN><em|format>:<em|message><render-key|DATA_END>>>
  </quotation>

  erlaubt das <TeXmacs>-Meta-Format auch noch Blöcke der Form

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN><em|channel>#<em|message><render-key|DATA_END>>>
  </quotation>

  Hier spezifiziert <verbatim|<em|channel>> einen \RAusgabe-Kanal'', an den
  der Rumpf, \ <verbatim|<em|message>>, gerichtet ist. der
  Standard-Ausgabe-Kanal ist <verbatim|output>. es gibt aber noch die Kanäle
  <verbatim|prompt> und <verbatim|input>, um die Eingabe-Aufforderung und
  eine Vorgabe für die nächst folgende Eingabe zu erzeugen. Für die Zukunft
  ist auch die Unterstützung von Fehler- und Status-Kanälen
  (<verbatim|error>, <verbatim|status>) geplant. Die Voreinstellung von
  Eingaben kann vor allem für Beispiele in Computer Algebra Systemen sein.\ 

  <paragraph*|Das <verbatim|prompt> plugin>

  das Beispiel-Plugin <verbatim|prompt> zeigt, wie man Eingabe-Aufforderungen
  verwenden kann. Es besteht aus den Dateien:

  <\verbatim>
    \ \ \ \ <example-plugin-link|prompt/Makefile>

    \ \ \ \ <example-plugin-link|prompt/progs/init-prompt.scm>

    \ \ \ \ <example-plugin-link|prompt/src/prompt.cpp>
  </verbatim>

  Die Methode, die die nächste Eingabe-Aufforderung erzeugt ist diese

  <\cpp-fragment>
    void

    next_input () {

    \ \ counter++;

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "prompt#";

    \ \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </cpp-fragment>

  Die folgende Methode dient zur Darstellung eines Begrüÿungstextes und einer
  Eingabe-Aufforderung:

  <\cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "A LaTeX -\<gtr\> TeXmacs converter";

    next_input ();

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  mit den folgenden Zeilen in der Hauptschleife

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> DATA_BEGIN;

    cout \<less\>\<less\> "latex:$" \<less\>\<less\> buffer \<less\>\<less\>
    "$";

    cout \<less\>\<less\> DATA_END;

    next_input ();

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

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