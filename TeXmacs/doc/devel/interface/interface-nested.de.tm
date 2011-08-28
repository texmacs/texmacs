<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Formatierte und strukturierte Ausgabe>

  Im vorgängigen <hyper-link|Kapitel|interface-pipes.de.tm>, haben wir
  beschrieben, dass Ausgabe in Blöcke der Form

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN><em|format>:<em|message><render-key|DATA_END>>>
  </quotation>

  einzukapseln sind. Die Ausgabe <verbatim|<em|message>> kann rekursiv mehrer
  Blöcke der gleichen Form in einander verschachtelt enthalten. Als Formate
  sind z.Z. <verbatim|verbatim>, <verbatim|latex>, <verbatim|html>,
  <verbatim|ps>, <verbatim|scheme> implementiert. Das <verbatim|scheme>
  Format dient zur Übertragung von <TeXmacs>-Bäumen codiert als
  \ <value|scheme>-Ausdrücke.

  <paragraph*|Das <verbatim|formula> plugin>

  Das Plugin <verbatim|formula> zeigt, wie das <LaTeX>-Format als
  Ausgabe-Format verwendet wird. Es besteht aus den Dateien

  <\verbatim>
    \ \ \ \ <example-plugin-link|formula/Makefile>

    \ \ \ \ <example-plugin-link|formula/progs/init-formula.scm>

    \ \ \ \ <example-plugin-link|formula/src/formula.cpp>
  </verbatim>

  Der Rumpf der Hauptschleife in <verbatim|formula.cpp> besteht aus den
  folgenden Zeilen:

  <\cpp-fragment>
    int i, nr;

    cin \<gtr\>\<gtr\> nr;

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:";

    cout \<less\>\<less\> "$";

    for (i=1; i\<less\>nr; i++)

    \ \ cout \<less\>\<less\> "x_{" \<less\>\<less\> i \<less\>\<less\> "}+";

    cout \<less\>\<less\> "x_{" \<less\>\<less\> i \<less\>\<less\> "}$";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  In ähnlicher Weise wird mit dem Plugin <verbatim|nested> gezeigt, wie man
  mit verschachtelten Ausgabe-Blöcken arbeitet. Studieren Sie vor allem die
  Quellcode-Datei <example-plugin-link|nested/src/nested.cpp>.

  <\remark>
    Im Moment haben wir nur <LaTeX> als ein Standard-Übertragungsformat für
    mathematische Formeln definiert, denn es ist das am häufigsten benutzte
    Format. Für die Zukunft planen wir die Implementierung weiterer,
    semantisch sichererer Formate. Wir empfehlen, daran zu denken, dass das
    Baum-Format benutzt werden kann, um die Ausgabe zu übertragen.

    Zum Standard-<LaTeX>haben wir <verbatim|\\*> und <verbatim|\\bignone>
    Befehle für die Multiplikation und zum Abschluss von groÿen
    Operator-Symbolen. Damit kann unterschieden werden, zwischen

    <\verbatim>
      \ \ \ \ a \\* (b + c)
    </verbatim>

    (d.h. <with|mode|math|a> multipliziert mit <with|mode|math|b+c>)\ 

    und

    <\verbatim>
      \ \ \ \ f(x + y).
    </verbatim>

    (d.h <with|mode|math|f> angewendet auf <with|mode|math|x+y>).\ 

    Ganz ähnlich:

    <\verbatim>
      \ \ \ \ \\sum_{i=1}^m a_i \\bignone + \\sum_{j=1}^n b_j \\bignone
    </verbatim>

    Hier wird der <verbatim|\\bignone> Befehl benutzt, um das Kontext-Ende
    für den <verbatim|\\sum> Operator zu markieren.

    Es zeigt sich, dass der systematische Gebrauch von <verbatim|\\*> und
    <verbatim|\\bignone> in Kombination mit sauberer <LaTeX>-Ausgabe für die
    verbleibenden Konstrukte, es <with|font-shape|italic|a priori>
    ermöglichst, der Ausgabe eine passende Bedeutung zu geben. Insbesondere
    können zusätzliche Routinen zum Kopieren und Einfügen zwischen
    verschiedenartigen Systemen formuliert werden.
  </remark>

  <paragraph*|Das <verbatim|markup> plugin>

  Man sollte immer im Gedächtnis behalten, dass eine strukturierte Ausgabe
  mit den Stärken von <TeXmacs> als strukturierter Editor genutzt werden kann
  und sollte. Im folgenden wollen wir mit dem Beispiel-Plugin
  <verbatim|markup> zeigen, wie man ein neues Makro <markup|foo> einführt,
  das als zusätzliches Konstrukt in der Ausgabe der Anwendung genutzt wird.
  Das Plugins <verbatim|markup> besteht aus den folgenden Zeilen:

  <\verbatim>
    \ \ \ \ <example-plugin-link|markup/Makefile>

    \ \ \ \ <example-plugin-link|markup/packages/session/markup.ts>

    \ \ \ \ <example-plugin-link|markup/progs/init-markup.scm>

    \ \ \ \ <example-plugin-link|markup/src/markup.cpp>
  </verbatim>

  Das Stil-Paket <tmpackage|markup.ts> enthält die folgende Definition von
  <markup|foo>:

  <\tm-fragment>
    <with|preamble|true|<with|mode|math|<inactive|<assign|foo|<macro|x|<frac|1|1+<arg|x>>>>>>>
  </tm-fragment>

  Das Makro <markup|foo> wird jetzt folgendermaÿen im Rumpf der Hauptschleife
  von <verbatim|markup.cpp> genutzt:

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:";

    cout \<less\>\<less\> "$\\\\foo{" \<less\>\<less\> buffer
    \<less\>\<less\> "}$";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  Beachten Sie bitte, dass das <tmpackage|markup.ts> Paket auch
  <markup|markup-output> definiert:

  <\tm-fragment>
    <with|preamble|true|<inactive|<assign|markup-output|<macro|body|<generic-output|<with|par-mode|center|<arg|body>>>>>>>
  </tm-fragment>

  Auf diese Weise wird die Ausgabe zentriert, wenn sie mit
  <menu|Insert|Session|Markup> gestartet werden.

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