<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Der Aufbau von Stil-Definitionen und -Paketen>

  Jede <TeXmacs>-Standard-Stil-Definition und jedes
  <TeXmacs>-Standard-Stil-Paketberuht auf einer endlichen Anzahl von
  Unterpaketen. In abstrakter Darstellung ist dies ein Baum mit Labeln. Der
  Baum, der zum Artikel-Stil, <tmstyle|article>, gehört, sieht so aus:

  <\big-figure|<tree|<tmstyle|article>|<tree|<tmpackage|std>|<stack|<tformat|<table|<row|<cell|<tmpackage|std-counter>>>|<row|<cell|<tmpackage|std-markup>>>|<row|<cell|<tmpackage|std-symbol>>>|<row|<cell|<tmpackage|std-math>>>|<row|<cell|<tmpackage|std-list>>>|<row|<cell|<tmpackage|std-automatic>>>|<row|<cell|<tmpackage|list>>>|<row|<cell|<tmpackage|session>>>>>>>|<tree|<tmpackage|env>|<stack|<tformat|<table|<row|<cell|<tmpackage|env-base>>>|<row|<cell|<tmpackage|env-math>>>|<row|<cell|<tmpackage|env-theorem>>>|<row|<cell|<tmpackage|env-float>>>>>>>|<tree|<tmpackage|header-article>|<tmpackage|title-generic>>|<tree|<tmpackage|section-article>|<tmpackage|section-base>>>>
    Der Baum, der den Stil Artikel, <tmstyle|article>, repräsentiert. Um
    Platz zu sparen, wurden die Kinder von <tmpackage|std> und
    <tmpackage|env> vereinfachend als vertikale Listen dargestellt.
  </big-figure>

  Die meisten Stilpakete entsprechen einer <abbr|D.T.D.> (data type
  definition), die die \Rabstrakte Schnittstelle'' des Pakets darstellt, d.h.
  die exportierten, von auÿen zugänglichen, Befehle. Z.B. entspricht das
  Paket <tmpackage|std-markup> der <abbr|D.T.D.> <tmdtd|std-markup>. Manchmal
  können auch mehrere Stil-Pakete der gleichen <abbr|D.T.D.> entsprechen.
  Dies ist beispielsweise bei <tmpackage|header-article> und
  <tmpackage|header-book> der Fall, die beide der <abbr|D.T.D.>
  <tmdtd|header> entsprechen, da sie nur verschiedene Arten der Darstellung
  von sonst gleichen Befehlen implementieren.

  Wenn Sie Ihre eigene Stil-Definition oder Ihr eigenes Stil-Paket schreiben,
  können Sie das Konstrukt <markup|use-package> benutzen, um andere Pakete
  einzubinden Beispielsweise besteht der Stil <tmstyle|article> im
  wesentlichen aus der Zeile

  <\tm-fragment>
    <inactive*|<use-package|std|env|header-article|section-article>>
  </tm-fragment>

  Genauer, das Paket <markup|use-package> läd die Stil-Pakete in der
  Reihenfolge seiner Argumente. Die Pakete müssen im Pfad
  \ <verbatim|$TEXMACS_PACKAGE_PATH> liegen, die gemäÿ Vorgabe
  \ <verbatim|.>, <verbatim|~/.TeXmacs/packages> und
  <verbatim|$TEXMACS_PATH/packages> enthalten. Auÿerdem werden Befehle, die
  der Darstellung von Quellcode dienen, wie z.B. <markup|style-with>, vor der
  Evaluierung entfernt.

  <\remark>
    Wir empfehlen sehr, bevor man sich an das Schreiben eigener
    Stil-Definitionen macht, einige der Standard-Stil-Definitionen anzusehen.
    Diese findet man unter

    <\verbatim>
      \ \ \ \ $TEXMACS_PATH/styles

      \ \ \ \ $TEXMACS_PATH/packages
    </verbatim>

    Wenn man <shortcut|(interactive load-buffer)> benutzt, um Dateien zu laden, dann sind die oben
    genannten Pfade mit im Standard-Pfad. Wenn Sie also das
    <tmpackage|std-markup>-Paket ansehen wollen, brauchen sie nur <key|C-x
    C-f>, eingeben und den Datei-Namen <verbatim|std-markup.ts> und
    schlieÿlich \ <shortcut|(kbd-return)>.
  </remark>

  <\remark>
    Man kann auch die Darstellung von Quellcode in den Stil-Definitionen und
    -<no-break>Paketen anpassen, indem man andere Pakete gemeinsam mit dem
    Basisstil \R<localize|source>``, <tmstyle|source>, oder einer auf diesem
    Basis-Stil basierende Stil-Definition verwendet. In diesem Fall können
    die so bereitgestellten Makros zur Darstellung des Quellcodes genutzt
    werden, sie werden aber nicht exportiert, wenn Sie Ihr Paket oder Ihre
    Stildefinition in einer anderen Datei verwenden.
  </remark>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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