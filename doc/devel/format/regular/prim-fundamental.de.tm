<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Basis-Konstrukte >

  <\explain|<explain-macro|document|par-1|<with|mode|math|\<cdots\>>|par-n><explain-synopsis|Vertikale
  Folge von Absätzen>>
    Dieses Konstrukt wird für Folgen von zusammengehörigen Absätzen benutzt.
    Ein einfaches Textdokument besteht in der Regel aus einer Folge von
    Absätzen. Z.B.

    <\tm-fragment>
      Ein einfaches Dokument.

      Das Dokument besteht aus mehreren Absätzen. Da zu lang, muss ein
      Seitenumbruch durchgeführt werden. Lange Worte am Zeilenende werden
      getrennt.
    </tm-fragment>

    wird intern als <markup|document> mit zwei Unter-Bäumen gespeichert:

    <\tm-fragment>
      <with|src-special|raw|<\inactive*>
        Ein einfaches Dokument.

        Das Dokument besteht aus mehreren Absätzen. Da zu lang, muÿ ein
        Seitenumbruch durchgeführt werden. Lange Worte am Zeilenende werden
        getrennt.
      </inactive*>>
    </tm-fragment>

    Auf dem Bildschirm und im Druck werden auf einander folgende Absätze oft
    durch Leerraum oder durch Erstzeileneinzüge markiert. Die Wurzel eines
    <TeXmacs>-Dokuments ist normalerweise ein <markup|document>-Knoten.

    Das <markup|document>-Konstrukt wird häufig für Inhalte benutzt, die aus
    mehreren Absätzen bestehen, wenn sie innerhalb von anderen Konstrukten
    wie z.B. Listen oder Theoremen vorkommen. Kontexte, die ein
    <markup|document>-Konstrukt benötigen, heiÿen \RBlockkontext''.
  </explain>

  <\explain|<explain-macro|paragraph|unit-1|<with|mode|math|\<cdots\>>|unit-n><explain-synopsis|Vertikale
  Folge von Absatzeinheiten>>
    Dieses noch nicht implementierte Konstrukt ist eine Variante von
    <markup|document>. Während ein Dokument eine Folge von Absätzen ist, ist
    ein <with|color|blue|paragraph>, ein Absatz, \ eine Folge
    \RAbsatzeinheiten'', sprich einzelnen Zeilen. Auch sind eigenständige
    Formeln Absatzeinheiten in einem gröÿeren Absatz.
  </explain>

  <\explain|<explain-macro|concat|item-1|<with|mode|math|\<cdots\>>|item-n><explain-synopsis|Horizontale
  Folge von Zeileninhalt>>
    Dieses Konstrukt definiert eine horizontale Folge kurzen Textstücken oder
    von Konstrukten, die kurze Textstücke speziell darstellen, Zeileninhalt.
    Z.B.

    <\tm-fragment>
      Dies ist Text<em| hervorgehoben mit der Form Italic>.
    </tm-fragment>

    wird intern gespeichert als:

    <\tm-fragment>
      <with|src-special|raw|<inactive*|Dies ist Text<em| hervorgehoben mit
      der Form Italic>.>>
    </tm-fragment>

    Das <markup|concat>-Konstrukt wird gebraucht, um Konstrukte in einen Baum
    einzufügen, die mehrere Parameter haben. Das vorstehende Textfragment
    soll z.B. in einen Text mit mehreren Absätzen eingefügt werden:\ 

    <\tm-fragment>
      Mehrere Absätze.

      Dies ist Text<em| hervorgehoben mit der Form Italic>.
    </tm-fragment>

    In diesem Beispiel benötigen wir das <markup|concat>-Konstrukt, um
    klarzustellen, dass \RDies ist Text<em| hervorgehoben mit der Form
    Italic>.`` ein einzelner Absatz ist.

    <\tm-fragment>
      <with|src-special|raw|<\inactive*>
        Mehrere Absätze.

        Dies ist Text <em| hervorgehoben mit der Form Italic>.
      </inactive*>>
    </tm-fragment>

    Beachten Sie bitte, dass Block-Konstrukte wie <markup|document>
    Zeilen-Konstrukte wie <markup|concat> als Kinder haben dürfen aber nicht
    umgekehrt. \ Um Zeileninhalt vor oder hinter Blockinhalt zu platzieren,
    muss man den Konstrukt <markup|surround> benutzen (s.u.).
  </explain>

  <\explain>
    <explain-macro|surround|left|right|body><explain-synopsis|Blockinhalt mit
    Zeileninhalt umgeben>
  <|explain>
    Obwohl es in <TeXmacs> nicht möglich ist, Blockinhalt in horizontalen
    Aufreihungen zu benutzen, kann es manchmal nützlich sein, zusätzlichen
    Zeileninhalt vor oder hinter Blockinhalten zu platzieren. Dazu dient das
    <markup|surround>-Konstrukt, der Zeileninhalt <src-arg|left> und
    Zeileninhalt <src-arg|right> dem Blockinhalt <src-arg|body> hinzufügt.
    Beispielsweise produziert

    <\tm-fragment>
      <\inactive*>
        <\surround|<active*|<with|mode|math|<with|color|red|\<lightning\>>>
        >|>
          <\theorem>
            <active*|Gegeben <with|mode|math|P\<in\>\<bbb-T\>{F}> und
            <with|mode|math|f\<less\>g\<in\>\<bbb-T\>> mit
            <with|mode|math|P(f)*P(g)\<less\>0>, dann existiert ein
            <with|mode|math|h\<in\>\<bbb-T\>> mit <with|mode|math|P(h)=0>.>
          </theorem>
        </surround>
      </inactive*>
    </tm-fragment>

    das folgende

    <\tm-fragment>
      <\surround|<with|mode|math|<with|color|red|\<lightning\>>> |>
        <\theorem>
          Gegeben <with|mode|math|P\<in\>\<bbb-T\>{F}> und
          <with|mode|math|f\<less\>g\<in\>\<bbb-T\>> mit
          <with|mode|math|P(f)*P(g)\<less\>0>, dann existiert ein
          <with|mode|math|h\<in\>\<bbb-T\>> mit <with|mode|math|P(h)=0>.
        </theorem>
      </surround>
    </tm-fragment>

    Gewöhnlich wird <markup|surround> in Stildefinitionen gebraucht.
    Gelegentlich ist es in normalen Text auch recht nützlich.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

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