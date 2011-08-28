<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Absatz-Layout>

  <\explain>
    <var-val|par-mode|justify><explain-synopsis|Absatzausrichtung>
  <|explain>
    Diese Kontextvariable legt fest wie Zeilen ausgerichtet werden, also wie
    sie in Bezug auf die Absatzränder gesetzt werden. Es gibt vier mögliche
    Werte: <verbatim|left>, <verbatim|center>, <verbatim|right> und
    <verbatim|justify>, die den Ausrichtungen linksbündig, zentriert,
    rechtsbündig und Blocksatz entsprechen:

    \;

    <\big-table>
      <\quote-env>
        <\quote-env>
          <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
            <\with|par-mode|left>
              Dieser Absatz ist linksbündig ausgerichtet. Dieser Absatz ist
              linksbündig ausgerichtet. Dieser Absatz ist linksbündig
              ausgerichtet. Dieser Absatz ist linksbündig ausgerichtet.Dieser
              Absatz ist linksbündig ausgerichtet.
            </with>
          </cell>|<\cell>
            <\with|par-mode|center>
              Dieser Absatz ist zentriert ausgerichtet. Dieser Absatz ist
              zentriert ausgerichtet. Dieser Absatz ist zentriert
              ausgerichtet. Dieser Absatz ist zentriert ausgerichtet.\ 
            </with>
          </cell>>|<row|<\cell>
            <\with|par-mode|right>
              Dieser Absatz ist rechtsbündig ausgerichtet. Dieser Absatz ist
              rechtsbündig ausgerichtet. Dieser Absatz ist rechtsbündig
              ausgerichtet. Dieser Absatz ist rechtsbündig ausgerichtet.
            </with>
          </cell>|<\cell>
            Dieser Absatz ist im Blocksatz gesetzt. Dieser Absatz ist im
            Blocksatz gesetzt. Dieser Absatz ist im Blocksatz gesetzt. Dieser
            Absatz ist im Blocksatz gesetzt. Blocksatz ist die Vorgabe.
          </cell>>>>>
        </quote-env>
      </quote-env>
    <|big-table>
      Unterstützte Absatzausrichtungen.
    </big-table>
  </explain>

  <\explain>
    <var-val|par-hyphen|normal><explain-synopsis|Qualität der Trennungen>
  <|explain>
    Dieser Parameter steuert die Qualität der Trennungs-Algorithmen. Mögliche
    Werte sind \ <verbatim|normal> und <verbatim|professional>. Der
    professionelle Trennungs-Algorithmus verwendet einen Algorithmus der den
    ganzen Absatz umfasst. Der normale ist schneller.

    <\big-table>
      <\with|font-base-size|10>
        <\quote-env>
          <\quote-env>
            <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
              <\with|par-hyphen|normal>
                Die Unterschiede zwischen den verschiedenen möglichen
                Trennungs-Algorithmen sieht man in der Regel erst bei
                längeren Absätzen und zwar dann, wenn die Absätze in schmale
                Kolumnen gesetzt werden.\ 
              </with>
            </cell>|<\cell>
              <\with|par-hyphen|professional>
                Die Unterschiede zwischen den verschiedenen möglichen
                Trennungs-Algorithmen sieht man in der Regel erst bei
                längeren Absätzen und vor allem dann, wenn die Absätze in
                schmale Kolumnen gesetzt werden. \ 
              </with>
            </cell>>>>>
          </quote-env>
        </quote-env>
      </with>
    <|big-table>
      Vergleich zwischen verschiedenen Trennungs-Algorithmen. Links der
      normale Algorithmus, rechts der professionelle. Auch wenn auf der
      rechten Seite noch einige unschöne Lücken verbleiben, so wurden von dem
      professionellen Algorithmus doch die unschönen Lücken in der vorletzten
      Zeile um das Wort \RAbsätze`` vermieden.
    </big-table>
  </explain>

  <\explain>
    <var-val|par-width|auto><explain-synopsis|Absatzbreite>
  <|explain>
    Diese Kontextvariable steuert die Breite der Absätze. Normalerweise wird
    die Absatzbreite automatisch aus der Papierbreite und den Breiten der
    Ränder berechnet.
  </explain>

  <\explain>
    <var-val|par-left|0cm>

    <var-val|par-right|0cm><explain-synopsis|Linker und rechter Rand>
  <|explain>
    Diese Kontextvariablen bestimmen die Breite des linken bzw. rechten
    Randes von Absätzen und zwar in Bezug auf die Vorgabewerte, die von dem
    Seitenlayout bestimmt werden.\ 

    Ein Beispiel:

    <\tm-fragment>
      Dieser Text benutzt die Vorgabe.

      <\with|par-left|1cm>
        Dieser Text hat einen linken Rand von <verbatim|1cm.>
      </with>

      <\with|par-left|2cm>
        Dieser Text hat einen linken Rand von <verbatim|2cm.>
      </with>

      <\with|par-left|3cm>
        Dieser Text hat einen linken Rand von <verbatim|3cm.>
      </with>

      <\with|par-left|3cm|par-right|3cm>
        Die linken und die rechten Ränder dieses Textes sind beide auf
        <verbatim|3cm> gesetzt worden.
      </with>
    </tm-fragment>

    Layoutelemente wie Auflistungen, <markup|itemize>, oder
    <markup|quote-env>, die verschachtelt werden können, berechnen in der
    Regel neue Randabstände als Funktion der alten, indem sie vorgegebene
    Abstände hinzufügen oder abziehen. Das verdeutlicht die typischen
    Makrodefinition für <markup|quote-env> des folgenden Beispiels:

    <\tm-fragment>
      <inactive*|<assign|quote-env|<macro|body|<style-with|src-compact|none|<surround|<vspace*|0.5fn>|<right-flush><vspace|0.5fn>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|par-first|0fn|par-par-sep|0.25fn|<arg|body>>>>>>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|par-first|1.5fn><explain-synopsis|Erstzeileneinzug>
  <|explain>
    Der <src-var|par-first>-Parameter spezifiziert den Erstzeileneinzug. Ein
    solcher Einzug hat den Sinn, den Beginn eines neuen Absatzes zu
    kennzeichnen. Eine andere Alternative ist ein erhöhter Zeilenabstand.

    <\big-table>
      <\with|par-hyphen|professional>
        <\quote-env>
          <\quote-env>
            <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
              <\with|par-par-sep|0fn>
                <\with|par-first|1.5fn>
                  In den <TeXmacs>-Basis-Stilen
                  <active*|<tmstyle|<translate|article|english|german>>> und
                  <tmstyle|<translate|book|english|german>> wird der Anfang
                  eines neuen Absatzes durch den Einzug der ersten Zeile
                  gekennzeichnet.

                  Der <TeXmacs>-<tmstyle|<translate|generic|english|german>><tmstyle|>-
                  und der <tmstyle|<translate|letter|english|german>>-Basis-Stil
                  benutzen dagegen senkrechten Abstand.
                </with>
              </with>
            </cell>|<\cell>
              Der <TeXmacs>-<tmstyle|<translate|generic|english|german>><tmstyle|>-
              und der <tmstyle|<translate|letter|english|german>>-Basis-Stil
              benutzen einen zusätzlichen senkrechten Abstand zur
              Kennzeichnung des Beginns von neuen Absätzen.

              <\with|par-par-sep|0.5fn>
                <\with|par-first|0fn>
                  In den <TeXmacs>-Basis-Stilen
                  <active*|<tmstyle|<translate|article|english|german>>> und
                  <tmstyle|<translate|book|english|german>> wird der Anfang
                  eines neuen Absatzes dagegen durch den Einzug der ersten
                  Zeile gekennzeichnet..
                </with>
              </with>
            </cell>>>>>
          </quote-env>
        </quote-env>
      </with>
    <|big-table>
      <label|par-first-tab>Zwei klassische Weisen zur Kennzeichnung eines
      neuen Absatzes.
    </big-table>
  </explain>

  <\explain>
    <var-val|par-par-sep|0.5fn*><explain-synopsis|Zusätzlicher Abstand
    zwischen Absätzen>
  <|explain>
    Der <src-var|par-par-sep>-Parameter legt den Abstand zwischen zwei auf
    einander folgenden Absätzen fest. Der Abstand wird in
    \ <hyper-link|kontextabhängigen Längeneinheiten|../basics/lengths.de.tm>
    gemessen. In der Regel erzeugt <TeXmacs> keinen vergröÿerten
    Zeilenabstand zwischen auf einander folgenden Absätzen, es sei denn, es
    würde kein vernünftiger Seitenumbruch gefunden. Darum wird die
    kontextabhängige Längeneinheiten <verbatim|fn*> benutzt. Normalerweise
    wird der Erstzeileneinzug benutzt (Tabelle <reference|par-first-tab>).
  </explain>

  <\explain>
    <var-val|par-line-sep|0.025fn*><explain-synopsis|Leerraum zwischen
    Zeilen>
  <|explain>
    Dieser Parameter legt die <em|Breite des Leerraums> zwischen den Zeilen
    in einem Absatz fest. Dies entspricht nicht dem, was man normalerweise
    mit Zeilenabstand bezeichnet. Die übliche Definition des Zeilenabstands
    ist die Summe des Leerraums und der Schrifthöhe.

    <\tm-fragment>
      <\with|par-line-sep|1fn>
        Ein doppelter \RZeilenabstand'' entspricht
        \ <var-val|par-line-sep|1fn>. Dieser wird oft von faulen Menschen
        verwendet, die vorgeben wollen, viele Seiten geschrieben zu haben,
        die sich aber um das Wohlergehen der Wälder nicht kümmern..
      </with>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|par-sep|0.2fn><explain-synopsis|Minimaler vertikaler Abstand
    zwischen Zeilen>
  <|explain>
    Diese Variable definiert einen Mindestabstand zwischen Boxen in den
    einzelnen Zeilen. Das verhindert Kollisionen von besonders groÿen Kästen
    mit solchen in vorausgehenden bzw. den nachfolgenden Zeilen.
  </explain>

  <\explain>
    <var-val|par-hor-sep|0.5fn><explain-synopsis|Minimaler horizontaler
    Abstand>
  <|explain>
    Wenn ein Absatz mehrere besonders groÿe Boxen enthält, dann versucht
    <TeXmacs> die auf einander folgenden Zeilen in einander zu schieben,
    solange Boxen nicht mit einander kollidieren:

    <\with|font-base-size|10>
      <\tm-fragment>
        Betrachten Sie einen Bruch, der sich tiefer als die Unterlängen der
        normalen Schrift erstreckt wie beispielsweise <with|mode|math|den
        Bruch <frac|1|x+1>> und einen Ausdruck, der höher als normal ist wie
        <with|mode|math|\<mathe\><rsup|\<mathe\><rsup|x>>>. Wie Sie sehen
        versucht <TeXmacs> eine kompakte Darstellung zu erreichen.

        Wenn der Bruch <with|mode|math|<frac|1|x+1>> und der besonders hohe
        Ausdruck aber an der falschen Stelle liegen, wie
        <with|mode|math|\<mathe\><rsup|\<mathe\><rsup|x>>> hier, dann bleiben
        die Boxen im Abstand <src-var|par-sep>.
      </tm-fragment>
    </with>

    Wenn der horizontale Abstand zwischen zwei groÿen Boxen kleiner ist als
    <src-var|par-hor-sep> dann wird das als Kollision betrachtet.
  </explain>

  <\explain>
    <var-val|par-fnote-sep|0.2fn><explain-synopsis|Minimaler Abstand zwischen
    Fuÿnoten>
  <|explain>
    Dieser Parameter definiert den Abstand zwischen auf einander folgenden
    Fuÿnoten.
  </explain>

  <\explain>
    <var-val|par-columns|1><explain-synopsis|Spaltenanzahl>
  <|explain>
    Diese Variable legt die Anzahl der Spalten fest. Innerhalb eines
    Dokuments können unterschiedliche Spaltenanzahlen verwendet werden.
  </explain>

  <\explain>
    <var-val|par-columns-sep|2fn><explain-synopsis|Spaltenabstand>
  <|explain>
    Die Variable definiert die horizontale Breite des Leerraums zwischen
    Spalten.
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
    <associate|sfactor|4>
  </collection>
</initial>