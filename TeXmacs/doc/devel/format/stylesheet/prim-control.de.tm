<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Steuerung des logischen Ablaufs>

  <\explain>
    <explain-macro|if|condition|if-body>

    <explain-macro|if|condition|if-body|else-body><explain-synopsis|Bedingung>
  <|explain>
    Dieses Konstrukt setzt <src-arg|if-body> nur dann, wenn die Bedingung
    <src-arg|condition> erfüllt ist. Wenn ein optionales <src-arg|else-body>
    spezifiziert wurde, dann wird diese nur gesetzt, wenn die Bedingung
    <src-arg|condition> falsch ist.

    <\remark>
      Man sollte beachten, das Verwendung von Bedingungen fehleranfällig ist.
      Das kommt davon, dass die Erreichbarkeit von Argumenten nicht vorab
      getestet werden kann. In dem Makro\ 

      <\tm-fragment>
        <inactive*|<macro|x|<if|<visibility-flag>|<arg|x>>>>
      </tm-fragment>

      ist das Makro-Argument <src-arg|x> dann und nur dann erreichbar, wenn
      <inactive*|<visibility-flag>> wahr ist. Das kann aber vorher nicht
      getestet werden. Bestimmte Editier-Operationen, wie Suchen oder
      Rechtschreibprüfung kann eine falsche Bestimmung der Erreichbarkeit
      dazu führen, dass der Cursor in nicht erreichbare Positionen gesetzt
      wird oder dass bestimmte Konstrukte ignoriert werden. Wir wollen dieses
      Verhalten des Editors verbessern. Bis dahin ist es besser, Bedingungen
      zu vermeiden.
    </remark>

    <\remark>
      Die Bedingungs-Konstrukte sind nur für Zeileninhalt voll implementiert.
      Wenn man Bedingungen im Blockkontext braucht, dann muss man den
      Wenn-ja-Fall und den Wenn-nein-Fall getrennt in Makros definieren und
      mit einem unbenannten Makro die Bedingung einführen. Z.B.:

      <\tm-fragment>
        <inactive*|<assign|kalt|<macro|x|<with|color|blue|<arg|x>>>>>

        <inactive*|<assign|heiss|<macro|x|<with|color|red|<arg|x>>>>>

        <inactive*|<assign|adaptive|<macro|x|<compound|<if|<summer>|<value|heiss>|<value|kalt>>|<arg|x>>>>>
      </tm-fragment>
    </remark>
  </explain>

  <\explain>
    <explain-macro|case|cond-1|body-1|<with|mode|math|\<cdots\>>|cond-n|body-n>

    <explain-macro|case|cond-1|body-1|<with|mode|math|\<cdots\>>|cond-n|body-n|else-body><explain-synopsis|Fallunterscheidungen>
  <|explain>
    Diese Befehle sind äquivalent zu

    <\tm-fragment>
      <inactive*|<if|<arg|cond-1>|<arg|body-1>|<active*|<with|mode|math|\<cdots\>>><if|<arg|cond-n>|<arg|body-n>>>>

      <inactive*|<if|<arg|cond-1>|<arg|body-1>|<active*|<with|mode|math|\<cdots\>>><if|<arg|cond-n>|<arg|body-n>|<arg|else-body>>>>
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|while|condition|body><explain-synopsis|wiederholte
    Ausführung>
  <|explain>
    Solange die Bedingung <src-arg|condition> erfüllt ist, wird
    <src-arg|body> ausgeführt. Beispielsweise erzeugt der folgende das Makro

    <\tm-fragment>
      <inactive*|<assign|count|<macro|from|to|<with|i|<arg|from>|<style-with|src-compact|none|<while|<less|<value|i>|<arg|to>>|<value|i>,
      <assign|i|<plus|<value|i>|1>>><arg|to>>>>>>
    </tm-fragment>

    aus <inactive*|<count|1|50>> dies:

    <\tm-fragment>
      <with|count|<macro|from|to|<with|i|<arg|from>|<while|<less|<value|i>|<arg|to>>|<value|i>,
      <assign|i|<plus|<value|i>|1>>><arg|to>>>|<count|1|50>>
    </tm-fragment>
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