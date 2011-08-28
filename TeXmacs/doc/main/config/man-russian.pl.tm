<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Uwagi dla u»ytkowników rosyjskich i ukrai«skich.>

  Pisanie tekstu po rosyjsku (ukrai«sku) mo»liwe jest na kilka sposobów:

  <\itemize>
    <item>Wybór j¦zyka rosyjskiego jako domy±lnego j¦zyka programu poprzez
    <menu|Edit|Preferences|Language|Russian>. Je±li <TeXmacs> startuje z
    rosyjskimi menu, to znaczy »e zostaªo to ustawione automatycznie na
    podstawie konfiguracji systemu.

    <item>Wybór j¦zyka rosyjskiego dla caªego dokumentu poprzez
    <menu|Document|Language|Russian>.

    <item>Wybór j¦zyka rosyjskiego dla fragmentu tekstu u»ywaj¡c
    <menu|Format|Language|Russian>.
  </itemize>

  Je±li serwer X u»ywa rozszerzenia xkb i jest ustawiony do przeª¡czania
  pomi¦dzy ªaci«skim i rosyjskim trybem klawiatury, to nie trzeba nic wi¦cej
  robi¢. Po prostu przeª¡czy¢ klawiatur¦ na rosyjsk¡ i pisa¢. Caªe potrzebne
  oprogramowanie jest wª¡czone w wspóªczesne dystrybucje Linuxa, a
  rozszerzenie xkb jest wª¡czone domy±lnie w
  <with|font-family|tt|XF86Config>. Przy rozszerzeniu xkb, symbole klawiszy
  s¡ 2-bajtowe i rosyjskie litery s¡ w 0x6??

  <verbatim| \ \ \ ru basic grp:shift_toggle>

  To oznacza i» tryb klawiatury jest zmieniany przez <key|l-S-
  r-S->. Inne popularne metody to <prefix|C-S->
  lub <prefix|A-C->, w
  <with|font-family|tt|/usr/X11R6/lib/X11/xkb/> mo»na znale¹¢ szczegóªy. To
  zalecana konfiguracja klawiatury dla wspóªczesnych systemów linuxowych,
  je±li planowane jest cz¦ste korzystanie z cyrylicy.

  W starszych systemach xkb jest cz¦sto wyª¡czone. kody klawiszy s¡ 1-bajtowe
  i konfigurowane przez <verbatim|xmodmap>. Gdy X startuj¡, uruchamiane jest
  polecenie ustawiaj¡ce klawiatur¦, przewa»nie na podstawie pliku u»ytkownika
  <with|font-family|tt|~/.Xmodmap>, je±li on istnieje. Mo»na ustawi¢ klawisz
  zmieniaj¡cy tryb i u»y¢ 1-bitowego kodowania cyrylicy (takiego jak koi8-r)
  w trybie rosyjskim. Jednak pro±ciej jest zainstalowa¢ pakiet
  <verbatim|xruskb>, i po prostu uruchomi¢

  <verbatim| \ \ \ xrus jcuken-koi8>

  na pocz¡tku sesji X. To ustawi ukªad jcuken (zobacz poni»ej) i kodowanie
  koi8-r dla klawiatury w trybie rosyjskim. Je±li tak b¦dzie ustawiona, to
  nale»y wybra¢ <menu|Edytuj|Ustawienia|Klawiatura|Metoda wpisu
  cyrylicy|koi8-r>.

  Jest mo»liwe korzystanie z kodowania Windows cp1251 zamiast koi8-r, jednak
  to jest rzadko stosowane w UNIX. Je±li stosuje si¦
  <with|font-family|tt|xrus jcuken-cp1251>, nale»y wybra¢ cp1251 zamiast
  koi8-r.

  Wszystkie metody opisane powy»ej wymagaj¡ pewnych dziaªa« do
  zrusyfikowania'' klawiatury. to nie jest trudne, zalecana jest lektura
  Cyrillic-HOWTO, lub jego aktualnej wersji

  <verbatim|http://www.inp.nsk.su/<with|font-family|tt|~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html>>

  Dodatkowo powy»sze metody wpªywaj¡ na wszystkie programy w X: edytory
  tekstowe, terminale, <TeXmacs> itp.

  Je±li wprowadzanie cyrylicy ma miejsce rzadko, prawidªowe ustawienie
  klawiatury mo»e by¢ wi¦kszym problemem ni» jest to warte. <TeXmacs> ma
  metody pisania cyrylic¡, które nie wymagaj¡ wcze±niejszej pracy. Oczywi±cie
  te metody dziaªaj¡ wyª¡cznie w <TeXmacs>, bez wpªywu na inne proramy.

  Najprostsz¡ metod¡ pisania cyrylic¡ na standardowej klawiaturze, bez
  programowego wsparcia, jest wybranie <menu|Edit|Preferences|Keyboard|Cyrillic
  input method|translit>. Wtedy wprowadzenie ªaci«skiego znaku wyprodukuje
  najbardziej podobny'' znak rosyjski. Aby uzyska¢ niektóre znaki potrzeba
  wpisa¢ 2- lub 3-literowe kombinacje:<vspace|1fn>

  <big-table|<descriptive-table|<tformat|<cwith|2|11|1|1|cell-halign|l>|<cwith|2|11|2|2|cell-halign|l>|<cwith|2|11|2|2|cell-halign|c>|<cwith|2|11|4|4|cell-halign|l>|<cwith|2|11|4|4|cell-halign|c>|<table|<row|<cell|Skrót>|<cell|Znak>|<cell|Skrót(y)>|<cell|znak>>|<row|<cell|<key|accent:umlaut
  e>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<key|accent:umlaut
  E>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|j
  var>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|J
  var>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|÷>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|×>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|ø>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|Ø>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|ù>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|Ù>>>|<row|<cell|<key|e
  var>>|<cell|<with|language|russian|font|cyrillic|ý>>|<cell|<key|E
  var>>|<cell|<with|language|russian|font|cyrillic|Ý>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|þ>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|Þ>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|ÿ>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|ß>>>>>>|Wpisywanie cyrylicy
  na klawiaturze ªaci«skiej.>

  Je±li potrzeba uzyska¢, np. <with|language|russian|<with|font|cyrillic|ñõ>''>
  a nie <with|language|russian|<with|font|cyrillic|ø>>'', to nale»y wpisa¢
  <key|s / h>. Oczywi±cie, wybór optymalnego'' odwzorowania ªaci«skich liter
  na cyrylic¦ nie jest jedyny. Mo»na zbada¢ odwzorowanie dostarczone przez
  <TeXmacs> i nadpisa¢ je poprzez <with|font-family|tt|~/.TeXmacs/progs/my-init-texmacs.scm>.

  Wybór jcuken zamiast translit daje oficjalny'' ukªad rosyjskiej maszyny do
  pisania. Nazwany jest tak poniewa» klawisze qwerty'' daj¡
  <with|language|russian|<with|font|cyrillic|éöóêåí>>''. Ta metoda
  wprowadzania jest bardzo u»yteczna je±li klawiatura ma dodatkowe podpisy na
  klawiszach (wyprodukowana w Rosji, lub z r¦cznie dodanymi naklejkami).

  Je±li na klawiaturze nie ma podpisów cyrylic¡ wygodniejszy mo»e by¢ ukªad
  yawerty, gdzie klawisze qwerty'' daj¡ <with|language|russian|<with|font|cyrillic|ÿâåðòû>>''.
  Ka»dy znak ªaci«ski jest przeksztaªcany w podobny'' rosyjski; niektóre
  dodatkowe litery rosyjskie s¡ uzyskiwane poprzez <prefix|S->-cyfra.
  <TeXmacs> u»ywa troch¦ zmodyfikowanego ukªadu yawerty, poniewa» nie u»ywa
  dla niego klawiszy <key|$>, <key|¿>, <key|\\>, które s¡ wa»ne dla
  <TeXmacs>. Odpowiadaj¡ce znaki rosyjskie s¡ tworzone przez kombinacje
  <prefix|S->-cyfra

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
    <associate|language|polish>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>