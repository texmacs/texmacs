<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Ustawienia klawiszy modyfikuj¡cych>

  <TeXmacs> korzysta z pi¦ciu klawiszy modyfikuj¡cych: <prefix|S->,
  <prefix|C->, <prefix|A->, <prefix|M-> i
  <prefix|M-A->, skracanych do <prefix|S->, <prefix|C->, <prefix|A->, <prefix|M-> i
  <prefix|M-A->. Klawisze <prefix|S-> i <key|control> s¡ obecne na praktycznie
  ka»dej klawiaturze, a <key|alternate> jest na prawie wszystkich. Wi¦kszo±¢
  wspóªczesnych klawiatur posiada klawisz <key|windows>, który jest zwykle
  równowa»ny klawiszowi <key|meta> dla <TeXmacs>.

  Przed zmian¡ ustawie« klawiatury nale»y najpierw sprawdzi¢ czy jest to
  rzeczywi±cie potrzebne. Je±li dost¦pne s¡ klawisze w wygodny sposób
  pasuj¡ce do <prefix|S->, <prefix|C->, <prefix|A-> i
  <prefix|M->, to nie trzeba nic robi¢. Prawdopodobny wyj¡tek to
  sytuacja gdy potrzeba prostego klawisza jak <key|capslock> do wpisywania
  symboli matematycznych. W takim przypadku nale»y odwzorowa¢ <key|capslock>
  na <key|hyper>.

  Niestety w X Windows konfiguracja obejmuje od razu caªy system. Zatem je±li
  nast¡pi przedefiniowanie klawisza <key|capslock> wewn¡trz <TeXmacs>
  to jego nowe zachowanie b¦dzie równie» w innych aplikacjach. Czyli powinno
  si¦ przestawia¢ tylko klawisze dla które nie s¡ u»ywane przez inne
  programy. Dla przykªadu klawisz <key|windows> jest u»ywany przez
  niewiele apliakcji, zatem jego przedefiniowanie nie powinno wyrz¡dzi¢
  szkody. Wygodniej mo»e by¢ okre±li¢ odpowiednio konfiguracj¦ caªego
  systemu. To mo»na zrobi¢ przy u»yciu polecenia <verbatim|xmodmap>;
  dokªadniejsze informacje w jego dokumentacji.

  W niektórych wypadkach, na klawiaturze s¡ klawisze odpowiadaj¡ce
  <prefix|A->, <prefix|M-> i <prefix|M-A->, jednak
  ustawione inaczej ni» pasuje u»ytkownikowi. Mo»na zmieni¢ przypisanie
  prefiksów <prefix|A->, <prefix|M->, i <prefix|M-A-> do modyfikatorów poprzez menu
  <menu|Edit|Preferences|Keyboard>.

  Na przykªad, aby zachowa¢ kompatybilno±¢ z Emacsem mo»na spermutowa¢
  klawisz <prefix|M-> lub <key|windows> z <prefix|A->
  bez zmian dla innych cz¦±ci systemu. Nale»y znale¹¢ które modyfikatory s¡
  powi¡zane z tymi klawiszami; zwykle b¦dzie to <key|Mod1> dla
  <prefix|A-> i <key|Mod4> dla <prefix|M-> lub
  <key|windows>. Nast¦pnie zadan¡ permutacj¦ ustawia si¦ wybieraj¡c
  <menu|A modifier|Równowa»ny Modulo4> i <menu|M modifier|Równowa»ny Modulo1>
  w menu <menu|Edit|Preferences|Keyboard>.

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