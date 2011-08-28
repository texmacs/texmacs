<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Tworzenie etykiet, linków i odsyªaczy>

  Now¡ etykiet¦ mo»na stworzy¢ poprzez <shortcut|(make-label)> lub menu
  <menu|Insert|Link|Label> a odsyªacz do niej u»ywaj¡c <shortcut|(make 'reference)> lub
  <menu|Insert|Link|Reference>. Nale»y umieszcza¢ etykiety tak, aby
  przyjmowaªy prawidªowe numery. Przy akapitach rekomendowane jest
  umieszczanie ich tu» za nazw¡ akapitu. Gdy oznaczane s¡ równania, zalecane
  jest umieszczenie na pocz¡tku, wewn¡trz równania.

  Mo»na tworzy¢ aktywne powi¡zania do innych dokumentów u»ywaj¡c <key|inactive
  \<gtr\>> lub <menu|Insert|Link|Hyperlink>. Pierwsze pole to powi¡zany
  tekst, wy±wietlany na niebiesko w aktywnym linku. Druga pozycja to nazwa
  dokumentu, który mo»e by¢ w internecie. Jak zwykle przy odsyªaczach forma
  <verbatim|#nazwa> kieruje na etykiet¦ w bie»¡cym dokumencie, a forma
  <verbatim|url#nazwa> kieruje na etykiet¦ w dokumencie zlokalizowanym w
  <verbatim|url>.

  W podobny sposób, operacja mo»e by¢ dowi¡zana do fragmentu tekstu lub
  grafiki, poprzez <key|inactive *> lub <menu|Insert|Link|Action>. Drugie pole
  zawiera polecenie skryptowe Guile/Scheme, wykonywane po dwuklikni¦ciu na
  tekst. Ze wzgl¦dów bezpiecze«stwa takie skrypty nie zawsze s¡ akceptowane.
  Domy±lnie program prosi o akceptacje. To zachowanie mo»e by¢ zmienione
  poprzez <menu|Options|Security>. Polecenie Guile/Scheme

  <\verbatim>
    \ \ \ \ (system "shell-command")
  </verbatim>

  wykonuje <verbatim|shell-command> jako polecenie shellowe

  Ostatecznie, mo»na bezpo±rednio wª¡cza¢ inne dokumenty w zadany u»ywaj¡c
  <key|inactive i> lub <menu|Insert|Link|Include>. W ten sposób wszystkie
  pó¹niejsze zmiany wstawionego dokumentu b¦d¡ uwzgl¦dnione.

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