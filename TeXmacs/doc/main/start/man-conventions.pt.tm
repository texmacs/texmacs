<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Convenções para este manual>

  Ao longo de todo o manual do <TeXmacs>, entradas em menus serão
  tipografadas com uma fonte <em|sem serifas>, por exemplo:
  <apply|menu|Document>, <apply|menu|File|Load> or <apply|menu|Insert|Font
  shape|Italic>. Caracteres que devem ser digitados no teclado serão
  mostrados com uma fonte <em|monoespaçada> dentro de caixas, da seguinte
  forma: <key|C-s>. \ Você verá atalhos de teclado do lado direito das
  entradas de um menu, quando estes existirem. As seguintes abreviações são
  usadas para estes atalhos:

  <\description>
    <expand|item*|<prefix|S->>Para combinações precedidas pela tecla shift.

    <expand|item*|<prefix|C->>Para combinações precedidas pela tecla control.

    <expand|item*|<verbatim|><prefix|A->>Para combinações precedidas pela tecla
    alt.

    <expand|item*|<prefix|M->>Para combinações precedidas pela tecla meta.

    <expand|item*|<prefix|M-A->>Para combinações precedidas pela tecla hyper.
  </description>

  Por exemplo, <shortcut|(make-with font-series bold)> significa <key|A-C-b>.
  Espaços dentro de atalhos de teclado significam que você deve digitar as
  teclas individualmente. Por exemplo, <key|table N b> significa
  <prefix|table> <key|N> <key|b>.

  As teclas <prefix|A->, <prefix|M-> e
  <prefix|M-A-> não estão disponíveis em todos os teclados. Em
  computadores pessoais atuais, a tecla <prefix|M-> em geral é
  substituida pela tecla <key|windows>. Quando uma ou mais
  teclas modificadores não existem no seu teclado, você pode usar
  <key|escape> ao invés de <prefix|M->, <key|escape
  escape> no lugar de <prefix|A-> e <prefix|math:greek>,
  <key|escape escape escape> ou
  <prefix|A-C-> no lugar de <prefix|M-A->. Por exemplo, <key|escape w>
  equivale a <key|A-w>. Você pode também <apply|hyper-link|configurar as
  teclas modificadoras|../config/man-config-kbd-modkeys.pt.tm> para
  aproveitar completamente o poderoso conjunto de atalhos de teclado que é
  fornecido com o <TeXmacs>.

  Observe que os menus do <TeXmacs> e o comportamento do teclado são
  <em|contextuais>, isto é, dependem do modo atual (``modo texto'' ou ``modo
  matemático''), da língua atual e da posição do cursor dentro do texto. Por
  exemplo, no modo matemático, você tem acesso a atalhos especiais que são
  úteis para digitar fórmulas matemáticas, mas que são inúteis no modo texto

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdorf>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|portuguese>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Documento>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Arquivo>|<with|font
      family|<quote|ss>|Carregar>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Formato da fonte>|<with|font
      family|<quote|ss>|Itálico>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
