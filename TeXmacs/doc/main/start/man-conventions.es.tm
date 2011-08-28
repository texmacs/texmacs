<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Convenciones para este manual>

  A través del manual de <TeXmacs>, las entradas de menu serán escritas
  usando una fuente <em|sans serif>, como en <apply|menu|Document>,
  <apply|menu|File|Load> o <apply|menu|Format|Font shape|Italic>. Las entradas
  de teclado serán escritas en fuente de <em|máquina de escribir> dentro de
  cajas, como en in <key|C-s>. En la parte lateral izquierda de las entradas
  de menu, verá el equivalente de teclado, cuando estos estén disponibles.
  Las siguientes abreviaciones son usadas para tales teclas:

  <\description>
    <expand|item*|<prefix|S->>Para combinaciones con la tecla shift.

    <expand|item*|<prefix|C->>Para combinaciones con la tecla control.

    <expand|item*|<verbatim|><prefix|A->>Para combinaciones con la tecla
    alternate.

    <expand|item*|<prefix|M->>Para combinaciones con la tecla meta.

    <expand|item*|<prefix|M-A->>Para combinaciones con la tecla hyper.
  </description>

  Por ejemplo, <shortcut|(make-with font-series bold)> representa <key|alt-ctrl-b>. Epacios dentro de los
  atajos de teclado indican múltiple presiones de tecla. Por ejemplo,
  <key|table N b> representa <key|meta-t> <key|N> <key|b>.

  Las teclas <key|alt>, <key|meta> e <key|hyper> no están disponibles en
  todos los teclados. En los PC's recientes, la tecla <key|meta> es a menudo
  reemplazada por la tecla <key|windows>. In the case when one or several
  modifier keys are missing on your keyboard, you may use <key|escape>
  instead of <prefix|M->, <key|escape escape> instead of <prefix|A-> and <prefix|math:greek>,
  <key|escape escape escape> or <prefix|A-C-> instead of <prefix|M-A->. For instance,
  <key|escape w> is equivalent to <key|A-w>. Uste puede también
  <apply|hyper-link|configurar los modificadores de
  teclado|../config/man-config-kbd-modkeys.es.tm> a fin de tomar ventaja
  total del poderoso conjunto de atajos de teclado que es proveido by
  <TeXmacs>.

  Note que el comportamiento de los menus de <TeXmacs> \ y el teclado son
  <em|contextuales>, <abbr|e.d.> ellos dependen del modo actual (e.j. mode
  texto o ``modo matemático''), el lenguaje actual y la posición del cursor
  dentro de su documento. Por ejemplo, dentro del modo matemático, usted
  tiene atajos de teclado especiales los cuales son útiles para teclear
  fórmulas matemáticas, pero inútiles en modo texto\ 

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
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
    <associate|language|spanish>
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

      <tuple|<tuple|<with|font family|<quote|ss>|Fichero>|<with|font
      family|<quote|ss>|Cargar>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Forma del tipo>|<with|font
      family|<quote|ss>|Cursiva>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
