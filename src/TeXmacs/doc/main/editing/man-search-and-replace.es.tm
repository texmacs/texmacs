<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Buscar y sustituir>

  Puede iniciar una búsquesda de texto tecleando <key|C-s> o
  <apply|menu|Edit|Search>. Durante una búsqueda la ``cadena de búsqueda'' se
  muestra en el lado izquierdo de la parte inferior de la ventana. La
  búsqueda es incremental, lo que significa que cada carácter que introduce
  se añade a la cadena de búsqueda y su siguiente aparición queda rodeada por
  una caja de bordes rojos. Cada vez que teclea <key|C-s> el programa busca
  la siguiente aparición de la palabra. Si <TeXmacs> no encuentra más
  apariciones de la cadena en el documento, debería oir un bip. La búsqueda
  es también cíclica, en el sentido de que, si llegado ese momento, vuelve a
  pulsar <key|C-s> continuará desde comienzo del documento. Puede teclear
  <key|backspace> para deshacer la digitación de teclas durante una búsqueda.

  La búsqueda descrita es hacia adelante, empezando en la posición actual del
  cursor. También puede buscar hacia atrás, utilizando <key|C-r>. Las
  búsquedas se producen sólo en texto que esté en el mismo modo e idioma que
  los activos en la posición de comienzo. En otras palabras, al buscar
  <with|mode|math|x> en modo matemático no encontrarás ninguna x de texto
  ordinario. Una limitación actual es que la cadena de búsqueda puede
  contener sólo texto ordinario y no símbolos matemáticos o texto
  estructurado más complicado.

  La búsqueda con sustitución se arranca tecleando <key|C-=> o
  <apply|menu|Edit|Replace>. <TeXmacs> le pide la cadena que quiere sustituir
  y la cadena que aparecerá en su lugar. Cada vez que se encuentre una de las
  cadenas a sustituir se le pedirá que elija entre sustituirla (y), no
  sustituirla (n) y sustituir todas las apariciones siguientes (a). Como
  antes, la búsqueda con sustitución afecta sólo al texto que esté en el
  mismo modo e idioma.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Álvaro Cantero
  Tejero|Pablo Ruiz Múzquiz|David Moriano Garcia|Offray Vladimir Luna
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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Buscar>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Sustituir>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
