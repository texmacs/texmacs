<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Deshacer y rehacer>

  Es posible deshacer gradualmente todos los cambios que haya hecho en el
  documento hasta llegar al momento de lanzar <TeXmacs>, usando
  <apply|menu|Edit|Undo> o las combinaciones <shortcut|(undo 0)> o <shortcut|(undo 0)>.
  Los cambios deshechos pueden ser ``rehechos`` mediante
  \ <apply|menu|Edit|Redo> o <shortcut|(redo 0)>.

  Para ahorrar memoria el número de acciones sucesivas que se pueden deshacer
  está limitado por defecto a 100. Es posible incrementar este número,
  añadiendo un comando como:

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  en nuestro archivo personal de arranque (ver <apply|menu|Help|Scheme> ).
  Cuando se especifica un número negativo <TeXmacs> permite deshacer
  <em|todas> las acciones de la sesión.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Deshacer>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Rehacer>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ayuda>|<with|font
      family|<quote|ss>|Scheme>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
