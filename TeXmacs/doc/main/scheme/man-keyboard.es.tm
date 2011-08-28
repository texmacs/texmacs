<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Crear sus propios atajos de teclado>

  Los mapas de teclas son especificados usando el comando

  <\verbatim>
    \ \ \ \ (set-keymap (lista de predicados) (list of keymaps))
  </verbatim>

  La lista de los predicados espeficica bajo cuales circunstancias los mapas
  de teclas son validas. Ejemplso de predicados son <verbatim|always?>,
  <verbatim|in-math?> y <verbatim|in-french?>, pero el usuario puede definir
  sus propios predicados. Cada mapa es de una de las seguiente formas

  <\verbatim>
    \ \ \ \ (combinacion-de-teclas accion_1 ... accion_n)<format|next line>
    \ \ \ (combinacion-de-teclas resultado)<format|next line>
    \ \ \ (combinacion-de-teclas resultado mensage-ayuda)
  </verbatim>

  En el primer caso, las <verbatim|accion_i> son comandos de <name|Scheme>
  asociados con la cadena <verbatim|combinacion-de-teclas><verbatim|>. En el
  segundo y tercer caso, <verbatim|resultado> es una cadena que está para ser
  insertada en el texto cuando la <verbatim|combinacion-de-teclas> ha sido
  completada. Un <verbatim|mensage-ayuda> opcional puede ser mostrado cuando
  la <verbatim|combinacion-de-teclas> es finalizada.

  Los mapas de teclas pueden ser limpiados de nuevo usando el comando

  <\verbatim>
    \ \ \ \ (remove-keymap (lista de predicados) (lista de combinaciones de
    teclas))
  </verbatim>

  donde el segundo argumento es una lista de cadenas, cada uno de las cuales
  es una <verbatim|combinacion-de-teclas> como las anteriores.

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
