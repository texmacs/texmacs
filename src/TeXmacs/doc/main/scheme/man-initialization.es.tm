<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Escribir sus propios archivos de inicialización>

  Cuando inicia, <TeXmacs> ejecuta el archivo
  <verbatim|$TEXMACS_PATH/progs/init-texmacs.scm>, a menos que usted haya
  especificado su propio archivo de inicialización en
  <verbatim|$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm>. Pro defecto, la
  ruta <verbatim|$TEXMACS_HOME_PATH> iguala a <verbatim|.TeXmacs>.
  Usualmente, usted quiere añadir algunas acciones extra al archivo de
  inicialización por defecto. En este caso, no debería olvidar incluir el
  comando

  <\verbatim>
    \ \ \ \ (exec-file "$TEXMACS_PATH/progs" "init-texmacs.scm")
  </verbatim>

  en su archivo de inicialización personal. Similarmente, el archivo
  <verbatim|$TEXMACS_PATH/progs/init-buffer.scm> es ejecutado, cada vez que
  usted crea un nuevo <em|buffer>, a menos que usted provea su propio archivo
  de inicialización <verbatim|$TEXMACS_HOME_PATH/progs/my-init-buffer.scm>

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
