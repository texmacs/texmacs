<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Formato de documento \ (0.3.4)>

  El formato de documento TeXmacs ha cambiado profundamente a fin de hacer
  TeXmacs compatible con XML en el futuro. Principalmente, los viejos estilos
  de entornos como

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>environment\|open\|close\<gtr\>\<gtr\>,
  </verbatim>

  que son aplicados vía pares coincidentes
  <verbatim|\<less\>begin\|env\<gtr\>text\<less\>end\|env\<gtr\>>, han sido
  reemplazados por macros\ 

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>macro\|body\|open\<less\>body\<gtr\>close\<gtr\>\<gtr\>,
  </verbatim>

  que son aplicados via expasiones simples de macro
  <verbatim|\<less\>expand\|env\|text\<gtr\>>. Similarmente, los pares
  coincidentes <verbatim|\<less\>set\|var\|val\<gtr\>text\<less\>reset\|var\<gtr\>>
  de cambios de variables de entorno son reemplazados por un constructo
  <verbatim|\<less\>with\|var\|val\|text\<gtr\>> (cercanos a los atributos
  XML). Desde un punto de vista técnico, estos cambios permiten varias
  complicaciones si e cuerpo de <verbatim|text> consite de varios párrafos.
  Como una consecuencia, documentos malamente estructurados pueden algunas
  veces mostrarse diferente en una nueva versión (aunque yo sólo noté un
  cambio menor en mis propios documentos). Más aún, a fin de mantener un alto
  nivel de estructura en el documento, el comportamiento del editor con
  respecto a ambientes multipárrafo ha cambiado ligeramente.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>
