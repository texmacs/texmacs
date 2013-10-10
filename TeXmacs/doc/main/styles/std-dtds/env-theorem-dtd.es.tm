<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Entornos tipo teorema>

  El <abbr|d.t.d.> <tmdtd|env-theorem> provee etiquetas para la capa de los
  entornos tipo teorema. Las etiquetas de \ mayor importancia son

  <\explain|<markup|render-theorem>>
    Un macro para mostrar entornos tipo teorema. El primer argumento
    especifica el nombre del teorema, como ``Teorema 1.2'' y el segundo
    argumento contiene el cuerpo del teorema. Este entorno es usado para
    entornos definidos por <markup|new-theorem>.
  </explain>

  <\explain|<markup|render-remark>>
    Similar a <markup|render-theorem>, pero para entornos tipo observación.
  </explain>

  <\explain|<markup|render-exercise>>
    Similar a <markup|render-theorem>, pero para entornos tipo ejercicio.
  </explain>

  <\explain|<markup|render-proof>>
    Similar a <markup|render-theorem>, pero para pruebas. Este entorno es
    principalmente usado para personalizar el nombre de una prueba, como en
    ``Fin de la prueba del teorema 1.2''.\ 
  </explain>

  <\explain|<markup|dueto>>
    Un entorno que puede ser usado para especificar los inventores de un
    teorema.
  </explain>

  <\explain|<markup|corollary*>>
    Para corolarios no numerados. Este entorno es basado en
    <markup|render-theorem>.
  </explain>

  <\explain|<markup|proof>>
    Para pruebas de teoremas. Este entorno está basado en
    <markup|render-proof>.
  </explain>

  Las siguientes etiquetas pueden ser usadas para personalización posterior
  de los entornos.

  <\explain|<markup|theorem-name>>
    Un macro que controla la apariencia de los nombres de los ambientes tipo
    teorema <em|y> tipo observación. La mayoría de los estilos usa fuente
    resaltada o pequeñas mayúsculas.
  </explain>

  <\explain|<markup|exercise-name>>
    Similar a <markup|theorem-name>, pero para ejercicios.
  </explain>

  <\explain|<markup|theorem-sep>>
    El separador entre el nombre de un entorno tipo teorema o tipo
    observación y su cuerpo principal. Por defecto, este es un punto seguido
    por un espacio.
  </explain>

  <\explain|<markup|exercise-sep>>
    Similar a <markup|theorem-sep>, pero para ejercicios.
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|spanish>
  </collection>
</initial>