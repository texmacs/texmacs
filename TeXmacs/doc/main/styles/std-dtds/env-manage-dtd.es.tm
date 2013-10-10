<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Definir nuevos entornos>

  El <abbr|d.t.d> <tmdtd|env-manage> contiene etiquetado de alto nivle el
  cual puede ser usado por el usuario para defninir nuevos entornos para
  teoremas, ejercicios y figuras:

  <\explain|<markup|new-theorem>>
    Define un entorno tipo teorema. Usted debe especificar un nombre para el
    entorno (como ``experimento'') y el texto correspondiente (como
    ``Experimento'').
  </explain>

  <\explain|<markup|new-remark>>
    Similar a <markup|new-theorem>, pero para avisos.
  </explain>

  <\explain|<markup|new-exercise>>
    Similar a <markup|new-theorem>, pero para ejercicios.
  </explain>

  <\explain|<markup|new-figure>>
    Similar a <markup|new-theorem>, pero para figuras (en pares grandes y
    pequeños).
  </explain>

  El <abbr|d.t.d.> también contienen etiquetado de bajo nivel para las
  definiciones actuales de los entornos. De hecho, la definición de nuevos
  teoremas es hecha en dos estados. En un primer estado, la etiqueta
  <markup|new-theorem> es usada a fin de especificar que un entorno tipo
  teorema debería ser definido. En un segundo estado (justo antes de que el
  documento del usuario es procesado) los entornos tipo teorema son
  definidos. Este mecanismo hace posible personalizar los entornos en
  paquetes que son procesados entre los dos estados. Por ejemplo, la
  numeración de teoremas es personalizada de esta forma.

  <\warning>
    En el momento, usted debería sólo usar la etiqueta <markup|new-theorem> y
    similares dentro de archivos o paquetes de estilo personales. Si usa
    <markup|new-theorem> directamente dentro de un documento, entoces la
    numeración puede ser incorrecta, debudo a el esquema de dos estados
    explicados arriba. Esta inconveniencia desaparecerá tan prnto como sea
    posible especificar preámbulos límpios para documentos <TeXmacs>.
  </warning>

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