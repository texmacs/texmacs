<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Personalizando los estilos de las sesiones>

  Cada entorno de sesión toma dos argumentos: El lenguaje de programación y
  el nombre para una sesión. Todas entrada a evaluarse es redireccionada al
  paquete que eimplementa el lenguaje de programación y el nombre de la
  sesión es pasado como un argumento extra.

  Es posible redefinir el esquema de pantalla de una sesión como sigue: Para
  cada sesión en un lenguaje de programación `p', usted tiene entonces que
  modificar los entornos `entrada-p' y `salida-p' los cuales corresponden
  respectivamente al esquema de la entrada y la salida. El primer argumento
  para `entrada-p' es el <em|prompt> para la entrada.

  Es posible darle a las sesiones un nombre (siendo el nombre por defecto
  ``default''): haciendo <em|click> en <apply|menu|Insert|Session|Other>, uno
  puede tanto insertar un nombre de sesión como un tipo de sesión. Sesiones
  diferentes que tienen el mismo tipo y el mismo nombre compartido
  corresponden a una instancia de la aplicación que está siendo ejecutada.
  Recíprocamente, tales seciones comparten un ambiente común. Mediante el uso
  de nombres diferentes de sesiones, uno puede lanzar concurrentemente varias
  intancias de la misma aplicación.

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
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Sesión>|<with|font
      family|<quote|ss>|Otro>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
