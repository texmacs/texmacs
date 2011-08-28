<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Uso básico>

  Una sesión puede ser iniciada desde el menú <apply|menu|Insert|Session>.
  Una sesión consiste de una secuencia de entornos de entrada y salida y de
  un posible texto entre ellos. Cuanto teclea <key|enter> dentro de un
  entorno de entrada, el texto dentro del entorno es evaluado y el resultado
  es mostrado como un entorno de salida.

  Cuando se ingresa un comando en una sesión, la aplicación intenta
  ejecutarlo. Varios comandos pueden ser lanzados concurrentemente en el
  mismo documento, pero la salida sólo será activada en la sesión donde el
  cursor está y en la posición del cursor. Por tanto, recomendamos usar
  diferentes <em|buffers> para ejecuciones paralelas. Las ejecuciones pueden
  ser interrumpidas desde la barra de iconos. También es posible desconectar
  (cerrar) la aplicación; en este caso ningún comando posterior puede ser
  ejecutado en la sesión correspondiente.

  En la segunda barra de iconos usted puede tener también unos pequeños
  botones para seleccionar entradas matemáticas e interrumpir la ejecución.
  Cuando está implementada para un sistema dado, la entrada matemática le
  permite teclear la entrada en una forma gráfica bidimensional. Los otros
  dos botones le permite interrumpir la ejecución de un comando particular
  (aunque estno no trabaja bien para ciertos sistemas) o desconectar el
  sistema externo. Cuando presiona enter en la entrada de un sistema no
  conectado, el sistema será reiniciado automáticamente.

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
      family|<quote|ss>|Sesión>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
