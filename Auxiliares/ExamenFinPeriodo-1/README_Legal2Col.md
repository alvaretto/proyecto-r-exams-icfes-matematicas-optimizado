# Examen Fin de Periodo - Formato Legal a Doble Columna

**ACTUALIZACIÓN IMPORTANTE**: Hemos cambiado completamente el enfoque para solucionar los problemas con las tablas en el entorno de doble columna. En lugar de modificar las plantillas LaTeX, ahora utilizamos un script R que modifica el archivo .tex generado por R-exams antes de compilarlo con LaTeX. Esto nos permite reemplazar todas las ocurrencias de `longtable` por `tabular` de manera más efectiva y evitar los errores que estábamos encontrando.

Este directorio contiene plantillas y scripts para generar exámenes en formato legal (oficio) con diseño a doble columna y márgenes estrechos.

## Características

- **Tamaño de papel**: Legal (216mm x 356mm)
- **Márgenes**: 10mm en todos los lados
- **Formato**: Doble columna con línea separadora
- **Compatibilidad**: Imágenes y tablas centradas automáticamente

## Archivos incluidos

1. **Scripts R**:
   - `ExamenLegal2Col.R`: Script simple para generar exámenes en formato legal a doble columna
   - `SemilleroLegal2Col.R`: Script completo para generar exámenes en diferentes formatos

## Uso

1. Asegúrese de tener instalado R y los paquetes necesarios:
   ```R
   install.packages(c("exams", "knitr", "rmarkdown"))
   ```

2. Ejecute uno de los scripts R para generar los exámenes:
   ```R
   # Para un ejemplo simple
   source("ExamenLegal2Col.R")

   # Para generar exámenes completos
   source("SemilleroLegal2Col.R")
   ```

3. Los archivos generados se guardarán en el directorio `salida`.

## Personalización

### Para imágenes que ocupen ambas columnas

Para incluir imágenes que ocupen el ancho completo de la página (ambas columnas), utilice el entorno `widefigure` en lugar de `figure`:

```latex
\begin{widefigure}
\centering
\includegraphics[width=0.8\textwidth]{imagen.png}
\caption{Descripción de la imagen}
\end{widefigure}
```

### Para tablas que ocupen ambas columnas

Para incluir tablas que ocupen el ancho completo de la página (ambas columnas), utilice el entorno `widetable` en lugar de `table`:

```latex
\begin{widetable}
\centering
\begin{tabular}{|c|c|c|}
% contenido de la tabla
\end{tabular}
\caption{Descripción de la tabla}
\end{widetable}
```

### Sobre las tablas largas (longtable)

El script R modifica el archivo .tex generado por R-exams para reemplazar todas las ocurrencias de `longtable` por `tabular` dentro de un entorno `table`. Esto significa que puede seguir usando `longtable` en su código como lo haría normalmente, y el script se encargará de convertirlo a un formato compatible con el entorno de doble columna.

```latex
\begin{longtable}{|c|c|p{5cm}|}
\hline
\textbf{Columna 1} & \textbf{Columna 2} & \textbf{Columna 3} \\
\hline
% contenido de la tabla
\hline
\end{longtable}
```

**Notas importantes**:

1. Esta solución tiene una limitación: las tablas ya no podrán dividirse automáticamente entre páginas. Si tiene una tabla muy grande, considere dividirla manualmente en varias tablas más pequeñas.

2. El script también elimina los comandos específicos de `longtable` como `\endhead`, `\endfirsthead`, `\endfoot` y `\endlastfoot`, por lo que puede seguir usándolos en su código sin problemas.

3. El formato de columnas se convierte a `{ccc}` (tres columnas centradas) para garantizar la compatibilidad con el formato de doble columna. Si necesita un formato diferente, puede modificar el script R.

## Notas importantes

- Las imágenes y tablas regulares se centrarán automáticamente dentro de su columna.
- Para elementos que requieran más espacio, utilice los entornos `widefigure` y `widetable`.
- El formato a doble columna optimiza el espacio y permite incluir más contenido en cada página.
- El tamaño legal (oficio) proporciona más espacio vertical que el tamaño carta estándar.
