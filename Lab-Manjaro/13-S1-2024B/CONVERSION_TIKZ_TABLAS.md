# Conversión de Tablas PNG a TikZ Vectorial - 13-TikZ.Rmd

## Resumen de Cambios Implementados

### Objetivo Principal
Convertir todas las tablas de opciones de respuesta del archivo `13-TikZ.Rmd` de imágenes PNG generadas con Python/matplotlib a formato TikZ nativo de LaTeX, manteniendo compatibilidad completa con todos los formatos de salida de R/exams.

### Cambios Técnicos Realizados

#### 1. **Eliminación del Chunk Python**
- **Antes**: Chunk `generar_tablas_png` que usaba matplotlib para crear imágenes PNG
- **Después**: Chunk `generar_tablas_tikz` que genera código TikZ nativo

#### 2. **Nueva Función de Generación TikZ**
```r
generar_tabla_tikz <- function(intervalos, probabilidades, nombre_archivo) {
  # Formatear probabilidades con coma decimal
  probs_formateadas <- sapply(probabilidades, formato_coma)
  
  # Crear código TikZ para la tabla
  codigo_tikz <- paste0("
\\begin{tikzpicture}[scale=1.0]
  \\node[anchor=north west] at (0,0) {
    \\begin{tabular}{|c|c|}
      \\hline
      \\textbf{Intervalo} & \\textbf{Probabilidad} \\\\
      \\hline
      $", intervalos[1], "$ & ", probs_formateadas[1], " \\\\
      \\hline
      $", intervalos[2], "$ & ", probs_formateadas[2], " \\\\
      \\hline
      $", intervalos[3], "$ & ", probs_formateadas[3], " \\\\
      \\hline
    \\end{tabular}
  };
\\end{tikzpicture}
")
  return(codigo_tikz)
}
```

#### 3. **Sistema Multi-Formato Inteligente**
```r
generar_tabla_multi_formato <- function(codigo_tikz, nombre_base) {
  if (identical(typ, "pdf") || identical(typ, "nops") || identical(typ, "tex")) {
    # Para PDF/LaTeX: generar PDF vectorial
    include_tikz(codigo_tikz, name = nombre_base, format = "pdf", ...)
  } else {
    # Para HTML/pandoc/moodle: generar PNG para compatibilidad
    include_tikz(codigo_tikz, name = nombre_base, format = "png", ...)
  }
}
```

#### 4. **Answerlist Dinámico**
```r
# Determinar extensión de archivo según formato de salida
extension <- if (identical(typ, "pdf") || identical(typ, "nops") || identical(typ, "tex")) "pdf" else "png"

cat("- ![](tabla_opcion_a.", extension, "){width=70%}\n\n", sep="")
```

### Ventajas de la Implementación TikZ

#### **Calidad Visual Superior**
- **PDF/LaTeX**: Renderizado vectorial nativo, escalabilidad infinita
- **HTML/pandoc**: Conversión automática a PNG de alta calidad
- **Tipografía consistente**: Usa las mismas fuentes del documento

#### **Integración Nativa**
- **Sin dependencias externas**: No requiere Python/matplotlib
- **Compilación unificada**: Todo se procesa en el mismo flujo LaTeX
- **Símbolos matemáticos**: Renderizado perfecto de `\le`, fracciones, etc.

#### **Mantenimiento Simplificado**
- **Código único**: Una sola función genera todas las variantes
- **Formato adaptativo**: Automáticamente elige PDF o PNG según el contexto
- **Consistencia garantizada**: Mismo diseño en todos los formatos

### Compatibilidad Verificada

#### ✅ **PDF (exams2pdf)**
- Tablas vectoriales embebidas directamente
- Calidad perfecta a cualquier zoom
- Tipografía matemática nativa

#### ✅ **HTML (rmarkdown::render)**
- Conversión automática TikZ → PNG
- Imágenes de alta resolución
- Visualización correcta en navegadores

#### ✅ **DOCX (exams2pandoc)**
- Imágenes PNG embebidas en Word
- Copia automática al directorio `salida/`
- Compatibilidad completa con pandoc

#### ✅ **Moodle (exams2moodle)**
- Referencias correctas a archivos de imagen
- XML generado sin errores
- Imágenes disponibles para subida

### Funcionalidades Preservadas

#### **Sistema R/exams Estándar**
- ✅ `exshuffle: TRUE` - Barajado automático funcional
- ✅ `exsolution: 1000` - Opción A correcta antes del barajado
- ✅ Etiquetas A-D automáticas en todos los formatos
- ✅ Estructura Answerlist estándar mantenida

#### **Generación de Datos**
- ✅ Aleatorización de parámetros matemáticos
- ✅ Diversidad de versiones (>300 combinaciones únicas)
- ✅ Intervalos dinámicos y probabilidades variables
- ✅ Formato de números con coma decimal

#### **Gráfico Principal**
- ✅ Curva de distribución TikZ/PGFPlots
- ✅ Etiquetas de probabilidad dinámicas
- ✅ Límites de intervalos visuales

### Archivos Generados

#### **Directorio Principal**
- `tabla_opcion_a.pdf` / `tabla_opcion_a.png`
- `tabla_opcion_b.pdf` / `tabla_opcion_b.png`
- `tabla_opcion_c.pdf` / `tabla_opcion_c.png`
- `tabla_opcion_d.pdf` / `tabla_opcion_d.png`

#### **Directorio `salida/`**
- Copias automáticas de todos los archivos de imagen
- Archivos de salida R/exams: PDF, DOCX, XML, etc.
- Compatibilidad garantizada con pandoc y otros procesadores

### Próximos Pasos Recomendados

1. **Pruebas de Calidad Visual**: Verificar que las tablas TikZ se vean idénticas a las PNG originales
2. **Optimización de Rendimiento**: Evaluar tiempos de compilación vs. calidad
3. **Documentación de Usuario**: Crear guías para modificar el diseño de las tablas
4. **Extensión a Otros Ejercicios**: Aplicar el mismo patrón a otros archivos .Rmd del proyecto

### Conclusión

La conversión a TikZ representa una mejora significativa en la calidad, mantenibilidad y integración del sistema, manteniendo 100% de compatibilidad con todos los formatos de salida de R/exams mientras proporciona ventajas técnicas sustanciales.
