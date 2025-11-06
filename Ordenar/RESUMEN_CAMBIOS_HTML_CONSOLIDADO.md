# Resumen de Cambios - HTML Consolidado

## Objetivo
Modificar el archivo `SemilleroCloze.R` para que cuando se ejecute la función `exams2html()` solicitando generar n preguntas, genere un único archivo HTML que contenga todas las n preguntas en una sola página, en lugar de crear n archivos HTML individuales.

## Problema Original
El código original en `SemilleroCloze.R` utilizaba:
```r
exams2html(archivo_examen,
           n = config$archivos,  # Esto genera n archivos separados
           name = paste0(nombre_base, "_html"),
           ...)
```

**Resultado**: Se generaban 350 archivos HTML individuales (uno por pregunta).

## Solución Implementada
Basándome en el análisis de los archivos `SemilleroUnico_v2.R` y `SemilleroMoodle_v2.R`, encontré la clave en la línea 56 de `SemilleroUnico_v2.R`:

```r
exams2html(rep(archivo_examen, numpreg),  # La clave está aquí
           svg = FALSE,
           verbose = TRUE,
           template = "plain",
           name = paste0(nombre_sin_extension, "_semillero"))
```

## Cambios Realizados

### 1. Modificación de la función `generar_html()`
**Antes:**
```r
resultado <- exams2html(archivo_examen,
                       n = config$archivos,
                       name = paste0(nombre_base, "_html"),
                       ...)
```

**Después:**
```r
resultado <- exams2html(rep(archivo_examen, config$archivos),
                       name = paste0(nombre_base, "_consolidado"),
                       template = "plain",
                       mathjax = TRUE,
                       svg = FALSE,
                       ...)
```

### 2. Cambios específicos:
- **Parámetro principal**: Cambié de `archivo_examen, n = config$archivos` a `rep(archivo_examen, config$archivos)`
- **Nombre del archivo**: Cambié de `"_html"` a `"_consolidado"` para claridad
- **Template**: Agregué `template = "plain"` para mejor compatibilidad
- **MathJax**: Agregué `mathjax = TRUE` para soporte matemático
- **SVG**: Agregué `svg = FALSE` para evitar problemas de renderizado
- **Comentarios**: Agregué comentarios explicativos en español

### 3. Mejoras en la información mostrada:
- Cambié los mensajes de "archivos HTML" a "archivo HTML consolidado"
- Agregué información sobre el tamaño del archivo generado
- Mejoré la descripción del proceso

## Explicación Técnica

### ¿Por qué funciona `rep(archivo_examen, n)`?
- `rep(archivo_examen, n)` crea un vector con n repeticiones del nombre del archivo
- `exams2html()` interpreta esto como un solo examen con n ejercicios
- Resultado: 1 archivo HTML con n preguntas secuenciales

### ¿Por qué no funciona `archivo_examen, n = n`?
- El parámetro `n` le dice a `exams2html()` que genere n versiones diferentes del examen
- Cada versión se guarda en un archivo HTML separado
- Resultado: n archivos HTML individuales

## Archivos Modificados
1. **`SemilleroCloze.R`**: Función `generar_html()` modificada (líneas 130-173)
2. **Comentarios agregados**: Explicación de los cambios en el header del archivo

## Archivos Creados para Prueba
1. **`test_html_consolidado.R`**: Script de prueba que demuestra la diferencia
2. **`RESUMEN_CAMBIOS_HTML_CONSOLIDADO.md`**: Este archivo de documentación

## Resultado Final
- **Antes**: 350 archivos HTML individuales (ej: `archivo1.html`, `archivo2.html`, ...)
- **Después**: 1 archivo HTML consolidado con 350 preguntas secuenciales

## Ventajas del Cambio
1. **Facilidad de uso**: Un solo archivo para revisar todas las preguntas
2. **Mejor organización**: No hay cientos de archivos dispersos
3. **Navegación secuencial**: Las preguntas aparecen una tras otra en el mismo documento
4. **Menor fragmentación**: Menos archivos en el sistema de archivos
5. **Compatibilidad**: Mantiene toda la funcionalidad de R/exams

## Prueba de Funcionamiento
Ejecutar el script `test_html_consolidado.R` para ver la diferencia entre ambos métodos con un ejemplo de 5 preguntas.

## Notas Importantes
- La funcionalidad para otros formatos (Moodle, PDF, etc.) permanece sin cambios
- Solo se modificó la generación de HTML
- La semilla aleatoria se mantiene para reproducibilidad
- Todos los parámetros de configuración existentes siguen funcionando
