# Reglas para Código R/Markdown

## Pre-Edit/Write: Regla de Oro

Antes de editar cualquier archivo .Rmd, verifica OBLIGATORIAMENTE:

- ✓ Entendiste completamente el error a corregir
- ✓ Verificaste solución en ejemplos funcionales (`/A-Produccion/Ejemplos-Funcionales-Rmd/`)
- ✓ NO harás cambios innecesarios o experimentales
- ✓ Validarás en los 4 formatos después del cambio

## Advertencias Críticas

### ❌ NUNCA hacer:

1. **NO usar `include_tikz()` sin renderizado condicional**
   ```r
   # MAL - falla en HTML
   include_tikz("grafico.tex")

   # BIEN - renderizado condicional
   if (knitr::is_latex_output()) {
     include_tikz("grafico.tex")
   } else {
     knitr::include_graphics("grafico.png")
   }
   ```

2. **NO mezclar código Python/R sin validar en ambos**
   - Siempre probar con reticulate activo e inactivo
   - Validar que variables se transfieren correctamente

3. **NO crear ejercicios con < 200 versiones únicas**
   - Validar con `exams2html("archivo.Rmd", n = 200)`
   - Verificar diversidad de parámetros aleatorios
   - Razón del umbral: ejercicios con restricciones algebraicas fuertes
     (cubos perfectos, primos, etc.) no pueden generar más sin degradar
     la diagnosticidad de los distractores

4. **NO omitir validación en los 4 formatos**
   - HTML, PDF, DOCX, NOPS son OBLIGATORIOS

5. **NO modificar ejemplos funcionales**
   - Los archivos en `/A-Produccion/Ejemplos-Funcionales-Rmd/` son INMUTABLES
   - Solo copiar patrones, nunca editar directamente

6. **NO usar `exshuffle: FALSE`**
   ```yaml
   # ❌ MAL - permite patrones predecibles
   exshuffle: FALSE

   # ✓ BIEN - mezcla aleatoria obligatoria
   exshuffle: TRUE
   ```
   **Razón**: ICFES requiere distractores avanzados. `exshuffle: TRUE` garantiza:
   - Opciones de respuesta mezcladas aleatoriamente
   - Estudiante no puede identificar patrones visuales/textuales
   - Obliga al estudiante a analizar cada opción individualmente
   - Genera versiones únicas en cada renderizado

## ⚠️ 5 Coherencias OBLIGATORIAS

Antes de aprobar cualquier ejercicio, verificar las 5 coherencias:

### 1. Coherencia Semántica
- Gramática correcta en español
- Sin errores ortográficos en etiquetas y texto
- Redacción clara y precisa estilo ICFES
- Terminología matemática apropiada

### 2. Coherencia Visual-Texto
- Gráfico coincide exactamente con el enunciado
- Valores en el gráfico = valores en el texto
- Etiquetas del gráfico consistentes con la pregunta
- Colores/estilos descritos coinciden con lo renderizado

### 3. Coherencia Matemática
- Fórmulas correctas y bien formateadas
- Cálculos verificables paso a paso
- Proporciones y escalas correctas en gráficos
- Respuesta correcta es matemáticamente válida
- Distractores son plausibles pero incorrectos

### 4. Coherencia de Código
- Código dinámico (usa variables aleatorias)
- Compatible con R-exams en 4 formatos
- Sin dependencias externas no declaradas
- Gráficos generados programáticamente (no estáticos)
- Variables R interpoladas correctamente en TikZ/Python

### 5. Coherencia General
- Legible en todos los formatos (HTML, PDF, DOCX, NOPS)
- Estilo visual consistente con estándares ICFES
- Dificultad apropiada al nivel declarado (n1-n4)
- Tiempo de resolución razonable para el contexto

## ✓ SIEMPRE hacer:

- Validar gráficos dinámicos en PDF Y HTML
- Consultar ejemplos funcionales antes de cualquier corrección
- Ejecutar ciclo completo de validación (FASE 1→2→3)
- Documentar solo después de confirmar solución 100%
- Usar metadatos ICFES completos (6 dimensiones)

## Metadatos ICFES Requeridos

Todo ejercicio DEBE incluir:
```yaml
exname: [Nombre descriptivo]
extype: [schoice|cloze]
exsolution: [Respuesta correcta]
exshuffle: TRUE          # ⚠️ OBLIGATORIO - NUNCA usar FALSE
extol: 0.01

# Metadatos ICFES (6 dimensiones OBLIGATORIAS)
exextra[Type]: [SCHOICE|CLOZE]
exextra[Competencia]: [Interpretación|Formulación|Argumentación]
exextra[Componente]: [Aleatorio|Cambio|Datos|Espacial|Medida]
exextra[Afirmacion]: [Descripción específica]
exextra[Evidencia]: [Descripción específica]
exextra[Nivel]: [1|2|3|4]
```

## Ejemplos Funcionales = Fuente de Verdad

Ante cualquier error, SIEMPRE consultar primero:
```bash
/A-Produccion/Ejemplos-Funcionales-Rmd/
```

Los ejemplos funcionales son la ÚNICA fuente de verdad para patrones de solución validados.
