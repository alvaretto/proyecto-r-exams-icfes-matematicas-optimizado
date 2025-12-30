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

3. **NO crear ejercicios con < 250 versiones únicas**
   - Validar con `exams2html("archivo.Rmd", n = 300)`
   - Verificar diversidad de parámetros aleatorios

4. **NO omitir validación en los 4 formatos**
   - HTML, PDF, DOCX, NOPS son OBLIGATORIOS

5. **NO modificar ejemplos funcionales**
   - Los archivos en `/A-Produccion/Ejemplos-Funcionales-Rmd/` son INMUTABLES
   - Solo copiar patrones, nunca editar directamente

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
exshuffle: TRUE
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
