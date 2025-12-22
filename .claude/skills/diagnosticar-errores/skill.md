---
name: diagnosticar-errores
description: Clasifica automáticamente errores post-renderizado en 4 categorías - Gráficos, Texto, Estructura, Coherencia.
---

# Skill: Diagnosticador de Errores Post-Renderizado

## Propósito
Analizar errores de compilación/renderizado y clasificarlos en una de las 4 categorías definidas para aplicar la solución correcta.

## Categorías de Errores

### 1. ERRORES DE GRÁFICOS (ERR_G)

| Código | Error | Patrón de Detección | Solución |
|--------|-------|---------------------|----------|
| ERR_G1 | No visualizadas | `File '*.png' not found` | Verificar include_tikz y rutas |
| ERR_G2 | Solapadas | Inspección visual requerida | Ajustar posicionamiento/márgenes |
| ERR_G3 | Renderizado incorrecto | Gráfico visible pero distorsionado | Revisar código TikZ/Python/R |
| ERR_G4 | Tamaño inadecuado | Gráfico muy grande/pequeño | Ajustar scale/width/height |

### 2. ERRORES DE TEXTO (ERR_T)

| Código | Error | Patrón de Detección | Solución |
|--------|-------|---------------------|----------|
| ERR_T1 | LaTeX no compila | `LaTeX failed to compile` | Revisar sintaxis LaTeX |
| ERR_T2 | Encoding incorrecto | Caracteres especiales mal renderizados | Configurar UTF-8 |
| ERR_T3 | Metadatos faltantes | `exname`, `extype` ausentes | Completar Meta-information |

### 3. ERRORES DE ESTRUCTURA (ERR_S)

| Código | Error | Patrón de Detección | Solución |
|--------|-------|---------------------|----------|
| ERR_S1 | Opciones incorrectas | Menos de 4 opciones, duplicados | Regenerar opciones |
| ERR_S2 | Solución no coincide | exsolution no corresponde | Recalcular respuesta correcta |

### 4. ERRORES DE COHERENCIA (ERR_C)

| Código | Error | Patrón de Detección | Solución |
|--------|-------|---------------------|----------|
| ERR_C1 | Coherencia matemática | Fórmulas/cálculos incorrectos | Revisar lógica matemática |
| ERR_C2 | Coherencia imagen-texto | Descripción no coincide con gráfico | Alinear texto y visual |
| ERR_C3 | Coherencia de código | Variables R/Python/TikZ desincronizadas | Sincronizar parámetros |

## Algoritmo de Diagnóstico

```r
diagnosticar_error <- function(mensaje_error, archivo_rmd) {
  
  # Paso 1: Clasificar por patrones conocidos
  if (grepl("not found|File.*png", mensaje_error)) {
    return(list(categoria = "GRAFICOS", codigo = "ERR_G1"))
  }
  
  if (grepl("LaTeX failed|undefined control sequence", mensaje_error)) {
    return(list(categoria = "TEXTO", codigo = "ERR_T1"))
  }
  
  if (grepl("non-numeric argument|abs\\(.*formateado", mensaje_error)) {
    return(list(categoria = "COHERENCIA", codigo = "ERR_C3"))
  }
  
  # Paso 2: Si no hay patrón conocido, analizar archivo
  contenido <- readLines(archivo_rmd)
  
  # Verificar estructura
  if (!any(grepl("^exname:", contenido))) {
    return(list(categoria = "TEXTO", codigo = "ERR_T3"))
  }
  
  # Paso 3: Error no clasificado
  return(list(categoria = "DESCONOCIDO", codigo = "ERR_X"))
}
```

## Flujo de Diagnóstico

```
Error detectado
    ↓
Analizar mensaje de error
    ↓
┌─────────────────────────────────────┐
│ ¿Patrón conocido?                   │
│   Sí → Clasificar automáticamente   │
│   No → Analizar archivo .Rmd        │
└─────────────────────────────────────┘
    ↓
Generar diagnóstico:
  - Categoría del error
  - Código específico
  - Solución recomendada
  - Skill a activar
    ↓
Activar skill de corrección apropiado
```

## Salida del Diagnóstico

```
╔════════════════════════════════════════╗
║     DIAGNÓSTICO DE ERROR               ║
╠════════════════════════════════════════╣
║ Categoría: GRÁFICOS                    ║
║ Código: ERR_G1                         ║
║ Descripción: Gráficas no visualizadas  ║
║ Causa: include_tikz en chunk incorrecto║
║                                        ║
║ Solución recomendada:                  ║
║ → Usar renderizado condicional         ║
║ → Ver: patrones-errores-conocidos.md   ║
║                                        ║
║ Skill a activar: corregir-graficos     ║
╚════════════════════════════════════════╝
```

## Integración

- **Entrada**: Mensaje de error de validar-renderizado
- **Salida**: Diagnóstico clasificado + skill recomendado
- **Siguiente paso**: Activar skill de corrección específico

## Referencias

- `.claude/docs/patrones-errores-conocidos.md`
- `.claude/Mermaid_Chart.txt` (categorías de errores)

