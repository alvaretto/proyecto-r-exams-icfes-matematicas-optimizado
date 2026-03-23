---
name: generar-schoice
description: >
  Genera ejercicio R-exams tipo SCHOICE (seleccion unica) METACOGNITIVO.
  TODO ejercicio debe aplicar Progressive Disclosure y analisis de errores conceptuales.
  Usa cuando el analisis ICFES indica schoice, necesites ejercicio de opciones multiples,
  o quieras crear pregunta con 1 respuesta correcta y 3+ distractores.
  SIEMPRE consulta ejemplos funcionales ANTES de generar codigo.
license: Proyecto Educativo - IE Pedacito de Cielo
compatibility: Requiere R (>= 4.0), tinytex, paquetes exams y tidyverse. Linux/macOS.
metadata:
  author: alvaretto
  version: "3.0"
  language: es
  model_recommendation: opus
allowed-tools:
  - Read
  - Write
  - Edit
  - Grep
  - Glob
  - Bash(ls:*)
  - Bash(mkdir:*)
  - Bash(Rscript:*)
---

# Generador de Ejercicios SCHOICE Metacognitivos

## REGLA CRÍTICA

**⚠️ TODO ejercicio SCHOICE DEBE ser metacognitivo con Progressive Disclosure.**

Ver regla completa: `.claude/rules/ejercicios-metacognitivos.md`

## Decision Tree

```
User task -> Tiene analisis ICFES?
    |-- No -> Ejecutar /analizar-icfes primero
    +-- Si -> Tiene graficos?
        |-- Si -> Preguntar version grafica (TikZ/Python/R)
        |        +-- Consultar ejemplos funcionales similares con grafico
        +-- No -> Consultar ejemplos funcionales similares sin grafico
                 +-- Seleccionar PATRON METACOGNITIVO
                    |-- Patron 1: Analisis de Error Ajeno
                    |-- Patron 2: Evaluacion de Afirmacion
                    +-- Patron 3: Comparacion de Procedimientos
                 +-- Generar .Rmd con nomenclatura oficial
                    +-- Validar: Rscript scripts/validar-renderizado.R
```

## Proceso paso a paso

### PASO 0: Seleccionar patron metacognitivo (OBLIGATORIO)

| Patron | Cuando usar | Bloom | DOK |
|--------|-------------|-------|-----|
| **Analisis de Error Ajeno** | Ejercicios de calculo donde hay errores comunes | Analizar/Evaluar | 3 |
| **Evaluacion de Afirmacion** | Ejercicios conceptuales sobre propiedades | Evaluar | 3 |
| **Comparacion de Procedimientos** | Ejercicios con multiples metodos de solucion | Analizar | 3 |

### PASO 0.5: Seleccion de version grafica (si aplica)

Preguntar al usuario: 1. TikZ / 2. Python (reticulate) / 3. R/ggplot2 (RECOMENDADO). NO continuar sin respuesta.

### PASO 1: Verificar analisis ICFES

Confirmar que existe clasificacion previa: Nivel, Competencia, Componente, Tipo = schoice.

### PASO 2: Consultar ejemplos funcionales METACOGNITIVOS

NUNCA generar codigo sin consultar ejemplos primero. Buscar en `A-Produccion/03-En-Produccion/**/*metacognitivo*.Rmd` y `A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd`. Leer ejemplo completo y copiar patrones.

### PASO 3: Definir pool de errores conceptuales (OBLIGATORIO)

Minimo 4-6 errores con codigos, descripcion_corta, descripcion_larga, causa_raiz y funcion `calcula()`.

Ver [pool-errores-conceptuales.md](references/pool-errores-conceptuales.md) para estructura completa, patron de seleccion generica y taxonomia de codigos.

### PASO 4: Generar nombre con nomenclatura

Formato: `[ejercicio]_metacognitivo_[competencia]_n[nivel]_schoice_v[version].Rmd`

`metacognitivo` es OBLIGATORIO en el nombre. Nivel minimo: n2. Ver: `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`

### PASO 5: Crear carpeta en En-Desarrollo

```bash
mkdir -p A-Produccion/02-En-Desarrollo/[nombre_ejercicio]
```

### PASO 6: Generar codigo .Rmd METACOGNITIVO

Ver [anatomia metacognitiva del .Rmd](references/anatomia-metacognitiva.md) para las 8 secciones obligatorias.

Estructura requerida: YAML (con taxonomias cognitivas) → setup → data_generation (pool errores) → version_diversity_test → validaciones_matematicas → Question (patron metacognitivo) → Solution (analisis error + reflexion) → META-INFORMATION (DOK, Bloom, SOLO).

### PASO 7: Validar renderizado

```bash
Rscript .claude/skills/generar-schoice/scripts/validar-renderizado.R ejercicio.Rmd
```

### PASO 8: Verificar checklist metacognitivo

Ver [checklist-metacognitivo.md](references/checklist-metacognitivo.md) para lista completa (pre/durante/post generacion).

### PASO 9: Promocion

```bash
/promover-ejercicio [nombre_ejercicio]
```

## Antipatrones PROHIBIDOS

Ver [antipatrones.md](references/antipatrones.md) para ejemplos con codigo incorrecto/correcto.

Resumen: (1) NO ejercicios puramente procedimentales, (2) NO distractores aleatorios, (3) NO Solution sin analisis de error.

## Referencias

- [Anatomia Metacognitiva .Rmd](references/anatomia-metacognitiva.md) - Las 8 secciones obligatorias
- [Anatomia .Rmd basica](references/anatomia-rmd.md) - Estructura general
- [Pool de Errores Conceptuales](references/pool-errores-conceptuales.md) - Estructura, seleccion, taxonomia
- [Antipatrones](references/antipatrones.md) - Patrones prohibidos con correcciones
- [Checklist Metacognitivo](references/checklist-metacognitivo.md) - Pre/durante/post generacion
- [Errores comunes](references/errores-comunes.md) - Patrones incorrecto/correcto
- [Ejemplos completos](references/ejemplos.md) - Nivel 1 aritmetica + Nivel 3 estadistica
- Regla Metacognitiva: `.claude/rules/ejercicios-metacognitivos.md`
- Ejemplos Funcionales: `A-Produccion/Ejemplos-Funcionales-Rmd/`
- Nomenclatura: `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`
- Ciclo Validacion: `.claude/rules/ciclo-validacion.md`
- Metadatos: `.claude/rules/codigo-rmd.md`

## Integracion con otros skills

```
analizar-icfes -> generar-schoice -> validar-renderizado -> promover-ejercicio
                       ↑
                       |
              Regla ejercicios-metacognitivos.md OBLIGATORIA
```
