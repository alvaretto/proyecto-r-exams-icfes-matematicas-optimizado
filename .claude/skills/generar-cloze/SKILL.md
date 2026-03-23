---
name: generar-cloze
description: >
  Genera ejercicio R-exams tipo CLOZE (pregunta compuesta) METACOGNITIVO.
  TODO ejercicio CLOZE DEBE aplicar Progressive Disclosure con minimo 4 partes.
  Usa cuando el analisis ICFES indica tipo cloze, necesites pregunta con multiples partes,
  o quieras combinar opciones multiples + respuestas numericas en un solo ejercicio.
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

# Generador de Ejercicios CLOZE Metacognitivos

## REGLA CRÍTICA

**⚠️ TODO ejercicio CLOZE DEBE ser metacognitivo con Progressive Disclosure de minimo 4 partes.**

Ver regla completa: `.claude/rules/ejercicios-metacognitivos.md`

## Decision Tree

```
User task -> Tiene analisis ICFES?
    |-- No -> Ejecutar /analizar-icfes primero
    +-- Si -> Tipo = cloze confirmado?
        |-- No -> Usar /generar-schoice
        +-- Si -> Definir estructura Progressive Disclosure
            |-- Parte 1: schoice (Identificar error)
            |-- Parte 2: num (Calcular correcto)
            |-- Parte 3: mchoice (Evaluar afirmaciones)
            +-- Parte 4: schoice V/F (Transferir)
            +-- Consultar ejemplos funcionales similares
                 +-- Generar .Rmd con nomenclatura oficial
                    +-- Validar: Rscript scripts/validar-renderizado.R
```

## Cuando usar CLOZE vs SCHOICE

**Usa CLOZE cuando:** problema requiere multiples niveles cognitivos en secuencia, necesitas Progressive Disclosure completo, hay varios pasos a responder por separado, nivel 3 o 4, competencia = Argumentacion.

**Usa SCHOICE cuando:** solo hay 1 aspecto a evaluar, nivel 1 o 2 (pero siempre metacognitivo).

## Proceso paso a paso

### PASO 0: Definir estructura Progressive Disclosure (OBLIGATORIO)

Ver [estructura-progressive-disclosure.md](references/estructura-progressive-disclosure.md) para la secuencia de 4 partes, tabla de tipos de gap y plantilla obligatoria del Question.

### PASO 1: Verificar analisis ICFES

Confirmar: Nivel, Competencia, Componente, Tipo = cloze.

### PASO 2: Consultar ejemplos funcionales METACOGNITIVOS

NUNCA generar codigo sin consultar ejemplos primero. Ejemplo canonico:

```bash
cat A-Produccion/03-En-Produccion/.../promedios_borrados_metacognitivo_argumentacion_n3_cloze_v1.Rmd
```

### PASO 3: Definir pool de errores conceptuales (OBLIGATORIO)

Minimo 4-6 errores con codigos, descripciones, causa_raiz y funciones `calcula()` deterministicas.

Ver [pool-errores-afirmaciones.md](references/pool-errores-afirmaciones.md) para estructura completa del pool de errores (Parte 1).

### PASO 4: Definir pool de afirmaciones (OBLIGATORIO para Parte 3)

Minimo 6 afirmaciones verdaderas + 6 falsas basadas en errores conceptuales reales.

Ver [pool-errores-afirmaciones.md](references/pool-errores-afirmaciones.md) para estructura del pool de afirmaciones.

### PASO 5: Definir pool de enunciados V/F (OBLIGATORIO para Parte 4)

Minimo 4 enunciados usando datos concretos del contexto generado.

Ver [pool-errores-afirmaciones.md](references/pool-errores-afirmaciones.md) para estructura del pool V/F.

### PASO 6: Generar nombre con nomenclatura

Formato: `[ejercicio]_metacognitivo_[competencia]_n[nivel]_cloze_v[version].Rmd`

`metacognitivo` y `cloze` son OBLIGATORIOS en el nombre. Nivel minimo: n3. Ver: `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`

### PASO 7: Crear carpeta en En-Desarrollo

```bash
mkdir -p A-Produccion/02-En-Desarrollo/[nombre_ejercicio]
```

### PASO 8: Generar codigo .Rmd CLOZE METACOGNITIVO

Ver [anatomia CLOZE](references/anatomia-cloze.md) para estructura de GAPS y [anatomia metacognitiva](references/anatomia-metacognitiva.md) para secciones obligatorias.

Estructura Solution obligatoria: Analisis del Error → Procedimiento Correcto → Propiedades del Concepto → Caso Especifico → Reflexion Metacognitiva.

### PASO 9: Validar renderizado

```bash
Rscript .claude/skills/generar-schoice/scripts/validar-renderizado.R ejercicio.Rmd
```

NOPS fallara si hay gaps tipo num/string — esto es ESPERADO, no es error.

### PASO 10: Verificar checklist metacognitivo CLOZE

Ver [checklist-cloze.md](references/checklist-cloze.md) para lista completa y metadatos OBLIGATORIOS.

### PASO 11: Promocion

```bash
/promover-ejercicio [nombre_ejercicio]
```

## Antipatrones PROHIBIDOS

Ver [antipatrones-cloze.md](references/antipatrones-cloze.md) para los 4 antipatrones con codigo incorrecto/correcto.

Resumen: (1) NO menos de 4 partes, (2) NO partes sin progresion cognitiva, (3) NO afirmaciones sin base conceptual, (4) NO ##ANSWERi## mal ubicado.

## Referencias

- [Estructura Progressive Disclosure](references/estructura-progressive-disclosure.md) - Secuencia de 4 partes, tipos de gap, plantilla Question, metadatos
- [Pool de Errores y Afirmaciones](references/pool-errores-afirmaciones.md) - PASOs 3, 4 y 5 con codigo R completo
- [Antipatrones CLOZE](references/antipatrones-cloze.md) - 4 antipatrones con correcciones
- [Checklist CLOZE](references/checklist-cloze.md) - Checklist + condiciones criticas + metadatos obligatorios
- [Anatomia CLOZE](references/anatomia-cloze.md) - Estructura GAPS y metadatos
- [Anatomia Metacognitiva](references/anatomia-metacognitiva.md) - Las 8 secciones obligatorias
- Regla Metacognitiva: `.claude/rules/ejercicios-metacognitivos.md`
- generar-schoice: `.claude/skills/generar-schoice/SKILL.md` (estructura base)
- Ejemplo Canonico: `A-Produccion/03-En-Produccion/.../promedios_borrados_metacognitivo_argumentacion_n3_cloze_v1.Rmd`
- Nomenclatura: `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`
- Ciclo Validacion: `.claude/rules/ciclo-validacion.md`

## Integracion con otros skills

```
analizar-icfes -> generar-cloze -> validar-renderizado -> promover-ejercicio
                       ↑
                       |
              Regla ejercicios-metacognitivos.md OBLIGATORIA
              + Progressive Disclosure de 4 partes MINIMO
```
