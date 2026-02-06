---
name: generar-cloze
description: >
  Genera ejercicio R-exams tipo CLOZE (pregunta compuesta).
  Usa cuando el analisis ICFES indica tipo cloze, necesites pregunta con multiples partes,
  o quieras combinar opciones multiples + respuestas numericas en un solo ejercicio.
  SIEMPRE consulta ejemplos funcionales ANTES de generar codigo.
license: Proyecto Educativo - IE Pedacito de Cielo
compatibility: Requiere R (>= 4.0), tinytex, paquetes exams y tidyverse. Linux/macOS.
metadata:
  author: alvaretto
  version: "2.1"
  language: es
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

# Generador de Ejercicios CLOZE (Pregunta Compuesta)

## Decision Tree

```
User task -> Tiene analisis ICFES?
    |-- No -> Ejecutar /analizar-icfes primero
    +-- Si -> Tipo = cloze confirmado?
        |-- No -> Usar /generar-schoice
        +-- Si -> Cuantos gaps necesita?
            |-- 2-6 gaps -> Definir tipos (schoice|num|string)
            +-- Consultar ejemplos funcionales similares
                 +-- Generar .Rmd con nomenclatura oficial
                    +-- Validar: Rscript scripts/validar-renderizado.R
```

## Cuando usar CLOZE vs SCHOICE

**Usa CLOZE cuando:**

- Problema requiere multiples respuestas en secuencia
- Necesitas combinar tipos (seleccion + numerica + texto)
- Ejercicio tiene varios pasos a responder por separado
- Nivel de dificultad 3 o 4

**Usa SCHOICE cuando:**

- Solo hay 1 respuesta final
- Todas las opciones son del mismo tipo
- Nivel de dificultad 1 o 2

## Proceso paso a paso

### PASO 0: Determinar tipos de gap

Definir para cada parte del problema:

| Tipo | Cuando usar | Ejemplo |
|------|-------------|---------|
| schoice | Seleccion unica | A, B, C, D |
| mchoice | Seleccion multiple | Checkbox |
| num | Respuesta numerica | 42.5 |
| string | Texto libre | "exponencial" |

### PASO 1: Verificar analisis ICFES

Confirmar: Nivel, Competencia, Componente, Tipo = cloze.

### PASO 2: Consultar ejemplos funcionales

NUNCA generar codigo sin consultar ejemplos primero.

```bash
ls /A-Produccion/Ejemplos-Funcionales-Rmd/*cloze*.Rmd
grep -l "extype: cloze" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

### PASO 3: Generar nombre con nomenclatura

Formato: `[ejercicio]_[componente]_[competencia]_n[nivel]_v[version].Rmd`

Ver: .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md

### PASO 4: Crear carpeta en En-Desarrollo

```bash
mkdir -p /A-Produccion/En-Desarrollo/[nombre_ejercicio]
```

### PASO 5: Generar codigo .Rmd

Ver [anatomia CLOZE](references/anatomia-cloze.md) para:

- Estructura de GAPS (##ANSWER1##, ##ANSWER2##, etc.)
- Formato exsolution para CLOZE
- Formato extol para CLOZE
- exclozetype obligatorio

### PASO 6: Validar renderizado

```bash
Rscript .claude/skills/generar-schoice/scripts/validar-renderizado.R ejercicio.Rmd
```

NOTA: NOPS fallara si hay gaps tipo num/string (esperado).

### PASO 7: Ciclo de correccion

Si hay errores, consultar ejemplos funcionales.
Volver a PASO 6.

### PASO 8: Promocion

```bash
/promover-ejercicio [nombre_ejercicio]
```

## Condiciones criticas

### Pre-generacion

- Analisis ICFES completado con tipo = cloze
- Ejemplo funcional CLOZE identificado y leido
- Tipos de gap definidos (schoice|num|string)
- Nomenclatura calculada

### Durante generacion

- Funcion `generar_datos()` con aleatorizacion
- GAPS numerados secuencialmente (1, 2, 3...)
- exclozetype con tipos por gap separados por `|`
- exsolution con respuestas por gap separadas por `|`
- extol con tolerancias por gap separadas por `|`

### Post-generacion

- HTML, PDF, DOCX: OK
- NOPS: Puede fallar (esperado si hay gaps num/string)
- Test de diversidad > 250 versiones unicas

NO terminar con errores inesperados.

## Referencias

- [Anatomia CLOZE](references/anatomia-cloze.md) - Estructura GAPS y metadatos
- generar-schoice: .claude/skills/generar-schoice/SKILL.md (estructura base)
- Ejemplos Funcionales: /A-Produccion/Ejemplos-Funcionales-Rmd/
- Nomenclatura: .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md
- Ciclo Validacion: .claude/rules/ciclo-validacion.md

## Integracion con otros skills

```
analizar-icfes -> generar-cloze -> validar-renderizado -> promover-ejercicio
```
