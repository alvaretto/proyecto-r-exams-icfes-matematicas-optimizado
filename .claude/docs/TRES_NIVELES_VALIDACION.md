# Los Tres Niveles de Validación - Sistema R/exams

> **Filosofía:** Todo ejercicio debe pasar por tres niveles de validación antes de considerarse completamente verificado.

---

## 🎯 Visión General

El sistema de validación refleja el flujo de trabajo real desde el desarrollo hasta el aula:

```
┌─────────────────────────────────────────────────────────────────┐
│                    FLUJO DE VALIDACIÓN                          │
└─────────────────────────────────────────────────────────────────┘

    NIVEL 1                  NIVEL 2                  NIVEL 3
┌─────────────┐          ┌─────────────┐          ┌─────────────┐
│   RStudio   │   ✅    │   Scripts   │   ✅    │  Estudiantes│
│  Run > All  │  ────▶  │  Semillero  │  ────▶  │    Aula     │
└─────────────┘          └─────────────┘          └─────────────┘
   Desarrollo             Generación Masiva         Producción
  Interactivo              Automática              Validación Real
```

---

## Nivel 1: RStudio (Run > Run all)

### 🎯 Propósito
Validar que el ejercicio funciona interactivamente durante el desarrollo.

### 📝 Método
1. Abrir el archivo `.Rmd` en RStudio
2. Ejecutar: **Run > Run All**
3. Verificar el output generado

### ✅ Criterios de Éxito

| Criterio | Descripción |
|----------|-------------|
| **Chunks ejecutan** | Todos los chunks se ejecutan sin errores |
| **Output generado** | Se genera el formato configurado en YAML (pdf/html/word) |
| **Gráficos visibles** | Los diagramas TikZ se visualizan correctamente |
| **Variables definidas** | Todas las variables necesarias están disponibles |

### ⚠️ Problemas Comunes

**Error:** Chunk no ejecuta
```r
# Solución: Verificar dependencias
library(exams)
library(tikzDevice)
```

**Error:** Gráfico no se muestra
```r
# Solución: Verificar configuración del chunk
```{r nombre, echo=FALSE, results='asis'}
```

### 🔍 Qué Detecta Este Nivel
- Errores de sintaxis R
- Variables no definidas
- Problemas con librerías
- Configuración incorrecta de chunks
- Errores básicos de TikZ

### 🚫 Qué NO Detecta
- Problemas con exams2pdf(), exams2nops(), etc.
- Errores en templates específicos
- Problemas de aleatorización
- Errores matemáticos sutiles

---

## Nivel 2: Generación Masiva (SemilleroUnico_v2.R)

### 🎯 Propósito
Validar que el ejercicio funciona en TODOS los formatos de exportación usados en producción.

### 📝 Método

**Opción A: Ejecutar SemilleroUnico_v2.R**
```bash
# Desde la carpeta del .Rmd
Rscript SemilleroUnico_v2.R
```

**Opción B: Ejecutar test completo**
```bash
# Desde la carpeta del .Rmd
Rscript test_todos_formatos.R
```

### ✅ Criterios de Éxito

| Formato | Función | Estado Esperado |
|---------|---------|-----------------|
| **HTML** | `exams2html()` | ✅ EXITOSO |
| **PDF** | `exams2pdf()` | ✅ EXITOSO |
| **DOCX** | `exams2pandoc()` | ✅ EXITOSO |
| **NOPS** | `exams2nops()` | ✅ EXITOSO |

**Resultado esperado:**
```
Tasa de éxito: 4 de 4 formatos (100%)
```

### 🛠️ Funciones Validadas

#### `exams2html()`
- Genera vista previa HTML
- Usa `include_tikz()` para generar PNG
- Template: "plain"

#### `exams2pdf()`
- Genera PDF imprimible
- Usa código TikZ directo (con `knitr::is_latex_output()`)
- Template: "solpcielo"

#### `exams2pandoc()`
- Genera archivo DOCX (Word)
- Convierte contenido a formato Office
- Template: "pcielo.tex"

#### `exams2nops()`
- Genera exámenes escaneables
- Formato específico para lectura óptica
- Configuración: español, institución, fecha

### 🔍 Qué Detecta Este Nivel
- Errores de compilación LaTeX
- Problemas con templates específicos
- Incompatibilidades entre formatos
- Errores de renderizado TikZ
- Problemas con rutas de archivos
- Errores de encoding

### ⚠️ Problemas Comunes

**Error:** `File 'imagen.png' not found`
```
Solución: Usar renderizado condicional
Ver: .claude/docs/patrones-errores-conocidos.md#error-1
```

**Error:** LaTeX compilation failed
```bash
# Ver log de errores
cat salida/archivo.log | grep -A 5 "Error"
```

**Error:** Template not found
```r
# Verificar templates disponibles
list.files(system.file("tex", package = "exams"))
```

### 🚫 Qué NO Detecta
- Errores de comprensión del enunciado
- Ambigüedades en el lenguaje
- Contextos inapropiados para el nivel
- Distractores demasiado obvios o confusos
- Errores matemáticos sutiles que pasan tests

---

## Nivel 3: Terreno (Estudiantes)

### 🎯 Propósito
Validar que el ejercicio funciona en condiciones reales con estudiantes.

### 📝 Método
1. Aplicar ejercicio en el aula
2. Observar proceso de resolución
3. Recopilar feedback de estudiantes
4. Analizar patrones de error

### ✅ Criterios de Validación

| Aspecto | Criterio |
|---------|----------|
| **Claridad** | Enunciado comprensible sin ambigüedades |
| **Contexto** | Situación apropiada para el nivel educativo |
| **Distractores** | Opciones incorrectas plausibles pero distinguibles |
| **Tiempo** | Resolución en tiempo razonable (5-10 min) |
| **Matemática** | Solución correcta sin errores de cálculo |
| **Lenguaje** | Vocabulario apropiado para la edad |

### 🔍 Qué Detecta Este Nivel
- Ambigüedades en el enunciado
- Contextos confusos o inapropiados
- Errores matemáticos no detectados en tests
- Distractores muy obvios o muy confusos
- Vocabulario inadecuado
- Suposiciones incorrectas sobre conocimiento previo
- Errores tipográficos que pasaron revisión
- Problemas de formato visual

### 📊 Indicadores de Problemas

| Indicador | Problema Potencial |
|-----------|-------------------|
| **Tasa de acierto < 25%** | Ejercicio muy difícil o mal planteado |
| **Tasa de acierto > 95%** | Ejercicio muy fácil o respuesta obvia |
| **Preguntas frecuentes** | Enunciado ambiguo |
| **Tiempo > 15 min** | Ejercicio muy complejo |
| **Todos fallan en lo mismo** | Error en solución o distractor engañoso |

### 🛠️ Proceso de Corrección

1. **Recopilar evidencia**
   - Tasa de acierto
   - Patrones de error
   - Comentarios de estudiantes

2. **Identificar problema**
   - ¿Enunciado ambiguo?
   - ¿Contexto confuso?
   - ¿Error matemático?
   - ¿Distractor engañoso?

3. **Corregir ejercicio**
   - Modificar `.Rmd`
   - Re-validar Niveles 1 y 2
   - Crear nueva versión

4. **Documentar lección aprendida**
   - Agregar a knowledge base si es patrón recurrente
   - Actualizar guías de diseño

### 🚫 Qué NO Detecta
- Problemas técnicos de compilación (ya detectados en Nivel 2)
- Errores de sintaxis R (ya detectados en Nivel 1)

---

## 📊 Tabla Comparativa

| Aspecto | Nivel 1 | Nivel 2 | Nivel 3 |
|---------|---------|---------|---------|
| **Cuando** | Durante desarrollo | Antes de promoción | Después de promoción |
| **Quien** | Desarrollador | Automatizado | Estudiantes |
| **Donde** | RStudio | Scripts | Aula |
| **Detecta** | Errores técnicos | Problemas de formato | Problemas pedagógicos |
| **Tiempo** | < 1 min | < 5 min | Días/semanas |
| **Automatizable** | Parcial | Total | No |
| **Obligatorio** | Sí | Sí | Sí (post-producción) |

---

## 🔄 Flujo Completo de Validación

```
┌────────────────────────────────────────────────────────────┐
│                    PROCESO COMPLETO                        │
└────────────────────────────────────────────────────────────┘

1. DESARROLLO
   ├── Crear .Rmd
   ├── NIVEL 1: Run > Run all
   │   ├── ✅ Pasa → Continuar
   │   └── ❌ Falla → Corregir y repetir
   └── Desarrollo completo

2. PRE-PRODUCCIÓN
   ├── NIVEL 2: SemilleroUnico_v2.R / test_todos_formatos.R
   │   ├── ✅ 100% éxito → Promocionar
   │   └── ❌ < 100% → Corregir y repetir
   ├── /promover-ejercicio
   └── Ejercicio en producción

3. PRODUCCIÓN
   ├── Aplicar en aula
   ├── NIVEL 3: Validación con estudiantes
   │   ├── ✅ Funciona bien → Mantener
   │   ├── ⚠️ Problemas menores → Ajustar en próxima versión
   │   └── ❌ Problemas graves → Retirar y corregir urgente
   └── Feedback al sistema

4. MEJORA CONTINUA
   ├── Documentar lecciones aprendidas
   ├── Actualizar knowledge base
   └── Mejorar proceso de validación
```

---

## 🎯 Mejores Prácticas

### Durante Desarrollo (Nivel 1)
1. ✅ Ejecutar Run > Run all frecuentemente
2. ✅ Probar diferentes valores aleatorios manualmente
3. ✅ Verificar que gráficos se visualizan
4. ✅ Revisar solución matemática cuidadosamente

### Antes de Promoción (Nivel 2)
1. ✅ Ejecutar `test_todos_formatos.R` SIEMPRE
2. ✅ Revisar TODOS los formatos generados
3. ✅ Verificar diversidad con `/validar-diversidad-300`
4. ✅ Comprobar metadatos ICFES completos
5. ✅ Tasa de éxito debe ser 100% (4 de 4 formatos)

### Durante Producción (Nivel 3)
1. ✅ Aplicar primero con grupo pequeño (piloto)
2. ✅ Observar proceso de resolución
3. ✅ Recopilar feedback sistemáticamente
4. ✅ Documentar problemas encontrados
5. ✅ Corregir rápidamente si hay errores graves

---

## 📋 Checklist de Validación Completa

```
NIVEL 1: RSTUDIO
[ ] .Rmd abre sin errores
[ ] Run > Run all ejecuta completamente
[ ] Output generado (PDF/HTML/Word)
[ ] Gráficos TikZ visibles
[ ] Variables todas definidas

NIVEL 2: GENERACIÓN MASIVA
[ ] exams2html(): EXITOSO
[ ] exams2pdf(): EXITOSO
[ ] exams2pandoc(): EXITOSO
[ ] exams2nops(): EXITOSO
[ ] Tasa de éxito: 100%
[ ] Diversidad ≥ 250/300
[ ] Metadatos ICFES completos

NIVEL 3: TERRENO (post-producción)
[ ] Aplicado en aula
[ ] Tasa de acierto razonable (25-95%)
[ ] Sin preguntas frecuentes sobre enunciado
[ ] Tiempo de resolución apropiado
[ ] Sin errores reportados
[ ] Feedback positivo de estudiantes
```

---

## 🔗 Recursos Relacionados

- **Patrón de Error TikZ:** `.claude/docs/patrones-errores-conocidos.md#error-1`
- **Skill Corrección:** `.claude/skills/corregir-error-imagen/skill.md`
- **Troubleshooting:** `.claude/TROUBLESHOOTING.md`
- **Resumen TikZ:** `.claude/docs/RESUMEN_CORRECCION_TIKZ.md`

---

**Última actualización:** 2025-12-19
**Versión:** 1.0
**Estado:** ✅ Documentado y validado
