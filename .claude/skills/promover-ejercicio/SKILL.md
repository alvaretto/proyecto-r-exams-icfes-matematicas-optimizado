---
description: Mueve un ejercicio validado desde En-Desarrollo a 03-En-Produccion/[categoría ICFES]/ después de completar el Ciclo de Validación.
---

# Promover Ejercicio Validado

## ⚡ CONTEXTO: Ciclo de Validación y Corrección Automática

Este skill se ejecuta **SOLO** después de completar exitosamente el ciclo completo:

```
🔄 FASE 1: Renderizado Inicial ✅
    │
    ▼
🔍 FASE 2: Validación Visual y Funcional ✅
    │
    ▼
⚡ FASE 3: Decisión y Acción
    │
    └── ❌ SIN ERRORES → PROMOVER EJERCICIO ← ESTE SKILL
```

Mueve un archivo .Rmd desde `/A-Produccion/En-Desarrollo/` a `/A-Produccion/03-En-Produccion/[categoría ICFES]/`
después de validar que cumple todos los criterios de calidad.

## Parámetros de entrada

- **$ARGUMENTS**: Nombre del archivo .Rmd a promover

## ⛔ PRERREQUISITO OBLIGATORIO

**El ejercicio DEBE haber pasado el Ciclo de Validación Automática completo:**

1. ✅ FASE 1: Renderizado exitoso en 4 formatos (HTML, PDF, DOCX, NOPS)
2. ✅ FASE 2: Coherencia matemática, imagen-texto, código verificada
3. ✅ FASE 3: Sin errores pendientes

## Criterios de validación previos

Antes de promover, verificar que el ejercicio cumple:

### 1. Diversidad de versiones
```bash
# Ejecutar test de diversidad
Rscript -e 'testthat::test_file("tests/testthat/test_[nombre].R")'
```
✅ Debe generar **250+ versiones únicas** (de 300 intentos)

### 2. Compilación exitosa
```bash
# Probar compilación en RStudio
Rscript -e 'library(exams); exams2html("[nombre].Rmd", n=3)'
```
✅ Debe compilar sin errores en HTML, PDF y Moodle

### 3. Metadatos ICFES completos

- ✅ Competencia definida
- ✅ Nivel de dificultad (1-4)
- ✅ Componente especificado
- ✅ Tipo de ejercicio (schoice/cloze)

### 4. Calidad del contenido

- ✅ Distractores pedagógicos efectivos
- ✅ Explicación detallada en Solution
- ✅ Formato numérico correcto (sin notación científica)
- ✅ Tolerancias apropiadas (para tipo cloze)

## Proceso de promoción

### Paso 1: Verificar ubicación actual
```bash
ls -la /A-Produccion/En-Desarrollo/[nombre].Rmd
```

### Paso 2: Mover archivo a categoría ICFES correspondiente
```bash
mv /A-Produccion/En-Desarrollo/[nombre].Rmd /A-Produccion/03-En-Produccion/[categoría-ICFES]/[nombre].Rmd
```

### Paso 3: Confirmar movimiento
```bash
ls -la /A-Produccion/03-En-Produccion/[categoría-ICFES]/[nombre].Rmd
```

### Paso 4: Actualizar registro (opcional)
Agregar entrada en el directorio correspondiente con:

- Nombre del ejercicio
- Fecha de promoción
- Competencia y nivel
- Tipo de ejercicio

## Ejemplo de uso

```
/promover-ejercicio probabilidad_aleatorio_interpretacion_n2_v1.Rmd
```

## ⛔ CONDICIONES CRÍTICAS (NO NEGOCIABLES)

1. ❌ **NUNCA promover** un ejercicio con errores pendientes
2. ❌ **NUNCA promover** sin completar el Ciclo de Validación
3. ✓ **SIEMPRE** verificar los 4 criterios de calidad
4. ✓ **SIEMPRE** confirmar renderizado exitoso en 4 formatos
5. ✓ El ejercicio debe haber pasado FASE 1, FASE 2 y FASE 3 sin errores

## Regla de Oro
**NUNCA promuevas** un ejercicio sin completar el Ciclo de Validación Automática.

## Referencias

- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (fuente de verdad)
- `.claude/docs/TRES_NIVELES_VALIDACION.md`

