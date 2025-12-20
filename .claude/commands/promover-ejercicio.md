---
description: Mueve un ejercicio testeado desde En-Desarrollo a Nuevos-Ejercicios.
---

# Promover Ejercicio Testeado

Mueve un archivo .Rmd desde `/A-Produccion/En-Desarrollo/` a `/A-Produccion/Nuevos-Ejercicios/` 
después de validar que cumple todos los criterios de calidad.

## Parámetros de entrada
- **$ARGUMENTS**: Nombre del archivo .Rmd a promover

## Criterios de validación previos

Antes de promover, verificar que el ejercicio cumple:

### 1. Diversidad de versiones
```bash
# Ejecutar test de diversidad
Rscript -e 'testthat::test_file("tests/testthat/test_[nombre].R")'
```
✅ Debe generar **300+ versiones únicas**

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

### Paso 2: Mover archivo
```bash
mv /A-Produccion/En-Desarrollo/[nombre].Rmd /A-Produccion/Nuevos-Ejercicios/[nombre].Rmd
```

### Paso 3: Confirmar movimiento
```bash
ls -la /A-Produccion/Nuevos-Ejercicios/[nombre].Rmd
```

### Paso 4: Actualizar registro (opcional)
Agregar entrada en `/A-Produccion/Nuevos-Ejercicios/README.md` con:
- Nombre del ejercicio
- Fecha de promoción
- Competencia y nivel
- Tipo de ejercicio

## Ejemplo de uso

```
/promover-ejercicio probabilidad_aleatorio_interpretacion_n2_v1.Rmd
```

## Regla de Oro
**NUNCA promuevas** un ejercicio sin validar los 4 criterios anteriores.

