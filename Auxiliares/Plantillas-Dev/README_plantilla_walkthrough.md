# Guía de Uso: Plantilla Walkthrough para Ejercicios R-exams

## Descripción

Esta plantilla está basada en el walkthrough exhaustivo del ejercicio `empaques_tetra_pak_argumentacion_n3_v1` y proporciona una estructura completa para documentar cualquier ejercicio del repositorio RepositorioMatematicasICFES_R_Exams.

## Archivos Incluidos

- `plantilla_walkthrough_ejercicio.md` - Plantilla principal completa
- `README_plantilla_walkthrough.md` - Esta guía de uso

## Cómo Usar la Plantilla

### Paso 1: Copiar la Plantilla

```bash
cp Auxiliares/Plantillas-Dev/plantilla_walkthrough_ejercicio.md Lab/[nombre_ejercicio]/walkthrough.md
```

### Paso 2: Reemplazar Marcadores

Buscar y reemplazar todos los marcadores `[TEXTO_EN_MAYUSCULAS]` con información específica:

#### Marcadores Básicos Obligatorios:
- `[NOMBRE_EJERCICIO]` → Nombre descriptivo del ejercicio
- `[COMPETENCIA_ICFES]` → Competencia evaluada (interpretacion_representacion, formulacion_ejecucion, argumentacion)
- `[CONCEPTO_PRINCIPAL]` → Concepto matemático principal
- `[CONTEXTO_APLICACION]` → Contexto de aplicación (empaques, deportes, etc.)
- `[NOMBRE_ARCHIVO]` → Nombre del archivo .Rmd principal

#### Marcadores de Metadatos ICFES:
- `[COMPETENCIA_VALOR]` → argumentacion, interpretacion_representacion, etc.
- `[NIVEL_NUMERO]` → 1, 2, 3, o 4
- `[CATEGORIA]` → algebra_calculo, geometria, estadistica
- `[TIPO]` → generico, no_generico
- `[CONTEXTO_VALOR]` → familiar, laboral, comunitario, matematico
- `[EJE_VALOR]` → eje1, eje2, eje3, eje4
- `[COMPONENTE_VALOR]` → geometrico_metrico, numerico_variacional, aleatorio

#### Marcadores Técnicos:
- `[HERRAMIENTA_GRAFICO]` → matplotlib, ggplot2, tikz
- `[LENGUAJE_GRAFICO]` → Python, R
- `[NUMERO]` → Números específicos (25 paletas, 300 versiones, etc.)
- `[RANGO_LINEAS]` → Rangos de líneas del código

### Paso 3: Personalizar Contenido

#### Secciones que Requieren Personalización Completa:

1. **Objetivos de Aprendizaje** (líneas ~20-25)
   - Definir 4 objetivos específicos del ejercicio

2. **Ejemplo Conceptual** (línea ~26)
   - Crear analogía simple del concepto principal

3. **Características Especiales** (líneas ~100-120)
   - Describir 3 características únicas del ejercicio

4. **Bloques de Código** (líneas ~380-550)
   - Adaptar ejemplos de código a la estructura real del ejercicio

5. **Casos de Uso** (líneas ~580-610)
   - Personalizar escenarios según el contexto educativo

### Paso 4: Verificar Coherencia

- [ ] Todos los marcadores han sido reemplazados
- [ ] Los metadatos ICFES coinciden con el archivo .Rmd
- [ ] Los ejemplos de código son representativos
- [ ] Las instrucciones de uso son precisas
- [ ] Los casos de uso son realistas

## Estructura de la Plantilla

### Secciones Principales:

1. **Encabezado y Descripción General**
   - Introducción para novatos
   - Objetivos de aprendizaje
   - Ejemplo conceptual

2. **Competencia ICFES**
   - Metadatos completos
   - Explicación pedagógica

3. **Archivos del Proyecto**
   - Lista de archivos principales
   - Scripts de uso fácil

4. **Guía de Uso**
   - 3 opciones de uso (principiantes, Moodle, vista previa)
   - Instrucciones paso a paso

5. **Características Especiales**
   - Elementos únicos del ejercicio
   - Ventajas técnicas

6. **Análisis Técnico**
   - Estructura del ejercicio
   - Validaciones y pruebas

7. **Solución de Problemas**
   - Errores comunes
   - Soluciones paso a paso

8. **Análisis Exhaustivo por Bloques**
   - Explicación técnica detallada
   - Ejemplos de código
   - Analogías para principiantes

9. **Casos de Uso Prácticos**
   - Escenarios reales de uso
   - Soluciones específicas

10. **FAQ y Glosario**
    - Preguntas frecuentes
    - Términos técnicos explicados

## Consejos de Personalización

### Para Ejercicios de Geometría:
- Enfatizar elementos visuales (TikZ, gráficos geométricos)
- Incluir validaciones de medidas y ángulos
- Adaptar contextos a situaciones espaciales

### Para Ejercicios de Estadística:
- Destacar generación de datos aleatorios
- Incluir validaciones estadísticas
- Adaptar a contextos de análisis de datos

### Para Ejercicios de Álgebra:
- Enfatizar manipulación algebraica
- Incluir validaciones de ecuaciones
- Adaptar a contextos de modelado matemático

## Mantenimiento de la Plantilla

### Actualizaciones Recomendadas:
- Revisar estructura cuando se actualice el formato R-exams
- Actualizar ejemplos de código según mejores prácticas
- Incorporar nuevos tipos de validaciones
- Agregar nuevos casos de uso según feedback de usuarios

### Control de Versiones:
- Mantener historial de cambios en la plantilla
- Documentar modificaciones importantes
- Sincronizar con actualizaciones del repositorio principal

---

**Creado**: Junio 2025  
**Basado en**: empaques_tetra_pak_argumentacion_n3_v1/walkthrough.md  
**Propósito**: Estandarizar documentación de ejercicios R-exams  
**Estado**: Lista para uso en producción
