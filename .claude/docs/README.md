# Documentación Técnica - Sistema de Automatizaciones

Esta carpeta contiene la documentación técnica del sistema de automatizaciones para la generación y corrección de ejercicios R/exams.

## Estructura

```
.claude/
├── docs/                           # Documentación técnica
│   ├── README.md                   # Este archivo
│   ├── TRES_NIVELES_VALIDACION.md  # Metodología de validación
│   ├── patrones-errores-conocidos.md   # Base de conocimiento de errores
│   └── casos-resueltos/            # Historial de casos específicos
│       └── 2025-12-19-cilindro-tikz.md
├── agents/                         # Agentes especializados
│   ├── clasificador-icfes.md       # Análisis de ejercicios ICFES
│   └── graficador-tikz.md          # Replicación visual TikZ
├── commands/                       # Comandos ejecutables
│   ├── analizar-icfes.md
│   ├── generar-schoice.md
│   ├── generar-cloze.md
│   ├── corregir-error-imagen.md
│   └── promover-ejercicio.md
└── skills/                         # Skills de automatización
    ├── corregir-error-imagen/      # Corrector de errores de imágenes
    ├── validar-diversidad/         # Validador de 300+ versiones
    └── validar-icfes/              # Validador de metadatos ICFES
```

## Filosofía del Sistema

### Principio de Documentación Verificada

**Solo se documenta lo que está 100% verificado y funcionando.**

Este principio fundamental asegura que:
- ✅ Todas las soluciones documentadas han sido probadas
- ✅ Cada patrón incluye código de ejemplo funcional
- ✅ Los resultados son reproducibles
- ✅ La documentación es confiable como referencia

### ¿Qué NO documentamos?

- ❌ Errores sin solución confirmada
- ❌ Soluciones parciales o incompletas
- ❌ "Posibles soluciones" o aproximaciones
- ❌ Teorías o hipótesis sin validación

### ¿Qué SÍ documentamos?

- ✅ Errores identificados con solución verificada
- ✅ Código antes/después completamente funcional
- ✅ Pruebas de validación exitosas (PDF + HTML)
- ✅ Referencias a archivos .Rmd en producción
- ✅ Historial con resultados específicos

## Documentos Principales

### 1. TRES_NIVELES_VALIDACION.md

**Propósito:** Guía completa del sistema de validación en tres niveles.

**Los tres niveles:**
1. **Nivel 1 - RStudio:** Run > Run all (validación interactiva)
2. **Nivel 2 - Generación Masiva:** SemilleroUnico_v2.R (todos los formatos)
3. **Nivel 3 - Terreno:** Validación con estudiantes en el aula

**Contenido:**
- Descripción detallada de cada nivel
- Criterios de éxito específicos
- Qué detecta y qué no detecta cada nivel
- Flujo completo de validación
- Checklist de validación completa

### 2. patrones-errores-conocidos.md

**Propósito:** Base de conocimiento de errores comunes y sus soluciones verificadas.

**Estructura de cada patrón:**
```markdown
## Error N: [Título]

### ❌ Mensaje de Error
[Error exacto]

### 🔍 Causa Raíz
[Explicación técnica]

### ✅ Solución Verificada
[Código antes/después con ejemplos completos]

### 🧪 Validación de la Solución
[Criterios y comandos de prueba]

### 📋 Checklist de Corrección
[Pasos específicos]

### 📅 Historial
[Tabla con resultados de validación]
```

**Proceso de agregado:**
1. Identificar el error recurrente
2. Desarrollar y probar la solución
3. Validar en archivo .Rmd real (PDF + HTML)
4. Documentar con código completo
5. Agregar resultados de validación

## Skills de Automatización

Los skills son procedimientos automatizados que Claude puede ejecutar para tareas específicas.

### Skill: corregir-error-imagen

**Función:** Corrige errores de compilación LaTeX causados por `include_tikz()`.

**Cuándo usar:**
```bash
/corregir-error-imagen
```

**Qué hace:**
1. Identifica chunks con `include_tikz()`
2. Aplica patrón de renderizado condicional
3. Prueba compilación PDF y HTML
4. Valida resultados

**Documentación completa:** `.claude/skills/corregir-error-imagen/skill.md`

### Skill: validar-diversidad-300

**Función:** Valida que un ejercicio genere 250+ versiones únicas de 300.

**Cuándo usar:**
```bash
/validar-diversidad-300
```

**Documentación completa:** `.claude/skills/validar-diversidad/skill.md`

### Skill: validar-icfes

**Función:** Verifica que los metadatos ICFES estén completos y correctos.

**Documentación completa:** `.claude/skills/validar-icfes/skill.md`

## Flujo de Trabajo: Error → Solución → Documentación

### Paso 1: Identificación del Error
```
Usuario reporta: "Error: File 'imagen.png' not found"
```

### Paso 2: Investigación y Desarrollo
- Analizar la causa raíz
- Desarrollar solución candidata
- Probar en archivo real

### Paso 3: Validación
```r
# Probar PDF
exams2pdf("archivo.Rmd", n = 1)

# Probar HTML
exams2html("archivo.Rmd", n = 1)
```

### Paso 4: Documentación (Solo si paso 3 es exitoso)
1. Agregar patrón a `patrones-errores-conocidos.md`
2. Incluir código completo (antes/después)
3. Documentar resultados de validación
4. Referenciar archivo .Rmd verificado

### Paso 5: Automatización
1. Crear skill en `.claude/skills/[nombre-skill]/`
2. Documentar algoritmo de corrección
3. Agregar casos de uso
4. Linkear con patrón documentado

## Criterios de Calidad

### Para Documentación de Errores

**Mínimo requerido:**
- [ ] Error reproducible con mensaje exacto
- [ ] Causa raíz identificada y explicada
- [ ] Solución con código completo (antes/después)
- [ ] Validación exitosa en PDF
- [ ] Validación exitosa en HTML
- [ ] Archivo .Rmd de referencia funcionando
- [ ] Tabla de historial con resultados

### Para Skills de Automatización

**Mínimo requerido:**
- [ ] Descripción clara de la función
- [ ] Algoritmo paso a paso documentado
- [ ] Casos de uso específicos
- [ ] Link a patrón de error documentado
- [ ] Instrucciones de ejecución
- [ ] Criterios de validación

## Mantenimiento

### Actualización de Patrones Existentes

Si un patrón documentado necesita actualización:
1. ✅ Probar nueva solución completamente
2. ✅ Validar en múltiples archivos .Rmd
3. ✅ Actualizar sección de código
4. ✅ Agregar nueva entrada en historial
5. ✅ Incrementar versión (v1.0 → v1.1)

### Obsolescencia de Patrones

Si un patrón ya no es relevante:
1. No eliminar (preservar historial)
2. Agregar nota al inicio: `⚠️ OBSOLETO - Ver [nuevo_patron]`
3. Explicar por qué quedó obsoleto
4. Referenciar nuevo enfoque recomendado

## Convenciones

### Símbolos Utilizados

- ✅ Verificado / Exitoso
- ❌ No válido / Error
- ⚠️ Advertencia / Precaución
- 🔍 Análisis / Investigación
- 🧪 Prueba / Validación
- 📋 Checklist / Lista de tareas
- 📅 Historial / Versiones
- 🔗 Referencia / Link
- 🎯 Aplicable / Caso de uso
- 🎉 Éxito completo

### Formato de Código

**Bloques de código siempre incluyen:**
```r
# Comentario explicativo
codigo_funcional <- function() {
  # Implementación completa
  return(resultado)
}
```

**Nunca usar:**
```r
# ... resto del código ...
# [código omitido]
# etc.
```

## Contribución

Para agregar nueva documentación:

1. Seguir el template exacto del tipo de documento
2. Validar completamente antes de documentar
3. Incluir resultados específicos de pruebas
4. Referenciar archivos reales del repositorio
5. Actualizar este README si es necesario

## Contacto y Soporte

Para preguntas sobre la documentación:
- Ver primero `patrones-errores-conocidos.md`
- Revisar skills existentes en `.claude/skills/`
- Consultar archivos .Rmd de referencia en `/A-Produccion/`

---

**Última actualización:** 2025-12-19
**Versión del sistema:** 1.0
**Estado:** ✅ Operacional
