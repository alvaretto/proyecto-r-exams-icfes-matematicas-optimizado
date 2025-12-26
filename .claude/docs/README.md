# Documentación Técnica - Sistema de Automatizaciones

Esta carpeta contiene la documentación técnica del sistema de automatizaciones para la generación y corrección de ejercicios R/exams.

---

## 🚀 INICIO RÁPIDO

**¿Buscas algo específico?**

👉 **[INDICE_DOCUMENTACION.md](INDICE_DOCUMENTACION.md)** - Índice completo de toda la documentación

**¿Primera vez usando el sistema?**

1. **[WORKFLOW_PASO_A_PASO.md](WORKFLOW_PASO_A_PASO.md)** - Guía completa paso a paso
2. **[GUIA_RAPIDA_VISUAL.md](GUIA_RAPIDA_VISUAL.md)** - Referencia visual rápida
3. **[GUIA_USUARIO.md](GUIA_USUARIO.md)** - Referencia de comandos

---

## Arquitectura del Sistema

El sistema está organizado en una **arquitectura modular** con componentes especializados:

```
┌─────────────────────────────────────────────────────────┐
│              ARQUITECTURA MODULAR                      │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  🤖 Agentes Especializados                             │
│     ├── ClasificadorICFES (6 dimensiones)             │
│     └── AgenteTikZ (Replicación visual 98%+)           │
│           ↓                                            │
│  🎯 Sistema de Skills                                  │
│     ├── /analizar-icfes                                │
│     ├── /generar-schoice, /generar-cloze              │
│     ├── /generar-grafica-nueva                        │
│     ├── /validar-diversidad-300                        │
│     ├── /corregir-error-imagen                         │
│     └── /promover-ejercicio                            │
│           ↓                                            │
│  🔌 Sistema de Hooks                                   │
│     ├── pre-edit-rmd-validation                        │
│     ├── post-exams2-validation                         │
│     ├── post-grafica-generada                          │
│     └── post-error-diagnostic                          │
│           ↓                                            │
│  📦 Repositorio TikZ                                   │
│     └── Gráficas reutilizables parametrizables         │
│           ↓                                            │
│  📚 Documentación                                      │
│     └── Fuentes de verdad (ejemplos funcionales)       │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

**Diagrama completo:** Ver [`.claude/Mermaid_Chart.txt`](../Mermaid_Chart.txt)

## Estructura

```
.claude/
├── docs/                           # Documentación técnica
│   ├── README.md                   # Este archivo
│   ├── INDICE_DOCUMENTACION.md     # Índice completo de documentación
│   ├── WORKFLOW_PASO_A_PASO.md     # Guía completa del workflow
│   ├── GUIA_RAPIDA_VISUAL.md       # Referencia visual rápida
│   ├── GUIA_USUARIO.md             # Guía completa de usuario
│   ├── TRES_NIVELES_VALIDACION.md  # Metodología de validación
│   ├── COMANDOS_DEPRECADOS.md      # Registro de comandos deprecados
│   ├── FASE5_CHECKLIST_PRE_ELIMINACION.md  # Checklist Fase 5
│   ├── FASE5_PROCEDIMIENTO_ELIMINACION.md  # Procedimiento Fase 5
│   ├── FASE5_RESUMEN_EJECUTIVO.md  # Resumen ejecutivo Fase 5
│   ├── patrones-errores-conocidos.md   # Base de conocimiento de errores
│   └── casos-resueltos/            # Historial de casos específicos
│       ├── 2025-12-19-cilindro-tikz.md
│       ├── 2025-12-21-recta-abs-formateado.md
│       └── 2025-01-XX-recta-abs-formateado.md
├── agents/                         # 🤖 Agentes especializados
│   ├── clasificador-icfes.md       # Análisis según 6 dimensiones ICFES
│   ├── graficador-tikz.md          # Replicación visual TikZ 98%+
│   ├── validador-visual.md         # Validación visual sistemática
│   ├── corrector-coherencia.md    # Corrección de coherencia
│   └── diagnosticador-errores.md   # Diagnóstico automático de errores
├── skills/                         # 🎯 Sistema de Skills (Workflow)
│   ├── analizar-icfes/             # Análisis ICFES según 6 dimensiones
│   ├── generar-schoice/            # Generador de ejercicios SCHOICE
│   ├── generar-cloze/              # Generador de ejercicios CLOZE
│   ├── generar-grafica-nueva/      # Generador de gráficas TikZ nuevas
│   ├── consultar-grafica-tikz/     # Consulta al repositorio TikZ
│   ├── promover-ejercicio/         # Promoción a producción
│   ├── corregir-error-imagen/      # Corrector de errores TikZ
│   ├── corregir-graficos/          # Corrector de gráficos
│   ├── validar-diversidad/         # Validador de 300+ versiones
│   ├── validar-icfes/              # Validador de metadatos ICFES
│   ├── validar-coherencia/         # Validador de coherencia
│   ├── validar-renderizado/         # Validador de renderizado
│   └── diagnosticar-errores/       # Diagnóstico de errores
├── hooks/                          # 🔌 Sistema de Hooks
│   ├── README.md                   # Documentación de hooks
│   ├── pre-edit-rmd-validation.md  # Validación antes de editar .Rmd
│   ├── post-exams2-validation.md   # Validación después de exams2*
│   ├── post-grafica-generada.md    # Hook después de generar gráfica
│   └── post-error-diagnostic.md    # Hook después de diagnosticar error
├── commands/                       # Comandos (legacy - ver skills/)
│   └── [archivos de referencia]
├── deprecated/                     # ⚠️ Archivos deprecados
│   ├── analizar-ejercicio.md       # (Deprecado - Usar analizar-icfes)
│   └── corregir-error-imagen.md    # (Duplicado - Movido a skills/)
├── scripts/                        # Scripts de automatización
│   ├── README.md                   # Documentación de scripts
│   ├── gestionar_repo_tikz.sh      # Gestión del repositorio TikZ
│   ├── fase5_eliminar_comando_deprecado.sh  # Script de eliminación
│   ├── fase5_tests_post_eliminacion.sh      # Tests post-eliminación
│   └── fase5_rollback.sh           # Plan de rollback
├── tests/                          # Tests de validación
│   └── test_comandos_workflow.md   # Tests de workflow
├── backups/                        # Backups de archivos
├── logs/                           # Logs de ejecución
├── Mermaid_Chart.txt               # 🆕 Diagrama de flujo completo actualizado
├── settings.json                   # Hooks y configuración global
├── settings.local.json             # Permisos para skills
└── MIGRACION_COMPLETADA.md         # Reporte de migración (2025-12-20)
```

**Notas importantes:**
- ✅ Arquitectura modular con agentes, skills, hooks y repositorio TikZ
- ✅ 10+ skills activos del workflow en `skills/`
- ✅ 4 hooks implementados para validación automática
- ✅ Repositorio TikZ centralizado para reutilización de gráficas
- ⚠️ `analizar-ejercicio.md` deprecado - Ver `COMANDOS_DEPRECADOS.md` para detalles

## Filosofía del Sistema

### ⚡ Ciclo de Validación y Corrección Automática (OBLIGATORIO)

**Cada vez que se renderiza un archivo .Rmd, se ejecuta automáticamente:**

```
🔄 FASE 1: RENDERIZADO INICIAL
    └── Ejecutar exams2html, exams2pdf, exams2docx, exams2nops
    └── Capturar errores/advertencias

🔍 FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL
    └── Coherencia Matemática, Imagen-Texto, Código, 4 formatos

⚡ FASE 3: DECISIÓN Y ACCIÓN
    ├── ❌ SIN ERRORES → Continuar workflow
    └── ✓ CON ERRORES:
        ├── 📚 SUBFASE 3A: Consultar /A-Produccion/Ejemplos-Funcionales-Rmd/
        ├── 🔄 SUBFASE 3B: VOLVER A FASE 1 (ciclo obligatorio)
        └── 📊 SUBFASE 3C: Documentar en patrones-errores-conocidos.md
```

**Condiciones Críticas:**
- ❌ NO terminar con errores sin resolver
- ✓ Ejemplos funcionales = Fuente de verdad absoluta
- ✓ Documentar SOLO después de confirmar solución

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
- ✅ Patrón utilizado de ejemplos funcionales (SUBFASE 3C)

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

## Componentes de la Arquitectura

### 🤖 Agentes Especializados

Agentes con responsabilidades específicas en el workflow:

#### ClasificadorICFES
- **Función:** Análisis de ejercicios según 6 dimensiones ICFES
- **Ubicación:** `.claude/agents/clasificador-icfes.md`
- **Uso:** Activado automáticamente por skill `/analizar-icfes`

#### AgenteTikZ (Graficador-Experto)
- **Función:** Replicación visual TikZ con 98%+ de fidelidad
- **Ubicación:** `.claude/agents/graficador-tikz.md`
- **Uso:** Activado cuando se requiere nueva gráfica TikZ
- **Integración:** Guarda automáticamente en Repositorio TikZ

### 🎯 Sistema de Skills

Los skills son procedimientos automatizados que Claude puede ejecutar para tareas específicas.

#### Skills Principales del Workflow

| Skill | Función | Documentación |
|-------|---------|---------------|
| `/analizar-icfes` | Análisis según 6 dimensiones ICFES | `.claude/skills/analizar-icfes/skill.md` |
| `/generar-schoice` | Generar ejercicio tipo SCHOICE | `.claude/skills/generar-schoice/skill.md` |
| `/generar-cloze` | Generar ejercicio tipo CLOZE | `.claude/skills/generar-cloze/skill.md` |
| `/generar-grafica-nueva` | Generar nueva gráfica TikZ | `.claude/skills/generar-grafica-nueva/skill.md` |
| `/consultar-grafica-tikz` | Consultar repositorio TikZ | `.claude/skills/consultar-grafica-tikz/skill.md` |
| `/validar-diversidad-300` | Validar 300+ versiones únicas | `.claude/skills/validar-diversidad/skill.md` |
| `/corregir-error-imagen` | Corregir errores TikZ | `.claude/skills/corregir-error-imagen/skill.md` |
| `/promover-ejercicio` | Promover a producción | `.claude/skills/promover-ejercicio/skill.md` |

#### Skills de Validación

| Skill | Función | Documentación |
|-------|---------|---------------|
| `/validar-icfes` | Validar metadatos ICFES | `.claude/skills/validar-icfes/skill.md` |
| `/validar-coherencia` | Validar coherencia matemática | `.claude/skills/validar-coherencia/skill.md` |
| `/validar-renderizado` | Validar renderizado 4 formatos | `.claude/skills/validar-renderizado/skill.md` |
| `/diagnosticar-errores` | Diagnosticar errores automáticamente | `.claude/skills/diagnosticar-errores/skill.md` |

### 🔌 Sistema de Hooks

Hooks que se activan automáticamente durante el workflow:

| Hook | Evento | Función | Documentación |
|------|--------|---------|---------------|
| `pre-edit-rmd-validation` | Antes de editar .Rmd | Validar código antes de insertar | `.claude/hooks/pre-edit-rmd-validation.md` |
| `post-exams2-validation` | Después de exams2* | Capturar errores de renderizado | `.claude/hooks/post-exams2-validation.md` |
| `post-grafica-generada` | Después de generar gráfica | Guardar en repositorio TikZ | `.claude/hooks/post-grafica-generada.md` |
| `post-error-diagnostic` | Después de diagnosticar error | Activar corrección automática | `.claude/hooks/post-error-diagnostic.md` |

**Documentación completa:** `.claude/hooks/README.md`

### 📦 Repositorio TikZ Centralizado

Repositorio de gráficas TikZ validadas y reutilizables:

- **Ubicación:** `Repositorio-Graficas-TikZ/`
- **Estructura:** Por categoría/subcategoría con metadata JSON
- **Gestión:** Script `gestionar_repo_tikz.sh`
- **Integración:** Consulta automática durante generación de ejercicios

**Documentación:** `Repositorio-Graficas-TikZ/README.md`

## Flujo de Trabajo: Ciclo de Validación y Corrección Automática

**Diagrama completo:** Ver [`.claude/Mermaid_Chart.txt`](../Mermaid_Chart.txt)

### 🔄 FASE 1: Renderizado Inicial (OBLIGATORIO)

**Hook activado:** `post-exams2-validation`

```r
# Ejecutar renderizado completo
exams2html("archivo.Rmd", n = 1)
exams2pdf("archivo.Rmd", n = 1)
exams2pandoc("archivo.Rmd", n = 1, type = "docx")
exams2nops("archivo.Rmd", n = 1)
# Hook captura automáticamente errores/advertencias
```

**Qué hace el hook:**
- Captura resultados de cada función exams2*
- Registra errores y advertencias en log
- Activa diagnóstico automático si hay errores

### 🔍 FASE 2: Validación Visual y Funcional

Validación sistemática de 4 tipos de coherencia:

1. **Coherencia Matemática**: Fórmulas, cálculos, respuesta correcta
2. **Coherencia Imagen-Texto**: Descripción vs gráfico, valores sincronizados
3. **Coherencia de Código**: R ↔ Python ↔ TikZ sincronizado
4. **Renderizado 4 formatos**: HTML, PDF, DOCX, NOPS correctos

### ⚡ FASE 3: Decisión y Acción

**SI NO hay errores** → Continuar workflow normal → Promoción a producción

**SI hay errores** → Ejecutar subfases:

#### 📚 SUBFASE 3A: Corrección Basada en Ejemplos

**Hook activado:** `post-error-diagnostic`

```bash
# Hook activa automáticamente consulta a fuentes de verdad:
# 1. A-Produccion/Ejemplos-Funcionales-Rmd/
# 2. Repositorio-Graficas-TikZ/
# 3. .claude/docs/patrones-errores-conocidos.md

# Identificar patrones de solución en archivos similares
# Aplicar correcciones basadas en ejemplos validados
```

**Qué hace el hook:**
- Clasifica el tipo de error
- Consulta automáticamente ejemplos funcionales
- Sugiere correcciones basadas en patrones conocidos

#### 🔄 SUBFASE 3B: Ciclo de Revalidación (OBLIGATORIO)

```
⚠️ VOLVER AUTOMÁTICAMENTE A FASE 1
→ Repetir renderizado completo
→ NO TERMINAR hasta resolver TODOS los errores
```

**Ciclo se repite hasta:** Todos los errores resueltos

#### 📊 SUBFASE 3C: Gestión de Resultados (Solo si éxito)

**Actualización de múltiples fuentes de verdad:**

1. **Documentar error y solución** en `patrones-errores-conocidos.md`
2. **Actualizar ejemplos funcionales** si se crea nuevo patrón
3. **Actualizar repositorio TikZ** si se genera nueva gráfica
4. Incluir código completo (antes/después)
5. Documentar ejemplo funcional utilizado
6. Referenciar archivo .Rmd verificado

### ⛔ CONDICIONES CRÍTICAS
- ❌ NO terminar con errores sin resolver
- ❌ NUNCA proceder con errores pendientes
- ✓ Documentar SOLO después de confirmar solución
- ✓ Ejemplos funcionales = Fuente de verdad absoluta

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

## Integración con Repositorio TikZ

El sistema consulta automáticamente el **Repositorio TikZ** cuando se detectan gráficas matemáticas:

1. **Consulta automática** durante generación de ejercicios
2. **Decisión:** ¿Gráfica existe? → Reutilizar / Generar nueva
3. **Si existe:** Cargar código TikZ parametrizable del repositorio
4. **Si no existe:** Activar AgenteTikZ → Generar nueva → Guardar en repositorio
5. **Hook `post-grafica-generada`:** Guarda automáticamente nuevas gráficas

**Script de gestión:** `.claude/scripts/gestionar_repo_tikz.sh`

## Referencias Cruzadas

- **Diagrama de flujo completo:** [`.claude/Mermaid_Chart.txt`](../Mermaid_Chart.txt)
- **Guía de usuario:** [GUIA_USUARIO.md](GUIA_USUARIO.md)
- **Workflow paso a paso:** [WORKFLOW_PASO_A_PASO.md](WORKFLOW_PASO_A_PASO.md)
- **Sistema de hooks:** [`.claude/hooks/README.md`](../hooks/README.md)
- **Scripts de automatización:** [`.claude/scripts/README.md`](../scripts/README.md)

---

**Última actualización:** 2025-12-21
**Versión del sistema:** 2.0 (Arquitectura Modular)
**Estado:** ✅ Operacional con arquitectura modular completa
