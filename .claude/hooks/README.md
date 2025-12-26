# 🔌 Hooks de Automatización - Claude Code

Este directorio contiene la documentación de hooks que se activan automáticamente
durante el workflow de generación de ejercicios R/exams.

## ¿Qué son los Hooks?

Los hooks son puntos de activación automática que ejecutan validaciones o acciones
específicas cuando ocurren ciertos eventos en el workflow. Forman parte del
**Sistema de Hooks** integrado en la arquitectura modular.

**Diagrama completo:** Ver [`.claude/Mermaid_Chart.txt`](../Mermaid_Chart.txt)

## Hooks Implementados

### 1. `post-exams2-validation`

**Evento**: Después de ejecutar cualquier función exams2* (exams2html, exams2pdf, exams2docx, exams2nops)

**Ubicación:** `.claude/hooks/post-exams2-validation.md`

**Acción automática**:
1. Capturar resultado de la compilación
2. Registrar errores y advertencias en log
3. Si hay error → Activar skill `diagnosticar-errores`
4. Si éxito → Sugerir inspección visual
5. Consolidar resultados para FASE 2

**Integración con workflow:**
```
FASE 1: Renderizado Inicial
  ├── exams2html() → Hook captura resultado
  ├── exams2pdf() → Hook captura resultado
  ├── exams2docx() → Hook captura resultado
  └── exams2nops() → Hook captura resultado
      ↓
Hook: post-exams2-validation
  └── CAPTURAR Y REGISTRAR Errores y Advertencias
      ↓
FASE 2: Validación Visual y Funcional
```

**Documentación completa:** [post-exams2-validation.md](post-exams2-validation.md)

### 2. `pre-edit-rmd-validation`

**Evento**: Antes de editar un archivo .Rmd con cualquier herramienta de edición

**Ubicación:** `.claude/hooks/pre-edit-rmd-validation.md`

**Acción automática**:
1. Verificar coherencia del código a insertar
2. Detectar patrones problemáticos conocidos
3. Advertir si hay errores potenciales
4. Sugerir correcciones preventivas

**Patrones a detectar**:
- `abs(.*formateado)` → Advertir sobre Error #2 (formato numérico)
- `include_tikz` en chunk de generación → Advertir sobre Error #1 (renderizado condicional)
- Variables hardcodeadas en TikZ → Sugerir sincronización con R/Python
- Metadatos ICFES incompletos → Sugerir completar

**Integración con workflow:**
```
Antes de editar .Rmd
  ↓
Hook: pre-edit-rmd-validation
  ├── Analizar código nuevo
  ├── Buscar patrones problemáticos
  └── Si patrón detectado:
      └── Advertir + Sugerir corrección
  ↓
Proceder con edición (si todo OK)
```

**Documentación completa:** [pre-edit-rmd-validation.md](pre-edit-rmd-validation.md)

### 3. `post-error-diagnostic`

**Evento**: Después de diagnosticar un error automáticamente

**Ubicación:** `.claude/hooks/post-error-diagnostic.md`

**Acción automática**:
1. Clasificar el tipo de error según categorías conocidas
2. Consultar automáticamente fuentes de verdad:
   - `/A-Produccion/Ejemplos-Funcionales-Rmd/`
   - `Repositorio-Graficas-TikZ/`
   - `.claude/docs/patrones-errores-conocidos.md`
3. Activar skill de corrección apropiado
4. Iniciar SUBFASE 3A del ciclo de corrección

**Patrones manejados**:
| Patrón | Categoría | Acción |
|--------|-----------|--------|
| `File '*.png' not found` | ERR_G1 (Gráficos) | Activar `/corregir-error-imagen` |
| `LaTeX failed to compile` | ERR_T1 (Texto/Formato) | Consultar ejemplos funcionales |
| `non-numeric argument` | ERR_C3 (Coherencia) | Revisar sincronización R/Python/TikZ |
| `undefined control sequence` | ERR_T1 (Texto/Formato) | Verificar paquetes LaTeX |

**Integración con workflow:**
```
FASE 3: Decisión y Acción
  └── ✓ SÍ hay errores
      ↓
SUBFASE 3A: Corrección Basada en Ejemplos
  ↓
Hook: post-error-diagnostic
  ├── Clasificar error
  ├── Consultar fuentes de verdad
  └── Activar corrección automática
      ↓
Aplicar correcciones basadas en ejemplos validados
```

**Documentación completa:** [post-error-diagnostic.md](post-error-diagnostic.md)

### 4. `post-grafica-generada`

**Evento**: Después de generar una nueva gráfica TikZ con AgenteTikZ

**Ubicación:** `.claude/hooks/post-grafica-generada.md`

**Acción automática**:
1. Validar fidelidad visual (objetivo: 98%+)
2. Parametrizar código TikZ con placeholders
3. Generar metadata JSON con información completa
4. Guardar en `Repositorio-Graficas-TikZ/` según categoría
5. Actualizar índice del repositorio
6. Crear preview PNG para visualización

**Integración con workflow:**
```
E1a/E2a: Con Gráficos Matemáticos
  ↓
Consultar Repositorio TikZ
  └── No existe
      ↓
AgenteTikZ: Generar nueva gráfica
  ↓
Skill: /generar-grafica-nueva
  ↓
Hook: post-grafica-generada
  ├── Validar fidelidad visual
  ├── Parametrizar código
  ├── Generar metadata JSON
  └── Guardar en Repositorio TikZ
      ↓
Gráfica lista para reutilización
```

**Documentación completa:** [post-grafica-generada.md](post-grafica-generada.md)

## Integración con Ciclo de Validación Automática

Los hooks están integrados en el **Ciclo de Validación y Corrección Automática**:

```
┌─────────────────────────────────────────────────────────────┐
│         CICLO DE VALIDACIÓN Y CORRECCIÓN AUTOMÁTICA        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  🔄 FASE 1: RENDERIZADO INICIAL                            │
│     ├── exams2html()                                       │
│     ├── exams2pdf()                                        │
│     ├── exams2docx()                                       │
│     └── exams2nops()                                       │
│           ↓                                                │
│     🔌 Hook: post-exams2-validation                        │
│           └── Capturar errores/advertencias                │
│           ↓                                                │
│  🔍 FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL                 │
│     └── Coherencia: Matemática, Imagen-Texto, Código      │
│           ↓                                                │
│  ⚡ FASE 3: DECISIÓN Y ACCIÓN                             │
│     │                                                       │
│     ├── ❌ SIN ERRORES → Continuar workflow                │
│     │                                                       │
│     └── ✓ CON ERRORES:                                     │
│           ↓                                                │
│         📚 SUBFASE 3A: Corrección Basada en Ejemplos      │
│               ↓                                            │
│         🔌 Hook: post-error-diagnostic                     │
│               ├── Clasificar error                         │
│               ├── Consultar fuentes de verdad              │
│               └── Activar corrección automática            │
│               ↓                                            │
│         🔄 SUBFASE 3B: Revalidación Obligatoria            │
│               └── VOLVER A FASE 1 (ciclo se repite)       │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Flujo Específico de Hooks

### Hook: `pre-edit-rmd-validation`
```
Antes de editar .Rmd
  ↓
Hook valida código nuevo
  ├── Detecta patrones problemáticos
  └── Advierte si hay errores potenciales
  ↓
Proceder con edición (si todo OK)
```

### Hook: `post-exams2-validation`
```
Renderizado exams2*
  ↓
Hook captura resultados
  ├── Registra errores/advertencias
  └── Activa diagnóstico si hay errores
  ↓
FASE 2: Validación Visual y Funcional
```

### Hook: `post-error-diagnostic`
```
Error detectado
  ↓
Hook clasifica y consulta
  ├── Consulta ejemplos funcionales
  ├── Consulta repositorio TikZ
  └── Activa skill de corrección apropiado
  ↓
Aplicar correcciones basadas en ejemplos
```

### Hook: `post-grafica-generada`
```
AgenteTikZ genera nueva gráfica
  ↓
Hook valida y guarda
  ├── Valida fidelidad visual (98%+)
  ├── Parametrizar código TikZ
  ├── Generar metadata JSON
  └── Guardar en Repositorio TikZ
  ↓
Gráfica lista para reutilización
```

## Configuración

Los hooks están configurados en:
- **`.claude/settings.json`** - Configuración global de hooks
- **`.claude/settings.local.json`** - Permisos específicos para skills

## Referencias

- **Diagrama completo:** [`.claude/Mermaid_Chart.txt`](../Mermaid_Chart.txt)
- **Base de errores:** [`.claude/docs/patrones-errores-conocidos.md`](../docs/patrones-errores-conocidos.md)
- **Skills de corrección:** [`.claude/skills/`](../skills/)
- **Agentes especializados:** [`.claude/agents/`](../agents/)
- **Documentación técnica:** [`.claude/docs/README.md`](../docs/README.md)

---

**Última actualización:** 2025-12-21
**Versión:** 2.0 (Arquitectura Modular)
**Estado:** ✅ Operacional con 4 hooks implementados

