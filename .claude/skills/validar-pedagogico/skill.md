---
name: validar-pedagogico
description: Análisis pedagógico avanzado basado en evidencias científicas y taxonomías cognitivas modernas.
hidden: false
---

# Validador Pedagógico ICFES (Skill)

**IMPORTANTE**: Este skill es CONSULTIVO. Claude lo sugiere proactivamente pero requiere aprobación del usuario para ejecutar.

## 🎯 Cuándo Activar (Proactivo)

Claude DEBE sugerir este skill cuando:

1. **Ejercicio completado y validado**
   - Archivo .Rmd generado
   - Ciclo de Validación completado (FASE 1, 2, 3)
   - Todos los formatos renderizados correctamente

2. **Usuario solicita análisis de calidad**
   - "¿Qué tan bueno está este ejercicio?"
   - "¿Cumple con estándares pedagógicos?"
   - "¿Los distractores están bien diseñados?"

3. **Ejercicio listo para promover**
   - Antes de ejecutar `/promover-ejercicio`
   - Para verificar calidad pedagógica final

## ⛔ Cuándo NO Activar

NO sugerir este skill si:
- El ejercicio aún tiene errores de compilación
- Faltan metadatos ICFES
- No se ha completado el Ciclo de Validación
- Es una consulta rápida (use `/analizar-icfes` en su lugar)

## 📝 Mensaje de Activación

Cuando Claude sugiere este skill:

```markdown
## ✨ Análisis Pedagógico Disponible

El ejercicio ha sido validado exitosamente. ¿Te gustaría realizar un
**análisis pedagógico profundo** que incluya:

- ✓ Clasificación con 3 taxonomías cognitivas (Bloom, SOLO, Webb)
- ✓ Validación exhaustiva del Marco Conceptual ICFES 2026
- ✓ Evaluación de distractores por tipología de error
- ✓ Optimización con 7 principios de aprendizaje científicos
- ✓ Puntuación compuesta 0-100 con recomendaciones

Este análisis toma 5-10 minutos y usa Claude Opus 4.5.

**¿Deseas ejecutar el análisis pedagógico?** (Sí/No/Más tarde)
```

## 🔧 Ejecución

Si el usuario aprueba:

1. Invocar agente PedagogoICFES con modelo Opus 4.5
2. Pasar ruta del archivo .Rmd
3. Esperar análisis completo (5-10 min)
4. Presentar reporte estructurado al usuario
5. Ofrecer aplicar recomendaciones automáticamente

## 📊 Integración con Workflow

```
/generar-schoice o /generar-cloze
    │
    ▼
🔄 FASE 1: Validación automática (renderizado)
    │
    ▼
🔍 FASE 2: Preview visual (coherencias)
    │
    ▼
✅ Validación exitosa
    │
    ├─→ [Sugerencia automática] 📚 /validar-pedagogico ← ESTE SKILL
    │
    └─→ /promover-ejercicio
```

## 🧠 Agente Asociado

**Archivo**: `.claude/agents/pedagogo-icfes.md`

**Nombre**: PedagogoICFES

**Modelo**: Claude Opus 4.5 (máxima capacidad cognitiva)

**Herramientas**: read, glob, grep, bash

**Documentos consultados**:
- `errores-conceptuales-matematicas.md` (500+ líneas)
- `principios-aprendizaje-evidencias.md` (600+ líneas)
- `taxonomias-cognitivas-integradas.md` (600+ líneas)
- `marco-conceptual-icfes-2026.md` (600+ líneas)
- `diseno-distractores-tipologia.md` (600+ líneas)

## 📚 Base Científica

- 30+ referencias de investigación peer-reviewed
- Taxonomías: Anderson & Krathwohl (2001), Biggs & Collis (1982), Webb (1997)
- Aprendizaje: Karpicke (2008), Cepeda (2006), Rohrer (2007), Dunlosky (2013)
- Errores matemáticos: Clement (1982), Fischbein (1985), Vinner (1980)
- Marco ICFES oficial 2024-2026

## 🎓 Ejemplo de Uso

**Contexto**: Usuario acaba de completar validación de ejercicio

```
Claude: ✅ El ejercicio se validó exitosamente. Todos los formatos
        renderizados correctamente.

        📚 Análisis Pedagógico Disponible

        ¿Te gustaría un análisis pedagógico profundo? Incluye:
        - Taxonomías cognitivas (Bloom, SOLO, Webb)
        - Validación ICFES 2026 exhaustiva
        - Evaluación de distractores
        - Optimización con principios científicos
        - Puntuación 0-100 + recomendaciones

        ⏱️ Duración: 5-10 min | Modelo: Opus 4.5

        ¿Ejecutar análisis? (Sí/No/Más tarde)

Usuario: Sí

Claude: Perfecto. Iniciando análisis pedagógico con PedagogoICFES...

        [Task tool invoca PedagogoICFES]

        [10 minutos después]

        # ANÁLISIS PEDAGÓGICO COMPLETO

        ## Puntuación Final: 87/100 (A-)

        ### Fortalezas
        - Excelente diseño de distractores (24/25 pts)
        - Validación ICFES completa (20/20 pts)

        ### Áreas de Mejora
        - Aplicar más principios de aprendizaje (15/20 pts)
        - Nivel DOK podría ser más alto (17/20 pts)

        [Reporte completo de 5 módulos...]

        ¿Deseas que aplique las recomendaciones automáticamente?
```

---

**Versión**: 1.0.0
**Fecha**: 2026-02-04
**Tipo**: Skill consultivo proactivo
**Modelo**: Claude Opus 4.5
