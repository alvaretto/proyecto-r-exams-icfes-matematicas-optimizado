---
description: Mueve un ejercicio a 03-En-Produccion/ SOLO despues de validacion en aula con estudiantes reales (Nivel 3). ULTIMO paso del workflow.
---

# Promover Ejercicio a Produccion

## ⚡ PRERREQUISITO: Validacion en Terreno (Nivel 3) Completada

Este comando es el **ULTIMO PASO** del workflow. Se ejecuta **SOLO** despues de que el ejercicio ha sido probado con estudiantes reales en el aula.

```
Niveles 1+2: Validacion automatica ✅
    │
    ▼
Ejercicio en 02-En-Desarrollo/ (Listo para Aula)
    │
    ▼
Nivel 3: Aplicado en aula con estudiantes ✅
    │
    ▼
/promover-ejercicio ← ESTE COMANDO (ULTIMO PASO)
```

## Parametros de entrada

- **$ARGUMENTS**: Nombre del archivo .Rmd a promover

## ⛔ PRERREQUISITOS (TODOS OBLIGATORIOS)

### Automaticos (Nivel 1+2)
- ✅ Renderizado exitoso en 4 formatos
- ✅ 5 coherencias verificadas
- ✅ 200+ versiones unicas
- ✅ Detractor aprobado

### Terreno (Nivel 3) - OBLIGATORIO
- ✅ **Aplicado en aula** con estudiantes reales
- ✅ **Tasa de acierto** entre 25% y 95%
- ✅ **Sin ambiguedades** reportadas
- ✅ **Tiempo de resolucion** razonable
- ✅ **Feedback** documentado

**⛔ SIN EVIDENCIA DE NIVEL 3, BLOQUEAR PROMOCION.**

## Proceso

### 1. Confirmar evidencia de Nivel 3
Preguntar al usuario:
- ¿Aplicado en aula? ¿Tasa de acierto? ¿Ambiguedades? ¿Feedback?

### 2. Mover a 03-En-Produccion/[categoria]/
### 3. Registrar datos de validacion en terreno

## Regla de Oro
**`03-En-Produccion/` = ejercicios probados con estudiantes reales.** Validacion automatica sola NO es suficiente.

## Referencias

- `.claude/docs/TRES_NIVELES_VALIDACION.md`
- `.claude/skills/promover-ejercicio/SKILL.md`

