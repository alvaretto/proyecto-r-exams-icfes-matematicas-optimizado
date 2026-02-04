---
name: post-exams2-validation
trigger: PostToolUse (Bash)
implementation: .claude/hooks/post-exams2-validation.sh
script: .claude/scripts/validar_coherencia_matematica.R
description: >
  Hook AUTOMÁTICO que se activa después de cada comando Bash exitoso.
  Detecta si el comando contiene exams2* y ejecuta dos fases:
  FASE 2A: Validación de coherencia matemática (R script)
  FASE 2B: Generación automática de preview visual (PDF → PNG con magick)
  Soporta SCHOICE y CLOZE.
---

# Hook: Post-Renderizado exams2* (AUTOMÁTICO)

## Arquitectura

```
settings.json
  └── PostToolUse → matcher: "Bash"
        └── .claude/hooks/post-exams2-validation.sh
              ├── Detecta: ¿comando contiene exams2?
              │     └── NO → exit 0 (silencioso)
              │     └── SÍ → extrae archivo .Rmd
              │
              ├── FASE 2A: Coherencia matemática
              │     └── Ejecuta: validar_coherencia_matematica.R
              │           ├── Chunks R en entorno aislado
              │           ├── Metadatos (extype, exsolution, ICFES 6 dim)
              │           ├── Coherencia SCHOICE o CLOZE
              │           ├── Coherencia matemática general
              │           ├── Coherencia de código
              │           └── APROBADO → continuar / ERRORES → detener
              │
              └── FASE 2B: Preview visual (solo si 2A aprobó)
                    ├── Busca PDF generado en directorios conocidos
                    ├── Convierte PDF → PNG con magick (-density 150)
                    ├── Reporta rutas de PNGs generados
                    └── Emite instrucción OBLIGATORIA:
                          → Claude DEBE Read() cada PNG
                          → Claude DEBE verificar 5 coherencias
                          → Claude DEBE solicitar aprobación del usuario
```

## Activación

**AUTOMÁTICA** - Se registra en `settings.json` como hook `PostToolUse` para `Bash`.

Cada vez que Claude ejecuta un comando Bash que contiene `exams2html()`,
`exams2pdf()`, `exams2pandoc()`, `exams2nops()`, o `exams2moodle()`, el hook:

1. Extrae el nombre del archivo .Rmd del comando
2. **FASE 2A**: Ejecuta `validar_coherencia_matematica.R` sobre ese archivo
3. **FASE 2B**: Busca el PDF, convierte a PNG con `magick`, reporta rutas
4. Emite instrucción obligatoria para inspección visual

**NO requiere invocación manual.** Es transparente para el usuario.

## FASE 2A: Validaciones matemáticas

### Para SCHOICE:
- Formato binario de exsolution (ej: "1000")
- Exactamente 1 respuesta correcta
- Longitud de exsolution = número de opciones en Answerlist
- Variables numéricas sin NA/NaN/Inf
- Coherencia entre distancia, rapidez, tiempo (si aplica)

### Para CLOZE:
- Número de tipos en exclozetype = número de soluciones en exsolution
- Número de tolerancias en extol = número de tipos
- Tipos válidos (num, string, schoice, mchoice)
- Variables de solución sin NA/NaN/Inf
- solucion_schoice tiene exactamente 1 TRUE
- opciones_mezcladas sin duplicados
- Coherencia entre variables matemáticas (si aplica)

### Para ambos:
- Metadatos obligatorios presentes (exname, extype, exsolution)
- exshuffle = TRUE
- 6 dimensiones ICFES completas
- Sin funciones matemáticas sobre variables formateadas (ERR_C3)
- Chunks R ejecutan sin errores

## FASE 2B: Validación visual automática

### Qué hace el hook:
1. **Busca PDF** en orden de prioridad:
   - Directorio `dir=` explícito del comando exams2*
   - `output_pdf/` (directorio estándar)
   - `output/`
   - Directorio de trabajo actual
2. **Convierte PDF → PNG**: `magick -density 150 [pdf] -quality 90 preview_[nombre].png`
3. **Reporta rutas** de todos los PNGs generados (soporta múltiples páginas)
4. **Emite instrucción obligatoria** para que Claude ejecute Read() y verifique coherencias

### Si no encuentra PDF:
- Emite aviso indicando que Claude DEBE ejecutar `exams2pdf()` primero
- Al ejecutar `exams2pdf()`, el hook se vuelve a activar y genera el preview

### Dependencias:
- `magick` (ImageMagick 7+) debe estar instalado en el sistema
- `python3` con módulo `json` (para parsear JSON de stdin — jq no disponible)
- Si magick no está disponible, emite instrucciones para conversión manual

### Notas importantes:
- El comando en settings.json usa `$CLAUDE_PROJECT_DIR` (variable que Claude Code inyecta)
- **Cambios a settings.json NO toman efecto inmediato** — requieren reinicio de sesión o revisar `/hooks`
- El script es ejecutable (`chmod +x`)

## Archivos involucrados

| Archivo | Rol |
|---------|-----|
| `.claude/settings.json` | Registra el hook PostToolUse |
| `.claude/hooks/post-exams2-validation.sh` | Script shell: filtra, valida, genera preview |
| `.claude/scripts/validar_coherencia_matematica.R` | Script R: validación matemática |

## Flujo integrado con ciclo de validación

```
FASE 1: Renderizado (exams2html/pdf/pandoc/nops)
    ↓ [Compilación exitosa]
    ↓
FASE 2A: VALIDACIÓN MATEMÁTICA AUTOMÁTICA ← ESTE HOOK
    ↓ [Hook PostToolUse → .sh → .R]
    ↓
    ├── ERRORES → Claude corrige → Volver a FASE 1
    │
    └── APROBADO ↓
                 ↓
FASE 2B: PREVIEW VISUAL AUTOMÁTICO ← ESTE HOOK
    ↓ [Hook → magick PDF→PNG → reporta rutas]
    ↓
    Claude DEBE:
    ├── Read() cada PNG generado
    ├── Verificar 5 coherencias visualmente
    ├── Documentar hallazgos con checklist
    └── Solicitar aprobación del usuario
         ↓
         ├── Aprobado → FIN ✅
         └── Rechazado → Corregir → Volver a FASE 1
```

## Versión

- **v4.1** (2026-02-03): Fix: jq → python3 para parsear JSON, $CLAUDE_PROJECT_DIR para ruta
- **v4.0** (2026-02-03): FASE 2B integrada — preview visual automático con magick
- **v3.0** (2026-02-03): Hook real ejecutable en settings.json (solo FASE 2A)
- **v2.0** (2025-12-31): Documentación de referencia (sin ejecución automática)
- **v1.0** (2025-12-30): Versión inicial
