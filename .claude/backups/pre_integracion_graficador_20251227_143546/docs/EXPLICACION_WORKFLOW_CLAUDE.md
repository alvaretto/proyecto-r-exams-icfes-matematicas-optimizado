# Workflow del Sistema .claude - Explicacion Completa

Este documento explica la arquitectura y funcionamiento del directorio `.claude/` que automatiza la generacion de ejercicios ICFES R-Exams.

---

## Estructura del Directorio

```
.claude/
├── agents/           # Agentes especializados con IA
├── skills/           # Skills invocables con /comando
├── hooks/            # Automatizaciones post-evento
├── commands/         # Comandos legacy (referencia)
├── docs/             # Documentacion tecnica
├── scripts/          # Scripts de automatizacion
├── tests/            # Tests de validacion
├── backups/          # Backups de archivos
├── logs/             # Logs de ejecucion
├── deprecated/       # Archivos deprecados
├── Mermaid_Chart.txt # Diagrama de flujo maestro
├── settings.json     # Configuracion de hooks
└── settings.local.json # Permisos de skills
```

---

## Arquitectura Modular

El sistema tiene 5 capas que trabajan en conjunto:

```
┌─────────────────────────────────────────────┐
│     1. AGENTES ESPECIALIZADOS               │
│     ClasificadorICFES, AgenteTikZ           │
│                    ↓                        │
│     2. SISTEMA DE SKILLS                    │
│     /analizar-icfes, /generar-schoice...    │
│                    ↓                        │
│     3. SISTEMA DE HOOKS                     │
│     post-exams2-validation, etc.            │
│                    ↓                        │
│     4. REPOSITORIO TIKZ                     │
│     Graficas reutilizables parametrizables  │
│                    ↓                        │
│     5. DOCUMENTACION (Fuentes de Verdad)    │
│     patrones-errores-conocidos.md           │
└─────────────────────────────────────────────┘
```

---

## 1. Agentes Especializados (`agents/`)

Agentes con responsabilidades especificas:

| Agente | Funcion | Archivo |
|--------|---------|---------|
| **ClasificadorICFES** | Analiza ejercicios segun 6 dimensiones ICFES | `clasificador-icfes.md` |
| **AgenteTikZ** | Replica graficas TikZ con 98%+ fidelidad | `graficador-tikz.md` |
| **ValidadorVisual** | Valida renderizado sistematicamente | `validador-visual.md` |
| **CorrectorCoherencia** | Corrige coherencias matematicas | `corrector-coherencia.md` |
| **DiagnosticadorErrores** | Diagnostica errores automaticamente | `diagnosticador-errores.md` |

### Las 6 Dimensiones ICFES

El ClasificadorICFES analiza:

1. **Nivel de Dificultad**: 1 (0-35pts) a 4 (71-100pts)
2. **Competencia**: Interpretacion (34%), Formulacion (43%), Argumentacion (23%)
3. **Componente**: Numerico-Variacional, Geometrico-Metrico, Aleatorio
4. **Tipo de Pensamiento**: Numerico, Espacial, Metrico, Variacional, Aleatorio
5. **Contenido Curricular**: Algebra/Calculo, Geometria, Estadistica
6. **Eje Axial**: Puramente Matematico vs Aplicado/Contextualizado

---

## 2. Sistema de Skills (`skills/`)

Skills son comandos invocables con `/nombre`:

### Skills de Generacion

| Skill | Proposito |
|-------|-----------|
| `/analizar-icfes` | Analiza imagen segun 6 dimensiones ICFES |
| `/generar-schoice` | Genera ejercicio seleccion unica |
| `/generar-cloze` | Genera ejercicio pregunta compuesta |
| `/generar-grafica-nueva` | Genera grafica TikZ y guarda en repositorio |

### Skills de Validacion

| Skill | Proposito |
|-------|-----------|
| `/validar-renderizado` | Valida renderizado en 4 formatos |
| `/validar-coherencia` | Valida coherencia matematica/imagen/codigo |
| `/validar-diversidad` | Valida 300+ versiones unicas |
| `/diagnosticar-errores` | Diagnostica errores automaticamente |

### Skills de Correccion

| Skill | Proposito |
|-------|-----------|
| `/corregir-error-imagen` | Corrige errores de imagenes faltantes |
| `/corregir-graficos` | Corrige errores graficos |
| `/promover-ejercicio` | Mueve ejercicio a produccion |

---

## 3. Sistema de Hooks (`hooks/`)

Hooks se activan automaticamente en eventos:

| Hook | Evento Trigger | Accion |
|------|----------------|--------|
| `pre-edit-rmd-validation` | Antes de editar .Rmd | Valida codigo antes de insertar |
| `post-exams2-validation` | Despues de exams2* | Captura errores de renderizado |
| `post-grafica-generada` | Al generar grafica | Guarda en repositorio TikZ |
| `post-error-diagnostic` | Al detectar error | Activa correccion automatica |

---

## 4. Ciclo de Validacion y Correccion

El ciclo tiene 3 fases obligatorias:

### FASE 1: Renderizado Inicial

```r
exams2html("archivo.Rmd", n = 1)
exams2pdf("archivo.Rmd", n = 1)
exams2pandoc("archivo.Rmd", n = 1, type = "docx")
exams2nops("archivo.Rmd", n = 1)
```

**Hook activo:** `post-exams2-validation` captura errores.

### FASE 2: Validacion Visual y Funcional

Validacion de 4 tipos de coherencia:

- **Coherencia Matematica**: Formulas, calculos, respuesta correcta
- **Coherencia Imagen-Texto**: Descripcion vs grafico sincronizado
- **Coherencia de Codigo**: R ↔ Python ↔ TikZ sincronizado
- **Renderizado 4 formatos**: HTML, PDF, DOCX, NOPS

### FASE 3: Decision y Accion

```
¿ERRORES ENCONTRADOS?
       │
       ├── NO → Continuar workflow → Promocion
       │
       └── SI → Subfases:
                ├── 3A: Correccion basada en ejemplos
                ├── 3B: VOLVER A FASE 1 (ciclo obligatorio)
                └── 3C: Documentar solucion exitosa
```

### Condiciones Criticas

- **NO** terminar con errores sin resolver
- **NUNCA** proceder con errores pendientes
- Documentar **SOLO** despues de confirmar solucion
- Ejemplos funcionales = Fuente de verdad absoluta

---

## 5. Repositorio TikZ (`Repositorio-Graficas-TikZ/`)

Repositorio centralizado de graficas reutilizables:

```
Repositorio-Graficas-TikZ/
├── geometria/
│   ├── cilindros/
│   ├── conos/
│   └── prismas/
├── estadistica/
│   ├── barras/
│   └── histogramas/
└── probabilidad/
    └── arboles_decision/
```

Cada grafica incluye:
- `[nombre].tikz` - Codigo TikZ parametrizable
- `[nombre].json` - Metadata completa
- `[nombre].png` - Preview visual

### Integracion Automatica

1. Durante generacion, el sistema consulta repositorio
2. Si existe grafica similar → Reutiliza
3. Si no existe → Genera nueva con AgenteTikZ
4. Hook `post-grafica-generada` guarda automaticamente

---

## Workflow Tipico de Usuario

### Crear Nuevo Ejercicio

```bash
# 1. Analizar imagen del ejercicio ICFES
/analizar-icfes ruta/imagen.png

# 2. Generar ejercicio (SCHOICE o CLOZE)
/generar-schoice

# 3. Validar renderizado
/validar-renderizado

# 4. Si hay errores, diagnosticar
/diagnosticar-errores

# 5. Validar diversidad
/validar-diversidad

# 6. Promover a produccion
/promover-ejercicio nombre_ejercicio.Rmd
```

---

## Documentos Clave

| Documento | Proposito |
|-----------|-----------|
| `Mermaid_Chart.txt` | Diagrama de flujo completo del sistema |
| `docs/WORKFLOW_PASO_A_PASO.md` | Guia paso a paso detallada |
| `docs/GUIA_USUARIO.md` | Referencia rapida de skills |
| `docs/patrones-errores-conocidos.md` | Base de conocimiento de errores |
| `docs/TRES_NIVELES_VALIDACION.md` | Metodologia de validacion |
| `TROUBLESHOOTING.md` | Solucion de problemas comunes |

---

## Fuentes de Verdad

El sistema usa estas fuentes para correccion automatica:

1. **`A-Produccion/Ejemplos-Funcionales-Rmd/`** - Ejercicios validados
2. **`Repositorio-Graficas-TikZ/`** - Graficas validadas
3. **`.claude/docs/patrones-errores-conocidos.md`** - Soluciones verificadas

---

## Configuracion

### `settings.json` - Hooks globales

Define que hooks se activan y cuando.

### `settings.local.json` - Permisos de skills

Define que skills pueden ejecutarse sin confirmacion del usuario.

---

## Resumen Visual del Flujo

```
IMAGEN ICFES
     ↓
┌────────────────────┐
│ /analizar-icfes    │ ← AgenteClasficadorICFES (6 dimensiones)
└────────────────────┘
     ↓
┌────────────────────┐
│ /generar-schoice   │ ← Consulta Repositorio TikZ
│ /generar-cloze     │   Si no existe grafica → AgenteTikZ
└────────────────────┘
     ↓
┌────────────────────┐
│ FASE 1: Renderizar │ ← Hook: post-exams2-validation
│ exams2html/pdf...  │
└────────────────────┘
     ↓
┌────────────────────┐
│ FASE 2: Validar    │ ← 4 coherencias + 4 formatos
│ Visual y Funcional │
└────────────────────┘
     ↓
┌────────────────────┐
│ FASE 3: Decidir    │
│ ¿Hay errores?      │
└────────────────────┘
     │
     ├── NO → /promover-ejercicio → PRODUCCION
     │
     └── SI → SUBFASE 3A (corregir)
              SUBFASE 3B (volver FASE 1)
              SUBFASE 3C (documentar)
```

---

**Ubicacion de este archivo:** `.claude/docs/EXPLICACION_WORKFLOW_CLAUDE.md`

**Fecha de creacion:** 2025-12-27

**Version del sistema:** 2.0 (Arquitectura Modular)
