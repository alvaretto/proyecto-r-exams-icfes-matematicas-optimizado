# Ciclo de Validación y Corrección Automática

## OBLIGATORIO para todo archivo .Rmd

Cada vez que se renderiza un archivo .Rmd, se ejecuta automáticamente:

### 🔄 FASE 1: RENDERIZADO INICIAL
```r
exams2html("archivo.Rmd", n = 1)
exams2pdf("archivo.Rmd", n = 1)
exams2pandoc("archivo.Rmd", n = 1, type = "docx")
exams2nops("archivo.Rmd", n = 1)
```
Capturar y registrar todos los errores/advertencias.

### 🔍 FASE 2: VALIDACIÓN VISUAL ITERATIVA (OBLIGATORIA)

**⚠️ CRÍTICO: Esta fase requiere INSPECCIÓN VISUAL REAL, no solo verificar que el archivo existe.**

#### PASO 2.1: Convertir PDF a imagen y MOSTRAR al usuario
```bash
# OBLIGATORIO: Convertir PDF a PNG para inspección visual
magick -density 150 output_pdf/plain1.pdf -quality 90 preview.png

# OBLIGATORIO: Leer la imagen con Read tool para ver el resultado
# Read("preview.png")
```

#### PASO 2.2: Verificar las 5 coherencias VISUALMENTE

| # | Coherencia | Verificación VISUAL obligatoria |
|---|------------|--------------------------------|
| 1 | **Semántica** | ¿Texto legible? ¿Tildes correctas? ¿Gramática OK? |
| 2 | **Visual-Texto** | ¿Gráfico coincide con enunciado? ¿Valores sincronizados? |
| 3 | **Matemática** | ¿Fórmulas correctas? ¿Proporciones correctas? |
| 4 | **Código** | ¿Elementos dinámicos funcionan? ¿Datos cambian? |
| 5 | **General** | ¿Legible? ¿Estilo ICFES? ¿Opciones visibles? |

#### PASO 2.3: Documentar hallazgos

```markdown
## Revisión Visual Iteración [N]

### Coherencias verificadas:
- [ ] Semántica: [OK/Problema: descripción]
- [ ] Visual-Texto: [OK/Problema: descripción]
- [ ] Matemática: [OK/Problema: descripción]
- [ ] Código: [OK/Problema: descripción]
- [ ] General: [OK/Problema: descripción]

### Problemas detectados:
1. [Descripción del problema]
2. [Descripción del problema]

### Acción requerida:
- [ ] Ninguna - Pasar a FASE 3
- [ ] Corregir y volver a FASE 1
```

#### PASO 2.4: COMPARAR con imagen original (si aplica)

Si el ejercicio tiene gráfico basado en imagen ICFES original:
```bash
# Mostrar imagen original
Read("imagen_original.png")

# Mostrar imagen generada
Read("preview.png")

# OBLIGATORIO: Comparar visualmente y documentar diferencias
```

### ⚡ FASE 3: DECISIÓN Y ACCIÓN

**SI todas las coherencias OK** → Solicitar aprobación del usuario antes de continuar

**SI hay problemas detectados** → Ejecutar subfases OBLIGATORIAS:

#### 📚 SUBFASE 3A: Corrección Basada en Ejemplos
```bash
# SIEMPRE consultar ejemplos funcionales ANTES de corregir
ls /A-Produccion/Ejemplos-Funcionales-Rmd/
# Identificar patrones de solución en archivos similares
# Aplicar correcciones basadas en ejemplos validados
```

#### 🔄 SUBFASE 3B: Ciclo de Revalidación (OBLIGATORIO)
```
⚠️ VOLVER AUTOMÁTICAMENTE A FASE 1
→ Repetir renderizado completo
→ NO TERMINAR hasta resolver TODOS los errores
```

#### 📊 SUBFASE 3C: Gestión de Resultados (Solo si éxito completo)

1. Documentar error y solución en `patrones-errores-conocidos.md`
2. Incluir código completo (antes/después)
3. Documentar ejemplo funcional utilizado
4. Referenciar archivo .Rmd verificado

## ⛔ CONDICIONES CRÍTICAS

- ❌ NO terminar con errores sin resolver
- ❌ NUNCA proceder con errores pendientes
- ❌ **NUNCA marcar como "completado" sin inspección visual REAL**
- ❌ **NUNCA asumir que "archivo existe" = "renderizado correcto"**
- ✓ Documentar SOLO después de confirmar solución
- ✓ Ejemplos funcionales = Fuente de verdad absoluta
- ✓ VOLVER A FASE 1 después de cada corrección
- ✓ **SIEMPRE mostrar preview.png al usuario antes de aprobar**
- ✓ **SIEMPRE documentar las 5 coherencias con checklist**

## 🔁 PROCESO ITERATIVO OBLIGATORIO

```
┌─────────────────────────────────────────────────────────┐
│  ITERACIÓN N                                            │
├─────────────────────────────────────────────────────────┤
│  1. FASE 1: Renderizar (HTML, PDF, DOCX)               │
│  2. FASE 2: Convertir PDF → PNG                         │
│  3. FASE 2: MOSTRAR imagen al usuario (Read tool)       │
│  4. FASE 2: Verificar 5 coherencias VISUALMENTE         │
│  5. FASE 2: Documentar hallazgos                        │
│  6. FASE 3: ¿Problemas detectados?                      │
│     │                                                   │
│     ├── SÍ → Corregir → VOLVER A ITERACIÓN N+1         │
│     │                                                   │
│     └── NO → Solicitar aprobación usuario               │
│              │                                          │
│              ├── Usuario aprueba → FIN ✅               │
│              │                                          │
│              └── Usuario rechaza → VOLVER A ITERACIÓN   │
└─────────────────────────────────────────────────────────┘
```

## ❌ ANTIPATRONES PROHIBIDOS

### 1. Validación "ciega" (PROHIBIDO)
```r
# ❌ INCORRECTO - Solo verificar que el archivo existe
exams2pdf("archivo.Rmd", n = 1)
# "El PDF se generó correctamente" ← INSUFICIENTE
```

### 2. Asumir éxito sin inspección (PROHIBIDO)
```markdown
# ❌ INCORRECTO
"Los 3 formatos se generaron correctamente. Ejercicio completado."
# ← NO se mostró ninguna imagen, NO se verificaron coherencias
```

### 3. Saltarse comparación visual (PROHIBIDO)
```markdown
# ❌ INCORRECTO - Ejercicio con gráfico
"El gráfico se generó." ← Sin comparar con original
```

## ✅ PATRÓN CORRECTO

```markdown
## Ciclo de Validación - Iteración 1

### FASE 1: Renderizado
- HTML: ✅ plain1.html (XXkb)
- PDF: ✅ plain1.pdf (XXkb)
- DOCX: ✅ pandoc1.docx (XXkb)

### FASE 2: Inspección Visual

[Imagen preview.png mostrada aquí]

#### Coherencias verificadas:
- [x] Semántica: Texto legible, tildes correctas
- [x] Visual-Texto: Gráfico coincide con enunciado, valores correctos
- [x] Matemática: Fórmulas correctas, cálculos verificados
- [x] Código: Elementos dinámicos funcionando (contexto: parque natural, precios: $27,5/$20)
- [x] General: Legible, estilo ICFES, opciones visibles

#### Comparación con original (si aplica):
[Imagen original mostrada]
[Imagen generada mostrada]
Diferencias: Ninguna significativa / [Lista de diferencias]

### FASE 3: Decisión
✅ Todas las coherencias OK - Solicitar aprobación del usuario

**¿Aprueba este ejercicio?**
```

---

**Versión**: 2.0 (Validación Visual Iterativa Obligatoria)
**Fecha**: 2025-12-31
**Cambio crítico**: FASE 2 ahora requiere inspección visual REAL con imagen mostrada
