# Ejemplos Funcionales - Biblioteca de Referencia

## 🎯 Propósito

Este directorio contiene **copias exactas de ejercicios .Rmd validados** que han pasado todas las fases del ciclo de validación y están en producción.

### Función en el Sistema

1. **Fuente de Verdad**: Patrones de solución 100% validados
2. **SUBFASE 3A**: Consulta obligatoria durante corrección de errores
3. **Generación**: Base de conocimiento para nuevos ejercicios
4. **Detractor**: Comparación con estándares validados

---

## 📋 Criterios de Inclusión

### Un ejercicio entra aquí SOLO si cumple:

- ✅ Pasó FASE 1 (renderizado en 4 formatos sin errores)
- ✅ Pasó FASE 2A (validación matemática automática)
- ✅ Pasó FASE 2B (inspección visual con 5 coherencias OK)
- ✅ Pasó FASE 2C (revisión detractor APROBADO)
- ✅ Pasó FASE 3 (aprobación del usuario)
- ✅ Generó 200+ versiones únicas verificadas
- ✅ Está en producción y ha sido usado exitosamente

**⚠️ NO incluir**:

- Ejercicios en desarrollo
- Ejercicios con errores conocidos
- Ejercicios experimentales
- Ejercicios parcialmente validados

---

## 📁 Estructura Actual

```
Ejemplos-Funcionales-Rmd/
├── README.md                    [Este archivo - Guía técnica]
├── CATALOGO.md                  [Índice searchable de ejercicios]
│
├── [Ejercicios .Rmd validados]  [Actualmente en raíz]
│   ├── Ejemplo_00_numeros_triangulares_sucesion_argumentacion_n2_v1.Rmd
│   ├── Ejemplo_01.Rmd
│   ├── Ejemplo_02.Rmd
│   ├── Ejemplo_03.Rmd
│   └── estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd
│
└── [Assets compartidos]
    ├── diagrama_a.png
    ├── diagrama_b.png
    ├── diagrama_c.png
    ├── diagrama_d.png
    └── preview_*.png
```

### Estructura Futura Recomendada

```
Ejemplos-Funcionales-Rmd/
├── README.md
├── CATALOGO.md
│
├── schoice/
│   ├── estadistica/
│   ├── algebra/
│   ├── geometria/
│   └── funciones/
│
├── cloze/
│   ├── estadistica/
│   ├── algebra/
│   └── progressive-disclosure/
│
└── assets/
    └── [gráficos compartidos]
```

---

## 🔄 Workflow para Agregar Nuevo Ejercicio

### Paso 1: Ejercicio Validado en En-Desarrollo o En-Produccion

```bash
# Opción A: Ejercicio en desarrollo que pasó FASE 3
A-Produccion/02-En-Desarrollo/[ejercicio]/
├── [ejercicio].Rmd              # ← Archivo a copiar
├── output_pdf/
├── output_html/
├── output_docx/
└── output_nops/

# Opción B: Ejercicio ya en producción organizado por tema
A-Produccion/03-En-Produccion/[Tema]/Pensamiento-XXX/
└── [ejercicio].Rmd              # ← Archivo a copiar
```

### Paso 2: Decisión de Promoción

**¿Cuándo copiar a Ejemplos-Funcionales-Rmd/?**

- ✅ El ejercicio ha sido usado en producción
- ✅ Representa un **patrón útil** para futuros ejercicios
- ✅ Implementa técnicas que queremos reutilizar
- ✅ Es ejemplo de **buenas prácticas**

**NO copiar si**:

- ❌ Es ejercicio experimental
- ❌ Tiene características muy específicas/únicas
- ❌ No aporta nuevos patrones al catálogo

### Paso 3: Crear Symlink

```bash
# Desde el directorio Ejemplos-Funcionales-Rmd
cd A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/

# Opción A: Desde En-Desarrollo (ejercicio recién validado)
ln -s ../../02-En-Desarrollo/[ejercicio]/[ejercicio].Rmd [ejercicio].Rmd

# Opción B: Desde En-Produccion (ejercicio ya organizado por tema)
ln -s ../06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/[ejercicio].Rmd [ejercicio].Rmd

# Opción C: Usar script automatizado (recomendado)
.claude/scripts/agregar_ejemplo_funcional.sh \
  A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/[ejercicio].Rmd \
  EST-INT-05
```

**⚠️ IMPORTANTE**: Usar rutas relativas (no absolutas) para portabilidad.

### Paso 4: Renombrar según Convención

```bash
# Formato:
[area]_[tema]_[competencia]_[componente]_[nivel]_[tipo]_v[version].Rmd

# Ejemplos:
estadistica_diagramas_caja_interpretacion_representacion_Nivel2_schoice_v2.Rmd
algebra_ecuaciones_lineales_formulacion_aleatorio_Nivel3_schoice_v1.Rmd
geometria_triangulos_semejanza_argumentacion_espacial_Nivel2_cloze_v1.Rmd
```

### Paso 5: Asignar Código de Identificación

```
[AREA]-[COMP]-[NUM]

Ejemplos:
EST-INT-01   # Estadística, Interpretación, 01
ALG-FOR-02   # Álgebra, Formulación, 02
GEO-ARG-01   # Geometría, Argumentación, 01
```

### Paso 6: Actualizar CATALOGO.md

Agregar entrada completa con:

```markdown
### [CODIGO]: [Título Descriptivo]

**Archivo**: `[nombre_archivo].Rmd`

**Metadatos**:
- **Nivel**: [1|2|3|4]
- **Competencia**: [Interpretación|Formulación|Argumentación]
- **Componente**: [Aleatorio|Datos|Espacial|Medida|Cambio]
- **Tipo**: [SCHOICE|CLOZE]
- **Características técnicas**:
  - [Lista de técnicas implementadas]

**Patrón útil para**:
- [Caso de uso 1]
- [Caso de uso 2]

**Versiones únicas**: [número]

**⚠️ Notas especiales**: [Si aplica]
```

### Paso 7: Actualizar Estadísticas

```bash
# Recalcular en CATALOGO.md:
- Total ejercicios
- SCHOICE vs CLOZE
- Por nivel
- Por componente
- Por competencia
```

### Paso 8: Commit

```bash
git add A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/
git commit -m "feat(ejemplos): Agregar [CODIGO] - [Título]

- Archivo: [nombre].Rmd
- Características: [lista breve]
- Patrón útil para: [uso principal]
"
```

---

## 🔍 Cómo Consultar Esta Biblioteca

### Caso 1: Corregir Error (SUBFASE 3A)

```
1. Identificar tipo de error
2. Abrir CATALOGO.md
3. Buscar en "Por Patrón Técnico" o "Por Error a Corregir"
4. Abrir ejercicio de referencia
5. Consultar sección relevante (data_generation, question, solution)
6. Aplicar mismo patrón al ejercicio con error
```

### Caso 2: Generar Nuevo Ejercicio

```
1. Definir características del ejercicio (competencia, componente, técnicas)
2. Abrir CATALOGO.md
3. Buscar ejercicios con características similares
4. Revisar "Patrón útil para..."
5. Abrir ejercicio(s) de referencia
6. Usar como inspiración/base
```

### Caso 3: Revisión Detractor

```
1. Detractor identifica posible desviación del estándar
2. Consultar CATALOGO.md para ejercicios similares validados
3. Comparar implementación actual vs patrón validado
4. Documentar desviación si es problema
```

---

## 📚 Patrones Técnicos Documentados

### 1. Gráficos como Opciones SCHOICE

**Archivo de referencia**: `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`

**Secciones clave a consultar**:

- **data_generation**: Mezcla interna + tracking de letra_correcta
- **question**: Generación de PNGs individuales sin títulos
- **answerlist**: Referencias a imágenes con letras
- **solution**: Mostrar diagrama correcto dinámico

**Reglas implementadas**:

- `.claude/rules/graficos-como-opciones.md`
- exshuffle: TRUE obligatorio
- Sin títulos con letras en gráficos

---

### 2. Pool de Errores Conceptuales

**Archivo de referencia**: `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`

**Secciones clave**:

- **data_generation**: Definición de `errores_conceptuales` list
- Estructura con `codigo`, `nombre`, `descripcion_corta`, `calcula()`
- Selección aleatoria del error
- Verificación de coherencia

**Reglas implementadas**:
- `.claude/rules/ejercicios-metacognitivos.md`

---

### 3. Tablas Dinámicas

**Archivo de referencia**: `Ejemplo_00_numeros_triangulares_sucesion_argumentacion_n2_v1.Rmd`

**Secciones clave**:

- **data_generation**: Construcción de data.frame
- **question**: `knitr::kable()` con align
- Formato apropiado para LaTeX/HTML

---

### 4. Contextos Narrativos Variables

**Archivo de referencia**: `Ejemplo_01.Rmd`

**Secciones clave**:

- **data_generation**: Pool de contextos con `sample()`
- Interpolación de variables de contexto en question
- Coherencia entre contexto y valores

---

## 🚨 Antipatrones Detectados y Corregidos

### ❌ Antipatrón 1: grid.arrange() para opciones

**Problema**: Mostrar todos los gráficos juntos en una sola imagen

**Solución validada**: Ver EST-REP-01

- Generar PNGs individuales
- Answerlist con referencias separadas

---

### ❌ Antipatrón 2: Títulos con letras en gráficos

**Problema**: `labs(title = "A")` no se reordena con exshuffle

**Solución validada**: Ver EST-REP-01

- `labs(title = NULL)`
- R-exams asigna (a), (b), (c), (d) automáticamente

---

### ❌ Antipatrón 3: Distractores aleatorios sin fundamento

**Problema**: `distractor <- respuesta + sample(-10:10, 1)`

**Solución validada**: Ver EST-REP-01

- Pool de errores conceptuales documentados
- Cada error tiene código, descripción, función `calcula()`

---

### ❌ Antipatrón 4: exshuffle: FALSE

**Problema**: Opciones siempre en el mismo orden

**Solución validada**: Ver todos los ejercicios

- `exshuffle: TRUE` es OBLIGATORIO
- Ver `.claude/rules/codigo-rmd.md`

---

## 📊 Métricas de Calidad

### Ejercicio Considerado "Ejemplo Funcional" si:

- ✅ **Diversidad**: 200+ versiones únicas
- ✅ **Renderizado**: 4 formatos sin errores (HTML, PDF, DOCX, NOPS)
- ✅ **Coherencias**: 5 coherencias verificadas y aprobadas
- ✅ **Detractor**: APROBADO sin objeciones críticas/altas
- ✅ **Testing**: Tests de regresión pasan 100%
- ✅ **Producción**: Usado exitosamente en entorno real

### Métricas a Documentar por Ejercicio

```yaml
versiones_unicas: 287        # De exams2html(n=300)
tiempo_resolucion_estimado: 3-5 minutos
formatos_validados:
  - HTML
  - PDF
  - DOCX
  - NOPS
fecha_validacion: 2026-02-07
fecha_produccion: 2026-02-07
usos_en_produccion: 12       # Veces que se ha usado
```

---

## 🔧 Mantenimiento

### Revisión Periódica

Cada 3 meses:

1. Verificar que ejercicios siguen funcionando con versiones actualizadas de R/exams
2. Actualizar ejercicios si hay nuevas mejores prácticas
3. Incrementar versión si hay cambios significativos
4. Documentar cambios en CATALOGO.md

### Deprecación de Ejercicios

Si un ejercicio queda obsoleto:

1. **NO eliminar** (preservar historial)
2. Agregar nota en CATALOGO.md: `⚠️ OBSOLETO - Ver [nuevo_ejercicio]`
3. Mover a `deprecated/`
4. Documentar razón de obsolescencia
5. Referenciar ejercicio reemplazo

---

## 🎯 Metas de Expansión

### Cobertura Mínima por Área

| Área | Actual | Meta |
|------|--------|------|
| Estadística | 4 | 10 |
| Álgebra | 0 | 8 |
| Geometría | 0 | 8 |
| Funciones | 0 | 6 |
| Probabilidad | 0 | 5 |

### Cobertura por Tipo

| Tipo | Actual | Meta |
|------|--------|------|
| SCHOICE | 4 | 25 |
| CLOZE | 0 | 15 |

### Cobertura por Nivel

| Nivel | Actual | Meta |
|-------|--------|------|
| Nivel 1 | 0 | 5 |
| Nivel 2 | 3 | 15 |
| Nivel 3 | 1 | 15 |
| Nivel 4 | 0 | 5 |

---

## 📖 Referencias

- **Reglas aplicables**:

  - `.claude/rules/ejercicios-metacognitivos.md`
  - `.claude/rules/graficos-como-opciones.md`
  - `.claude/rules/codigo-rmd.md`
  - `.claude/rules/ciclo-validacion.md`

- **Skills relacionados**:
  - `/generar-schoice` - Genera ejercicios SCHOICE
  - `/generar-cloze` - Genera ejercicios CLOZE
  - `/validar-renderizado` - FASE 1 del ciclo
  - `/validar-coherencia` - FASE 2 del ciclo
  - `/promover-ejercicio` - Mover a Nuevos-Ejercicios

- **Comandos útiles**:
  ```bash
  # Buscar ejercicios por patrón
  grep -r "gráficos como opciones" CATALOGO.md

  # Listar todos los SCHOICE
  grep "extype: schoice" *.Rmd

  # Contar versiones únicas
  Rscript -e "exams::exams2html('[archivo].Rmd', n=300)" | grep "unique"
  ```

---

**Versión**: 1.0\
**Fecha**: 2026-02-07\
**Mantenido por**: Sistema automatizado\
**Última actualización**: 2026-02-07
