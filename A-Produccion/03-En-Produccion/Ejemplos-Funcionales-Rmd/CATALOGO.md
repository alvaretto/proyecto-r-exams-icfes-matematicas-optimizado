# Catálogo de Ejemplos Funcionales

## 🎯 Propósito

Este catálogo contiene **ejercicios .Rmd 100% validados** que han pasado a producción.

**Uso obligatorio**: Consultar este catálogo ANTES de generar nuevos ejercicios o corregir errores (SUBFASE 3A).

---

## 📊 Estadísticas

- **Total ejercicios**: 4
- **SCHOICE**: 4
- **CLOZE**: 0
- **Nivel 2**: 3
- **Nivel 3**: 1

---

## 🔍 Índice por Características

### Por Tipo de Ejercicio

| Tipo | Cantidad | Archivos |
|------|----------|----------|
| SCHOICE | 4 | Ver sección SCHOICE |
| CLOZE | 0 | - |

### Por Componente ICFES

| Componente | Cantidad | Archivos |
|------------|----------|----------|
| Aleatorio | 3 | EST-INT-01, EST-INT-02, EST-INT-03 |
| Datos | 1 | EST-REP-01 |

### Por Competencia ICFES

| Competencia | Cantidad | Archivos |
|-------------|----------|----------|
| Interpretación | 4 | Todos |

### Por Características Técnicas

| Característica | Cantidad | Códigos |
|----------------|----------|---------|
| Gráficos TikZ | 0 | - |
| Gráficos Python | 1 | EST-REP-01 |
| Gráficos R (ggplot2) | 1 | EST-REP-01 |
| Tablas dinámicas | 2 | EST-INT-01, EST-INT-02 |
| Progressive Disclosure | 1 | EST-ARG-01 |
| Pool de errores conceptuales | 1 | EST-ARG-01 |

---

## 📚 SCHOICE - Ejercicios de Selección Única

### EST-INT-01: Números Triangulares y Sucesiones

**Archivo**: `Ejemplo_00_numeros_triangulares_sucesion_argumentacion_n2_v1.Rmd`

**Metadatos**:
- **Nivel**: 2
- **Competencia**: Interpretación
- **Componente**: Aleatorio
- **Tipo**: SCHOICE
- **Características técnicas**:
  - Tabla dinámica con sucesión
  - Fórmula LaTeX
  - 4 opciones con distractores basados en errores comunes

**Patrón útil para**:
- Sucesiones numéricas
- Reconocimiento de patrones
- Fórmulas generales

**Versiones únicas**: 250+

---

### EST-INT-02: Probabilidades y Eventos

**Archivo**: `Ejemplo_01.Rmd`

**Metadatos**:
- **Nivel**: 2
- **Competencia**: Interpretación
- **Componente**: Aleatorio
- **Tipo**: SCHOICE
- **Características técnicas**:
  - Contexto narrativo variable (dados, cartas, etc.)
  - Cálculos probabilísticos
  - Distractores algebraicos

**Patrón útil para**:
- Probabilidad básica
- Eventos independientes
- Contextos narrativos variados

**Versiones únicas**: 250+

---

### EST-INT-03: Proporcionalidad y Razones

**Archivo**: `Ejemplo_02.Rmd`

**Metadatos**:
- **Nivel**: 2
- **Competencia**: Interpretación
- **Componente**: Aleatorio
- **Tipo**: SCHOICE
- **Características técnicas**:
  - Proporciones directas
  - Conversión de unidades
  - Distractores por errores operacionales

**Patrón útil para**:
- Proporciones
- Razones
- Conversión de unidades

**Versiones únicas**: 250+

---

### EST-REP-01: Diagramas de Caja (Gráficos como Opciones)

**Archivo**: `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`

**Metadatos**:
- **Nivel**: 2
- **Competencia**: Interpretación
- **Componente**: Datos
- **Tipo**: SCHOICE
- **Características técnicas**:
  - ✅ **Gráficos como opciones individuales** (diagrama_a.png, diagrama_b.png, etc.)
  - ✅ Mezcla interna + exshuffle:TRUE
  - ✅ Sin títulos con letras en gráficos
  - ✅ Pool de errores conceptuales (excluye EST-BOX-01)
  - ✅ Escala Y compartida calculada dinámicamente
  - Gráficos Python (matplotlib) o R (ggplot2)
  - Diagramas de caja con cuartiles

**Patrón útil para**:
- Diagramas de caja (box plots)
- Gráficos como opciones de respuesta
- Interpretación de medidas de dispersión
- Comparación visual de distribuciones

**Versiones únicas**: 250+

**⚠️ PATRÓN DE REFERENCIA OBLIGATORIO**:
- Consultar ANTES de generar ejercicios con gráficos como opciones
- Implementa todas las reglas de `.claude/rules/graficos-como-opciones.md`

---

## 📚 CLOZE - Ejercicios Compuestos

*(Actualmente vacío - agregar cuando se validen ejercicios CLOZE)*

---

## 🔧 Patrones Técnicos Documentados

### 1. Gráficos como Opciones SCHOICE

**Referencia**: `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`

**Patrón**:
```r
# Mezcla interna
opciones_mezcladas <- sample(todas_opciones)
indice_correcto <- which(names(opciones_mezcladas) == "correcta")
solucion <- rep(0, 4)
solucion[indice_correcto] <- 1

# Generar PNGs individuales
crear_y_guardar_grafico(opciones_mezcladas$A, "A")
crear_y_guardar_grafico(opciones_mezcladas$B, "B")
# ...

# Answerlist con imágenes
cat("* ![](diagrama_a.png){width=60%}\n")
cat("* ![](diagrama_b.png){width=60%}\n")
```

**Metadatos clave**:
```yaml
exshuffle: TRUE
exsolution: `r paste(as.integer(solucion), collapse="")`
```

---

### 2. Pool de Errores Conceptuales

**Referencia**: `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`

**Patrón**:
```r
errores_conceptuales <- list(
  list(
    codigo = "EST-BOX-02",
    nombre = "Confusión mínimo/Q1",
    descripcion_corta = "Usa el valor mínimo en lugar del primer cuartil",
    calcula = function(stats) { ... }
  ),
  # ...
)
```

---

### 3. Tablas Dinámicas

**Referencia**: `Ejemplo_00_numeros_triangulares_sucesion_argumentacion_n2_v1.Rmd`

**Patrón**:
```r
# Generar tabla en chunk
knitr::kable(tabla_datos, align = "c")
```

---

## 🗂️ Convenciones de Nomenclatura

### Formato de Nombre de Archivo

```
[area]_[tema]_[competencia]_[componente]_[nivel]_[tipo]_v[version].Rmd
```

**Ejemplos**:
- `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`
- `algebra_ecuaciones_lineales_interpretacion_n2_schoice_v1.Rmd`

### Códigos de Identificación (Nuevos)

```
[AREA]-[COMP]-[NUM]
```

**Ejemplos**:
- `EST-INT-01` = Estadística, Interpretación, ejercicio 01
- `ALG-ARG-02` = Álgebra, Argumentación, ejercicio 02
- `GEO-FOR-01` = Geometría, Formulación, ejercicio 01

---

## 📖 Cómo Usar Este Catálogo

### Durante SUBFASE 3A (Corrección de Errores)

1. Identificar el tipo de error
2. Buscar en este catálogo ejercicios similares
3. Consultar el patrón de solución validado
4. Aplicar el mismo patrón al ejercicio con error

### Durante Generación de Nuevos Ejercicios

1. Identificar características del ejercicio a generar (competencia, componente, técnicas)
2. Buscar ejemplos con características similares
3. Consultar patrones técnicos documentados
4. Usar como base/inspiración

### Durante Revisión Detractor

1. Comparar ejercicio generado con ejemplos similares
2. Verificar que aplica los mismos patrones validados
3. Identificar desviaciones del estándar

---

## 📝 Agregar Nuevo Ejercicio al Catálogo

### Paso 1: Crear Symlink

```bash
# Ir al directorio de ejemplos funcionales
cd A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/

# Desde En-Desarrollo (recién validado)
ln -s ../../02-En-Desarrollo/[ejercicio]/[ejercicio].Rmd [ejercicio].Rmd

# O desde En-Produccion (ya organizado por tema)
ln -s ../[Tema]/Pensamiento-XXX/[ejercicio].Rmd [ejercicio].Rmd
```

**Nota**: Usar rutas relativas para portabilidad.

### Paso 2: Asignar Código de Identificación

```
[AREA]-[COMP]-[NUM]
```

### Paso 3: Actualizar CATALOGO.md

Agregar entrada en la sección correspondiente con:
- Código de identificación
- Archivo
- Metadatos completos
- Características técnicas
- Patrón útil para...
- Versiones únicas validadas

### Paso 4: Actualizar Estadísticas

Recalcular totales en la sección "📊 Estadísticas".

---

## 🔍 Búsqueda Rápida

### Por Patrón Técnico

| Necesito... | Ver ejercicio |
|-------------|---------------|
| Gráficos como opciones SCHOICE | EST-REP-01 |
| Pool de errores conceptuales | EST-REP-01 |
| Tabla dinámica | EST-INT-01, EST-INT-02 |
| Contexto narrativo variable | EST-INT-02 |
| Fórmulas LaTeX | EST-INT-01 |
| Progressive Disclosure | (Pendiente ejercicio CLOZE) |

### Por Error a Corregir

| Error común | Consultar ejercicio |
|-------------|---------------------|
| Gráficos no se mezclan | EST-REP-01 |
| Distractores aleatorios | EST-REP-01 (pool de errores) |
| Tabla estática | EST-INT-01 |
| exshuffle no funciona | EST-REP-01 |

---

## 🎯 Metas de Expansión

### Ejercicios Faltantes

- [ ] CLOZE con Progressive Disclosure (4+ partes)
- [ ] Gráficos TikZ dinámicos
- [ ] Geometría con figuras dinámicas
- [ ] Funciones y transformaciones
- [ ] Nivel 3 y Nivel 4

### Áreas por Cubrir

- [ ] Geometría (mínimo 3 ejercicios)
- [ ] Funciones (mínimo 3 ejercicios)
- [ ] Álgebra (mínimo 3 ejercicios)
- [ ] Probabilidad avanzada (mínimo 2 ejercicios)

---

**Versión**: 1.0
**Fecha**: 2026-02-07
**Mantenido por**: Sistema automatizado
**Última actualización**: 2026-02-07
