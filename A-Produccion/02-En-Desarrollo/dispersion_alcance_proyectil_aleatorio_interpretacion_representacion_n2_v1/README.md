# Ejercicio: Dispersión y Tipo de Relación en Gráficas

## Descripción General

Este conjunto de 4 archivos .Rmd genera ejercicios ICFES que evalúan la capacidad del estudiante para:

1. Identificar el **tipo de relación** entre dos variables (lineal vs no lineal)
2. Reconocer el **patrón de dispersión** en una gráfica de puntos

## Archivos y Variantes

| Archivo | Respuesta | Contexto Físico | Tipo Relación | Dispersión ↑ con |
|---------|-----------|-----------------|---------------|------------------|
| `*_opc_A.Rmd` | **A** | Ley de Hooke (resorte) | Lineal | Fuerza |
| `*_opc_B.Rmd` | **B** | Ley de Hooke (resorte) | Lineal | Elongación |
| `*_opc_C.Rmd` | **C** | Movimiento proyectil | No lineal | Ángulo |
| `*_opc_D.Rmd` | **D** | Movimiento proyectil | No lineal | Alcance |

## Estructura de Opciones (Común a todos)

```
A. [lineal]     + [disperso con variable independiente]
B. [lineal]     + [disperso con variable dependiente]
C. [no lineal]  + [disperso con variable independiente]
D. [no lineal]  + [disperso con variable dependiente]
```

## Modelos Matemáticos

### Archivos A y B: Ley de Hooke (Lineal)

$$x = \frac{F}{k}$$

| Variable | Descripción | Unidad |
|----------|-------------|--------|
| x | Elongación del resorte | cm |
| F | Fuerza aplicada | N |
| k | Constante del resorte | N/cm |

**Gráfica**: Recta ascendente.

### Archivos C y D: Movimiento de Proyectil (No Lineal)

$$R = \frac{v_0^2 \sin(2\theta)}{g}$$

| Variable | Descripción | Unidad |
|----------|-------------|--------|
| R | Alcance horizontal | m |
| v₀ | Velocidad inicial | m/s |
| θ | Ángulo de lanzamiento | rad |
| g | Gravedad | 9.8 m/s² |

**Gráfica**: Parábola con máximo en θ ≈ 0.78 rad (45°).

## Modelos de Dispersión (Heterocedasticidad)

| Archivo | Fórmula del Ruido | Patrón Visual |
|---------|-------------------|---------------|
| opc_A | `σ ∝ F` | Mayor dispersión a la derecha |
| opc_B | `σ ∝ √x` | Mayor dispersión arriba |
| opc_C | `σ ∝ θ` | Mayor dispersión a la derecha |
| opc_D | `σ ∝ √R` | Mayor dispersión en el centro |

## Clasificación ICFES

| Dimensión | Valor |
|-----------|-------|
| Competencia | Interpretación y Representación |
| Componente | Aleatorio |
| Nivel de Dificultad | 2 (Intermedio) |
| Contexto | Científico |
| Contenido | Estadística (No Genérico) |
| Eje Axial | Aplicado |

## Variabilidad

### Por Datos

| Archivo | Variables | Combinaciones |
|---------|-----------|---------------|
| A, B | k (9) × n (21) × ruido (5) | ~945 |
| C, D | v₀ (16) × n (21) × ruido (6) | ~2,016 |

### Por Texto

Todas las opciones tienen variantes sinónimas:

- "lineal" → proporcional, de tipo lineal, directamente proporcional
- "no lineal" → no proporcional, parabólico, cuadrático
- "más disperso" → con mayor variabilidad, más variable

**Total por archivo**: >1,000,000 versiones únicas.

## Uso

### Renderizar

```r
library(exams)

# Elegir archivo según respuesta correcta deseada
exams2html("*_opc_D.Rmd", n = 1)  # Respuesta D
exams2pdf("*_opc_A.Rmd", n = 5)   # Respuesta A
```

### Generar examen con mezcla de variantes

```r
# Usar diferentes variantes para mayor diversidad
archivos <- c("*_opc_A.Rmd", "*_opc_B.Rmd", "*_opc_C.Rmd", "*_opc_D.Rmd")
exams2pdf(sample(archivos, 1), n = 30)
```

## Formatos Validados

| Formato | opc_A | opc_B | opc_C | opc_D |
|---------|-------|-------|-------|-------|
| HTML | ✓ | ✓ | ✓ | ✓ |
| PDF | ✓ | ✓ | ✓ | ✓ |
| DOCX | ✓ | ✓ | ✓ | ✓ |
| NOPS | ✓ | ✓ | ✓ | ✓ |

## Archivos Relacionados

| Archivo | Descripción |
|---------|-------------|
| `WALKTHROUGH.md` | Guía paso a paso del código |
| `INFOGRAFIA_ICFES.md` | Alineación con marco ICFES |
| `salida/` | Outputs de renderizado |

## Selección del Archivo Apropiado

| Objetivo | Archivo Recomendado |
|----------|---------------------|
| Ejercicio estándar ICFES | `*_opc_D.Rmd` |
| Diversificar contexto físico | `*_opc_A.Rmd` o `*_opc_B.Rmd` |
| Evaluar relación no lineal + dispersión con X | `*_opc_C.Rmd` |
| Banco de preguntas variado | Rotar entre los 4 |

---

**Última actualización**: 2025-12-30
**Versión**: 2.0 (Consolidado para 4 variantes)
