# Ejercicio: Dispersión y Alcance de Proyectil

## Identificación

| Campo | Valor |
|-------|-------|
| **Nombre** | `dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1` |
| **Tipo** | SCHOICE (selección única) |
| **Nivel** | 2 (Intermedio) |
| **Competencia** | Interpretación y Representación |
| **Componente** | Aleatorio |
| **Contexto** | Científico |
| **Eje Axial** | Aplicado |

## Descripción

Ejercicio de estadística que presenta una gráfica de dispersión del alcance horizontal de un proyectil en función del ángulo de lanzamiento. El estudiante debe:

1. Identificar el tipo de relación (lineal vs no lineal)
2. Analizar el patrón de dispersión de los datos
3. Relacionar la variabilidad con la variable correcta (ángulo vs alcance)

## Contenido Matemático

### Ecuación del Proyectil

$$R = \frac{v_0^2 \cdot \sin(2\theta)}{g}$$

Donde:

- $R$ = Alcance horizontal (m)
- $v_0$ = Velocidad inicial (m/s)
- $\theta$ = Ángulo de lanzamiento (radianes)
- $g$ = Aceleración gravitacional ($9.8 \, \text{m/s}^2$)

### Respuesta Correcta
El comportamiento es **no lineal** (parabólico/senoidal) y la dispersión aumenta con el **alcance**, no con el ángulo.

## Variabilidad del Ejercicio

### Variables Aleatorias (Datos)

| Variable | Rango | Valores |
|----------|-------|---------|
| `v0` | 10.5 - 12.0 m/s | 16 |
| `n_lanzamientos` | 90 - 110 | 21 |
| `ruido_base` | 0.35 - 0.45 | 6 |
| `angulos` | Distribución por zonas | Continuo |

### Variables Aleatorias (Texto)

| Variable | Variantes |
|----------|-----------|
| `vars_lineal` | lineal, proporcional, de tipo lineal, directamente proporcional |
| `vars_no_lineal` | no lineal, no proporcional, de tipo no lineal, parabólico |
| `vars_disperso` | más disperso, con mayor variabilidad, más variable, con mayor dispersión |
| `vars_angulo` | el ángulo, el ángulo de lanzamiento, la inclinación inicial, el ángulo inicial |
| `vars_alcance` | el alcance, el alcance horizontal, la distancia recorrida, el alcance del proyectil |

### Combinaciones Totales

```
Datos:  16 * 21 * 6 = 2,016 combinaciones
Texto:  4 * 4 * 4 * 4 * 4 = 1,024 combinaciones
-----------------------------------------
TOTAL:  2,016 * 1,024 = 2,064,384 versiones únicas
```

## Archivos

| Archivo | Descripción |
|---------|-------------|
| `*_opc_D.Rmd` | Ejercicio principal con variación textual |
| `*_opc_C.Rmd` | Versión anterior (sin variación textual) |
| `README.md` | Este archivo |
| `WALKTHROUGH.md` | Guía detallada del código |
| `salida/` | Outputs de renderizado |
| `outputs/` | Archivos temporales |

## Uso

### Renderizar en R-exams

```r
library(exams)

# HTML (1 versión)
exams2html("dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1_opc_D.Rmd", n = 1)

# PDF (5 versiones)
exams2pdf("dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1_opc_D.Rmd", n = 5)

# DOCX
exams2pandoc("dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1_opc_D.Rmd", n = 1, type = "docx")

# NOPS (examen impreso)
exams2nops("dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1_opc_D.Rmd", n = 30)

# Moodle (banco de preguntas)
exams2moodle("dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1_opc_D.Rmd", n = 100)
```

### Validación de Diversidad

El ejercicio incluye un test automático que verifica >= 300 versiones únicas en 500 iteraciones.

## Formatos Validados

| Formato | Estado |
|---------|--------|
| HTML | OK |
| PDF | OK |
| DOCX | OK |
| NOPS | OK |

## Metadatos R-exams

```yaml
exname: dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1
extype: schoice
exsolution: 0001
exshuffle: TRUE
exsection: Estadística/Gráficas de Dispersión
```

## Clasificación ICFES

| Dimensión | Valor |
|-----------|-------|
| Competencia | Interpretación y Representación |
| Componente | Aleatorio |
| Nivel | 2 |
| Contexto | Científico |
| Contenido | Estadística (No Genérico) |
| Eje Axial | Aplicado |

## Autor

Generado con Claude Code (Graficador Experto ICFES)

## Versión

- **v1**: Versión inicial con opciones estáticas
- **v1_opc_D**: Versión con variación textual en opciones (actual)
