# Ejercicio: Dispersion y Alcance de Proyectil

## Identificacion

| Campo | Valor |
|-------|-------|
| **Nombre** | `dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1` |
| **Tipo** | SCHOICE (seleccion unica) |
| **Nivel** | 2 (Intermedio) |
| **Competencia** | Interpretacion y Representacion |
| **Componente** | Aleatorio |
| **Contexto** | Cientifico |
| **Eje Axial** | Aplicado |

## Descripcion

Ejercicio de estadistica que presenta una grafica de dispersion del alcance horizontal de un proyectil en funcion del angulo de lanzamiento. El estudiante debe:

1. Identificar el tipo de relacion (lineal vs no lineal)
2. Analizar el patron de dispersion de los datos
3. Relacionar la variabilidad con la variable correcta (angulo vs alcance)

## Contenido Matematico

### Ecuacion del Proyectil
```
R = (v0^2 * sin(2*theta)) / g
```

Donde:
- `R` = Alcance horizontal (m)
- `v0` = Velocidad inicial (m/s)
- `theta` = Angulo de lanzamiento (radianes)
- `g` = Aceleracion gravitacional (9.8 m/s^2)

### Respuesta Correcta
El comportamiento es **no lineal** (parabolico/senoidal) y la dispersion aumenta con el **alcance**, no con el angulo.

## Variabilidad del Ejercicio

### Variables Aleatorias (Datos)

| Variable | Rango | Valores |
|----------|-------|---------|
| `v0` | 10.5 - 12.0 m/s | 16 |
| `n_lanzamientos` | 90 - 110 | 21 |
| `ruido_base` | 0.35 - 0.45 | 6 |
| `angulos` | Distribucion por zonas | Continuo |

### Variables Aleatorias (Texto)

| Variable | Variantes |
|----------|-----------|
| `vars_lineal` | lineal, proporcional, de tipo lineal, directamente proporcional |
| `vars_no_lineal` | no lineal, no proporcional, de tipo no lineal, parabolico |
| `vars_disperso` | mas disperso, con mayor variabilidad, mas variable, con mayor dispersion |
| `vars_angulo` | el angulo, el angulo de lanzamiento, la inclinacion inicial, el angulo inicial |
| `vars_alcance` | el alcance, el alcance horizontal, la distancia recorrida, el alcance del proyectil |

### Combinaciones Totales

```
Datos:  16 * 21 * 6 = 2,016 combinaciones
Texto:  4 * 4 * 4 * 4 * 4 = 1,024 combinaciones
-----------------------------------------
TOTAL:  2,016 * 1,024 = 2,064,384 versiones unicas
```

## Archivos

| Archivo | Descripcion |
|---------|-------------|
| `*_opc_D.Rmd` | Ejercicio principal con variacion textual |
| `*_opc_C.Rmd` | Version anterior (sin variacion textual) |
| `README.md` | Este archivo |
| `WALKTHROUGH.md` | Guia detallada del codigo |
| `salida/` | Outputs de renderizado |
| `outputs/` | Archivos temporales |

## Uso

### Renderizar en R-exams

```r
library(exams)

# HTML (1 version)
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

### Validacion de Diversidad

El ejercicio incluye un test automatico que verifica >= 300 versiones unicas en 500 iteraciones.

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
exsection: Estadistica/Graficas de Dispersion
```

## Clasificacion ICFES

| Dimension | Valor |
|-----------|-------|
| Competencia | Interpretacion y Representacion |
| Componente | Aleatorio |
| Nivel | 2 |
| Contexto | Cientifico |
| Contenido | Estadistica (No Generico) |
| Eje Axial | Aplicado |

## Autor

Generado con Claude Code (Graficador Experto ICFES)

## Version

- **v1**: Version inicial con opciones estaticas
- **v1_opc_D**: Version con variacion textual en opciones (actual)
