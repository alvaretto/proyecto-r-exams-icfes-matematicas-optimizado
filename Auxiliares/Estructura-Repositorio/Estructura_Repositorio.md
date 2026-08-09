# Estructura del Repositorio - Matematicas ICFES R-Exams

## Descripcion General

Repositorio de ejercicios matematicos para las pruebas Saber 11 (ICFES), implementados con el framework R-exams. La organizacion se alinea con el **Marco de Referencia ICFES Matematicas 2026**, que define 3 competencias, 3 categorias de contenido y 4 niveles de desempeno.

**Framework**: R-exams (extype: schoice, cloze, mchoice, num, string)
**Formatos de salida**: HTML, PDF, DOCX, NOPS (Moodle XML)
**Requisito minimo**: 200+ versiones unicas por ejercicio

---

## Marco Conceptual ICFES Matematicas 2026

### Competencias Evaluadas (3)

| Competencia | Descripcion | Codigo Metadatos |
|-------------|-------------|------------------|
| **Interpretacion y representacion** | Comprender y transformar informacion matematica en distintas representaciones | `interpretacion_representacion` |
| **Formulacion y ejecucion** | Disenar y ejecutar planes de solucion utilizando herramientas matematicas | `formulacion_ejecucion` |
| **Argumentacion** | Justificar procedimientos y validar propiedades matematicas | `argumentacion` |

### Categorias de Contenido ICFES (3)

Desde 2014, el ICFES reemplazo los antiguos "Componentes" (Numerico-Variacional, Geometrico-Metrico, Aleatorio) por **Categorias de Contenido**, que agrupan los 5 pensamientos matematicos del MEN:

| Categoria ICFES | Pensamientos MEN Agrupados | Codigo Metadatos |
|-----------------|---------------------------|------------------|
| **Algebra y Calculo** | Numerico + Variacional | `numerico_variacional` |
| **Geometria** | Espacial + Metrico | `geometrico_metrico` |
| **Estadistica** | Aleatorio | `aleatorio` |

**Nota sobre terminologia**: Los codigos de metadatos (`numerico_variacional`, `geometrico_metrico`, `aleatorio`) conservan la nomenclatura historica del repositorio para compatibilidad. La equivalencia con la terminologia ICFES 2026 es la documentada arriba.

### Contenidos Genericos vs No Genericos

Cada categoria tiene dos tipos de contenido:

| Categoria | Contenido Generico (Saber 11 + Razonamiento Cuantitativo) | Contenido No Generico (solo Saber 11) |
|-----------|-----------------------------------------------------------|---------------------------------------|
| Algebra y Calculo | Operaciones basicas, proporcionalidad, ecuaciones lineales, funciones lineales/cuadraticas | Funciones exponenciales/logaritmicas, trigonometria, limites |
| Geometria | Perimetro, area, volumen, transformaciones, plano cartesiano basico | Geometria analitica avanzada (conicas), razones trigonometricas |
| Estadistica | Tablas/graficos, medidas de tendencia central, probabilidad basica | Distribuciones, probabilidad condicional, intervalos de confianza |

### Niveles de Desempeno (4)

| Nivel | Descripcion | Codigo |
|-------|-------------|--------|
| **1** | Reconoce informacion puntual en representaciones basicas | `n1` |
| **2** | Resuelve problemas rutinarios con una sola operacion o relacion | `n2` |
| **3** | Integra informacion de multiples fuentes, resuelve problemas no rutinarios | `n3` |
| **4** | Generaliza, argumenta formalmente, resuelve problemas complejos y abiertos | `n4` |

### Contextos de Evaluacion (4)

- **Familiares o personales**: Situaciones cotidianas del estudiante
- **Laborales u ocupacionales**: Contextos de trabajo y produccion
- **Comunitarios o sociales**: Problematicas de la comunidad
- **Matematicos o cientificos**: Contextos abstractos o de ciencia

### Estructura de la Prueba

- **38 preguntas** en total (todas de seleccion unica en el examen real)
- **30 preguntas** conforman tambien el subconjunto de Razonamiento Cuantitativo
- Las 8 preguntas restantes evaluan contenido no generico exclusivo de Matematicas 11

---

## Estructura de Directorios del Repositorio

### Pipeline de Desarrollo

```
A-Produccion/
├── 01-En-PreDesarrollo/      # Laboratorio y prototipos
├── 02-En-Desarrollo/         # Ejercicios en construccion/validacion
├── 03-En-Produccion/         # Ejercicios validados (PRODUCCION)
│   └── Ejemplos-Funcionales-Rmd/  # FUENTE DE VERDAD para patrones
└── perifericos/              # Archivos de soporte
```

### Arbol Completo de Produccion (03-En-Produccion/)

La organizacion usa la jerarquia: **Tema Matematico > Pensamiento MEN > Subtema > Ejercicio**.
Los directorios sin ejercicios son estructura planificada de destino para ejercicios futuros.
Todos los directorios contienen `.gitkeep` para preservar la estructura en Git.

Leyenda: `[N .Rmd]` = ejercicios activos | (vacio) = estructura de destino planificada

```
03-En-Produccion/
│
│ ══════════════════════════════════════════════════════
│  CATEGORIA ICFES: ALGEBRA Y CALCULO
│  (Pensamiento Numerico + Pensamiento Variacional)
│ ══════════════════════════════════════════════════════
│
├── 01-Numeros-Reales/
│   └── Pensamiento-Numerico/
│       ├── 01-Numeros-Racionales/
│       ├── 02-Numeros-Irracionales/
│       ├── 03-Numeros-Reales/
│       │   ├── 22-S2-2025-SEDQ-fracciones_reparto_premio/  [4 .Rmd]
│       │   └── 2024-CB-S1_S2-P02-calculo-de-ganancias/    [ejercicio legacy]
│       ├── 04-Propiedades-Expresiones-Decimales/
│       ├── 05-Conjunto-Reales-Desigualdades/
│       ├── 06-Valor-Absoluto/
│       └── 07-Proporcionalidad-Directa-E-Inversa/
│           └── excedente_almuerzo_proporcional_n4/            [reservado]
│
├── 02-Funciones/
│   └── Pensamiento-Variacional-Espacial/
│       ├── 01-Concepto-De-Funcion_Dominio-Y-Recorrido/
│       ├── 02-Operaciones-Con-Funciones/
│       ├── 03-Composicion-De-Funciones/
│       ├── 04-Funciones-Inyectivas_Funciones_Inversas/
│       ├── 05-Propiedades-De-Las-Funciones/
│       ├── 06-Funciones-Pares-E-Impares/
│       ├── 07-Funciones-Periodicas/
│       ├── 08-Funcion-Exponencial/
│       ├── 09-Funcion-Logaritmica/
│       ├── 10-Traslacion-Y-Dilatacion/
│       ├── 11-Variacion-Lineal-Y-Exponencial_Razon-De-Cambio/
│       │   ├── Variacion-Lineal-Auto-Viajero-09/               [~16 .Rmd]
│       │   └── Variacion-Lineal-Vuelo-Acrobatico/              [~11 .Rmd]
│       └── 12-Introduccion-Al-Limite-De-Una-Sucesion/
│
│ ══════════════════════════════════════════════════════
│  CATEGORIA ICFES: GEOMETRIA (contenido no generico)
│  (Pensamiento Espacial + Pensamiento Metrico)
│ ══════════════════════════════════════════════════════
│
├── 03-Razones-Trigonometricas/
│   └── Pensamiento-Espacial-Metrico-Y-Variacional/
│       ├── 01-Angulos-Y-Sus-Medidas/
│       ├── 02-Angulos-En-Posicion-Normal/
│       ├── 03-Circunferencia-Unitaria/
│       ├── 04-Seno-Coseno-Tangente/
│       ├── 05-Cosecante-Secante-Cotangente/
│       ├── 06-Razones-Trigonometricas-Angulos-Notables/
│       ├── 07-Signos-Razones-Trigonometricas-Cuadrantes/
│       ├── 08-Relaciones-Pitagoricas/
│       ├── 09-Resolucion-Triangulos-Rectangulos/
│       ├── 10-Ley-De-Senos/
│       ├── 11-Ley-De-Cosenos/
│       ├── 12-Aplicaciones-Triangulos-No-Rectangulos/
│       └── 13-Problemas-De-Aplicacion-Trigonometria/
│
├── 04-Funciones_Identidades-Trigonometricas/
│   └── Pensamiento-Espacial-Y-Variacional/
│       ├── 01-Funciones-Trigonometricas-Seno-Coseno/
│       ├── 02-Funcion-Tangente/
│       ├── 03-Transformaciones-Funciones-Trigonometricas/
│       ├── 04-Identidades-Trigonometricas-Fundamentales/
│       ├── 05-Identidades-Suma-Diferencia-Angulos/
│       ├── 06-Identidades-Angulo-Doble-Mitad/
│       ├── 07-Ecuaciones-Trigonometricas/
│       ├── 08-Funciones-Trigonometricas-Inversas/
│       ├── 09-Coordenadas-Polares/
│       ├── 10-Numeros-Complejos-Forma-Polar/
│       └── 11-Aplicaciones-Modelado-Trigonometrico/
│
│ ══════════════════════════════════════════════════════
│  CATEGORIA ICFES: GEOMETRIA (contenido generico y no generico)
│  (Pensamiento Espacial + Pensamiento Metrico)
│ ══════════════════════════════════════════════════════
│
├── 05-Geometria/
│   └── Pensamiento-Espacial/
│       ├── 01-Puntos-Y-Lineas/
│       ├── 02-Segmentos-Y-Rayos/
│       ├── 03-Angulos-Clasificacion-Y-Medida/
│       ├── 04-Rectas-Paralelas-Y-Perpendiculares/
│       ├── 05-Triangulos-Clasificacion-Y-Propiedades/
│       ├── 06-Cuadrilateros-Y-Poligonos/
│       ├── 07-Circunferencia-Y-Circulo/
│       ├── 08-Perimetro-Figuras-Planas/
│       ├── 09-Area-Figuras-Planas/
│       ├── 10-Teorema-De-Pitagoras/
│       ├── 11-Semejanza-Y-Congruencia/
│       ├── 12-Transformaciones-Geometricas/
│       ├── 13-Geometria-Analitica-Recta/
│       ├── 14-Secciones-Conicas/
│       ├── 15-Solidos-Y-Volumen/
│       ├── 16-Area-Superficie-Solidos/
│       ├── 17-Conversion-de-Unidades/
│       │   └── conversion_unidades_area_formulacion_ejecucion/  [1 .Rmd]
│       └── 18-Volumen-Y-Raiz-Cubica/
│           └── raiz_cubica_empaquetamiento_*/                   [2 .Rmd + 1 CLOZE]
│
│ ══════════════════════════════════════════════════════
│  CATEGORIA ICFES: ESTADISTICA
│  (Pensamiento Aleatorio)
│ ══════════════════════════════════════════════════════
│
├── 06-Estadistica-Y-Probabilidad/
│   └── Pensamiento-Aleatorio/
│       ├── 01-Variables-Cualitativas_Distribucion-De-Frecuencias/
│       │   ├── Accidentalidad_Vial_Genero-16/                  [~6 .Rmd]
│       │   ├── ExportacionesGraficos-Tebailandia/              [~6 .Rmd]
│       │   ├── Gas_natural_porcentaje_maximo_aleatorio_*/       [2 .Rmd]
│       │   ├── Graficos_Estadisticos_Adopcion_Mascotas/        [~8 .Rmd]
│       │   └── Pasteleria_sabores_ventas_*/                    [1 .Rmd]
│       ├── 02-Variables-Cuantitativas-Discretas_Distribucion-De-Frecuencias/
│       ├── 03-Variables-Cuantitativas-Continuas_Distribucion-De-Frecuencias/
│       │   └── poblaciones_paises_graficas_lineas_*/            [1 .Rmd]
│       ├── 04-Medidas-De-Tendencia-Central/
│       │   ├── 01-MediaMedianaModa/
│       │   │   └── Calificaciones-Universitarias/               [~3 .Rmd]
│       │   ├── Media/
│       │   │   └── Promedios-Borrados/                          [~2 .Rmd]
│       │   └── Mediana/
│       │       ├── Baterias-Celulares/                          [6 .Rmd]
│       │       ├── Mediana-Farmaceutica/                        [~5 .Rmd]
│       │       └── mediana_salas_cine_formulacion_ejecucion_n2_v1/ [~4 .Rmd]
│       ├── 05-Medidas-De-Dispersion/
│       │   └── confint2-cloze/                                  [2 .Rmd]
│       ├── 06-Medidas-De-Posicion/                              [2 .Rmd]
│       ├── 07-Probabilidad_Principios-Aditivo-Multiplicativo-Conteo/
│       │   ├── Diagramas-de-Venn/
│       │   │   └── GenerosMusicales/                            [~4 .Rmd]
│       │   └── Probabilidad-Bolas-Colores/                      [2 .Rmd]
│       ├── 08-Probabilidad-De-La-Union-De-Sucesos/
│       │   └── diagrama_venn_encuesta_metacognitivo_*/          [3 .Rmd]
│       ├── 09-Probabilidad-Condicionada_Independencia-De-Sucesos/
│       │   └── Probabilidad-Intervalos-Curva-13-S1-2024B/       [~5 .Rmd]
│       └── 10-Combinatoria_Permutaciones-Variaciones-Combinaciones/
│           └── permutaciones_pescadores_venia_n4/               [reservado]
│
│ ══════════════════════════════════════════════════════
│  RECURSOS Y PLANTILLAS
│ ══════════════════════════════════════════════════════
│
└── Ejemplos-Funcionales-Rmd/                        # FUENTE DE VERDAD
    ├── Ejemplo_00_numeros_triangulares_*.Rmd
    ├── Ejemplo_01.Rmd, Ejemplo_02.Rmd, Ejemplo_03.Rmd
    ├── Avances-Pedagogicos/
    ├── Plantillas/
    │   ├── Python/    # Plantillas con reticulate
    │   ├── Rmd/       # Plantillas R-exams puro
    │   │   └── cloze/ # Plantillas tipo CLOZE
    │   ├── Rnw/       # Plantillas Sweave
    │   ├── Tablas/    # TikZ y Kable
    │   ├── erres/     # Plantillas R-exams oficiales
    │   └── tex/       # Plantillas LaTeX
    ├── TikZ-Documentation/
    └── oficial-schoice/
```

### Mapeo: Directorios <-> Categorias ICFES

| Directorio | Categoria ICFES 2026 | Contenido |
|------------|---------------------|-----------|
| `01-Numeros-Reales/` | Algebra y Calculo | Numeros reales, fracciones, operaciones |
| `02-Funciones/` | Algebra y Calculo | Funciones, variacion lineal/exponencial |
| `03-Razones-Trigonometricas/` | Geometria (no generico) | Trigonometria basica |
| `04-Funciones_Identidades-Trigonometricas/` | Geometria (no generico) | Funciones e identidades trigonometricas |
| `05-Geometria/` | Geometria | Geometria analitica, medidas, conversiones, volumen |
| `06-Estadistica-Y-Probabilidad/` | Estadistica | Variables, medidas, probabilidad |

---

## Nomenclatura Obligatoria de Archivos .Rmd

### Formato

```
[tema]_[categoria]_[competencia]_n[nivel]_v[version].Rmd
```

### Componentes

| Parte | Valores | Ejemplo |
|-------|---------|---------|
| `[tema]` | Nombre descriptivo en snake_case | `raiz_cubica_empaquetamiento` |
| `[categoria]` | `geometrico_metrico`, `numerico_variacional`, `aleatorio` | `geometrico_metrico` |
| `[competencia]` | `interpretacion_representacion`, `formulacion_ejecucion`, `argumentacion` | `formulacion_ejecucion` |
| `n[nivel]` | `n1`, `n2`, `n3`, `n4` | `n2` |
| `v[version]` | `v1`, `v2`, ... | `v1` |

### Variantes CLOZE

Para ejercicios tipo CLOZE, agregar `_cloze` antes de la version:

```
[tema]_[categoria]_[competencia]_n[nivel]_cloze_v[version].Rmd
```

### Ejemplo completo

```
raiz_cubica_empaquetamiento_geometrico_metrico_formulacion_ejecucion_n2_v1.Rmd
consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_cloze_v1.Rmd
```

---

## Convenciones de Estructura por Ejercicio

### Directorios Estandar

| Directorio | Contenido |
|------------|-----------|
| `docus/` | Documentacion del ejercicio |
| `ejercicios/` | Archivos .Rmd (ejercicios legacy) |
| `salida/` | Archivos generados (HTML, PDF, DOCX, NOPS) |
| `images/` | Recursos graficos |
| `tikz_temp/` | Archivos temporales de TikZ |
| `_snaps/` | Snapshots de pruebas |

### Tipos de Ejercicios R-exams

| Tipo | `extype` | Descripcion |
|------|----------|-------------|
| Seleccion unica | `schoice` | Una respuesta correcta entre N opciones |
| Seleccion multiple | `mchoice` | Varias respuestas correctas posibles |
| Compuesto | `cloze` | Multiples subpreguntas de tipos mixtos |
| Numerico | `num` | Respuesta numerica con tolerancia |
| Texto | `string` | Respuesta de texto libre |

---

## Metadatos ICFES Obligatorios en R-exams

Todo archivo .Rmd debe incluir las 6 dimensiones ICFES en su seccion Meta-information:

```
Meta-information
================
exname: nombre_ejercicio_sin_tildes
extype: schoice
exsolution: 1000
exshuffle: TRUE
extol: 0.01
exextra[Type]: SCHOICE
exextra[Competencia]: Formulacion
exextra[Componente]: Geometrico-Metrico
exextra[Afirmacion]: Descripcion especifica de la afirmacion evaluada
exextra[Evidencia]: Descripcion especifica de la evidencia esperada
exextra[Nivel]: 2
```

**Nota**: Los valores de `exextra` usan ASCII sin tildes porque R-exams los parsea como identificadores.

### Correspondencia Metadatos <-> ICFES 2026

| Campo `exextra` | Valores Permitidos | Equivalencia ICFES 2026 |
|------------------|--------------------|------------------------|
| `Competencia` | `Interpretacion`, `Formulacion`, `Argumentacion` | 3 competencias evaluadas |
| `Componente` | `Numerico-Variacional`, `Geometrico-Metrico`, `Aleatorio` | 3 categorias de contenido |
| `Nivel` | `1`, `2`, `3`, `4` | 4 niveles de desempeno |

---

## Estado del Repositorio

### Ejercicios en Produccion por Categoria

| Categoria ICFES | Directorio | Ejercicios .Rmd | Estado |
|-----------------|-----------|-----------------|--------|
| Algebra y Calculo | `01-Numeros-Reales/` | 4 | Activo |
| Algebra y Calculo | `02-Funciones/` | 17 | Activo |
| Geometria (no generico) | `03-Razones-Trigonometricas/` | 0 | Estructura creada |
| Geometria (no generico) | `04-Funciones_Identidades-Trigonometricas/` | 0 | Estructura creada |
| Geometria | `05-Geometria/` | 3 | En crecimiento |
| Estadistica | `06-Estadistica-Y-Probabilidad/` | 54 | Mas desarrollado |
| **Total produccion** | | **78** | |

### Ejercicios en Desarrollo (02-En-Desarrollo/)

| Ejercicio | Tipo | Estado |
|-----------|------|--------|
| canciones/ | Ejercicio | En desarrollo |
| comparar_medianas_3_grupos_aleatorio_argumentacion_n2_schoice_v1/ | SCHOICE | En desarrollo |
| consumo_telefonico_adicional/ | Ejercicio | En desarrollo |
| diagrama_caja_estaturas_metacognitivo_interpretacion/ | Ejercicio | Listo para Aula |
| dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1/ | Ejercicio | En desarrollo |
| migracion_atun_representacion_grafica_n2_v1/ | Ejercicio | En desarrollo |
| piscinas_baldosas_patrones_numerico_variacional_interpretacion_representacion/ | Ejercicio | En desarrollo |
| probabilidad_condicional_diagrama_arbol_aleatorio_argumentacion/ | Ejercicio | En desarrollo |
| proceso_recaudacion_sitio_turistico_numerico_variacional_argumentacion_n2_v1/ | Ejercicio | En desarrollo |
| proyeccion_usuarios_parabola_geometrico_interpretacion/ | Ejercicio | En desarrollo |
| recta_geometria_analitica_interpretacion_representacion/ | Ejercicio | En desarrollo |
| tablas_frecuencia_argumentacion/ | Ejercicio | En desarrollo |
| volumen_cilindro_geometrico_metrico_interpretacion/ | Ejercicio | En desarrollo |

### Areas Pendientes de Desarrollo

| Categoria | Subtemas sin Ejercicios | Prioridad |
|-----------|------------------------|-----------|
| Algebra y Calculo | Funciones exponenciales, logaritmicas, ecuaciones cuadraticas | Alta |
| Geometria | Conicas (parabola, elipse, hiperbola), razonamiento espacial | Alta |
| Geometria (no generico) | Razones trigonometricas (13 subtemas vacios) | Alta |
| Geometria (no generico) | Funciones e identidades trigonometricas (11 subtemas vacios) | Alta |
| Geometria | Geometria basica: puntos, rectas, triangulos, poligonos (16 subtemas vacios) | Media |
| Estadistica | Probabilidad condicional avanzada, distribuciones | Media |
| Todas | Ejercicios nivel n3 y n4 | Alta |
| Todas | Contenido generico (Razonamiento Cuantitativo) | Alta |

---

## Pipeline de Calidad

### Flujo de Promocion

```
01-En-PreDesarrollo/  →  02-En-Desarrollo/  →  03-En-Produccion/
    (prototipo)           (construccion)         (validado)
                          + Ciclo de Validacion
                          + 5 Coherencias
                          + 200+ versiones unicas
                          + Renderizado 4 formatos
```

### 5 Coherencias Obligatorias

Cada ejercicio debe superar verificacion en:

1. **Semantica**: Gramatica espanola correcta, redaccion ICFES
2. **Visual-Texto**: Graficos coherentes con enunciado
3. **Matematica**: Calculos verificables, respuesta correcta valida
4. **Codigo**: Dinamico, sin hardcoding, compatible 4 formatos
5. **General**: Legible, estilo ICFES, nivel apropiado

### Validacion Automatica

- **Testing**: 12 suites, 110+ tests unitarios, cobertura 100%
- **Hooks**: 4 hooks activos (pre/post Edit/Write/Bash)
- **CI/CD**: GitHub Actions para validacion en remoto
- **Ortografia**: Verificacion automatica de tildes en espanol

---

## Referencias

- **Marco de Referencia ICFES**: Guia de Orientacion Saber 11 2024-2 (vigente 2026)
- **Estandares MEN**: Estandares Basicos de Competencias en Matematicas
- **R-exams**: https://www.R-exams.org/
- **Fuente de verdad**: `A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/`
- **Reglas del sistema**: `.claude/CLAUDE.md` (22 reglas obligatorias; 21 tienen archivo propio en
  `.claude/rules/` — la #5, las «5 Coherencias», se define en el propio indice)
- **Testing**: `tests/testthat/` (22 suites enganchadas a `tests/run_all_tests.R`)

---

*Actualizado: 2026-07-30 | Alineado con Marco ICFES Matematicas 2026 | Estructura fisica verificada con .gitkeep*

*Cambio 2026-07-30: nuevo nodo `10-Combinatoria_Permutaciones-Variaciones-Combinaciones/` bajo
`06-Estadistica-Y-Probabilidad/Pensamiento-Aleatorio/`, para conteo combinatorio (permutaciones,
variaciones, combinaciones). Se crea aparte del `07-` —que pese a llamarse «...-Conteo» solo
contiene probabilidad y diagramas de Venn— y se numera `10-` en vez de insertarse antes del `07-`
para no renumerar rutas ya citadas del arbol inmutable `03-En-Produccion/`.*
