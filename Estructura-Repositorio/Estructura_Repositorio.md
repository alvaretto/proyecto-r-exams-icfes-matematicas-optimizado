# 📁 Estructura del Repositorio - Matemáticas ICFES R-Exams

## 🎯 Descripción General

Este documento describe el árbol de ejercicios **en producción**: `A-Produccion/03-En-Produccion/`. Está organizado por área, pensamiento y tema ICFES.

- A producción solo llegan ejercicios validados en aula, mediante `/promover-ejercicio`.
- El desarrollo ocurre en `A-Produccion/01-En-PreDesarrollo/` y `02-En-Desarrollo/`.
- `03-En-Produccion/` no se edita a mano.

**Actualizado**: 2026-10-28, contra el disco. La versión de 2025 mostraba este árbol como si fuera la raíz del repositorio, con 173 directorios.

---

## 📊 Estadísticas (2026-10-28)

| Métrica | Cantidad |
|---------|----------|
| 📁 Directorios en `03-En-Produccion/` (sin contar la raíz) | 302 |
| 📄 `.Rmd` en las áreas temáticas | 100 |
| 📄 `.Rmd` en `Ejemplos-Funcionales-Rmd/` (inmutables) | 87 |

| Área | `.Rmd` |
|------|--------|
| `01-Numeros-Reales/` | 4 |
| `02-Funciones/` | 28 |
| `03-Razones-Trigonometricas/` | 0 |
| `04-Funciones_Identidades-Trigonometricas/` | 0 |
| `05-Geometría/` | 4 |
| `06-Estadística-Y-Probabilidad/` | 64 |

---

## 📊 Estructura (área → pensamiento → tema → ejercicio)

El árbol omite las carpetas internas de cada ejercicio (ver «Convenciones de Estructura»). Las cifras entre paréntesis son los `.Rmd` de cada carpeta de ejercicio; una carpeta sin cifra no tiene `.Rmd`.

```
A-Produccion/03-En-Produccion/
├── 01-Numeros-Reales/
│   └── Pensamiento-Numerico/
│       ├── 01-Numeros-Racionales/
│       ├── 02-Numeros-Irracionales/
│       ├── 03-Numeros-Reales/
│       │   ├── 2024-CB-S1_S2-P02-calculo-de-ganancias/
│       │   └── 22-S2-2025-SEDQ-fracciones_reparto_premio/  (4 .Rmd)
│       ├── 04-Propiedades-Expresiones-Decimales/
│       ├── 05-Conjunto-Reales-Desigualdades/
│       ├── 06-Valor-Absoluto/
│       └── 07-Proporcionalidad-Directa-E-Inversa/
│           └── excedente_almuerzo_proporcional_n4/
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
│       │   ├── Variacion-Lineal-Vuelo-Acrobatico/  (11 .Rmd)
│       │   └── Variación-Lineal-Auto-Viajero-09/  (17 .Rmd)
│       └── 12-Introduccion-Al-Limite-De-Una-Sucesion/
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
├── 05-Geometría/
│   └── Pensamiento-Espacial/
│       ├── 01-Puntos-Y-Lineas/
│       │   ├── coordenadas_vertices_plano_cartesiano_n2/
│       │   └── desplazamiento_avion_aeropuerto_n3/
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
│       ├── 17-Conversión-de-Unidades/
│       │   └── conversion_unidades_area_formulacion_ejecucion/  (1 .Rmd)
│       └── 18-Volumen-Y-Raíz-Cúbica/
│           └── raiz_cubica_empaquetamiento_geometrico_metrico_formulacion_ejecucion_n2_v1/  (3 .Rmd)
├── 06-Estadística-Y-Probabilidad/
│   └── Pensamiento-Aleatorio/
│       ├── 01-Variables-Cualitativas_Distribucion-De-Frecuencias/
│       │   ├── Accidentalidad_Vial_Genero-16/  (4 .Rmd)
│       │   ├── ExportacionesGraficos-Tebailandia/  (5 .Rmd)
│       │   ├── Gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1/  (2 .Rmd)
│       │   ├── Graficos_Estadisticos_Adopcion_Mascotas/  (8 .Rmd)
│       │   └── Pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1/  (1 .Rmd)
│       ├── 02-Variables-Cuantitativas-Discretas_Distribucion-De-Frecuencias/
│       ├── 03-Variables-Cuantitativas-Continuas-Distribución-de-Frecuencias/
│       │   └── poblaciones_paises_graficas_lineas_interpretacion_representacion_n2_v1/  (1 .Rmd)
│       ├── 04-Medidas-De-Tendencia-Central/
│       │   ├── 01-MediaMedianaModa/  (2 .Rmd)
│       │   ├── Media/  (7 .Rmd)
│       │   └── Mediana/  (15 .Rmd)
│       ├── 05-Medidas-De-Dispersión/
│       │   └── confint2-cloze/  (2 .Rmd)
│       ├── 06-Medidas-De-Posición/
│       │   └── estadistica_diagramas_caja_interpretacion_representacion/  (2 .Rmd)
│       ├── 07-Probabilidad_Principios-Aditivo-Multiplicativo-Conteo/
│       │   ├── Diagramas de Venn/  (4 .Rmd)
│       │   └── Probabilidad-Bolas-Colores/  (2 .Rmd)
│       ├── 08-Probabilidad-De-La-Union-De-Sucesos/
│       │   └── diagrama_venn_encuesta_metacognitivo_interpretacion_n2_schoice_v1/  (4 .Rmd)
│       ├── 09-Probabilidad-Condicionada_Independencia-De-Sucesos/
│       │   └── Probabilidad-Intervalos-Curva-13-S1-2024B/  (4 .Rmd)
│       └── 10-Combinatoria_Permutaciones-Variaciones-Combinaciones/
│           └── permutaciones_pescadores_venia_n4/
└── Ejemplos-Funcionales-Rmd/  (87 .Rmd; inmutable)
```

**Carpetas de ejercicio sin `.Rmd`** (se conservan; no son restos):

- Destinos **reservados** con un `.gitkeep` para ejercicios que aún no se promueven:
  - `coordenadas_vertices_plano_cartesiano_n2/`: ejercicio del barco, `b92f2f44`, 2026-07-28.
  - `desplazamiento_avion_aeropuerto_n3/`: ejercicio del avión, `b1407946`, 2026-07-28.
  - `permutaciones_pescadores_venia_n4/`: `3392b998`, 2026-07-30, junto con el nodo `10-Combinatoria…`.
- `excedente_almuerzo_proporcional_n4/`: reserva **local, sin versionar**. Su `.gitkeep` es del 2026-08-06, y el tema `07-Proporcionalidad-Directa-E-Inversa/` tampoco está en git.
- `2024-CB-S1_S2-P02-calculo-de-ganancias/`: guarda solo una imagen (`all.png`), versionada desde el 2026-01-30.

---

## 🎯 Organización por Competencias ICFES

### 📊 **Pensamiento Numérico**
- **Ubicación**: `01-Numeros-Reales/Pensamiento-Numerico/`
- **Contenidos**: números racionales, irracionales y reales, expresiones decimales, desigualdades, valor absoluto, proporcionalidad directa e inversa.

### 📈 **Pensamiento Variacional y Espacial**
- **Ubicación**:
  - `02-Funciones/Pensamiento-Variacional-Espacial/`
  - `03-Razones-Trigonometricas/Pensamiento-Espacial-Metrico-Y-Variacional/`
  - `04-Funciones_Identidades-Trigonometricas/Pensamiento-Espacial-Y-Variacional/`
- **Contenidos**: funciones, variación y razón de cambio, trigonometría, identidades trigonométricas.

### 📐 **Pensamiento Espacial**
- **Ubicación**: `05-Geometría/Pensamiento-Espacial/`. En 2025 la carpeta se llamaba `05-Geometria-Analitica/`.
- **Contenidos**: 18 temas, desde puntos y líneas hasta volumen y raíz cúbica. Incluye geometría analítica de la recta (13), secciones cónicas (14) y conversión de unidades (17).

### 🎲 **Pensamiento Aleatorio**
- **Ubicación**: `06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/`
- **Contenidos**: variables y distribuciones de frecuencias (01-03), medidas de tendencia central, dispersión y posición (04-06), probabilidad (07-09) y combinatoria (10, desde el 2026-07-30).

---

## 📍 Cómo ubicar un ejercicio

1. **Área y pensamiento**: por el componente de la ficha de alineación (`exextra[Componente]`).
2. **Tema**: por lo que el ítem **evalúa**, no por su estímulo. Una tabla de frecuencias puede ser solo el soporte de una pregunta de probabilidad.
3. **Descartar los temas vecinos** con la misma regla y dejar escrito por qué.
4. **Carpeta del ejercicio** dentro del tema (ver «Convenciones de Estructura»).
5. **Promoción**: la hace `/promover-ejercicio` cuando hay evidencia de aula. Mientras tanto, el destino se puede reservar con una carpeta que solo lleva `.gitkeep`.

### Ejemplo resuelto (2026-10-28): `probabilidad-fichas-tabla-doble-entrada-n2`

- **Componente Aleatorio**: va en `06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/`.
- **Tema**: el ítem compara la probabilidad simple (casos favorables / casos posibles) de dos tipos de ficha de una misma bolsa, con el descriptor D2.3. Para hacerlo suma celdas de la tabla que no se solapan, que es el principio aditivo. Por eso va en **`07-Probabilidad_Principios-Aditivo-Multiplicativo-Conteo/`**, junto a `Probabilidad-Bolas-Colores/`.
- **Descartes**:
  - **08**: trata uniones con intersección; ahí está el ejercicio de diagramas de Venn.
  - **09**: es probabilidad condicionada, y la invariante I-4 del ejercicio la prohíbe.
  - **01**: la tabla es el estímulo, pero lo evaluado es probabilidad.

---

## 📍 Destinos asignados (pendientes de promoción)

| Ejercicio | Destino | Reserva |
|-----------|---------|---------|
| `probabilidad-fichas-tabla-doble-entrada-n2` | `06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/07-Probabilidad_Principios-Aditivo-Multiplicativo-Conteo/` | Sin reservar. Aprobado para el aula el 2026-10-27; falta la evidencia de aula |
| Ejercicio del barco | `05-Geometría/Pensamiento-Espacial/01-Puntos-Y-Lineas/coordenadas_vertices_plano_cartesiano_n2/` | `.gitkeep`, `b92f2f44` |
| Ejercicio del avión | `05-Geometría/Pensamiento-Espacial/01-Puntos-Y-Lineas/desplazamiento_avion_aeropuerto_n3/` | `.gitkeep`, `b1407946` |
| Permutaciones de pescadores | `06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/10-Combinatoria_Permutaciones-Variaciones-Combinaciones/permutaciones_pescadores_venia_n4/` | `.gitkeep`, `3392b998` |
| Excedente del almuerzo | `01-Numeros-Reales/Pensamiento-Numerico/07-Proporcionalidad-Directa-E-Inversa/excedente_almuerzo_proporcional_n4/` | `.gitkeep` local, sin versionar |

El estado de cada ejercicio está en el `HANDOFF.md` de su carpeta en `01-En-PreDesarrollo/`.

---

## 📁 Convenciones de Estructura

### **Directorios estándar por ejercicio (convención de 2025)**
- `docus/` - Documentación del ejercicio
- `ejercicios/` - Archivos .Rmd del ejercicio
- `salida/` - Archivos generados (HTML, PDF, XML)
- `images/` - Recursos gráficos
- `tikz_temp/` - Archivos temporales de TikZ
- `_snaps/` - Snapshots de pruebas

### **Práctica de las promociones de 2026**
- La carpeta del ejercicio lleva su nombre base y el `.Rmd` va en su raíz, sin `ejercicios/`; los renders van en `salida/`. Ejemplo: `08-Probabilidad-De-La-Union-De-Sucesos/diagrama_venn_encuesta_metacognitivo_interpretacion_n2_schoice_v1/`, promovido el 2026-02-27.
- Las reservas de destino usan el nombre del ejercicio en minúsculas, con guiones bajos.
- Las carpetas temáticas vacías llevan `.gitkeep` para que git las conserve (commit `60e92581`, 2026-02-04).

### **Tipos de Ejercicios R-exams**
- `schoice/` - Selección única (single choice)
- `mchoice/` - Selección múltiple (multiple choice)
- `cloze/` - Ejercicios de completar
- `num/` - Respuesta numérica
- `string/` - Respuesta de texto

---

## 🔧 Cómo regenerar el árbol

- `estructura-actual-repositorio.sh` (en esta carpeta) es de 2025 y supone que la raíz del repositorio es este árbol:
  - recorre el directorio actual y escribe en él su `.md` y una carpeta `backups/`;
  - busca `05-Geometria-Analitica/`;
  - necesita `tree`, que hoy no está instalado.

  **No correrlo desde `03-En-Produccion/`**: dejaría archivos dentro de producción.
- Esta versión se generó con un recorrido equivalente a `tree -d -L 4` sobre `A-Produccion/03-En-Produccion/`, sin las carpetas internas de cada ejercicio.
- `Estructura_Repositorio.html` es la versión de pandoc de noviembre de 2025 y no refleja esta actualización.

---

## 🔧 Estado del Desarrollo (2026-10-28)

### ✅ **Áreas con ejercicios en producción**
- Pensamiento Aleatorio: 64 `.Rmd`
- Pensamiento Variacional (funciones): 28 `.Rmd`
- Pensamiento Numérico: 4 `.Rmd`
- Pensamiento Espacial (geometría): 4 `.Rmd`

### 🚧 **Áreas solo con estructura**
- `03-Razones-Trigonometricas/` y `04-Funciones_Identidades-Trigonometricas/`, sin `.Rmd`.

---

*Actualizado a mano el 2026-10-28 contra el disco (ver «Cómo regenerar el árbol»).*
