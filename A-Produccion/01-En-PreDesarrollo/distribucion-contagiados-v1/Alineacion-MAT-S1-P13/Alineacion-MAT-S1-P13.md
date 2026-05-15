---
title: "Alineación curricular ICFES Matemáticas — S1 P13 (ítem suelto)"
area: Matemáticas
fuente: "S1.pdf — Primera sesión respuestas (pág. 4, pregunta 13)"
fuente_ruta: "/home/bootcamp/Descargas/S1.pdf"
fecha_alineacion: "2026-05-14"
modo: "alineación standalone (sin subproyecto)"
catalogos_oficiales:
  niveles: "Matematicas/catalogos-oficiales-mat/niveles-mat.json (md5 7ed5aa1237a5db6d5ba07c9a38dd9a2a)"
  evidencias: "Matematicas/catalogos-oficiales-mat/evidencias-mat.json (md5 6339b53011f5e43480a19b3c6c5c9bab)"
  estandares: "Matematicas/catalogos-oficiales-mat/estandares-mat-ebc.json (md5 a784677fe4f380456cd1089e172ed9b2)"
output:
  html_document:
    css: ../tools/areas-dashboard.css
    self_contained: true
---

# Alineación curricular ICFES Matemáticas — S1 P13

## Nomenclatura

- **Cuadernillo**: S1 (Primera sesión respuestas, fuente externa al monorepo).
- **Código de ficha**: `MAT-S1-P13` (ítem 13 de la sesión primera).
- **Nota de origen**: el usuario solicitó alineación a partir de la imagen `Err-01.png` (mockup didáctico con un bug editorial — dos opciones correctas en la versión adaptada). La alineación se hace sobre el **ítem ICFES real** (PDF), no sobre el mockup.

* * *

### MAT-S1-P13 — Selección de gráfica que reproduce una distribución de frecuencias por rango de edad

- **Enunciado**: La tabla muestra la cantidad de personas contagiadas de un virus en un pequeño hospital, según su rango de edad (45-64: 56 personas; 65-74: 4 personas; 75+: 20 personas; total 80 personas). Se pregunta cuál de las cuatro gráficas presentadas muestra correctamente la distribución de las 80 personas contagiadas en el hospital.
- **Descripción Breve**: Selección de gráfica equivalente a tabla de frecuencias por grupos de edad (3 categorías; total 80).
- **Competencia**: Interpretación y representación
- **Componente**: Aleatorio
- **Aprendizaje**: Selecciona la representación gráfica que corresponde a una tabla de frecuencias preservando las categorías declaradas y la proporcionalidad entre sus valores.
- **Afirmación**: Comprende y transforma la información cuantitativa y esquemática presentada en distintos formatos.
- **Evidencia**: Transforma la representación de una o más piezas de información.
- **Tema Específico**: Representaciones gráficas de distribuciones de frecuencias (torta y barras); correspondencia tabla ↔ gráfica.
- **Contenido**: Estadística
- **¿Qué evalúa?**: Capacidad de leer una tabla de frecuencias y reconocer entre varias propuestas gráficas aquella que respeta tanto las categorías de la variable como las cantidades asociadas a cada una de ellas (sin inventar categorías, sin omitir datos y sin distorsionar las proporciones).
- **Tarea**: A partir de una tabla de frecuencias con tres categorías de una variable agrupada y sus cantidades respectivas, seleccionar la representación gráfica que reproduce fielmente la distribución observada, descartando las propuestas que incluyen categorías inexistentes, omiten datos o presentan proporciones que no corresponden a los valores tabulados.
- **Descriptor**: D3.1 — Selecciona la gráfica (que puede ser de doble entrada correspondiente a la información de una tabla, o a partir de verbalizaciones (características de crecimiento o decrecimiento deseadas), teniendo en cuenta para la selección la escala, el tipo de variable y el tipo de gráfica.
- **Nivel**: 3
- **Grado sugerido**: 6°-7°
- **Estándar asociado**: Interpreto, produzco y comparo representaciones gráficas adecuadas para presentar diversos tipos de datos. (diagramas de barras, diagramas circulares.) (6°-7°, Pensamiento aleatorio)
- **Genérico**: Sí
- **Clave**: B — La torta con tres sectores etiquetados 45-64, 65-74 y 75+ cuyas áreas reproducen proporcionalmente las frecuencias 56, 4 y 20 sobre el total de 80 personas (70 %, 5 % y 25 %, respectivamente).
- **Justificaciones MetaCognitivas**:
  - A: el estudiante elige la torta que añade una cuarta categoría "Otro" inexistente en la tabla; preserva el formato circular pero no advierte que la tabla declara exactamente tres rangos de edad cuya suma ya es 80 y, por tanto, no admite una categoría residual.
  - B: respuesta correcta — los tres sectores conservan tanto las categorías de la tabla como la proporción 56:4:20 entre las frecuencias.
  - C: el estudiante elige una torta de tres sectores (categorías correctas) pero con áreas aproximadamente equilibradas; se queda con la coincidencia de etiquetas y no verifica que el sector de 45-64 deba ser claramente dominante respecto a los otros dos.
  - D: el estudiante elige una gráfica de barras (tipo de representación válido en abstracto) pero ignora que las categorías del eje horizontal no coinciden con las de la tabla (introduce "0-45" inexistente y/o repite "75+"), confundiendo "ser barras" con "representar correctamente la tabla".

* * *

## Resumen Estadístico

### Distribución por Competencia

| Competencia | Cantidad |
|---|---|
| Interpretación y representación | 1 |
| Formulación y ejecución | 0 |
| Argumentación | 0 |
| **Total** | **1** |

### Distribución por Componente

| Componente | Cantidad |
|---|---|
| Numérico-variacional | 0 |
| Geométrico-métrico | 0 |
| Aleatorio | 1 |
| **Total** | **1** |

### Distribución por Nivel

| Nivel | Cantidad |
|---|---|
| N1 | 0 |
| N2 | 0 |
| N3 | 1 |
| N4 | 0 |
| **Total** | **1** |

* * *

## Notas de auditoría

- **Capa 0 (anti-paráfrasis)**: Afirmación, Evidencia, Descriptor y Estándar asociado se transcriben **literalmente** desde los catálogos canónicos JSON (md5 verificado en el preámbulo).
- **Diferencia con el mockup `Err-01.png`**: el mockup que se entregó como entrada presenta valores distintos (20-35:52 / 36-50:12 / 51+:16) y un bug editorial (las opciones B y D representan ambas correctamente la distribución, contradiciendo el enunciado "solo una"). La ficha aquí presentada alinea el **ítem ICFES real**, no el mockup; el cuadernillo S1.pdf no exhibe ese bug.
- **Clave**: confirmada como B por la marca de visto verde sobre el círculo de la opción B en el original (S1.pdf, pág. 4).
