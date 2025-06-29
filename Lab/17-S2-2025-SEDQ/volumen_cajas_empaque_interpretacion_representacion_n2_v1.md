---
output:
  html_document: default
  word_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float", "tikz", "xcolor"]

# Metadatos ICFES
icfes:
  competencia: interpretacion_representacion
  nivel_dificultad: 2
  contenido:
    categoria: geometria
    tipo: generico
  contexto: familiar
  eje_axial: eje2
  componente: geometrico_metrico
---








```
## Error: LaTeX failed to compile diagrama_cajas.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See diagrama_cajas.log for more info.
```



Question
========

La siguiente imagen muestra las dimensiones de 2 cajas: Caja 1 y Caja 2 que se utilizan en una empresa de té para empacar bolsitas de té.


```
## Error: LaTeX failed to compile diagrama_cajas_question.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See diagrama_cajas_question.log for more info.
```

De acuerdo con la información anterior, respecto al volumen de las cajas, es correcto afirmar que

Answerlist
----------
- tres cajas 2 ocupan el mismo volumen que una caja 1
- cuatro cajas 1 ocupan el mismo volumen que tres cajas 2
- la caja 2 ocupa el doble del volumen de la caja 1 (método alternativo)
- la caja 1 ocupa el doble del área de la caja 2

Solution
========

Para resolver este problema, necesitamos calcular el volumen de cada caja y establecer la relación entre ellos.

### Paso 1: Calcular el volumen de la Caja 1

La Caja 1 es un prisma rectangular con dimensiones:
- Largo: 40 cm
- Ancho: 10 cm
- Alto: 10 cm

**Volumen de la Caja 1 = Largo × Ancho × Alto**
Volumen₁ = 40 × 10 × 10 = 4000 cm³

### Paso 2: Calcular el volumen de la Caja 2

La Caja 2 es un cubo con dimensiones:
- Lado: 12 cm

**Volumen de la Caja 2 = Lado³**
Volumen₂ = 12³ = 1728 cm³

### Paso 3: Establecer la relación entre volúmenes

Para comparar los volúmenes, calculamos la relación:

**Relación Volumen₁/Volumen₂ = 4000/1728 = 2.31**

**Relación Volumen₂/Volumen₁ = 1728/4000 = 0.43**

### Paso 4: Interpretar el resultado

Como 2.31 ≈ 2, podemos concluir que tres cajas 2 ocupan el mismo volumen que una caja 1.

### Verificación de distractores:

- **Opción incorrecta sobre áreas**: Confunde volumen (3D) con área (2D)
- **Opción incorrecta sobre factores**: Error en el cálculo de la relación numérica
- **Opción incorrecta de inversión**: Invierte cuál caja tiene mayor volumen

Answerlist
----------
- Verdadero. Esta es la respuesta correcta: tres cajas 2 ocupan el mismo volumen que una caja 1
- Falso. Esta opción es incorrecta porque no refleja la relación real entre los volúmenes calculados.
- Falso. Esta opción es incorrecta porque no refleja la relación real entre los volúmenes calculados.
- Falso. Esta opción es incorrecta porque no refleja la relación real entre los volúmenes calculados.

Meta-information
================
exname: Volumen de cajas de empaque - Interpretación y representación
extype: schoice
exsolution: 1000
exshuffle: TRUE
exsection: Geometría/Volumen de prismas rectangulares
