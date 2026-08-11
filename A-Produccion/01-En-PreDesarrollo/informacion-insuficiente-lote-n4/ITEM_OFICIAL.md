# MAT-2026-1-010 — ítem oficial, transcripción verbatim

> **Por qué existe este archivo.** Tres agentes seguidos razonaron sobre este ítem **de memoria**
> porque el texto oficial no estaba en el subproyecto, y uno de ellos lo declaró explícitamente:
> «Ninguno de nosotros pudo contrastar contra `MAT-2026-1-010`: no está en el repo». El resultado
> fue una objeción **falsa** dada por buena y propagada: se afirmó que las opciones oficiales decían
> «área del lote» y «perímetro del lote», y se «corrigió» el ejercicio quitando «del empresario» —
> que es justo lo que el ítem oficial sí dice. La fidelidad verbatim se rompió al intentar arreglarla.
>
> **Contrasta contra este archivo, no contra tu recuerdo.**

## Fuente

- Cuadernillo: ERA-2026 Sesión 1, Matemáticas.
- Imagen: `…/Alineacion-Curricular-de-Items-Matematicas-2026-1/Originales/pagina_012.jpg`
- Ficha de alineación: `…/Alineacion-curricular-de-items-Matematicas-2026-1.md`, sección
  `### MAT-2026-1-010` (≈ línea 2404).

## Enunciado (verbatim)

> Un empresario compró un lote rectangular de 50 metros de largo y 120 metros de ancho, para
> repartirlo en partes iguales entre sus 8 hijos. Con base en esta información, ¿cuál de las
> siguientes preguntas NO es posible responder?

## Opciones (verbatim, con su letra oficial)

| Letra | Texto |
|---|---|
| A | ¿Cuál es el área del lote del empresario? |
| **B** | **¿Cuál es la ubicación que le corresponde a cada hijo dentro del lote?** |
| C | ¿Cuál es el perímetro del lote del empresario? |
| D | ¿Cuál es la fracción del lote que le corresponde a cada hijo? |

**Clave oficial: B.** La ubicación específica de la porción de cada hijo no se determina con las
medidas del lote y el número de hijos; hace falta saber cómo se realizan los cortes.

Obsérvese que **A y C llevan «del empresario»** y **B y D no**. Cualquier instancia canónica del
ejercicio debe reproducir esa asimetría tal cual: no es un descuido del cuadernillo.

## Clasificación oficial (para los `exextra[...]`)

| Campo | Valor literal |
|---|---|
| Competencia | Argumentación |
| Componente | Geométrico-métrico |
| Nivel | 4 |
| Afirmación | Valida procedimientos y estrategias matemáticas utilizadas para dar solución a problemas. |
| Evidencia | Establece la validez o pertinencia de una solución propuesta a un problema dado. |
| Descriptor | D4.9 — Justifica si hay falta de información en una situación problema para tomar una decisión. |
| Grado sugerido | 6°-7° |
| Genérico | Sí |

## Justificaciones metacognitivas del ítem oficial (de la ficha)

- **Opción A (área del lote)**: el estudiante descarta la pregunta como si fuera incalculable, sin
  advertir que largo × ancho = 50 × 120 = 6.000 m² se obtiene directamente de los dos datos.
- **Opción C (perímetro del lote)**: no reconoce que el perímetro es calculable con las dos medidas
  dadas: 2(50 + 120) = 340 m.
- **Opción D (fracción del lote)**: confunde la fracción (1/8, calculable dividiendo entre 8) con la
  ubicación física de cada porción; sí es posible decir «1/8» sin saber cómo se hacen los cortes.

## Regla de uso

La **instancia canónica** del `.Rmd` debe reproducir el enunciado y las cuatro opciones **verbatim**,
sin añadidos de contexto narrativo (nada de trazados, carreteras ni linderos que el cuadernillo no
menciona) y sin quitar «del empresario» de A y C. Las **demás versiones** parametrizan libremente,
siempre que conserven la estructura del descriptor D4.9.
