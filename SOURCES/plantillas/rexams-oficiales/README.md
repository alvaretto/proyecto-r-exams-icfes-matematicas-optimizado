# Plantillas oficiales de R/exams — material de referencia externo

Los 46 ejercicios-plantilla que distribuye el paquete `exams`, copiados verbatim
como referencia de sintaxis. **No son ejercicios ICFES y no forman parte del
workflow de producción.**

- Índice navegable: [CATALOGO.md](CATALOGO.md)
- Procedencia y verificación de integridad: [VERSION.txt](VERSION.txt)
- Licencia y atribución: [LICENSE-exams.txt](LICENSE-exams.txt)

```
rexams-oficiales/
├── rmd/   45 archivos .Rmd
└── rnw/   46 archivos .Rnw   (tstat_verbatim solo existe en este formato)
```

---

## Jerarquía de autoridad — leer antes de copiar nada de aquí

Este repositorio ya tenía una «Fuente de la Verdad» antes de que existiera este
directorio, y **no es esta**. Las dos autoridades cubren cosas distintas:

| Autoridad sobre… | La fuente es… |
|---|---|
| **Sintaxis y semántica de R/exams** — `exname`, `extype`, `exsolution`, `exshuffle`, `exclozetype`, `extol`, estructura `Question` / `Solution` / `Meta-information`, `answerlist()`, `include_supplement()`, tipos de gap CLOZE | **este directorio** |
| **Estándar pedagógico ICFES** — reglas #1 a #22: metacognición, Progressive Disclosure, pool de errores conceptuales, tildes, `{width=}`, guard `none`, letter-independence, diversidad sustantiva, diagnosticidad | `A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/` y `.claude/rules/` |

**En conflicto entre ambas, gana el estándar ICFES.**

Esto no es una formalidad. Los templates oficiales **incumplen por diseño** varias
reglas de este repo, y eso es correcto: están fuera de su alcance. Verificado sobre
los archivos reales:

| Regla del repo | Estado en los templates oficiales |
|---|---|
| #1 ejercicios metacognitivos | No la cumplen. Casi todos son cálculo o recuerdo directo (`capitals`, `switzerland`, `tstat`). Sin pool de errores conceptuales ni reflexión metacognitiva |
| #7 tildes en español | No aplica: están en inglés |
| #18 `{width=}` en imágenes Markdown | La cumplen donde emiten imágenes (`fruit`, `fruit2` usan `{width="0.85cm"}`), pero `Rlogo.Rmd` emite `![](Rlogo.png)` **sin** atributo |
| #19 letter-independence en `Solution` | No la garantizan |
| #20 guard del contador `none` | Ninguno lo incluye, pese a que varios emiten tablas |
| #22 diversidad sustantiva | Varían datos, pero ninguno declara ni verifica que la respuesta correcta cambie |

**Corolario operativo:** copiar de aquí *cómo se escribe* un CLOZE de tipos mixtos
es correcto. Copiar de aquí *cómo se diseña* un distractor es un error — para eso
está `Ejemplos-Funcionales-Rmd/`.

---

## Prohibiciones

1. **No editar ningún archivo de `rmd/` ni `rnw/`.** Son copias verbatim de
   upstream y su valor depende de serlo. Un incumplimiento de regla ICFES
   detectado en un template se anota en `CATALOGO.md`, no se corrige en el archivo.
2. **No mover este directorio dentro de `A-Produccion/`.** Es la razón por la que
   está aquí y no allí:
   - `A-Produccion/03-En-Produccion/` y `Ejemplos-Funcionales-Rmd/` son inmutables
     (regla #2 del `CLAUDE.md` raíz).
   - Bajo `01-En-PreDesarrollo/` o `02-En-Desarrollo/`, el hook
     `pre-write-rmd-gate.sh` exige `ejercicio_state.json` y nomenclatura ICFES;
     `deriv.Rmd` no la cumple ni puede cumplirla.
   - 23 suites de `tests/testthat/` barren `.Rmd` bajo `A-Produccion/`. Como los
     templates incumplen por diseño las reglas #18, #19 y #20, moverlos allí
     pondría el runner en rojo.
3. **No añadir este directorio a ninguna allowlist de tests.** Si un test empieza
   a barrerlo, el error está en el test, no aquí.

---

## Cómo usarlo

Buscar por técnica en el [índice del catálogo](CATALOGO.md#indice-por-tecnica)
(«necesito un ejemplo oficial de TikZ dinámico» → `logic`, `automaton`), leer el
archivo, y adaptar **la sintaxis** a un `.Rmd` ICFES que cumpla las reglas del repo.

Renderizar un template desde aquí, para inspeccionarlo:

```r
library(exams)
exams2html("SOURCES/plantillas/rexams-oficiales/rmd/deriv.Rmd", n = 1, dir = tempdir())
```

Los ejercicios que entregan datos al estudiante (`lm`, `boxhist`, `essayreg2`)
**generan su propio CSV en tiempo de render**; los que muestran imágenes (`fruit`,
`fruit2`) las llevan incrustadas en base64. Ninguno depende de un archivo que haya
que copiar aparte. La única excepción es `Rlogo.Rmd`, que resuelve su PNG con
`include_supplement("Rlogo.png", dir = find.package("exams"), recursive = TRUE)`
— es decir, desde el paquete instalado.

### Dos plantillas con requisitos extra

| Plantilla | Requisito | Estado con este entorno |
|---|---|---|
| `penguins` | `exams` ≥ 2.4-3 (usa `add_cloze()` y `format_metainfo()`) | **No renderiza** — hay 2.4.2 instalado. Error real: `no se pudo encontrar la función "add_cloze"` |
| `fruit`, `fruit2` | paquete `base64enc` | Disponible |

**Cobertura real de la prueba de render (2026-08-10):** se renderizaron **6 de los
45** `.Rmd`, elegidos por ejercitar las técnicas con riesgo de dependencia externa
—`Rlogo` (supplement), `fruit` (base64), `lm` (CSV generado al vuelo), `logic`
(TikZ), `vowels` (CLOZE multi-gap) y `deriv` (caso base)—. Los seis produjeron
HTML válido. Los **39 restantes no se probaron**: no hay evidencia de que fallen,
pero tampoco de que rendericen.

---

## Mantenimiento

Este directorio es una **instantánea**, no un espejo sincronizado. Para refrescarlo
tras actualizar el paquete `exams`, volver a copiar desde
`system.file("exercises", package = "exams")` y regenerar `CATALOGO.md` y
`VERSION.txt`. El procedimiento de verificación de integridad está descrito en
`VERSION.txt`.
