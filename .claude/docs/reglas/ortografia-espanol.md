> **Texto íntegro** de la regla. La versión compacta que Claude Code carga en cada sesión es `.claude/rules/ortografia-espanol.md`.
> Si cambias la norma, actualiza **ambas**; el historial y las mediciones viven aquí (política de contexto v3.28.0).

# Ortografía Española - Diccionario de Referencia

**Copiar palabras TAL CUAL de aquí. No improvisar.**

## REGLA DE GENERACIÓN (OBLIGATORIA)

**Al generar o escribir CUALQUIER archivo .Rmd (Write/Edit), Claude DEBE incluir tildes correctas en TODO texto español visible al estudiante DESDE EL PRIMER BORRADOR.**

No es aceptable generar sin tildes y corregir después. La generación con tildes correctas es obligatoria en:

1. **Strings `paste0()`** en plantillas de contextos narrativos
2. **Strings `paste0()`** en descripciones de errores conceptuales (descripcion_corta, descripcion_larga, causa_raiz)
3. **Strings `c()`** en reflexiones metacognitivas
4. **Strings `paste0()`** en sol_texts del Answerlist de Solution
5. **Texto Markdown** en secciones Question y Solution
6. **Encabezados Markdown** (### Análisis, ### Reflexión, etc.)

**Errores frecuentes de generación (detectados 2026-02-26):**

| Palabra sin tilde (ERROR) | Palabra correcta | Contexto típico |
|---------------------------|------------------|-----------------|
| `realizo` | `realizó` | Plantillas narrativas (pretérito) |
| `pregunto` | `preguntó` | Plantillas narrativas |
| `organizo` | `organizó` | Plantillas narrativas |
| `descubrio` | `descubrió` | Plantillas narrativas |
| `publico` | `publicó` | Plantillas narrativas |
| `encuesto` | `encuestó` | Plantillas narrativas |
| `identifico` | `identificó` | Descripción de errores |
| `excluyo` | `excluyó` | Descripción de errores |
| `confundio` | `confundió` | Descripción de errores |
| `sombreo` | `sombreó` | Descripción de errores |
| `cometio` | `cometió` | Descripción de errores |
| `aplico` | `aplicó` | Descripción de errores |
| `interpreto` | `interpretó` | Descripción de errores |
| `cafeteria` | `cafetería` | Contextos narrativos |
| `boletin` | `boletín` | Contextos narrativos |
| `periodico` | `periódico` | Contextos narrativos |
| `companeros` | `compañeros` | sujeto del contexto |
| `interseccion` | `intersección` | Errores + Solution |
| `exclusion` | `exclusión` | Errores + Solution (NO en variables R) |
| `union` (texto) | `unión` | Errores + Solution (NO en variables R) |
| `omision` | `omisión` | causa_raiz |
| `confusion` | `confusión` | causa_raiz |
| `region` | `región` | Solution markdown |
| `operacion` | `operación` | Solution markdown |
| `expresion` | `expresión` | Solution markdown |
| `analisis` | `análisis` | Encabezados Solution |
| `opcion` | `opción` | Solution ("Opción A") |
| `reflexion` | `reflexión` | Encabezados Solution |
| `tambien` | `también` | Solution markdown |
| `ademas` | `además` | Descripciones errores |
| `calculo` | `cálculo` | Solution markdown |
| `asi` | `así` | Plantillas narrativas |
| `area` | `área` | Descripciones errores |
| `Cual` | `¿Cuál` | Pregunta Question (agregar ¿) |
| `Que` (interrogativo) | `¿Qué` | Pregunta retórica (agregar ¿) |
| `como` (interrogativo) | `cómo` | Reflexiones metacognitivas |

### Concordancia en textos dinámicos (2026-10-29)

**Norma.** Si una cantidad o un sujeto se sortean, el número y el género también se resuelven por rama (`1 curso aporta` / `2 cursos aportan`; `los`/`las` según el sustantivo sorteado). El corrector de tildes no la revisa.

**Patrón.** El verbo y el sustantivo dependen de la cantidad sorteada, así que la frase se arma por rama y no se deja fija en la plantilla:

```r
frase_cursos <- if (k == 1) {
  "participó 1 curso más, que aporta"
} else {
  paste0("participaron ", k, " cursos más, que aportan")
}
```

Para el género, el artículo se elige junto con el sustantivo en la misma estructura de datos (por ejemplo, cada elemento del pool lleva su `articulo` y su `sustantivo`), nunca se fija en la plantilla.

**Casos medidos.**
- 2026-06-03: el profesor pidió artículo `los`/`las` según el contexto en `rango_colesterol` (concordancia de género).
- 2026-10-29, `totales-promedios-reciclaje-n3`: «participó 1 curso más, que aportan» apareció en el 25,7 % de 401 versiones (concordancia de número). Lo hallaron por separado el detractor, el auditor visual y el validador del paso 8.

**Por qué no basta el validador.** `corregir_ortografia_espanol.R` revisa tildes con un diccionario; no revisa la concordancia de número ni de género. Un texto puede salir «limpio» y concordar mal.

---

## Palabras Frecuentes

```
más  según  así  después  también  además  aquí  ahí

ángulo  gráfica  gráfico  función  número  cálculo  método  código  propósito  patrón  máximo  mínimo  análisis  éxito

dispersión  solución  ecuación  relación  variación  descripción  información  configuración  clasificación  validación  explicación  distribución  combinación  iteración  sección  versión  dimensión  selección

intersección  exclusión  unión  omisión  confusión  reflexión  operación  expresión  región  opción  comprensión  metacognición  interpretación  argumentación  formulación

matemático  estadística  científico  parabólico  geométrico  numérico  teórico  único  dinámico  automático  semántico  simbólico  simultáneo

realizó  preguntó  organizó  descubrió  publicó  encuestó  identificó  excluyó  confundió  sombreó  cometió  aplicó  interpretó  calculó  obtuvo

cafetería  boletín  periódico  compañeros  jóvenes  líder  géneros  área  comité  gastronómica
```

## Excepciones (SIN tilde)

### 1. Variables R
- `angulos`, `solucion`, `grafica`, `numero`, `codigo`, `metodo`
- Razón: Nombres de variables R deben ser ASCII para compatibilidad

### 2. Identificadores de R-exams (OBLIGATORIO ASCII)
Estos campos van en ASCII porque R-exams los usa como identificadores (nombre de archivo, sección, tipo, solución):

```
exname: nombre_ejercicio_sin_tildes
exsection: Numerico-Variacional/Argumentacion
extype: schoice
exsolution: 1000
exshuffle: TRUE
extol: 0.01
```

Las **claves** de `exextra` también van en ASCII: `exextra[Afirmacion]`, `exextra[GradoSugerido]`.

### 2b. Valores de `exextra`: LITERALES del catálogo ICFES (decisión del 2026-10-22)
Los **valores** de `exextra` se copian literales del catálogo canónico o de la ficha del ítem, con sus
tildes, mayúsculas y signos (`°`, `—`):

```
exextra[Competencia]: Argumentación
exextra[Componente]: Geométrico-métrico
exextra[Afirmacion]: Valida procedimientos y estrategias matemáticas utilizadas para dar solución a problemas.
exextra[GradoSugerido]: 6°-7°
```

- **Por qué**: la alineación ICFES exige literalidad. El catálogo canónico es `CANONICO_INMUTABLE`, los
  orquestadores lo copian «sin parafrasear» (§ Exigencia ICFES) y `NOMENCLATURA_ARCHIVOS_RMD.md` ya decía
  «Los `exextra[...]` llevan tildes y mayúsculas (literalidad ICFES)». Quitar las tildes es parafrasear.
- **Verificado el 2026-10-22**: un ejercicio de control con Competencia «Argumentación», Componente
  «Geométrico-métrico», un Descriptor con «—» y GradoSugerido «6°-7°» renderiza en HTML, PDF, DOCX y
  Moodle, y `exams::read_metainfo()` devuelve los cinco valores intactos en UTF-8. La razón que daba esta
  regla («R-exams parsea estos campos como identificadores») vale para `exname`/`exsection`, no para los
  valores de `exextra`, que R/exams guarda como texto.
- **Alcance**: ejercicios nuevos y los que se corrijan. Los valores en ASCII que ya existen no se migran
  en bloque (y `03-En-Produccion/` es inmutable): se corrigen al tocar el ejercicio.
- El corrector `corregir_ortografia_espanol.R` excluye las líneas `exextra`: la literalidad se comprueba
  contra el catálogo en el paso 10 (`validar_icfes`), no con el corrector. Una diferencia solo de formato
  frente a la ficha (tildes, «°», mayúsculas) se reporta como FORMATO, no como contenido distinto.
- **Historial**: hasta el 2026-10-22 esta sección decía «`exextra[...]` siempre ASCII» y contradecía a la
  nomenclatura y a los orquestadores; un ejercicio llegó a conservar `6-7` frente al `6°-7°` de su ficha
  amparado en la versión vieja.

### 3. Inglés técnico
- TikZ, LaTeX, R-exams, chunk, hash, reticulate, ggplot2

### 4. Pronombres demostrativos
- "Esta/Estas" (≠ "Está" verbo estar)
- "Este/Estos" (≠ "Esté" subjuntivo)

## Validación Automática

```bash
# Verificar errores (sin corregir)
Rscript .claude/scripts/corregir_ortografia_espanol.R archivo.Rmd

# Aplicar correcciones
Rscript .claude/scripts/corregir_ortografia_espanol.R archivo.Rmd --fix
```

El script excluye automáticamente:
1. Metadatos R-exams (exname, exsection, extype, etc.)
2. Nombres de variables R en contexto de código
3. Código inline `` `r variable` ``

## NUNCA usar --no-verify

Si el hook de ortografía detecta errores:
1. Verificar si son falsos positivos (metadatos R-exams → ya excluidos)
2. Si son errores reales → corregirlos con `--fix`
3. **NUNCA** usar `git commit --no-verify` para evadir el hook
