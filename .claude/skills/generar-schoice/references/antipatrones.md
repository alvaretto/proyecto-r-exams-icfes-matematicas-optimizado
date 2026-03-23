# Antipatrones PROHIBIDOS — SCHOICE Metacognitivo

## 1. Ejercicio puramente procedimental

```markdown
❌ "Calcula el area de un rectangulo con base 8 cm."
```

**Correccion:** Convertir a analisis de error:

```markdown
✓ "Un estudiante calculo 8 + 5 = 13 como area. ¿Cual fue su error conceptual?"
```

**Por que:** Los ejercicios procedimentales no desarrollan metacognicion (Schraw & Dennison, 1994).
El patron obligatorio es: *evaluar el razonamiento de otro*, no calcular directamente.

---

## 2. Distractores aleatorios

```r
❌ distractores <- respuesta + sample(-10:10, 3)
```

**Correccion:** Usar pool de errores conceptuales:

```r
✓ error_sel <- errores_conceptuales[[sample(errores_aplicables_idx, 1)]]
✓ respuesta_erronea <- error_sel$calcula(datos_ord)
```

**Por que:** Los distractores aleatorios no representan errores reales de estudiantes
y no permiten diagnostico pedagogico. Cada distractor debe tener codigo, causa_raiz
y funcion calcula() asociada.

---

## 3. Solucion sin analisis de error

```markdown
❌ Solution
========
La respuesta correcta es 40.
```

**Correccion:** Incluir analisis completo con las 6 subsecciones obligatorias:

```markdown
✓ ### Analisis del Error
✓ **Error identificado:** [descripcion_larga del error seleccionado]
✓ **Codigo de error:** [codigo, ej: EST-MTC-01]
✓ **Causa raiz:** [causa_raiz del error]
✓
✓ ### Procedimiento Correcto
✓ **Paso 1:** [descripcion + formula LaTeX]
✓ $$...$$
✓
✓ ### Propiedades del Concepto
✓ [afirmaciones sobre el concepto matematico]
✓
✓ ### Caso Especifico
✓ [transferencia a caso concreto]
✓
✓ ### Reflexion Metacognitiva
✓ `r sample(reflexiones_metacognitivas, 1)`
✓
✓ ### Estrategia para Evitar el Error
✓ 1. [paso preventivo]
✓ 2. [verificacion final]
```

**Por que:** La Solution debe desarrollar consciencia metacognitiva, no solo
confirmar la respuesta. Ver `.claude/rules/ejercicios-metacognitivos.md` —
seccion "Solucion Solution Obligatoria".
