---
name: validar-metadatos-icfes
description: Valida que el ejercicio cumpla con la estructura R-exams e ICFES.
metadata:
  model_recommendation: haiku
---

> **ROUTING**: Este skill tiene `model_recommendation: haiku`. Claude DEBE delegarlo via `Task(subagent_type="general-purpose", model="haiku")` pasando las instrucciones completas y la ruta del archivo .Rmd como contexto. Ver regla `.claude/rules/modelo-routing-obligatorio.md`.

1. Lee el archivo .Rmd generado.
2. Verifica la presencia de 'schoice' o 'cloze' según el tipo de ejercicio en el workflow [2].
3. Asegura que existan distractores pedagógicos y 250+ versiones únicas (de 300 intentos) [3].