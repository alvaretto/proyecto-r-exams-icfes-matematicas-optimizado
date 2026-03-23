# Checklist Metacognitivo — SCHOICE

## PASO 8: Checklist pre-promocion

Verificar TODOS los items antes de avanzar:

- [ ] Pool de errores conceptuales con codigos (minimo 4)
- [ ] Respuesta erronea ≠ respuesta correcta (guardia is.na() incluida)
- [ ] Distractores unicos entre si (digest::digest verificado)
- [ ] Solucion incluye analisis del error (descripcion_larga + causa_raiz)
- [ ] Solucion incluye reflexion metacognitiva (del pool de reflexiones)
- [ ] Metadatos DOK, Bloom, SOLO presentes en META-INFORMATION
- [ ] DOK >= 2 (preferible 3)
- [ ] Bloom incluye Analizar o Evaluar
- [ ] SOLO incluye Relacional o Abstracto-Extendido
- [ ] Test de diversidad > 250 versiones unicas de 300 intentos

## Condiciones criticas

### Pre-generacion

- Analisis ICFES completado con Tipo = schoice
- **Patron metacognitivo seleccionado** (Error Ajeno / Afirmacion / Comparacion)
- **Pool de errores conceptuales definido (minimo 4)**
- Ejemplo funcional similar identificado y leido
- Nomenclatura calculada (incluye "metacognitivo")
- Carpeta destino creada en `02-En-Desarrollo/`

### Durante generacion

- Funcion `generar_datos()` con aleatorizacion
- **Pool de errores con funciones `calcula`** (deterministicas)
- **Pool de reflexiones metacognitivas** (minimo 4)
- Distractores basados en errores conceptuales (NUNCA aleatorios)
- Formato espanol en todos los numeros (punto decimal, sin separadores de miles)
- Metadatos ICFES completos (6 dimensiones obligatorias)
- **Metadatos cognitivos: DOK, Bloom, SOLO**
- `exshuffle: TRUE` obligatorio
  - Excepcion: `FALSE` en SCHOICE con opciones graficas PNG (ver `graficos-como-opciones.md`)
- Guardia `is.na()` antes de comparacion con resultado de `calcula()`
- `set.seed()` en chunks de test debe guardar/restaurar `.Random.seed`

### Post-generacion

- Renderizado exitoso en 4 formatos (HTML, PDF, DOCX, NOPS)
- Coherencia matematica pregunta-respuesta-distractores
- **Respuesta erronea diferente de correcta** (test_that confirma)
- Test de diversidad > 250 versiones unicas
- **Solucion incluye todas las subsecciones obligatorias** (6 secciones)
- Ciclo de validacion completo (FASE 1 → 2A → 2B → 2C → 3)

NO terminar con errores pendientes.
