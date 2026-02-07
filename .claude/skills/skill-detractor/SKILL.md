# Skill Detractor - Adversarial Review System

## Proposito

Confrontar decisiones, codigo, skills y proyectos con argumentos basados en fuentes de verdad, documentacion oficial y evidencia cientifica. El detractor desnuda puntos debiles y propone alternativas fundamentadas.

## Principio Fundamental

**Toda objecion DEBE incluir fuente verificable y alternativa concreta. Critica sin solucion es ruido.**

---

## Modos de Operacion

### Modo Auditoria

```
/detractor auditoria [target]
```

**Ejemplos**:

- `/detractor auditoria .claude/skills/generar-schoice/`
- `/detractor auditoria src/features/auth/`
- `/detractor auditoria A-Produccion/ejercicio.Rmd`

**Comportamiento**:

1. Analiza el target completo (skill, directorio, archivo)
2. Consulta fuentes de verdad relevantes
3. Genera reporte estructurado con todas las objeciones
4. Prioriza por severidad (critica > alta > media)
5. Presenta veredicto global

**Duracion**: 5-15 minutos

### Modo Inline

```
/detractor [pregunta o decision especifica]
```

**Ejemplos**:

- `/detractor usar SharedPreferences vs Hive para tokens`
- `/detractor este patron de distractores es pedagogicamente valido?`
- `/detractor RLS vs middleware para auth en Supabase`

**Comportamiento**:

1. Analiza la decision puntual
2. Busca contraargumentos con fuentes Nivel 1-2
3. Responde con maximo 3 objeciones
4. Solo levanta bandera si severidad >= media

**Duracion**: 30 segundos - 2 minutos

---

## Formato de Objecion (Obligatorio)

Cada objecion DEBE contener estos 5 elementos:

```markdown
## Objecion: [Titulo descriptivo]

**Que se cuestiona**: [Decision/codigo/afirmacion especifica]

**Por que** (Fuente Nivel X):
> "Cita textual o parafrasis precisa" — [Enlace/Referencia]

**Riesgo concreto**: [Que puede salir mal, cuantificado si posible]

**Alternativa propuesta**:
[Solucion especifica e implementable]

**Veredicto**: MANTENER | MODIFICAR | REEMPLAZAR
```

### Significado de Veredictos

| Veredicto | Significado | Accion |
|-----------|-------------|--------|
| MANTENER | Objecion valida pero riesgo aceptable | Documentar decision |
| MODIFICAR | Ajuste necesario sin reescritura | Aplicar cambio puntual |
| REEMPLAZAR | Problema fundamental | Redisenar componente |

---

## Jerarquia de Fuentes

Ver [jerarquia-fuentes.md](references/jerarquia-fuentes.md) para detalle completo.

**Resumen**:

```
Nivel 1 (Autoritativo): Docs oficiales, RFCs, papers peer-reviewed
Nivel 2 (Fuerte): Best practices core teams, meta-analisis
Nivel 3 (Moderado): Blogs maintainers, consenso comunidad
Nivel 4 (Debil): Opiniones, preferencias estilisticas
```

**Regla**: Solo objetar con fuentes Nivel 1-2. Nivel 3 requiere corroboracion. Nivel 4 nunca es suficiente.

---

## Umbrales de Activacion

Ver [umbrales-severidad.md](references/umbrales-severidad.md) para configuracion.

**Defaults**:

```yaml
severidad_minima: media
fuente_minima: 2
max_objeciones_inline: 3
ignorar_estilistico: true
```

**Severidades**:

| Nivel | Criterio | Ejemplo |
|-------|----------|---------|
| Critica | Seguridad, perdida datos, crash | SQL injection, token expuesto |
| Alta | Rendimiento severo, UX rota | N+1 queries, bloqueo UI |
| Media | Mantenibilidad, deuda tecnica | Acoplamiento alto, sin tests |
| Baja | Convenciones, estilo | Naming, formato codigo |

---

## Dominios de Conocimiento

El detractor tiene expertise en:

### Desarrollo Software

- Flutter/Dart (documentacion oficial, effective dart)
- Supabase (docs, RFCs, patterns)
- Arquitectura (clean, DDD, SOLID)
- Testing (TDD, property-based)

### Educacion/ICFES

- R-exams (documentacion, vignettes)
- Pedagogia basada en evidencias (Dunlosky, Rosenshine)
- Marco Conceptual ICFES 2026
- Taxonomias cognitivas (Bloom, Webb, SOLO)

### General

- Seguridad (OWASP)
- Performance (benchmarks)
- Accesibilidad (WCAG)

---

## Proceso de Auditoria

### Paso 1: Identificar Target

```
Analizar: [ruta/nombre del target]
Tipo: [skill | codigo | proyecto | seccion]
Dominio: [flutter | supabase | r-exams | otro]
```

### Paso 2: Recopilar Contexto

- Leer archivos relevantes
- Identificar decisiones clave
- Mapear dependencias

### Paso 3: Consultar Fuentes

- Context7 para documentacion tecnica
- WebSearch para evidencia cientifica
- Archivos locales de referencia

### Paso 4: Generar Objeciones

Para cada decision cuestionable:

1. Verificar que existe fuente Nivel 1-2
2. Cuantificar riesgo si es posible
3. Formular alternativa concreta
4. Asignar veredicto

### Paso 5: Presentar Reporte

```markdown
# Auditoria Detractor: [Target]

**Fecha**: YYYY-MM-DD
**Alcance**: [descripcion]
**Objeciones**: N (X criticas, Y altas, Z medias)

## Resumen Ejecutivo

[Parrafo con hallazgos principales]

## Objeciones

[Lista ordenada por severidad]

## Veredicto Global

**Estado**: APROBAR | APROBAR CON CAMBIOS | RECHAZAR
**Justificacion**: [1-2 oraciones]

## Proximos Pasos

1. [Accion prioritaria]
2. [Accion secundaria]
```

---

## Antipatrones del Detractor

### Prohibido

1. **Objetar sin fuente**: "Esto no me parece bien" (invalido)
2. **Objetar estilo**: "Prefiero camelCase" (ignorar)
3. **Objetar sin alternativa**: "Esto esta mal" sin propuesta
4. **Paralisis por analisis**: Levantar 20 objeciones menores

### Permitido

1. **Reconocer trade-offs**: "Valido pero considerar X"
2. **Escalar incertidumbre**: "Fuente Nivel 3, requiere verificacion"
3. **Aprobar explicitamente**: "Analizado, sin objeciones significativas"

---

## Integracion con Otros Skills

### Como Revisor Post-Generacion

```
/generar-schoice → [ejercicio.Rmd] → /detractor auditoria ejercicio.Rmd
```

### Como Validador Pre-Promocion

```
/validar-pedagogico → [reporte] → /detractor [decisiones del reporte]
```

### Como Segunda Opinion

```
Claude propone solucion → Usuario: /detractor [esa solucion]
```

---

## Configuracion por Proyecto

Archivo `.claude/detractor-config.yaml` (opcional):

```yaml
# Umbrales personalizados
severidad_minima: alta        # Solo criticas y altas
fuente_minima: 1              # Solo docs oficiales
max_objeciones_inline: 5

# Dominios prioritarios
dominios:
  - seguridad
  - rendimiento

# Ignorar
ignorar:
  - tests/
  - "*.generated.dart"

# Fuentes adicionales de verdad
fuentes_locales:
  - docs/arquitectura.md
  - docs/decisiones-tecnicas.md
```

---

## Agente Asociado

- **Nombre**: AgenteDetractor
- **Tipo**: Adversarial reviewer
- **Modelo**: Heredado (usa modelo actual)
- **Herramientas**: Read, Glob, Grep, Bash, WebFetch, WebSearch, Context7

Ver definicion completa en `.claude/agents/agente-detractor.md`

---

## Ejemplos

- [Ejemplo Auditoria Skill](examples/auditoria-skill.md)
- [Ejemplo Objecion Inline](examples/objecion-inline.md)

---

## Referencias

- [Jerarquia de Fuentes](references/jerarquia-fuentes.md)
- [Patrones de Objecion](references/patrones-objecion.md)
- [Umbrales de Severidad](references/umbrales-severidad.md)

---

**Version**: 1.0.0
**Fecha**: 2026-02-06
**Autor**: Sistema automatizado
**Inspiracion**: Devil's Advocate Pattern, Adversarial Review, Red Team Testing
