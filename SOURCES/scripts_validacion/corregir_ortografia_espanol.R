#!/usr/bin/env Rscript
# =============================================================================
# Script: corregir_ortografia_espanol.R
# Propósito: Corrección automática de tildes en archivos .Rmd
# Uso: Rscript corregir_ortografia_espanol.R archivo.Rmd [--fix]
#      --fix: Aplica correcciones (sin este flag solo reporta)
#
# CRITERIO DE REPARTO ENTRE LOS DOS DICCIONARIOS (2026-08-06)
# -----------------------------------------------------------------------------
# Si la forma SIN tilde no es una palabra española válida ("consumio", "demas"),
# la corrección es unívoca -> `diccionario_tildes`, auto-corregible con --fix.
# Si la forma sin tilde TAMBIÉN es palabra válida ("formula" verbo, "seria"
# adjetivo, "publico" adjetivo, "realizo" 1ª persona), ninguna sustitución ciega
# es segura -> `diccionario_ambiguo`: se REPORTA para revisión humana, y --fix
# NUNCA la toca.
#
# POR QUÉ (punto ciego medido el 2026-08-06): este script firmó
# "✓ No se encontraron errores ortográficos" sobre un .Rmd que emitía al
# estudiante `formula` x14, `Si, porque` x11, `demas` x7, `consumio` x4,
# `seria` x3, `asistio` y `correspondia`. Ninguna de esas formas estaba en el
# diccionario. La regla #7 y el hook pre-commit-ortografia.sh se apoyan en esta
# salida, así que el defecto atravesaba las tres capas con un "limpio" firmado.
# El diccionario había derivado respecto de la tabla normativa de
# `.claude/rules/ortografia-espanol.md`, que ya listaba varias de esas formas.
# Desde este cambio, un caso ambiguo ya no imprime "limpio": imprime
# REVISION_MANUAL (marcador grepeable, distinto de la palabra que busca el hook).
# =============================================================================

# Diccionario de correcciones: palabra_sin_tilde -> palabra_con_tilde
diccionario_tildes <- c(

  # Sustantivos y términos técnicos
  "informacion" = "información",
  "descripcion" = "descripción",
  "explicacion" = "explicación",
  "configuracion" = "configuración",
  "solucion" = "solución",
  "validacion" = "validación",
  "clasificacion" = "clasificación",
  "ecuacion" = "ecuación",
  "dimension" = "dimensión",
  "version" = "versión",
  "seleccion" = "selección",
  "seccion" = "sección",
  "funcion" = "función",
  "relacion" = "relación",
  "distribucion" = "distribución",
  "variacion" = "variación",
  "dispersion" = "dispersión",
  "combinacion" = "combinación",
  "iteracion" = "iteración",
  "compilacion" = "compilación",
  "instalacion" = "instalación",
  "documentacion" = "documentación",
  "retroalimentacion" = "retroalimentación",
  "elongacion" = "elongación",
  "deformacion" = "deformación",
  "inclinacion" = "inclinación",
  "operacion" = "operación",
  "multiplicacion" = "multiplicación",
  "division" = "división",
  "adicion" = "adición",
  "sustraccion" = "sustracción",
  "fraccion" = "fracción",
  "proporcion" = "proporción",
  "razon" = "razón",
  "evaluacion" = "evaluación",
  "medicion" = "medición",
  "observacion" = "observación",
  "experimentacion" = "experimentación",
  "interpretacion" = "interpretación",
  "representacion" = "representación",
  "argumentacion" = "argumentación",
  "formulacion" = "formulación",
  "ejecucion" = "ejecución",

 # Términos matemáticos/científicos esdrújulos
  "grafica" = "gráfica",
  "grafico" = "gráfico",
  "graficas" = "gráficas",
  "graficos" = "gráficos",
  "matematico" = "matemático",
  "matematica" = "matemática",
  "estadistica" = "estadística",
  "estadistico" = "estadístico",
  "cientifico" = "científico",
  "cientifica" = "científica",
  "parabolico" = "parabólico",
  "parabolica" = "parabólica",
  "geometrico" = "geométrico",
  "geometrica" = "geométrica",
  "numerico" = "numérico",
  "numerica" = "numérica",
  "teorico" = "teórico",
  "teorica" = "teórica",
  "unico" = "único",
  "unica" = "única",
  "dinamico" = "dinámico",
  "dinamica" = "dinámica",
  "automatico" = "automático",
  "automatica" = "automática",
  "semantico" = "semántico",
  "semantica" = "semántica",
  "cuadratico" = "cuadrático",
  "cuadratica" = "cuadrática",
  "logaritmico" = "logarítmico",
  "logaritmica" = "logarítmica",
  "exponencial" = "exponencial",
  "periodico" = "periódico",
  "periodica" = "periódica",
  "simetrico" = "simétrico",
  "simetrica" = "simétrica",
  "asimetrico" = "asimétrico",
  "asimetrica" = "asimétrica",

  # Sustantivos comunes
  "codigo" = "código",
  "proposito" = "propósito",
  "analisis" = "análisis",
  "numero" = "número",
  "numeros" = "números",
  "angulo" = "ángulo",
  "angulos" = "ángulos",
  "calculo" = "cálculo",
  "calculos" = "cálculos",
  "metodo" = "método",
  "metodos" = "métodos",
  "exito" = "éxito",
  "patron" = "patrón",
  "maximo" = "máximo",
  "maxima" = "máxima",
  "minimo" = "mínimo",
  "minima" = "mínima",
  "area" = "área",
  "areas" = "áreas",
  "perimetro" = "perímetro",
  "diametro" = "diámetro",

  "triangulo" = "triángulo",
  "triangulos" = "triángulos",
  "rectangulo" = "rectángulo",
  "rectangulos" = "rectángulos",
  "circulo" = "círculo",
  "circulos" = "círculos",
  "piramide" = "pirámide",
  "energia" = "energía",
  "periodo" = "período",
  "vehiculo" = "vehículo",
  "vehiculos" = "vehículos",
  "pagina" = "página",
  "paginas" = "páginas",
  "linea" = "línea",
  "lineas" = "líneas",
  "termino" = "término",
  "terminos" = "términos",
  "limite" = "límite",
  "limites" = "límites",
  "hipotesis" = "hipótesis",
  "sintesis" = "síntesis",
  "tesis" = "tesis",
  "enfasis" = "énfasis",

  # Adverbios y conectores
  "mas" = "más",
  "tambien" = "también",
  "asi" = "así",
  "aqui" = "aquí",
  "ahi" = "ahí",
  "alla" = "allá",
  "despues" = "después",
  "segun" = "según",
  "ademas" = "además",
  "todavia" = "todavía",
  "quiza" = "quizá",
  "quizas" = "quizás",
  "dificil" = "difícil",
  "facil" = "fácil",
  "util" = "útil",
  "debil" = "débil",
  "agil" = "ágil",
  "fertil" = "fértil",
  "esteril" = "estéril",
  "movil" = "móvil",
  "habil" = "hábil",
  "fragil" = "frágil",
  "optima" = "óptima",
  "optimo" = "óptimo",
  # "cual"/"cuales" EXCLUIDOS — ambiguos:
  # "lo cual" (pronombre relativo, SIN tilde) vs "¿cuál?" (interrogativo, CON tilde)
  # El script no puede distinguir el contexto gramatical.

  # Verbos conjugados comunes
  # NOTA: "esta" EXCLUIDO - confunde pronombre demostrativo con verbo.
  # "estan" NO es ambiguo y sí se corrige (añadido 2026-08-06): no existe el
  # demostrativo plural *"estan" (es "estas"/"estos"), así que la única lectura
  # posible es el verbo "están". La nota original los excluía a los dos juntos.
  "estan" = "están",
  "estrategico" = "estratégico",
  "estrategica" = "estratégica",
  "estrategicos" = "estratégicos",
  "estrategicas" = "estratégicas",
  "sera" = "será",
  "seran" = "serán",
  "estara" = "estará",
  "estaran" = "estarán",
  "podra" = "podrá",
  "podran" = "podrán",
  "tendra" = "tendrá",
  "tendran" = "tendrán",
  "hara" = "hará",
  "haran" = "harán",
  "debera" = "deberá",
  "deberan" = "deberán",
  "habra" = "habrá",
  "habran" = "habrán",

  # ---------------------------------------------------------------------------
  # Pretéritos e imperfectos INEQUÍVOCOS (la forma sin tilde no es palabra).
  # Añadidos 2026-08-06 tras el punto ciego descrito en la cabecera.
  # NO añadir aquí formas -ar de 1ª persona ("realizo", "aplico"): son palabras
  # válidas y viven en `diccionario_ambiguo`.
  # ---------------------------------------------------------------------------
  "consumio" = "consumió",
  "asistio" = "asistió",
  "recibio" = "recibió",
  "decidio" = "decidió",
  "repartio" = "repartió",
  "dividio" = "dividió",
  "invirtio" = "invirtió",
  "convirtio" = "convirtió",
  "permitio" = "permitió",
  "respondio" = "respondió",
  "escribio" = "escribió",
  "cumplio" = "cumplió",
  "reunio" = "reunió",
  "eligio" = "eligió",
  "pidio" = "pidió",
  "siguio" = "siguió",
  "subio" = "subió",
  "salio" = "salió",
  "ocurrio" = "ocurrió",
  "sucedio" = "sucedió",
  "surgio" = "surgió",
  "descubrio" = "descubrió",
  "confundio" = "confundió",
  "cometio" = "cometió",
  "correspondio" = "correspondió",
  "correspondia" = "correspondía",
  "existia" = "existía",
  "queria" = "quería",
  "podia" = "podía",
  "debia" = "debía",
  "decia" = "decía",
  "veia" = "veía",
  "habia" = "había",
  "servia" = "servía",
  "sobraria" = "sobraría",
  "faltaria" = "faltaría",
  "quedaria" = "quedaría",
  "tendria" = "tendría",
  "podria" = "podría",
  "deberia" = "debería",
  "habria" = "habría",
  "haria" = "haría",

  # Sustantivos/adjetivos INEQUÍVOCOS de la tabla normativa de la regla #7
  # (`.claude/rules/ortografia-espanol.md`) que faltaban aquí.
  "demas" = "demás",
  "cafeteria" = "cafetería",
  "boletin" = "boletín",
  "companeros" = "compañeros",
  "jovenes" = "jóvenes",
  "lider" = "líder",
  "comite" = "comité",
  "generos" = "géneros",
  "interseccion" = "intersección",
  "exclusion" = "exclusión",
  "omision" = "omisión",
  "confusion" = "confusión",
  "comprension" = "comprensión",
  "metacognicion" = "metacognición",
  "expresion" = "expresión",
  "opcion" = "opción",
  "reflexion" = "reflexión",
  "region" = "región",
  "presion" = "presión",
  "teoria" = "teoría",
  "categoria" = "categoría",
  "categorias" = "categorías",
  "dia" = "día",
  "dias" = "días",
  "decimo" = "décimo",
  "simultaneo" = "simultáneo",
  "gastronomica" = "gastronómica",
  "erroneo" = "erróneo",
  "erronea" = "errónea",
  "erroneos" = "erróneos",
  "erroneas" = "erróneas",

  # Adverbios en -mente cuyo adjetivo base lleva tilde. Ninguna de estas formas
  # sin tilde es palabra válida, así que la corrección es unívoca.
  "explicitamente" = "explícitamente",
  "implicitamente" = "implícitamente",
  "unicamente" = "únicamente",
  "practicamente" = "prácticamente",
  "automaticamente" = "automáticamente",
  "matematicamente" = "matemáticamente",
  "logicamente" = "lógicamente",
  "rapidamente" = "rápidamente",
  "facilmente" = "fácilmente",
  "dificilmente" = "difícilmente",
  "especificamente" = "específicamente",
  "teoricamente" = "teóricamente",
  "numericamente" = "numéricamente",
  "basicamente" = "básicamente",
  "ultimamente" = "últimamente",
  "tipicamente" = "típicamente",
  "economicamente" = "económicamente",
  "historicamente" = "históricamente",
  "identicamente" = "idénticamente",

  # "ñ" escrita como "n" (mojibake o teclado sin ñ). Solo las formas que NO son
  # palabra española válida ni nombre propio frecuente: "campana", "ano",
  # "cana", "nino" y "sueno" SÍ lo son y viven en `diccionario_ambiguo`.
  # Origen: `pequeno` en texto visible al estudiante, 2026-08-06.
  "pequeno" = "pequeño",
  "pequena" = "pequeña",
  "pequenos" = "pequeños",
  "pequenas" = "pequeñas",
  "disenar" = "diseñar",
  "diseno" = "diseño",
  "disenado" = "diseñado",
  "disenada" = "diseñada",
  "senal" = "señal",
  "senales" = "señales",
  "ensenar" = "enseñar",
  "ensenanza" = "enseñanza",
  "manana" = "mañana",
  "tamano" = "tamaño",
  "tamanos" = "tamaños",
  "acompanar" = "acompañar",
  "acompanado" = "acompañado"
)

# Sustituciones DECIDIBLES POR CONTEXTO (regex), auto-corregibles con --fix.
# Solo entran aquí patrones cuya lectura alternativa es gramaticalmente
# imposible, no meramente improbable.
reglas_contextuales <- list(
  # --- Reglas MORFOLÓGICAS -----------------------------------------------
  # No existe en español ninguna palabra terminada en "-cion", "-sion" o
  # "-xion" sin tilde: todas son -ción / -sión / -xión. Una regla por sufijo
  # cubre de golpe el vocabulario abierto (situacion, operacion, presion,
  # conexion...) y evita que el diccionario tenga que enumerarlo palabra por
  # palabra — que es exactamente como se abrió el punto ciego de 2026-08-06:
  # "informacion" estaba y "situacion" no. Las entradas léxicas equivalentes
  # que ya existen arriba se aplican antes y son inofensivas (redundantes).
  # El `(?![áéíóúñÁÉÍÓÚÑ])` final es OBLIGATORIO y no decorativo (bug corregido
  # 2026-08-06): sin él, "Seleccionó" empareja hasta "-cion" —la "ó" siguiente
  # cuenta como carácter de palabra, así que `\b` no frena— y el resultado es
  # "Selecciónó", una palabra con dos tildes. Es el mismo guard que ya usaba el
  # pase léxico; la regla morfológica nació sin él.
  list(patron = "\\b([A-Za-zÁÉÍÓÚÑáéíóúñ]+)cion\\b(?![áéíóúñÁÉÍÓÚÑ])", reemplazo = "\\1ción",
       nombre = "-cion -> -ción", motivo = "no hay palabra española terminada en -cion sin tilde"),
  list(patron = "\\b([A-Za-zÁÉÍÓÚÑáéíóúñ]+)sion\\b(?![áéíóúñÁÉÍÓÚÑ])", reemplazo = "\\1sión",
       nombre = "-sion -> -sión", motivo = "no hay palabra española terminada en -sion sin tilde"),
  list(patron = "\\b([A-Za-zÁÉÍÓÚÑáéíóúñ]+)xion\\b(?![áéíóúñÁÉÍÓÚÑ])", reemplazo = "\\1xión",
       nombre = "-xion -> -xión", motivo = "no hay palabra española terminada en -xion sin tilde"),

  # --- Interrogativos ya marcados con "¿" ---------------------------------
  # Tras un signo de apertura, la lectura interrogativa es la única posible:
  # el pronombre relativo átono ("lo cual", "en cuanto a") no puede ir ahí.
  # Fuera de "¿", estas palabras siguen siendo ambiguas y NO se tocan.
  list(patron = "¿Cual\\b",    reemplazo = "¿Cuál",    nombre = "¿Cual -> ¿Cuál",       motivo = "interrogativo tras ¿"),
  list(patron = "¿Que\\b",     reemplazo = "¿Qué",     nombre = "¿Que -> ¿Qué",         motivo = "interrogativo tras ¿"),
  list(patron = "¿Como\\b",    reemplazo = "¿Cómo",    nombre = "¿Como -> ¿Cómo",       motivo = "interrogativo tras ¿"),
  list(patron = "¿Cuanto\\b",  reemplazo = "¿Cuánto",  nombre = "¿Cuanto -> ¿Cuánto",   motivo = "interrogativo tras ¿"),
  list(patron = "¿Cuantos\\b", reemplazo = "¿Cuántos", nombre = "¿Cuantos -> ¿Cuántos", motivo = "interrogativo tras ¿"),
  list(patron = "¿Cuanta\\b",  reemplazo = "¿Cuánta",  nombre = "¿Cuanta -> ¿Cuánta",   motivo = "interrogativo tras ¿"),
  list(patron = "¿Cuantas\\b", reemplazo = "¿Cuántas", nombre = "¿Cuantas -> ¿Cuántas", motivo = "interrogativo tras ¿"),
  list(patron = "¿Donde\\b",   reemplazo = "¿Dónde",   nombre = "¿Donde -> ¿Dónde",     motivo = "interrogativo tras ¿"),
  list(patron = "¿Cuando\\b",  reemplazo = "¿Cuándo",  nombre = "¿Cuando -> ¿Cuándo",   motivo = "interrogativo tras ¿"),
  list(patron = "¿Quien\\b",   reemplazo = "¿Quién",   nombre = "¿Quien -> ¿Quién",     motivo = "interrogativo tras ¿"),

  # --- Adverbio afirmativo "sí" -------------------------------------------
  list(
    patron = "\\bSi,",
    reemplazo = "Sí,",
    nombre = "Si, -> Sí,",
    # Un "si" condicional nunca va seguido de coma: "si, ..." no existe en
    # español. Si hay coma, es el adverbio afirmativo. Origen: 11 opciones de
    # respuesta "Si, porque ..." que el diccionario de palabras no podía cazar
    # (no se puede meter "si" -> "sí" en un diccionario léxico sin destrozar
    # todos los condicionales).
    motivo = "un 'si' condicional nunca va seguido de coma"
  ),
  list(
    patron = "\\bSi\\.",
    reemplazo = "Sí.",
    nombre = "Si. -> Sí.",
    motivo = "un 'si' condicional nunca cierra oración"
  )
)

# Patrones que solo se REPORTAN (requieren juicio humano, nunca --fix).
reglas_contextuales_ambiguas <- list(
  list(
    patron = "¿[^?]*\\bcomo\\b[^?]*\\?",
    nombre = "como -> cómo (dentro de interrogativa)",
    motivo = "en pregunta directa suele ser interrogativo ('¿cómo?'), pero 'como' comparativo también aparece dentro de preguntas"
  ),
  list(
    # Interrogativa SIN "¿" de apertura: falta el signo y probablemente la
    # tilde. No se auto-corrige porque el arreglo son dos ediciones (abrir el
    # signo en el sitio correcto + acentuar) y el sitio no es deducible.
    patron = "\\b(Como|Cual|Cuales|Que|Quien|Donde|Cuando|Cuanto|Cuantos|Cuanta|Cuantas)\\b[^?¿]*\\?",
    nombre = "interrogativa sin ¿ de apertura",
    motivo = "falta el signo '¿' y, con él, la tilde del interrogativo"
  ),
  list(
    patron = "\\bPor que\\b",
    nombre = "Por que -> Por qué",
    motivo = "'por que' átono existe ('la razón por que...') pero es rarísimo; casi siempre es 'por qué'"
  )
)

# Palabras cuya forma SIN tilde también es palabra española válida: se reportan
# con la forma probable, pero --fix NO las toca. Ver criterio en la cabecera.
diccionario_ambiguo <- list(
  "formula" = list(sug = "fórmula", motivo = "'formula' también es verbo (él formula)"),
  "formulas" = list(sug = "fórmulas", motivo = "'formulas' también es verbo (tú formulas)"),
  "seria" = list(sug = "sería", motivo = "'seria' también es adjetivo (una persona seria)"),
  "publico" = list(sug = "publicó / público", motivo = "dos lecturas acentuadas distintas"),
  "tenia" = list(sug = "tenía", motivo = "'tenia' también es sustantivo (parásito)"),
  "sabia" = list(sug = "sabía", motivo = "'sabia' también es adjetivo (mujer sabia)"),
  "continuo" = list(sug = "continuó / continúo / continuo", motivo = "tres lecturas distintas"),
  "realizo" = list(sug = "realizó", motivo = "'realizo' es 1ª persona del presente"),
  "pregunto" = list(sug = "preguntó", motivo = "'pregunto' es 1ª persona del presente"),
  "organizo" = list(sug = "organizó", motivo = "'organizo' es 1ª persona del presente"),
  "encuesto" = list(sug = "encuestó", motivo = "'encuesto' es 1ª persona del presente"),
  "identifico" = list(sug = "identificó", motivo = "'identifico' es 1ª persona del presente"),
  "aplico" = list(sug = "aplicó", motivo = "'aplico' es 1ª persona del presente"),
  "interpreto" = list(sug = "interpretó", motivo = "'interpreto' es 1ª persona del presente"),
  "excluyo" = list(sug = "excluyó", motivo = "'excluyo' es 1ª persona del presente"),
  "sombreo" = list(sug = "sombreó", motivo = "'sombreo' es 1ª persona del presente y sustantivo"),
  "cuanto" = list(sug = "cuánto", motivo = "'en cuanto a' / 'cuanto más' van sin tilde; el interrogativo indirecto la lleva"),
  "cuantos" = list(sug = "cuántos", motivo = "interrogativo indirecto vs relativo"),
  "cuanta" = list(sug = "cuánta", motivo = "interrogativo indirecto vs relativo"),
  "cuantas" = list(sug = "cuántas", motivo = "interrogativo indirecto vs relativo"),
  "gasto" = list(sug = "gastó", motivo = "'gasto' también es sustantivo (el gasto)"),
  "aporto" = list(sug = "aportó", motivo = "'aporto' es 1ª persona del presente"),
  "domino" = list(sug = "dominó / dominio", motivo = "'domino' es 1ª persona del presente")
)

# Palabras excluidas cuando son nombres de variables R
# Estas palabras NO deben corregirse si aparecen en contexto de código R
palabras_excluir_en_codigo <- c(
  "solucion", "angulos", "angulo", "funcion", "numero",
  "grafica", "grafico", "calculo", "codigo", "metodo",
  "area",  # Variable R frecuente (par_sel$area, ctx$area)
  # Añadidas 2026-08-06 junto con las entradas nuevas del diccionario: son
  # nombres de variable/campo habituales en los .Rmd de este repo.
  "interseccion", "exclusion", "omision", "region", "opcion",
  "expresion", "presion", "teoria", "categoria", "categorias",
  "dia", "dias", "formula", "decimo", "comite"
)

# Campos de metadatos R-exams que DEBEN permanecer ASCII
# Estos campos usan identificadores que no deben tener tildes
campos_rexams_ascii <- c(

  "exname",        # Nombre del ejercicio (identificador)
  "exsection",    # Sección/categoría (ruta con /)
  "extype",       # Tipo: schoice, mchoice, cloze, num, string
  "exsolution",   # Solución (código binario o valor)
  "exshuffle",    # TRUE/FALSE
  "extol",        # Tolerancia numérica
  "exextra"       # Metadatos extra (cualquier exextra[...])
)

# Función para verificar si una línea es metadato R-exams que debe ser ASCII
es_metadato_rexams_ascii <- function(linea) {
  # Verificar si la línea comienza con algún campo R-exams

  for (campo in campos_rexams_ascii) {
    # Patrón para campo exacto o con corchetes (exextra[Type])
    if (grepl(paste0("^", campo, "(\\[.*\\])?\\s*:"), linea, ignore.case = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Función para verificar si una línea es código R (no texto/comentario)
es_codigo_r <- function(linea) {
  # Detectar asignaciones de variables (var <- valor, var = valor)
  if (grepl("^\\s*[a-zA-Z_][a-zA-Z0-9_]*\\s*(<-|=)\\s*", linea)) {
    return(TRUE)
  }
  # Detectar llamadas a funciones con argumentos nombrados
  if (grepl("\\w+\\s*=\\s*\\w+", linea) && !grepl("^#", trimws(linea))) {
    return(TRUE)
  }
  # Detectar definiciones de función
  if (grepl("function\\s*\\(", linea)) {
    return(TRUE)
  }
  return(FALSE)
}

# Función para verificar si es un nombre de variable R
es_nombre_variable <- function(linea, palabra) {
  # Patrones que indican que la palabra es un nombre de variable
  patrones_variable <- c(
    paste0("\\b", palabra, "\\s*<-"),       # var <-
    paste0("\\b", palabra, "\\s*=\\s*c\\("), # var = c(
    paste0("\\b", palabra, "\\s*=\\s*list"), # var = list
    paste0("\\$", palabra, "\\b"),           # datos$var
    paste0("\\b", palabra, "\\$"),           # var$campo
    paste0("\\[\\[\"", palabra, "\"\\]\\]"), # [["var"]]
    paste0("\\bfunction\\s*\\([^)]*", palabra), # argumento de función
    paste0("\\b", palabra, "\\s*=\\s*[^\"']"),  # argumento nombrado
    paste0("^\\s*", palabra, "\\s*<-")       # inicio de línea con asignación
  )

  for (patron in patrones_variable) {
    if (grepl(patron, linea, perl = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Función para verificar si la palabra está dentro de un string R.
#
# Algoritmo robusto: enmascarar contenidos de strings reemplazándolos por "".
# Si después la palabra ya NO aparece, estaba dentro de un string.
# Si aparece, está FUERA de strings (variable R, identificador, etc.).
#
# Esto resuelve el falso positivo del regex anterior cuando la palabra es
# una variable R interpolada entre dos strings:
#   paste0("...período ", periodo, ".")
# El regex viejo veía la palabra entre dos comillas y la marcaba TRUE.
esta_en_string <- function(linea, palabra) {
  linea_limpia <- gsub('"[^"]*"', '""', linea, perl = TRUE)
  linea_limpia <- gsub("'[^']*'", "''", linea_limpia, perl = TRUE)
  patron <- paste0("\\b", palabra, "\\b")
  # ignore.case = TRUE es OBLIGATORIO (bug corregido 2026-08-06): la deteccion
  # de la ocurrencia se hace sin distinguir mayusculas, asi que esta sonda
  # tambien debe hacerlo. Si no, para un identificador capitalizado como
  # `Categoria` la sonda en minusculas NO lo encuentra tras enmascarar los
  # strings, concluye "estaba dentro de un string" y autoriza la correccion
  # sobre una linea de codigo puro. Sintoma real medido: un barrido masivo
  # reescribio `aes(fill = Categoria)` como `aes(fill = Categoría)` y
  # `plt.savefig('opcion.png')` como `'opción.png'`.
  return(!grepl(patron, linea_limpia, perl = TRUE, ignore.case = TRUE))
}

# Un nombre de archivo NO se acentua aunque este dentro de un string: rompe la
# busqueda del archivo y, en la ruta PDF, `\includegraphics{...}` con UTF-8.
# Cubre tanto `"opcion.png"` como los nombres construidos por interpolacion
# (`paste0("opcion", letra, ".png")`, `plt.savefig('grafica' + s + '.png')`).
es_contexto_nombre_archivo <- function(linea, palabra) {
  ext <- "png|jpg|jpeg|pdf|svg|tex|csv|txt|html|xml|docx|Rmd|R"
  if (grepl(paste0("\\b", palabra, "[[:alnum:]_-]*\\.(", ext, ")\\b"),
            linea, perl = TRUE, ignore.case = TRUE)) return(TRUE)
  # La linea construye o consume un nombre de archivo: no tocar nada en ella.
  # `paste0`/`sprintf`/`file.path` entran porque el caso frecuente es el nombre
  # construido por interpolacion, donde la palabra NO es adyacente a la
  # extension: paste0("opcion", letra, ".png").
  grepl(paste0("savefig|ggsave|include_graphics|file\\.path|file\\.copy|",
               "file\\.rename|readPNG|imread|\\bpaste0?\\s*\\(|\\bsprintf\\s*\\(|",
               "\\bpng\\s*\\(|\\bpdf\\s*\\(|\\bjpeg\\s*\\(|\\bsvg\\s*\\("),
        linea, perl = TRUE) &&
    grepl(paste0("\\.(", ext, ")[\"']"), linea, perl = TRUE, ignore.case = TRUE)
}

# Función para verificar si la palabra es parte de un anchor Markdown
# {#word-...} o {#...-word-...}. Los anchors son identificadores ASCII.
es_anchor_markdown <- function(linea, palabra) {
  patron <- paste0("\\{#[^}]*\\b", palabra, "\\b[^}]*\\}")
  return(grepl(patron, linea, perl = TRUE))
}

# Función para verificar si la palabra es parte de una ruta o nombre de
# archivo. Rutas/filenames usan ASCII por convención (Unix/Git compat).
es_ruta_archivo <- function(linea, palabra) {
  patron <- paste0("(?<=[/_-])", palabra, "(?=[/_.-])|",
                   "(?<=[/_-])", palabra, "\\b\\.[a-zA-Z]")
  return(grepl(patron, linea, perl = TRUE))
}

# Función para verificar si la palabra está en código R inline
esta_en_codigo_inline <- function(linea, palabra) {
  # Buscar la palabra dentro de `r ... `
  if (grepl(paste0('`r[^`]*', palabra, '[^`]*`'), linea, perl = TRUE)) {
    return(TRUE)
  }
  return(FALSE)
}

# Función para verificar si la palabra es un nombre de variable
es_palabra_excluida_en_codigo <- function(linea, palabra) {
  if (!(palabra %in% palabras_excluir_en_codigo)) {
    return(FALSE)
  }
  # Verificar si está en contexto de código R
  if (esta_en_codigo_inline(linea, palabra)) {
    return(TRUE)
  }
  # Verificar si es nombre de variable (asignación, argumento, etc.)
  patrones <- c(
    paste0("\\b", palabra, "\\s*<-"),        # var <-
    paste0("<-\\s*", palabra, "\\b"),        # <- var
    paste0("\\b", palabra, "\\s*="),         # var = (asignación o argumento)
    paste0("\\$", palabra, "\\b"),           # datos$var
    paste0("\\b", palabra, "\\$"),           # var$campo
    paste0("\\(", palabra, ","),             # función(var,
    paste0(",\\s*", palabra, "\\)"),         # , var)
    paste0(",\\s*", palabra, ","),           # , var,
    paste0("\\bfunction\\s*\\([^)]*", palabra)  # argumento de función
  )
  for (patron in patrones) {
    if (grepl(patron, linea, perl = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Función para verificar si es un comentario
es_comentario <- function(linea) {
  return(grepl("^\\s*#", linea))
}

# Función para verificar si estamos en sección Markdown (Question/Solution)
# Esta función se llama con el contexto de todas las líneas
en_seccion_markdown <- function(lineas, num_linea) {
  # Buscar hacia atrás la última sección
  for (i in num_linea:1) {
    if (grepl("^(Question|Solution|Answerlist)\\s*$", lineas[i])) {
      return(TRUE)
    }
    if (grepl("^```\\{r", lineas[i])) {
      return(FALSE)  # Estamos en un chunk de R
    }
    if (grepl("^```$", lineas[i]) && i < num_linea) {
      # Buscar si el ``` anterior era apertura o cierre
      for (j in (i-1):1) {
        if (grepl("^```\\{r", lineas[j])) {
          return(TRUE)  # El ``` era cierre, estamos en Markdown
        }
        if (grepl("^```$", lineas[j])) {
          break
        }
      }
    }
  }
  return(FALSE)
}

# Filtros de contexto compartidos por los dos diccionarios.
# Devuelve TRUE si la ocurrencia está en texto VISIBLE al estudiante (o en un
# comentario, que también debe estar bien escrito) y por tanto debe reportarse.
# Extraído a función el 2026-08-06 para que el diccionario ambiguo aplique
# exactamente los mismos filtros que el auto-corregible (si divergieran, uno de
# los dos volvería a tener puntos ciegos).
ocurrencia_es_texto_visible <- function(contenido, num_linea, palabra) {
  linea <- contenido[num_linea]

  # 1. No corregir metadatos R-exams (DEBEN ser ASCII)
  if (es_metadato_rexams_ascii(linea)) return(FALSE)
  # 2. No corregir nombres de variables R ni código inline
  if (es_nombre_variable(linea, palabra)) return(FALSE)
  # 3. No corregir palabras excluidas en contexto de código R
  if (es_palabra_excluida_en_codigo(linea, palabra)) return(FALSE)
  # 3b. No corregir si es anchor Markdown {#palabra-...} (identificador)
  if (es_anchor_markdown(linea, palabra)) return(FALSE)
  # 3c. No corregir si es parte de ruta/nombre de archivo
  if (es_ruta_archivo(linea, palabra)) return(FALSE)
  # 3d. Ni si la linea construye/consume un nombre de archivo (ver funcion)
  if (es_contexto_nombre_archivo(linea, palabra)) return(FALSE)
  # 3e. Ni dentro de codigo R inline `r variable`: ahi la palabra es un
  # identificador, no prosa. Acentuarlo rompe el ejercicio en silencio
  # (`r fraccion` -> `r fracción` = objeto no encontrado). El chequeo existia
  # pero solo se aplicaba a la lista `palabras_excluir_en_codigo`; desde
  # 2026-08-06 aplica a TODAS las palabras.
  if (esta_en_codigo_inline(linea, palabra)) return(FALSE)
  # 5. No corregir en metadatos YAML genéricos (excepto en valores de texto)
  if (grepl("^\\s*\\w+:", linea) && !grepl(':\\s*["\']', linea)) {
    if (grepl(paste0(":\\s*", palabra, "\\s*$"), linea)) return(FALSE)
  }

  # 6. Sí corregir: comentarios, strings, y secciones Markdown
  if (es_comentario(linea)) return(TRUE)
  if (esta_en_string(linea, palabra)) return(TRUE)
  if (en_seccion_markdown(contenido, num_linea)) return(TRUE)
  if (grepl("^\\s*\\*\\s+", linea)) return(TRUE)
  return(FALSE)
}

# Función para detectar y corregir
corregir_archivo <- function(archivo, aplicar_fix = FALSE) {
  if (!file.exists(archivo)) {
    stop(paste("Archivo no encontrado:", archivo))
  }

  # Leer contenido
  contenido <- readLines(archivo, encoding = "UTF-8", warn = FALSE)
  contenido_original <- contenido

  errores_encontrados <- list()
  casos_ambiguos <- list()

  # Buscar cada palabra del diccionario
  for (i in seq_along(diccionario_tildes)) {
    palabra_mal <- names(diccionario_tildes)[i]
    palabra_bien <- diccionario_tildes[i]

    # Crear patrón que busque la palabra completa.
    # Negative lookaheads/lookbehinds para evitar matches dentro de palabras
    # con sufijos/prefijos acentuados ("seleccion" dentro de "Seleccionó" o
    # "linea" dentro de "alinear"). Cubre: á é í ó ú ñ + sus mayúsculas.
    patron <- paste0(
      "(?<![áéíóúñÁÉÍÓÚÑ])",
      "\\b", palabra_mal, "\\b",
      "(?![áéíóúñÁÉÍÓÚÑ])"
    )

    for (num_linea in seq_along(contenido)) {
      linea <- contenido[num_linea]

      # Buscar coincidencias (case insensitive para la búsqueda)
      if (grepl(patron, linea, ignore.case = TRUE, perl = TRUE)) {

        # FILTROS: No corregir en ciertos contextos (ver
        # ocurrencia_es_texto_visible; los mismos que aplica el pase ambiguo)
        if (!ocurrencia_es_texto_visible(contenido, num_linea, palabra_mal)) {
          next
        }

        # Guardar error encontrado
        errores_encontrados[[length(errores_encontrados) + 1]] <- list(
          linea = num_linea,
          texto = linea,
          mal = palabra_mal,
          bien = palabra_bien
        )

        # La corrección se aplica SIEMPRE sobre la copia en memoria (solo se
        # escribe a disco si --fix). Así los pases posteriores ven el texto ya
        # corregido y no vuelven a reportar la misma ocurrencia — p.ej.
        # "reflexion" la caza el diccionario léxico y la cazaría otra vez la
        # regla morfológica -xion.
        {
          # Reemplazar preservando mayúsculas/minúsculas del original
          contenido[num_linea] <- gsub(
            paste0("\\b", palabra_mal, "\\b"),
            palabra_bien,
            contenido[num_linea],
            perl = TRUE
          )
          # También manejar versión con primera mayúscula
          palabra_mal_cap <- paste0(toupper(substr(palabra_mal, 1, 1)),
                                    substr(palabra_mal, 2, nchar(palabra_mal)))
          palabra_bien_cap <- paste0(toupper(substr(palabra_bien, 1, 1)),
                                     substr(palabra_bien, 2, nchar(palabra_bien)))
          contenido[num_linea] <- gsub(
            paste0("\\b", palabra_mal_cap, "\\b"),
            palabra_bien_cap,
            contenido[num_linea],
            perl = TRUE
          )
          # Y la versión en MAYÚSCULAS (bug corregido 2026-08-06): la detección
          # es case-insensitive pero la sustitución solo cubría minúscula y
          # Capitalizada, así que "OPCION" se reportaba en cada pasada y no se
          # corregía nunca — el hook pre-commit rechazaba el archivo para
          # siempre. En español la mayúscula CONSERVA la tilde (RAE).
          contenido[num_linea] <- gsub(
            paste0("\\b", toupper(palabra_mal), "\\b"),
            toupper(palabra_bien),
            contenido[num_linea],
            perl = TRUE
          )
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # PASE 2: reglas contextuales auto-corregibles (regex, no palabra suelta).
  # ---------------------------------------------------------------------------
  for (regla in reglas_contextuales) {
    for (num_linea in seq_along(contenido)) {
      linea <- contenido[num_linea]
      if (!grepl(regla$patron, linea, perl = TRUE)) next
      # Ancla de contexto: la primera palabra del TEXTO REALMENTE EMPAREJADO
      # (no del patrón, que puede llevar clases y grupos) sirve de sonda para
      # los mismos filtros (string / markdown / comentario) que usa el pase 1.
      encontrado <- regmatches(linea, regexpr(regla$patron, linea, perl = TRUE))
      ancla <- sub("^[^[:alnum:]áéíóúñÁÉÍÓÚÑ]*([[:alnum:]áéíóúñÁÉÍÓÚÑ]+).*$", "\\1", encontrado)
      if (!nzchar(ancla)) next
      if (!ocurrencia_es_texto_visible(contenido, num_linea, ancla)) next

      errores_encontrados[[length(errores_encontrados) + 1]] <- list(
        linea = num_linea, texto = linea,
        mal = regla$nombre, bien = paste0(regla$reemplazo, "  [", regla$motivo, "]")
      )
      contenido[num_linea] <- gsub(regla$patron, regla$reemplazo,
                                   contenido[num_linea], perl = TRUE)
    }
  }

  # ---------------------------------------------------------------------------
  # PASE 3: casos AMBIGUOS. Se reportan, nunca se auto-corrigen (ver cabecera).
  # ---------------------------------------------------------------------------
  for (palabra_amb in names(diccionario_ambiguo)) {
    info <- diccionario_ambiguo[[palabra_amb]]
    patron <- paste0("(?<![áéíóúñÁÉÍÓÚÑ])\\b", palabra_amb, "\\b(?![áéíóúñÁÉÍÓÚÑ])")
    for (num_linea in seq_along(contenido)) {
      if (!grepl(patron, contenido[num_linea], ignore.case = TRUE, perl = TRUE)) next
      if (!ocurrencia_es_texto_visible(contenido, num_linea, palabra_amb)) next
      casos_ambiguos[[length(casos_ambiguos) + 1]] <- list(
        linea = num_linea, texto = contenido[num_linea],
        mal = palabra_amb, sug = info$sug, motivo = info$motivo
      )
    }
  }
  for (regla in reglas_contextuales_ambiguas) {
    for (num_linea in seq_along(contenido)) {
      if (!grepl(regla$patron, contenido[num_linea], perl = TRUE)) next
      casos_ambiguos[[length(casos_ambiguos) + 1]] <- list(
        linea = num_linea, texto = contenido[num_linea],
        mal = regla$nombre, sug = "(revisar)", motivo = regla$motivo
      )
    }
  }

  # Reportar casos ambiguos ANTES del veredicto. Deliberadamente sin la palabra
  # que grepea el hook pre-commit: un caso ambiguo pide juicio humano, no
  # bloquea el commit. Lo que SÍ hace es impedir que el archivo se declare
  # limpio en silencio (ese era el punto ciego de 2026-08-06).
  if (length(casos_ambiguos) > 0) {
    cat("\n----------------------------------------\n")
    cat("REVISION_MANUAL:", length(casos_ambiguos), "caso(s) ambiguo(s) — --fix NO los toca\n")
    cat("Archivo:", archivo, "\n")
    cat("----------------------------------------\n\n")
    for (caso in casos_ambiguos) {
      cat(sprintf("Línea %d: '%s' → ¿'%s'?\n", caso$linea, caso$mal, caso$sug))
      cat(sprintf("  Motivo: %s\n", caso$motivo))
      cat(sprintf("  Contexto: %s\n\n", substr(caso$texto, 1, min(80, nchar(caso$texto)))))
    }
  }

  # Reportar resultados
  if (length(errores_encontrados) > 0) {
    cat("\n========================================\n")
    cat("ERRORES ORTOGRÁFICOS ENCONTRADOS:", length(errores_encontrados), "\n")
    cat("Archivo:", archivo, "\n")
    cat("========================================\n\n")

    for (error in errores_encontrados) {
      cat(sprintf("Línea %d: '%s' → '%s'\n",
                  error$linea, error$mal, error$bien))
      cat(sprintf("  Contexto: %s\n\n",
                  substr(error$texto, 1, min(80, nchar(error$texto)))))
    }

    if (aplicar_fix) {
      # Guardar archivo corregido
      writeLines(contenido, archivo, useBytes = TRUE)
      cat("\n✓ CORRECCIONES APLICADAS Y GUARDADAS\n")
      cat("  Total de correcciones:", length(errores_encontrados), "\n")
    } else {
      cat("\n⚠ Para aplicar correcciones, ejecute con --fix:\n")
      cat(sprintf("  Rscript corregir_ortografia_espanol.R %s --fix\n", archivo))
    }

    return(invisible(FALSE))
  } else if (length(casos_ambiguos) > 0) {
    cat("\n⚠ Sin correcciones automáticas pendientes, pero quedan",
        length(casos_ambiguos), "caso(s) ambiguo(s) por revisar a mano en:", archivo, "\n")
    return(invisible(TRUE))
  } else {
    cat("\n✓ No se encontraron errores ortográficos en:", archivo, "\n")
    return(invisible(TRUE))
  }
}

# Función para procesar múltiples archivos
corregir_directorio <- function(directorio, patron = "\\.Rmd$", aplicar_fix = FALSE) {
  archivos <- list.files(directorio, pattern = patron,
                         recursive = TRUE, full.names = TRUE)

  if (length(archivos) == 0) {
    cat("No se encontraron archivos .Rmd en:", directorio, "\n")
    return(invisible(NULL))
  }

  cat("Procesando", length(archivos), "archivos...\n\n")

  resultados <- sapply(archivos, function(f) {
    corregir_archivo(f, aplicar_fix)
  })

  # Resumen
  cat("\n========================================\n")
  cat("RESUMEN\n")
  cat("========================================\n")
  cat("Archivos procesados:", length(archivos), "\n")
  cat("Archivos con errores:", sum(!resultados), "\n")
  cat("Archivos correctos:", sum(resultados), "\n")
}

# Ejecución desde línea de comandos
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    cat("Uso: Rscript corregir_ortografia_espanol.R <archivo.Rmd> [--fix]\n")
    cat("     Rscript corregir_ortografia_espanol.R <directorio> [--fix]\n")
    cat("\nOpciones:\n")
    cat("  --fix    Aplica las correcciones (sin esto solo reporta)\n")
    quit(status = 1)
  }

  objetivo <- args[1]
  aplicar_fix <- "--fix" %in% args

  if (dir.exists(objetivo)) {
    corregir_directorio(objetivo, aplicar_fix = aplicar_fix)
  } else if (file.exists(objetivo)) {
    corregir_archivo(objetivo, aplicar_fix = aplicar_fix)
  } else {
    cat("Error: No se encontró el archivo o directorio:", objetivo, "\n")
    quit(status = 1)
  }
}
