# Informe: Sistema de Ontología ICFES Matemáticas

**Fecha**: 2026-03-19
**Repositorio**: `proyecto-r-exams-icfes-matematicas-optimizado`
**Destino de este informe**: Manjaro Plasma — Claude Code CLI lo leerá e implementará el sistema allí.

---

## Resumen Ejecutivo

Se implementó un sistema completo de ontología OWL 2 / RDF para el repositorio de ejercicios ICFES de matemáticas. El sistema permite:

1. Almacenar metadatos pedagógicos de 194 ejercicios .Rmd en un triplestore (Apache Jena Fuseki)
2. Consultar cobertura por competencia ICFES y nivel DOK vía SPARQL
3. Visualizar el grafo de conceptos matemáticos con cobertura en colores
4. Registrar ejercicios automáticamente vía git hooks

**Resultados verificados en Cinnamon**: 173/194 ejercicios cargados, SPARQL funcionando.

---

## Arquitectura

```
A-Produccion/03-En-Produccion/**/*.Rmd
        │
        ▼ (git commit en Cinnamon)
.git/hooks/post-commit
        │  registrar_ejercicio.R (HTTP POST → Fuseki)
        ▼
Apache Jena Fuseki 4.10.0
        │  http://localhost:3030/icfes
        │  Named graph: http://icfes.matematicas.edu.co/ejercicios
        ▼
SPARQL queries (consultar_cobertura.R)
        ▼
Visualización interactiva (visualizar_grafo.R)
```

**Sync entre máquinas**: GitHub únicamente. No hay LAN directa entre Cinnamon y Plasma.

- **Cinnamon** (aula): genera ejercicios, hace commit, registra en su Fuseki local vía `post-commit`
- **Plasma** (casa): hace `git pull`, repuebla su Fuseki local vía `post-checkout`

---

## Estructura de Archivos

```
ontologia/
├── matematicas-icfes.ttl     # Esquema OWL (commiteado)
├── conceptos.ttl             # 46 conceptos matemáticos (commiteado)
├── ejercicios.ttl            # GENERADO — en .gitignore, NO committear
└── .pending_registration     # Cola pendiente — en .gitignore

core/
├── poblar_ontologia.R        # Carga masiva de todos los .Rmd → Fuseki
├── registrar_ejercicio.R     # Registra un único .Rmd → Fuseki (post-commit)
├── consultar_cobertura.R     # Consultas SPARQL de cobertura
└── visualizar_grafo.R        # Grafo visNetwork interactivo

.git/hooks/
├── post-commit               # Cinnamon: registra ejercicio nuevo/modificado
└── post-checkout             # Plasma: repuebla Fuseki tras git pull

~/.config/systemd/user/
└── fuseki.service            # Servicio systemd de Fuseki (por usuario)

docs/ontologia/
└── ontologia-hooks-setup.md  # Instrucciones de setup de hooks
```

---

## Configuración de Fuseki (CRÍTICA)

### Ruta del binario
```
~/fuseki/fuseki-server
```

### Flag obligatorio: `--update`

Sin este flag, Fuseki arranca en modo solo lectura y el Graph Store Protocol (PUT/POST) falla con HTTP 405.

### Archivo: `~/.config/systemd/user/fuseki.service`

```ini
[Unit]
Description=Apache Jena Fuseki (ICFES ontologia)
After=network.target

[Service]
Type=simple
Environment="JAVA_HOME=/usr/lib/jvm/default"
ExecStart=%h/fuseki/fuseki-server --update --port=3030 --loc=%h/fuseki/data /icfes
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

**NOTA para Plasma**: verificar que `JAVA_HOME` apunte al JDK correcto. En Plasma puede ser diferente. Ejecutar `update-alternatives --list java` o `ls /usr/lib/jvm/` para confirmar.

### Activar el servicio

```bash
systemctl --user daemon-reload
systemctl --user enable fuseki.service
systemctl --user start fuseki.service

# Verificar
curl http://localhost:3030/$/ping
# Debe responder: {"when":"...", "uptime":...}
```

### Estructura de datos Fuseki

Fuseki guarda los datos en `~/fuseki/data/`. Al transferir el repositorio a Plasma, esta carpeta NO se transfiere (es local). Plasma debe repoblar su propio Fuseki.

---

## Ontología OWL: `ontologia/matematicas-icfes.ttl`

### Clases principales

| Clase | IRI | Descripción |
|-------|-----|-------------|
| `ConceptoMatematico` | `:ConceptoMatematico` | Nodo del grafo de conceptos |
| `Ejercicio` | `:Ejercicio` | Un .Rmd individual |
| `CompetenciaICFES` | `:CompetenciaICFES` | Clasificación ICFES |
| `ComponenteICFES` | `:ComponenteICFES` | Componente curricular |
| `NivelDOK` | `:NivelDOK` | Depth of Knowledge 1-4 |
| `ErrorConceptual` | `:ErrorConceptual` | Tipo de error (para distractores) |

**IRI base**: `http://icfes.matematicas.edu.co/ontologia#`

### Instancias fijas

**Competencias ICFES**:
- `:InterpretacionRepresentacion`
- `:FormulacionEjecucion`
- `:Argumentacion`
- `:ResolucionProblemas`

**Componentes ICFES**:
- `:NumericoVariacional`
- `:EspacialMetrico`
- `:AleatorioEstadistico`

**Niveles DOK**:
- `:DOK1` (básico), `:DOK2` (medio), `:DOK3` (alto), `:DOK4` (avanzado)

### Propiedades de objeto

| Propiedad | Dominio → Rango |
|-----------|-----------------|
| `tieneCompetencia` | Ejercicio → CompetenciaICFES |
| `tieneComponente` | Ejercicio → ComponenteICFES |
| `tieneNivel` | Ejercicio → NivelDOK |
| `cubreConcepto` | Ejercicio → ConceptoMatematico |
| `requiereConcepto` | ConceptoMatematico → ConceptoMatematico |
| `esSubconceptoDe` | ConceptoMatematico → ConceptoMatematico |
| `tieneError` | Ejercicio → ErrorConceptual |

### Propiedades de dato

| Propiedad | Tipo |
|-----------|------|
| `rutaArchivo` | `xsd:string` |
| `nivelBloom` | `xsd:string` |
| `nivelSOLO` | `xsd:string` |

---

## Conceptos Matemáticos: `ontologia/conceptos.ttl`

46 conceptos en 4 áreas:

**Álgebra** (`:Algebra`):
Función, FuncionLineal, FuncionCuadratica, FuncionExponencial, Ecuacion, Inecuacion, SistemaEcuaciones, Variable, Expresion, Polinomio, Factorizacion, Radicales

**Geometría** (`:Geometria`):
Angulo, Triangulo, Cuadrilatero, Circulo, Area, Perimetro, Volumen, TransformacionGeometrica, Semejanza, Congruencia, TeoremasPitagoras, CoordenadasCartesianas

**Estadística y Probabilidad** (`:AleatorioEstadistico` — en la ontología como área):
Media, Mediana, Moda, Rango, Varianza, DesviacionEstandar, Probabilidad, ProbabilidadCondicional, Combinatoria, Permutaciones, Combinaciones, DiagramaArbol, TablaDatos, Histograma, DiagramaBarras, DiagramaCircular, DiagramaCaja, Cuartil

**Numérico** (`:AreaNumerica`):
NumeroReal, NumeroRacional, Fraccion, Porcentaje, Proporcion, Potencia, Logaritmo, ProgressionAritmetica, ProgressionGeometrica

### Estructura owl:imports

`conceptos.ttl` importa `matematicas-icfes.ttl`:

```turtle
<http://icfes.matematicas.edu.co/conceptos>
  a owl:Ontology ;
  owl:imports <http://icfes.matematicas.edu.co/ontologia> .
```

---

## Formato Meta-information en .Rmd (CRÍTICO)

Los archivos .Rmd de R-exams NO usan YAML front matter para los metadatos pedagógicos. Usan una sección al **final del archivo** con este formato:

```
Meta-information
================
exname: nombre_ejercicio_sin_tildes
extype: schoice
exsolution: 1000
exshuffle: TRUE
exextra[Competencia]: Interpretacion
exextra[Componente]: Aleatorio
exextra[Nivel]: 2
exextra[Bloom]: Comprender
exextra[SOLO]: Multistructural
```

**La función `leer_yaml_rmd()` en `poblar_ontologia.R` maneja esto.**

### Claves relevantes para la ontología

| Campo en .Rmd | Campo alternativo | Mapeo |
|---------------|-------------------|-------|
| `exextra[Competencia]` | `Competencia` | → `tieneCompetencia` |
| `exextra[Componente]` | `Componente` | → `tieneComponente` |
| `exextra[Nivel]` | `Nivel` | → `tieneNivel` (DOK1-4) |
| `exextra[Bloom]` | `Bloom` | → `nivelBloom` |
| `exextra[SOLO]` | `SOLO` | → `nivelSOLO` |

### Normalización de valores

La función `normalizar_competencia()` acepta variaciones:
- "interpretaci", "represent" → `:InterpretacionRepresentacion`
- "formulaci", "ejecuci" → `:FormulacionEjecucion`
- "argumentaci" → `:Argumentacion`
- "resoluci", "problem" → `:ResolucionProblemas`

La función `normalizar_nivel()` acepta:
- "1", "basico", "básico" → `:DOK1`
- "2", "medio" → `:DOK2`
- "3", "alto" → `:DOK3`
- "4", "avanzado" → `:DOK4`

---

## Named Graph

Todos los ejercicios se almacenan en el named graph:

```
http://icfes.matematicas.edu.co/ejercicios
```

**CRÍTICO**: Las consultas SPARQL DEBEN incluir `GRAPH <...> {}`:

```sparql
WHERE {
  GRAPH <http://icfes.matematicas.edu.co/ejercicios> {
    ?ej a :Ejercicio .
    ?ej :tieneCompetencia ?competencia .
  }
}
```

Sin esta cláusula, las consultas devuelven 0 resultados aunque los datos estén cargados.

---

## Carga Inicial (Plasma: primera vez)

### Paso 1: Verificar Fuseki corriendo

```bash
curl http://localhost:3030/$/ping
```

### Paso 2: Cargar esquema OWL

```bash
curl -X PUT \
  "http://localhost:3030/icfes/data?graph=http://icfes.matematicas.edu.co/ontologia" \
  -H "Content-Type: text/turtle" \
  --data-binary @ontologia/matematicas-icfes.ttl
# Esperado: HTTP 200 o 201
```

### Paso 3: Cargar conceptos

```bash
curl -X PUT \
  "http://localhost:3030/icfes/data?graph=http://icfes.matematicas.edu.co/ontologia" \
  -H "Content-Type: text/turtle" \
  --data-binary @ontologia/conceptos.ttl
```

### Paso 4: Poblar ejercicios desde R

```r
source("core/poblar_ontologia.R")
poblar_ontologia()
# Procesa todos los .Rmd en A-Produccion/03-En-Produccion/
# Genera ontologia/ejercicios.ttl (local, no committear)
# Carga en Fuseki via HTTP PUT
```

### Paso 5: Verificar

```r
source("core/consultar_cobertura.R")
cobertura_por_competencia()
# Debe mostrar tabla con ejercicios por competencia
```

---

## Git Hooks

### post-commit (solo Cinnamon — ya configurado)

Registra automáticamente ejercicios nuevos/modificados en `A-Produccion/03-En-Produccion/`.

### post-checkout (Plasma — INSTALAR)

Repuebla Fuseki automáticamente después de `git pull`.

**Instalar en Plasma**:

```bash
# Desde la raíz del repositorio
cp .git/hooks/post-checkout.sample .git/hooks/post-checkout 2>/dev/null || true
cat > .git/hooks/post-checkout << 'EOF'
#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
PREV_HEAD="$1"
NEW_HEAD="$2"
BRANCH_CHECKOUT="$3"

# Solo ejecutar en branch checkout (no file checkout)
[ "$BRANCH_CHECKOUT" = "1" ] || exit 0

# Solo si cambió algo en A-Produccion
changed=$(git diff --name-only "$PREV_HEAD" "$NEW_HEAD" 2>/dev/null | grep "^A-Produccion/03-En-Produccion/.*\.Rmd$" | wc -l)
[ "$changed" -gt 0 ] || exit 0

echo "[post-checkout] Detectados $changed ejercicios nuevos/modificados. Repoblando Fuseki..."
cd "$REPO_ROOT"
Rscript -e "source('core/poblar_ontologia.R'); poblar_ontologia()" 2>&1 | tail -5
EOF
chmod +x .git/hooks/post-checkout
```

**Alternativa simple (repobla siempre en branch checkout)**:

```bash
cat > .git/hooks/post-checkout << 'EOF'
#!/usr/bin/env bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
[ "$3" = "1" ] || exit 0
cd "$REPO_ROOT"
echo "[post-checkout] Repoblando Fuseki..."
Rscript -e "source('core/poblar_ontologia.R'); poblar_ontologia()"
EOF
chmod +x .git/hooks/post-checkout
```

---

## Checklist de Implementación para Plasma

Ejecutar en orden:

```
[ ] 1. git pull (obtener todos los archivos del repo)
[ ] 2. Instalar Fuseki 4.10.0 en ~/fuseki/fuseki-server (si no está)
[ ] 3. Crear ~/fuseki/data/ (mkdir -p ~/fuseki/data)
[ ] 4. Copiar fuseki.service a ~/.config/systemd/user/
[ ] 5. Editar fuseki.service: verificar JAVA_HOME correcto para Plasma
[ ] 6. systemctl --user daemon-reload
[ ] 7. systemctl --user enable fuseki.service
[ ] 8. systemctl --user start fuseki.service
[ ] 9. curl http://localhost:3030/$/ping → debe responder
[ ] 10. Cargar matematicas-icfes.ttl (curl PUT)
[ ] 11. Cargar conceptos.ttl (curl PUT)
[ ] 12. source("core/poblar_ontologia.R"); poblar_ontologia()
[ ] 13. Verificar: cobertura_por_competencia()
[ ] 14. Instalar post-checkout hook (ver sección anterior)
[ ] 15. Instalar paquetes R: httr2, yaml, visNetwork, igraph
```

### Paquetes R requeridos

```r
install.packages(c("httr2", "yaml", "visNetwork", "igraph"))
```

---

## Comandos de Verificación

### Fuseki

```bash
# ¿Está corriendo?
curl http://localhost:3030/$/ping

# ¿Tiene el dataset /icfes?
curl http://localhost:3030/$/datasets/icfes

# ¿Cuántos triples en el named graph?
curl -s -X POST http://localhost:3030/icfes/sparql \
  -H "Accept: application/sparql-results+json" \
  --data-urlencode 'query=SELECT (COUNT(*) AS ?n) WHERE { GRAPH <http://icfes.matematicas.edu.co/ejercicios> { ?s ?p ?o } }' \
  | python3 -m json.tool | grep value
```

### Desde R

```r
source("core/consultar_cobertura.R")

# Ejercicios por competencia
cobertura_por_competencia()

# Ejercicios por nivel DOK
cobertura_por_nivel()

# Tabla cruzada
cobertura_competencia_x_nivel()

# Conceptos sin ningún ejercicio
brechas_conceptos()
```

```r
source("core/visualizar_grafo.R")
# Abre grafo interactivo en el navegador
# Rojo = 0 ejercicios, Amarillo = 1-2, Verde = 3+
```

---

## Bugs Corregidos (histórico)

### 1. `leer_yaml_rmd()` ignoraba Meta-information
**Síntoma**: 0 ejercicios cargados, todos los metadatos NULL.
**Causa**: La función solo buscaba YAML front matter `---`, pero los .Rmd usan `Meta-information\n===` al final.
**Fix**: Reescritura completa — busca primero la sección Meta-information, con fallback a YAML.

### 2. Fuseki HTTP 405 en GSP PUT
**Síntoma**: `fuseki_cargar_turtle()` devolvía FALSE, status 405.
**Causa**: Fuseki arrancaba sin `--update`, modo solo lectura.
**Fix**: Agregar `--update` en `ExecStart` del `.service`.

### 3. SPARQL devolvía 0 resultados
**Síntoma**: `cobertura_por_competencia()` retornaba data.frame vacío aunque había 173 ejercicios cargados.
**Causa**: Las queries buscaban en el grafo por defecto, no en el named graph.
**Fix**: Agregar `GRAPH <EJERCICIOS_GRAPH> { }` en todas las queries.

### 4. IRIs inválidas por `\_` de LaTeX
**Síntoma**: HTTP 400 al intentar cargar ciertos ejercicios.
**Causa**: `exname: probabilidad\_distribucion\_grafico` → el `\_` pasaba al IRI.
**Fix**: `exname_clean <- gsub("\\\\", "", exname)` antes de sanitizar.

### 5. `%||%` definido después de su primer uso
**Síntoma**: Error R "could not find function `%||%`" en ciertos contextos.
**Fix**: Mover la definición de `%||%` antes de `ejercicio_a_turtle()`.

### 6. `post-checkout` hook no ejecutaba la carga
**Síntoma**: `git pull` no actualizaba Fuseki.
**Causa**: `Rscript core/poblar_ontologia.R` solo define funciones, no las llama.
**Fix**: `Rscript -e "source('core/poblar_ontologia.R'); poblar_ontologia()"`.

### 7. ping con path incorrecto
**Síntoma**: `fuseki_ping()` siempre devolvía FALSE aunque Fuseki estaba activo.
**Causa**: `req_url_path("$/ping")` → faltaba `/` inicial.
**Fix**: `req_url_path("/$/ping")`.

---

## Constantes Importantes

```r
FUSEKI_BASE      <- "http://localhost:3030"
GRAPH_URI        <- "http://icfes.matematicas.edu.co/ejercicios"
ONTOLOGIA_IRI    <- "http://icfes.matematicas.edu.co/ontologia#"
SPARQL_ENDPOINT  <- "http://localhost:3030/icfes/sparql"
PENDING_FILE     <- "ontologia/.pending_registration"
```

---

## Skills Claude para el Proyecto

Ubicación: `.claude/skills/skill-ontologia/`

- `skill-consultar-ontologia.md`: Verifica prerequisitos conceptuales antes de generar ejercicio
- `skill-mapa-cobertura.md`: Genera mapa visual de cobertura y detecta brechas

Invocar con `/skill-consultar-ontologia` o `/skill-mapa-cobertura` en Claude Code CLI.

---

## Estado Verificado en Cinnamon (2026-03-19)

```
Ejercicios procesados: 173
Omitidos (sin YAML válido): 21
Total .Rmd: 194

Cobertura por competencia:
  InterpretacionRepresentacion: 15
  Argumentacion: 9
  FormulacionEjecucion: 7
  (sin competencia): ~142

Cobertura por nivel DOK:
  DOK2: 22
  DOK3: 2
```

Los ejercicios sin competencia corresponden a .Rmd que no tienen aún los campos `exextra[Competencia]` en sus metadatos. Son ejercicios válidos pero aún no clasificados.

---

## Referencia de Commits

| Commit | Contenido |
|--------|-----------|
| `040790ea` | `matematicas-icfes.ttl` — esquema OWL base |
| `fb403004` | `conceptos.ttl` — 46 conceptos + owl:imports + áreas |
| `0da9009c` | `poblar_ontologia.R` — versión inicial |
| `8d4fbcd6` | `poblar_ontologia.R` — fix leer_yaml_rmd Meta-information |
| `92e2b28e` | `post-checkout` hook — fix Rscript -e |
| `e1e0970c` | `poblar_ontologia.R` + `consultar_cobertura.R` — fix GRAPH clause |
| `0bd7ee7b` | `registrar_ejercicio.R` + `post-commit` hook |
| `56a58365` | `consultar_cobertura.R` — versión inicial |
| `634ed22b` | `visualizar_grafo.R` — visNetwork interactivo |
| `2197feec` | Skills: `skill-consultar-ontologia.md`, `skill-mapa-cobertura.md` |
| `1dd64ff1` | `docs/ontologia-hooks-setup.md` |
