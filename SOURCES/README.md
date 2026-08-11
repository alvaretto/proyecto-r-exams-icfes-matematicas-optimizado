# SOURCES/ - Archivos Originales del Sistema

## ⚠️ ADVERTENCIA CRÍTICA

**Este directorio contiene los ARCHIVOS ORIGINALES referenciados por múltiples módulos del sistema mediante symlinks.**

### Reglas de Oro

1. **NUNCA eliminar archivos de este directorio** sin verificar dependencias
2. **SIEMPRE editar aquí** cuando quieras modificar contenido compartido
3. **VERIFICAR symlinks** después de cualquier cambio con `.claude/scripts/verificar_symlinks.sh`

---

## 📁 Estructura

```
SOURCES/
├── documentacion_compartida/  # Documentación compartida entre skills
│   └── anatomia-metacognitiva.md
│
├── scripts_validacion/        # Scripts de validación compartidos
│   ├── validar_coherencia_matematica.R
│   ├── corregir_ortografia_espanol.R
│   └── arsenal_validacion_completa.R
│
└── plantillas/                # Plantillas base para ejercicios
    └── rexams-oficiales/      # Plantillas OFICIALES de R/exams (referencia externa)
        ├── README.md          #   jerarquía de autoridad + prohibiciones
        ├── CATALOGO.md        #   índice de 46 ejercicios por área y por técnica
        ├── LICENSE-exams.txt  #   GPL-2 | GPL-3 + atribución
        ├── VERSION.txt        #   procedencia + manifiesto con hashes
        ├── rmd/               #   45 archivos .Rmd
        └── rnw/               #   46 archivos .Rnw
```

### ⚠️ `plantillas/rexams-oficiales/` NO usa symlinks

A diferencia del resto de `SOURCES/`, este subdirectorio **no** es origen de ningún
symlink y **no** debe serlo. Son copias verbatim de upstream (paquete `exams`), su
valor depende de no modificarse, y enlazarlas dentro de `A-Produccion/` rompería el
runner de tests. Ver `plantillas/rexams-oficiales/README.md` §Prohibiciones.

---

## 🔗 Mapa de Dependencias (Symlinks)

### 1. documentacion_compartida/anatomia-metacognitiva.md

**Archivo original**: `SOURCES/documentacion_compartida/anatomia-metacognitiva.md`

**Symlinks que apuntan aquí**:
- `.claude/skills/generar-schoice/references/anatomia-metacognitiva.md`
- `.claude/skills/generar-cloze/references/anatomia-metacognitiva.md`

**Propósito**: Documentación de la estructura metacognitiva de 8 secciones compartida entre ambas skills de generación.

**Bidireccionalidad**: ✅
- Editas desde `generar-schoice/references/` → Modifica SOURCES/
- Editas desde `generar-cloze/references/` → Modifica SOURCES/
- Editas desde `SOURCES/` → Ambos symlinks reflejan cambios

---

### 2. scripts_validacion/validar_coherencia_matematica.R

**Archivo original**: `SOURCES/scripts_validacion/validar_coherencia_matematica.R`

**Symlinks que apuntan aquí**:
- `.claude/scripts/validar_coherencia_matematica.R`
- `.claude/hooks/scripts/validar_coherencia.R`

**Propósito**: Script principal de validación matemática usado por hooks y comandos directos.

**Bidireccionalidad**: ✅

---

### 3. scripts_validacion/corregir_ortografia_espanol.R

**Archivo original**: `SOURCES/scripts_validacion/corregir_ortografia_espanol.R`

**Symlinks que apuntan aquí**:
- `.claude/scripts/corregir_ortografia_espanol.R`

**Propósito**: Script de corrección ortográfica automática para archivos .Rmd.

**Bidireccionalidad**: ✅

---

### 4. scripts_validacion/arsenal_validacion_completa.R

**Archivo original**: `SOURCES/scripts_validacion/arsenal_validacion_completa.R`

**Symlinks que apuntan aquí**:
- `.claude/scripts/arsenal_validacion_completa.R`

**Propósito**: Suite completa de herramientas de validación y diagnóstico.

**Bidireccionalidad**: ✅

---

## 🛠️ Comandos Útiles

### Verificar Integridad de Symlinks

```bash
# Ejecutar script de verificación
.claude/scripts/verificar_symlinks.sh

# Salida esperada
✅ Todos los symlinks están intactos
```

### Encontrar Dependencias de un Archivo

```bash
# ¿Qué symlinks apuntan a este archivo?
find . -type l -ls | grep "anatomia-metacognitiva.md"

# Ejemplo de salida:
# ./.claude/skills/generar-schoice/references/anatomia-metacognitiva.md -> ../../../../SOURCES/...
# ./.claude/skills/generar-cloze/references/anatomia-metacognitiva.md -> ../../../../SOURCES/...
```

### Crear un Nuevo Symlink

```bash
# Desde el directorio destino, crear symlink relativo
cd .claude/skills/nueva-skill/references/
ln -s ../../../../SOURCES/documentacion_compartida/archivo.md archivo.md

# Verificar
.claude/scripts/verificar_symlinks.sh
```

### Eliminar un Symlink (Seguro)

```bash
# Eliminar SOLO el symlink (NO el archivo original)
rm .claude/skills/generar-cloze/references/anatomia-metacognitiva.md

# El archivo original permanece intacto
ls -la SOURCES/documentacion_compartida/anatomia-metacognitiva.md
# -rw-r--r-- ... anatomia-metacognitiva.md ← Intacto
```

---

## 🚨 Qué Pasa Si...

### ¿Eliminas el archivo ORIGINAL?

```bash
# ❌ PELIGRO
rm SOURCES/documentacion_compartida/anatomia-metacognitiva.md

# Resultado:
# - TODOS los symlinks que apuntaban aquí se ROMPEN
# - Verificación fallará
# - Skills no podrán leer el archivo

# Solución:
git restore SOURCES/documentacion_compartida/anatomia-metacognitiva.md
```

### ¿Eliminas un SYMLINK?

```bash
# ✅ SEGURO
rm .claude/skills/generar-cloze/references/anatomia-metacognitiva.md

# Resultado:
# - Archivo original INTACTO
# - Solo pierdes el "atajo"
# - Puedes recrearlo en cualquier momento

# Recrear:
cd .claude/skills/generar-cloze/references/
ln -s ../../../../SOURCES/documentacion_compartida/anatomia-metacognitiva.md anatomia-metacognitiva.md
```

### ¿Editas desde un SYMLINK?

```bash
# ✅ BIDIRECCIONAL - Funciona perfectamente
echo "Cambio desde symlink" >> .claude/skills/generar-schoice/references/anatomia-metacognitiva.md

# Resultado:
# - El archivo en SOURCES/ se modifica
# - TODOS los symlinks reflejan el cambio
# - Git registra cambio en SOURCES/
```

---

## 📊 Ventajas de Esta Arquitectura

1. **DRY (Don't Repeat Yourself)**: Un solo archivo, múltiples referencias
2. **Sincronización automática**: Cambios se propagan inmediatamente
3. **Git-friendly**: Git trackea symlinks correctamente
4. **Bidireccional**: Edita desde cualquier ubicación
5. **Mantenible**: Cambios centralizados en SOURCES/
6. **Verificable**: Script automático detecta problemas

---

## 📝 Convenciones

### Nombres de Symlinks

- **MISMO nombre** que el original para claridad
- Excepción: `.claude/hooks/scripts/validar_coherencia.R` (nombre más corto)

### Rutas Relativas

- **SIEMPRE** usar rutas relativas (no absolutas)
- Formato: `../../../../SOURCES/subdirectorio/archivo.ext`
- ✅ Portables entre sistemas
- ✅ Funcionan en cualquier máquina

### Verificación Periódica

```bash
# Agregar al workflow diario
git pull && .claude/scripts/verificar_symlinks.sh
```

---

## 🔄 Workflow de Edición

### Opción 1: Editar desde SOURCES/ (Recomendado)

```bash
# Editar directamente el original
code SOURCES/documentacion_compartida/anatomia-metacognitiva.md

# Commit
git add SOURCES/
git commit -m "docs(sources): Actualizar anatomía metacognitiva"
```

### Opción 2: Editar desde Symlink (Funciona igual)

```bash
# Editar desde skill
code .claude/skills/generar-schoice/references/anatomia-metacognitiva.md

# Git detecta cambio en el ORIGINAL
git status
# modified: SOURCES/documentacion_compartida/anatomia-metacognitiva.md

# Commit
git add SOURCES/
git commit -m "docs(sources): Actualizar anatomía metacognitiva"
```

Ambas opciones son equivalentes por la bidireccionalidad de symlinks.

---

## 🎯 Expansión Futura

### Candidatos para SOURCES/

1. **Plantillas base validadas**:
   - `plantillas/schoice_metacognitivo_base.Rmd`
   - `plantillas/cloze_metacognitivo_base.Rmd`

2. **Documentación compartida adicional**:
   - `documentacion_compartida/pool-errores-conceptuales.md`
   - `documentacion_compartida/patrones-icfes.md`

3. **Estilos y configuraciones**:
   - `estilos/tikz_base.tex`
   - `estilos/ggplot2_theme.R`

---

**Versión**: 1.0
**Fecha**: 2026-02-07
**Autor**: Sistema automatizado
**Arquitectura**: Symlinks bidireccionales con verificación automática
