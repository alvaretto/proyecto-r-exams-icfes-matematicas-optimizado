# 🚀 INICIO RÁPIDO: Agentes y Workflows ICFES R-Exams

## 📋 ÍNDICE DE RECURSOS

### 📚 Documentación Principal
- **[01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md](01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md)** - Guía completa y detallada

### 🤖 Agentes Disponibles
- **[agentes/03-generador-ejercicios.agent.md](agentes/03-generador-ejercicios.agent.md)** - Agente generador de ejercicios
- **[agentes/04-validador-codigo.agent.md](agentes/04-validador-codigo.agent.md)** - Agente validador de código
- **[agentes/05-graficador-tikz.agent.md](agentes/05-graficador-tikz.agent.md)** - Agente graficador TikZ
- **[agentes/06-gestor-metadatos.agent.md](agentes/06-gestor-metadatos.agent.md)** - Agente gestor de metadatos ICFES

### 🔄 Workflows Automatizados
- **[workflows/07-workflow-generacion-completa.md](workflows/07-workflow-generacion-completa.md)** - Generación completa de ejercicio
- **[workflows/08-workflow-correccion-optimizacion.md](workflows/08-workflow-correccion-optimizacion.md)** - Corrección y optimización
- **[workflows/09-workflow-validacion-masiva.md](workflows/09-workflow-validacion-masiva.md)** - Validación masiva

### 🛠️ Scripts Útiles
- **[scripts/10-validar-ejercicio.sh](scripts/10-validar-ejercicio.sh)** - Script de validación automática
- **[scripts/11-compilar-ejercicio.R](scripts/11-compilar-ejercicio.R)** - Script de compilación
- **[scripts/12-log-actividad.sh](scripts/12-log-actividad.sh)** - Sistema de logging

### ⚙️ Configuraciones
- **[configuraciones/13-config-global.json](configuraciones/13-config-global.json)** - Configuración global del sistema

---

## ⚡ INICIO EN 5 MINUTOS

### Paso 1: Crear estructura (30 segundos)
```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
mkdir -p Auxiliares/Agentes-IA/{agentes,workflows,configuraciones,scripts,logs}
```

### Paso 2: Copiar configuración (30 segundos)
```bash
# Ya está creada en configuraciones/13-config-global.json
# Revisar y ajustar rutas si es necesario
```

### Paso 3: Probar primer agente (2 minutos)
```bash
# En Augment/VSCode, usar:
# @generador-ejercicios Genera un ejercicio simple de suma, nivel 1
```

### Paso 4: Validar resultado (1 minuto)
```bash
# Ejecutar script de validación
./Auxiliares/Agentes-IA/scripts/10-validar-ejercicio.sh [archivo-generado].Rmd
```

### Paso 5: Compilar y verificar (1 minuto)
```r
# En R/RStudio
library(exams)
exams2html("[archivo-generado].Rmd")
```

---

## 🎯 CASOS DE USO COMUNES

### Caso 1: Generar ejercicio desde imagen
```
@generador-ejercicios Analiza esta imagen [adjuntar] y genera un ejercicio 
de estadística, competencia interpretacion_representacion, nivel 2
```

### Caso 2: Validar y corregir ejercicio existente
```
@validador-codigo Valida y corrige este archivo: 
Lab-Manjaro/01-S1-2024B/ejercicio.Rmd
```

### Caso 3: Generar gráfico TikZ desde imagen
```
@graficador-tikz Replica esta gráfica [adjuntar] en código TikZ 
con 98% de fidelidad visual
```

### Caso 4: Clasificar ejercicio con metadatos ICFES
```
@gestor-metadatos Analiza este ejercicio y asigna metadatos ICFES completos
```

---

## 📊 ESTRUCTURA DEL SISTEMA

```
Auxiliares/Agentes-IA/
├── 01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md  # Guía principal
├── 02-README_Inicio_Rapido.md                           # Este archivo
├── agentes/                                             # Definiciones de agentes
│   ├── 03-generador-ejercicios.agent.md
│   ├── 04-validador-codigo.agent.md
│   ├── 05-graficador-tikz.agent.md
│   └── 06-gestor-metadatos.agent.md
├── workflows/                                           # Workflows automatizados
│   ├── 07-workflow-generacion-completa.md
│   ├── 08-workflow-correccion-optimizacion.md
│   └── 09-workflow-validacion-masiva.md
├── scripts/                                             # Scripts ejecutables
│   ├── 10-validar-ejercicio.sh
│   ├── 11-compilar-ejercicio.R
│   └── 12-log-actividad.sh
├── configuraciones/                                     # Archivos de configuración
│   └── 13-config-global.json
└── logs/                                                # Logs de actividad
    └── actividad-YYYY-MM-DD.log
```

---

## 🔑 COMANDOS CLAVE

### Validación
```bash
# Validar un ejercicio
./Auxiliares/Agentes-IA/scripts/10-validar-ejercicio.sh archivo.Rmd

# Validar directorio completo
for f in Lab-Manjaro/01-S1-2024B/*.Rmd; do
    ./Auxiliares/Agentes-IA/scripts/10-validar-ejercicio.sh "$f"
done
```

### Compilación
```r
# Compilar a HTML
source("Auxiliares/Agentes-IA/scripts/11-compilar-ejercicio.R")
compilar_ejercicio("archivo.Rmd", "html")

# Compilar a PDF
compilar_ejercicio("archivo.Rmd", "pdf")

# Compilar a Moodle
compilar_ejercicio("archivo.Rmd", "moodle")
```

### Logging
```bash
# Registrar actividad
source Auxiliares/Agentes-IA/scripts/12-log-actividad.sh
log_actividad "INFO" "generador-ejercicios" "Generación de ejercicio" "ÉXITO"
```

---

## 📖 PRÓXIMOS PASOS

1. **Leer guía completa**: [01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md](01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md)
2. **Probar agentes**: Usar casos de uso comunes
3. **Personalizar**: Adaptar agentes a necesidades específicas
4. **Expandir**: Crear nuevos agentes y workflows
5. **Optimizar**: Mejorar basado en métricas y resultados

---

## 🆘 SOPORTE

- **Guía completa**: Ver archivo 01-GUIA_COMPLETA
- **Ejemplos funcionales**: `/A-Produccion/Ejemplos-Funcionales-Rmd/`
- **Biblioteca de soluciones**: `/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md`
- **Checklist de validación**: `/Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md`

---

**¡Comienza ahora y transforma tu flujo de trabajo!** 🚀

