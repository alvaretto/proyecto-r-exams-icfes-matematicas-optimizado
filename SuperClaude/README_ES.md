# SuperClaude – Framework de Desarrollo para Claude Code

[![Licencia: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Versión](https://img.shields.io/badge/version-2.0.1-blue.svg)](https://github.com/NomenAK/SuperClaude)
[![Issues de GitHub](https://img.shields.io/github/issues/NomenAK/SuperClaude)](https://github.com/NomenAK/SuperClaude/issues)
[![PRs Bienvenidos](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/NomenAK/SuperClaude/blob/master/CONTRIBUTING.md)

**Un framework de configuración que mejora Claude Code con comandos especializados, personas cognitivas y metodologías de desarrollo.**

## 🚀 Actualización Versión 2.0.1

IMPORTANTE: Comienza desde cero eliminando archivos antiguos y directorios en .claude (RULES.md MCP.md PERSONAS.md CLAUDE.md y directorio /commands)

SuperClaude v2 introduce mejoras arquitectónicas enfocadas en mantenibilidad y extensibilidad:

- **⚡ Arquitectura Optimizada**: Sistema de referencias @include para gestión de configuración
- **🎭 Personas como Flags**: 9 personas cognitivas integradas en el sistema de flags (`--persona-architect`, `--persona-security`, etc.)
- **📦 Instalador Mejorado**: install.sh con modo actualización, dry-run, manejo de respaldos y detección de plataforma
- **🔧 Diseño Modular**: Sistema de plantillas para agregar nuevos comandos y características
- **🎯 Experiencia Unificada**: Comportamiento consistente de flags en todos los comandos

Consulta [ROADMAP.md](ROADMAP.md) para ideas de desarrollo futuro y oportunidades de contribución.

## 🎯 Contexto

Claude Code proporciona capacidades poderosas pero puede beneficiarse de:
- **Experiencia especializada** para diferentes dominios técnicos
- **Eficiencia de tokens** para proyectos complejos
- **Enfoques basados en evidencia** para el desarrollo
- **Preservación de contexto** durante sesiones de depuración
- **Pensamiento específico del dominio** para diversas tareas

## ✨ Características de SuperClaude

SuperClaude mejora Claude Code con:
- **19 Comandos Especializados** cubriendo tareas del ciclo de vida de desarrollo
- **9 Personas Cognitivas** para enfoques específicos del dominio
- **Optimización de Tokens** con opciones de compresión
- **Metodología Basada en Evidencia** fomentando la documentación
- **Integración MCP** con Context7, Sequential, Magic, Puppeteer
- **Soporte de Checkpoints Git** para experimentación segura
- **Modo Introspección** para mejora del framework y resolución de problemas

## 🚀 Instalación

### Instalador Mejorado v2.0.1
El instalador proporciona varias opciones:

```bash
git clone https://github.com/NomenAK/SuperClaude.git
cd SuperClaude

# Instalación básica
./install.sh                           # Por defecto: ~/.claude/

# Opciones avanzadas
./install.sh --dir /opt/claude        # Ubicación personalizada
./install.sh --update                 # Actualizar instalación existente
./install.sh --dry-run --verbose      # Vista previa de cambios con detalles
./install.sh --force                  # Omitir confirmaciones (automatización)
./install.sh --log install.log        # Registrar todas las operaciones
```

**Características del Instalador v2.0.1:**
- 🔄 **Modo Actualización**: Preserva personalizaciones mientras actualiza
- 👁️ **Dry Run**: Vista previa de cambios antes de aplicar
- 💾 **Respaldos Inteligentes**: Respaldo automático con marcas de tiempo
- 🧹 **Actualizaciones Limpias**: Elimina archivos obsoletos
- 🖥️ **Detección de Plataforma**: Funciona con Linux, macOS, WSL
- 📊 **Seguimiento de Progreso**: Retroalimentación de instalación

Cero dependencias. Se instala en `~/.claude/` por defecto.

**Nota:** Después de la instalación, todos los archivos de configuración se ubican en `~/.claude/` (tu directorio home), no en el directorio del proyecto.

## 💡 Capacidades Principales

### 🧠 **Personas Cognitivas (¡Ahora como Flags!)**
Cambia entre diferentes enfoques con flags de persona:

```bash
/analyze --code --persona-architect     # Enfoque de pensamiento sistémico
/build --react --persona-frontend       # Desarrollo enfocado en UX
/scan --security --persona-security     # Análisis con prioridad en seguridad
/troubleshoot --prod --persona-analyzer # Enfoque de análisis de causa raíz
```

**Actualización v2.0.1**: Las 9 personas son ahora flags universales, disponibles en cada comando para acceso consistente a enfoques especializados.

### ⚡ **19 Comandos**
Cobertura del ciclo de vida de desarrollo:

**Comandos de Desarrollo**
```bash
/build --react --magic --tdd    # Desarrollo con componentes IA
/dev-setup --ci --monitor       # Configuración de entorno
/test --coverage --e2e --pup    # Estrategias de testing
```

**Análisis y Calidad**
```bash
/review --quality --evidence --persona-qa     # Revisión de código con IA
/analyze --architecture --seq   # Análisis de sistemas
/troubleshoot --prod --five-whys # Resolución de problemas
/improve --performance --iterate # Optimización
/explain --depth expert --visual # Documentación
```

**Operaciones y Seguridad**
```bash
/deploy --env prod --plan       # Planificación de despliegue
/scan --security --owasp --deps # Auditorías de seguridad
/migrate --dry-run --rollback   # Migraciones de base de datos
/cleanup --all --validate       # Tareas de mantenimiento
```

### 🎛️ **Integración MCP**
- **Context7**: Acceso a documentación de librerías
- **Sequential**: Capacidades de razonamiento multi-paso
- **Magic**: Componentes UI generados por IA
- **Puppeteer**: Testing y automatización de navegador

**⚠️ Importante:** SuperClaude no incluye servidores MCP. Necesitas instalarlos por separado en la configuración MCP de Claude Code para usar flags relacionados con MCP (--c7, --seq, --magic, --pup).

### 📊 **Eficiencia de Tokens**
El sistema de plantillas @include de SuperClaude ayuda a gestionar el uso de tokens:
- **Modo UltraComprimido** opción para reducción de tokens
- **Referencias de plantillas** para gestión de configuración
- **Mecanismos de caché** para evitar redundancia
- **Opciones de compresión conscientes del contexto**

## 🎮 Flujos de Trabajo de Ejemplo

### Flujo de Arquitectura Empresarial
```bash
/design --api --ddd --bounded-context --persona-architect    # Diseño dirigido por dominio
/estimate --detailed --worst-case --seq                      # Planificación de recursos
/scan --security --validate --persona-security               # Revisión de seguridad
/build --api --tdd --coverage --persona-backend              # Implementación
```

### Resolución de Problemas en Producción
```bash
/troubleshoot --investigate --prod --persona-analyzer        # Análisis
/analyze --profile --perf --seq                             # Revisión de rendimiento
/improve --performance --threshold 95% --persona-performance # Optimización
/test --integration --e2e --pup                             # Validación
```

### Resolución de Problemas y Mejora del Framework
```bash
/troubleshoot --introspect                                  # Depurar comportamiento de SuperClaude
/analyze --introspect --seq                                 # Analizar patrones del framework
/improve --introspect --uc                                  # Optimizar uso de tokens
```

### Desarrollo de Características Full-Stack
```bash
/build --react --magic --watch --persona-frontend           # Desarrollo UI
/test --coverage --e2e --strict --persona-qa                # Aseguramiento de calidad
/scan --validate --deps --persona-security                  # Verificación de seguridad
```

## 🎭 Personas Disponibles

| Persona | Área de Enfoque | Herramientas | Casos de Uso |
|---------|----------------|--------------|--------------|
| **architect** | Diseño de sistemas | Sequential, Context7 | Planificación de arquitectura |
| **frontend** | Experiencia de usuario | Magic, Puppeteer, Context7 | Desarrollo UI |
| **backend** | Sistemas de servidor | Context7, Sequential | Desarrollo API |
| **security** | Análisis de seguridad | Sequential, Context7 | Revisiones de seguridad |
| **analyzer** | Resolución de problemas | Todas las herramientas MCP | Depuración |
| **qa** | Aseguramiento de calidad | Puppeteer, Context7 | Testing |
| **performance** | Optimización | Puppeteer, Sequential | Ajuste de rendimiento |
| **refactorer** | Calidad de código | Sequential, Context7 | Mejora de código |
| **mentor** | Compartir conocimiento | Context7, Sequential | Documentación |

## 🛠️ Opciones de Configuración

### Control de Profundidad de Pensamiento
```bash
# Análisis estándar
/analyze --think

# Análisis más profundo
/design --think-hard

# Profundidad máxima
/troubleshoot --ultrathink
```

### Modo Introspección
```bash
# Habilitar análisis auto-consciente para mejora de SuperClaude
/analyze --introspect

# Depurar comportamiento de SuperClaude
/troubleshoot --introspect --seq

# Optimizar rendimiento del framework
/improve --introspect --persona-performance
```

### Gestión de Tokens
```bash
# Modo estándar
/build --react --magic

# Con compresión
/analyze --architecture --uc

# Solo herramientas nativas
/scan --security --no-mcp
```

### Desarrollo Basado en Evidencia
SuperClaude fomenta:
- Documentación para decisiones de diseño
- Testing para mejoras de calidad
- Métricas para trabajo de rendimiento
- Validación de seguridad para despliegues
- Análisis para decisiones arquitectónicas

## 📋 Categorías de Comandos

### Desarrollo (3 Comandos)
- `/build` - Constructor de proyectos con plantillas de stack
- `/dev-setup` - Configuración de entorno de desarrollo
- `/test` - Framework de testing

### Análisis y Mejora (5 Comandos)
- `/review` - Revisión de código con IA y recomendaciones basadas en evidencia
- `/analyze` - Análisis de código y sistemas
- `/troubleshoot` - Depuración y resolución de problemas
- `/improve` - Mejora y optimización
- `/explain` - Documentación y explicaciones

### Operaciones (6 Comandos)
- `/deploy` - Despliegue de aplicaciones
- `/migrate` - Migraciones de base de datos y código
- `/scan` - Seguridad y validación
- `/estimate` - Estimación de proyectos
- `/cleanup` - Mantenimiento de proyectos
- `/git` - Gestión de flujo de trabajo Git

### Diseño y Flujo de Trabajo (5 Comandos)
- `/design` - Arquitectura de sistemas
- `/spawn` - Ejecución de tareas paralelas
- `/document` - Creación de documentación
- `/load` - Carga de contexto de proyecto
- `/task` - Gestión de tareas

## 🔧 Arquitectura Técnica v2

La arquitectura de SuperClaude v2 habilita extensibilidad:

**🏗️ Configuración Modular**
- **CLAUDE.md** – Configuración principal con referencias @include
- **.claude/shared/** – Plantillas YAML centralizadas
- **commands/shared/** – Patrones de comandos reutilizables
- **Sistema @include** – Motor de plantillas para configuración

**🎯 Sistema de Comandos Unificado**
- **19 Comandos** – Cobertura del ciclo de vida de desarrollo
- **Herencia de Flags** – Flags universales en todos los comandos
- **Integración de Personas** – 9 modos cognitivos como flags
- **Validación de Plantillas** – Verificación de integridad de referencias

**📦 Beneficios de la Arquitectura**
- **Fuente Única de Verdad** – Actualizaciones centralizadas
- **Extensión Fácil** – Agregar nuevos comandos/flags
- **Comportamiento Consistente** – Manejo unificado de flags
- **Duplicación Reducida** – Configuración basada en plantillas

**✅ Características de Calidad**
- **Enfoque Basado en Evidencia** – Documentación fomentada
- **Integración de Investigación** – Acceso a documentación de librerías
- **Recuperación de Errores** – Manejo elegante de fallos
- **Salida Estructurada** – Ubicaciones de archivos organizadas

## 📊 Comparación

| Aspecto | Claude Code Estándar | Framework SuperClaude |
|---------|---------------------|----------------------|
| **Experiencia** | Respuestas generales | 9 personas especializadas |
| **Comandos** | Instrucciones manuales | 19 comandos de flujo de trabajo |
| **Contexto** | Basado en sesión | Soporte de checkpoints Git |
| **Tokens** | Uso estándar | Opciones de compresión |
| **Enfoque** | Propósito general | Basado en evidencia |
| **Documentación** | Según necesidad | Enfoque sistemático |
| **Calidad** | Variable | Patrones de validación |
| **Integración** | Herramientas básicas | Orquestación MCP |

## 🔮 Casos de Uso

**Equipos de Desarrollo**
- Enfoques consistentes entre dominios
- Flujos de trabajo estandarizados
- Decisiones basadas en evidencia
- Prácticas de documentación

**Líderes Técnicos**
- Revisiones de arquitectura
- Optimización de rendimiento
- Mejora de calidad de código
- Compartir conocimiento del equipo

**Operaciones**
- Procedimientos de despliegue
- Flujos de trabajo de depuración
- Gestión de seguridad
- Tareas de mantenimiento

## 🎯 Idoneidad

**Buena opción para:**
- ✅ Equipos que desean asistencia IA consistente
- ✅ Proyectos que necesitan enfoques especializados
- ✅ Prácticas de desarrollo basadas en evidencia
- ✅ Flujos de trabajo conscientes de tokens
- ✅ Necesidades de experiencia específica del dominio

**Puede no ser adecuado para:**
- ❌ Flujos de trabajo puramente manuales
- ❌ Preferencias de configuración mínima
- ❌ Estilos de desarrollo ad-hoc
- ❌ Enfoque de dominio único

## 🚦 Comenzando

1. **Instalar SuperClaude**
   ```bash
   git clone https://github.com/NomenAK/SuperClaude.git && cd SuperClaude && ./install.sh
   ```

2. **Validar Instalación**
   ```bash
   /load                                    # Cargar contexto del proyecto
   /analyze --code --think                  # Probar análisis
   /analyze --architecture --persona-architect  # Probar personas
   ```

3. **Flujo de Trabajo de Ejemplo**
   ```bash
   /design --api --ddd            # Diseño de arquitectura
   /build --feature --tdd         # Implementación
   /test --coverage --e2e         # Aseguramiento de calidad
   /deploy --env staging --plan   # Despliegue
   ```

## 🛟 Soporte

- **Ayuda de Instalación**: Ejecuta `./install.sh --help`
- **Detalles de Comandos**: Revisa `~/.claude/commands/`
- **Contribuir**: Ver [CONTRIBUTING.md](CONTRIBUTING.md)
- **Issues**: [GitHub Issues](https://github.com/NomenAK/SuperClaude/issues)

## 🤝 Comunidad

SuperClaude da la bienvenida a contribuciones:
- **Nuevas Personas** para flujos de trabajo especializados
- **Comandos** para operaciones específicas del dominio
- **Patrones** para prácticas de desarrollo
- **Integraciones** para herramientas de productividad

Únete a la comunidad: [Discussions](https://github.com/NomenAK/SuperClaude/discussions)

## 📈 Cambios de la Versión 2.0.1

**🎯 Mejoras de Arquitectura:**
- **Gestión de Configuración**: Sistema de referencias @include
- **Eficiencia de Tokens**: Opciones de compresión mantenidas
- **Sistema de Comandos**: Herencia unificada de flags
- **Sistema de Personas**: Ahora disponible como flags
- **Instalador**: Mejorado con nuevos modos
- **Mantenimiento**: Configuración centralizada

**📊 Detalles del Framework:**
- **Comandos**: 19 comandos especializados
- **Personas**: 9 enfoques cognitivos
- **Servidores MCP**: 4 integraciones
- **Metodología**: Enfoque basado en evidencia
- **Uso**: Por equipos de desarrollo

## 🎉 Mejora Tu Desarrollo

SuperClaude proporciona un enfoque estructurado para usar Claude Code con comandos especializados, personas y patrones de desarrollo.

---

*SuperClaude v2.0.1 – Framework de desarrollo para Claude Code*

[⭐ Estrella en GitHub](https://github.com/NomenAK/SuperClaude) | [💬 Discusiones](https://github.com/NomenAK/SuperClaude/discussions) | [🐛 Reportar Issues](https://github.com/NomenAK/SuperClaude/issues)
