# 🏗️ Diagrama de Arquitectura Optimizada - Repositorio ICFES R-Exams

## 📊 Comparación: Arquitectura Actual vs Propuesta

### Arquitectura Actual (Problemática)

```mermaid
graph TB
    subgraph RAIZ[Directorio Raíz - 15+ directorios mezclados]
        A1[01-Numeros-Reales]
        A2[02-Funciones]
        A3[05-Geometría ❌ Salto numérico]
        A4[06-Estadística]
        A5[Auxiliares - 35+ subdirectorios]
        A6[core ❓]
        A7[cr-01 ❓]
        A8[docus duplicado]
        A9[ejemplos duplicado]
        A10[Ordenar sin estructura]
        A11[revisor-visual-ai]
        A12[tests]
        A13[Lab-Manjaro]
        A14[otros...]
    end
    
    style A3 fill:#ff6b6b
    style A6 fill:#ffd43b
    style A7 fill:#ffd43b
    style A8 fill:#ff6b6b
    style A9 fill:#ff6b6b
    style A10 fill:#ff6b6b
```

### Arquitectura Propuesta (Optimizada)

```mermaid
graph TB
    subgraph REPO[RepositorioMatematicasICFES_R_Exams]
        subgraph CONT[📚 CONTENIDO - Ejercicios por categoría ICFES]
            C1[01-Numeros-Reales]
            C2[02-Funciones]
            C3[03-Algebra-Calculo]
            C4[04-Geometria]
            C5[05-Estadistica-Probabilidad]
        end
        
        subgraph HERR[🛠️ HERRAMIENTAS - Generación y Validación]
            H1[generacion/]
            H2[validacion/]
            H3[agente-graficador/]
            H4[revisor-visual/]
            H5[instalacion/]
        end
        
        subgraph DOC[📖 DOCUMENTACION - Centralizada sin duplicados]
            D1[guias-usuario/]
            D2[guias-desarrollo/]
            D3[referencias-icfes/]
            D4[ejemplos-funcionales/]
        end
        
        subgraph DEV[🧪 DESARROLLO - Work in progress]
            DV1[lab-experimental/]
            DV2[pruebas-concepto/]
            DV3[templates-dev/]
        end
        
        subgraph CONFIG[🔧 CONFIGURACION - Templates y settings]
            CF1[plantillas-latex/]
            CF2[plantillas-tikz/]
            CF3[.roo .vscode .mcps]
        end
        
        subgraph SAL[📊 SALIDAS - Generadas gitignored]
            S1[pdf/]
            S2[docx/]
            S3[html/]
            S4[moodle/]
            S5[nops/]
        end
        
        subgraph MEM[🧠 MEMORIA-PROYECTO]
            M1[augment-memories/]
            M2[decisiones-arquitectura/]
            M3[changelog/]
        end
    end
    
    style CONT fill:#51cf66
    style HERR fill:#339af0
    style DOC fill:#ff6b6b
    style DEV fill:#ffd43b
    style CONFIG fill:#9775fa
    style SAL fill:#adb5bd
    style MEM fill:#74c0fc
```

## 🔄 Flujo de Trabajo Optimizado

```mermaid
flowchart LR
    subgraph ENTRADA[Entrada]
        IMG[Imagen PNG problema matemático]
    end
    
    subgraph PROCESO[Procesamiento]
        DETECT[Sistema Condicional Automático]
        FLUJO_A[Flujo A: Sin gráficas]
        FLUJO_B[Flujo B: Con gráficas + TikZ]
        AGENT[Agente-Graficador 98%+ fidelidad]
    end
    
    subgraph GENERACION[Generación]
        RMD[Archivo .Rmd completo]
        VALID[Validación automática]
    end
    
    subgraph COMPILACION[Compilación Multi-formato]
        PDF[exams2pdf]
        HTML[exams2html]
        DOCX[exams2pandoc]
        MOODLE[exams2moodle]
        NOPS[exams2nops]
    end
    
    subgraph SALIDA[Salida]
        OUT[Ejercicios en SALIDAS/]
    end
    
    IMG --> DETECT
    DETECT --> FLUJO_A
    DETECT --> FLUJO_B
    FLUJO_B --> AGENT
    AGENT --> RMD
    FLUJO_A --> RMD
    RMD --> VALID
    VALID --> PDF
    VALID --> HTML
    VALID --> DOCX
    VALID --> MOODLE
    VALID --> NOPS
    PDF --> OUT
    HTML --> OUT
    DOCX --> OUT
    MOODLE --> OUT
    NOPS --> OUT
    
    style DETECT fill:#51cf66
    style AGENT fill:#339af0
    style VALID fill:#ffd43b
    style OUT fill:#ff6b6b
```

## 📁 Separación de Responsabilidades

```mermaid
graph LR
    subgraph CONTENIDO_EDUCATIVO[📚 CONTENIDO EDUCATIVO]
        CE1[Ejercicios .Rmd]
        CE2[Metadatos ICFES]
        CE3[Soluciones]
    end
    
    subgraph INFRAESTRUCTURA[🛠️ INFRAESTRUCTURA]
        IF1[Scripts generación]
        IF2[Validadores]
        IF3[Herramientas]
    end
    
    subgraph CONFIGURACION_SYS[🔧 CONFIGURACIÓN]
        CS1[Templates LaTeX]
        CS2[Templates TikZ]
        CS3[Settings proyecto]
    end
    
    subgraph DOCUMENTACION_SYS[📖 DOCUMENTACIÓN]
        DS1[Guías usuario]
        DS2[Guías desarrollo]
        DS3[Referencias ICFES]
    end
    
    CONTENIDO_EDUCATIVO -.->|usa| INFRAESTRUCTURA
    INFRAESTRUCTURA -.->|requiere| CONFIGURACION_SYS
    CONTENIDO_EDUCATIVO -.->|documenta en| DOCUMENTACION_SYS
    INFRAESTRUCTURA -.->|documenta en| DOCUMENTACION_SYS
    
    style CONTENIDO_EDUCATIVO fill:#51cf66
    style INFRAESTRUCTURA fill:#339af0
    style CONFIGURACION_SYS fill:#9775fa
    style DOCUMENTACION_SYS fill:#ff6b6b
```

## 🗂️ Jerarquía de Directorios Detallada

```mermaid
graph TD
    ROOT[RepositorioMatematicasICFES_R_Exams]
    
    ROOT --> CONT[CONTENIDO/]
    CONT --> C1[01-Numeros-Reales/]
    CONT --> C2[02-Funciones/]
    CONT --> C3[03-Algebra-Calculo/]
    CONT --> C4[04-Geometria/]
    CONT --> C5[05-Estadistica-Probabilidad/]
    
    C1 --> C1P[Pensamiento-Numerico/]
    C1P --> C1E[ejercicio1/...ejercicioN/]
    
    ROOT --> HERR[HERRAMIENTAS/]
    HERR --> H1[generacion/]
    HERR --> H2[validacion/]
    HERR --> H3[agente-graficador/]
    HERR --> H4[revisor-visual/]
    HERR --> H5[instalacion/]
    
    ROOT --> DOC[DOCUMENTACION/]
    DOC --> D1[guias-usuario/]
    DOC --> D2[guias-desarrollo/]
    DOC --> D3[referencias-icfes/]
    DOC --> D4[ejemplos-funcionales/]
    
    ROOT --> DEV[DESARROLLO/]
    DEV --> DV1[lab-experimental/]
    DEV --> DV2[pruebas-concepto/]
    DEV --> DV3[templates-dev/]
    
    ROOT --> CONFIG[CONFIGURACION/]
    CONFIG --> CF1[plantillas-latex/]
    CONFIG --> CF2[plantillas-tikz/]
    CONFIG --> CF3[.roo .vscode .mcps/]
    
    ROOT --> SAL[SALIDAS/ gitignored]
    SAL --> S1[pdf/]
    SAL --> S2[docx/]
    SAL --> S3[html/]
    SAL --> S4[moodle/]
    SAL --> S5[nops/]
    
    ROOT --> MEM[MEMORIA-PROYECTO/]
    MEM --> M1[augment-memories/]
    MEM --> M2[decisiones-arquitectura/]
    MEM --> M3[changelog/]
    
    style ROOT fill:#495057
    style CONT fill:#51cf66
    style HERR fill:#339af0
    style DOC fill:#ff6b6b
    style DEV fill:#ffd43b
    style CONFIG fill:#9775fa
    style SAL fill:#adb5bd
    style MEM fill:#74c0fc
```

## 🎯 Ciclo de Vida de un Ejercicio

```mermaid
sequenceDiagram
    participant U as Usuario
    participant A as Architect Mode
    participant C as Code Mode
    participant H as HERRAMIENTAS/
    participant D as DOCUMENTACION/
    participant S as SALIDAS/
    
    U->>A: Proporciona imagen PNG
    A->>A: Analiza estructura y planifica
    A->>D: Consulta ejemplos funcionales
    A->>U: Presenta plan
    U->>C: Aprueba e implementa
    C->>D: Usa plantillas y templates
    C->>H: Ejecuta validadores
    H->>C: Reporta validación OK
    C->>H: Genera con SemilleroUnico_v2.R
    H->>S: Crea archivos PDF/HTML/DOCX/etc
    S->>U: Entrega ejercicios listos
    
    Note over A,C: Modo Architect planifica<br/>Modo Code implementa
    Note over H,S: Herramientas generan<br/>Salidas se gitignore
```

## 📊 Métricas de Mejora

```mermaid
graph LR
    subgraph ANTES[Antes - Arquitectura Actual]
        A1[Numeración: Inconsistente ❌]
        A2[Directorios raíz: 15+ mezclados ❌]
        A3[Búsqueda: Confusa ❌]
        A4[Mantenimiento: Difícil ❌]
        A5[Escalabilidad: Limitada ❌]
    end
    
    subgraph DESPUES[Después - Arquitectura Optimizada]
        D1[Numeración: Secuencial 01-05 ✅]
        D2[Directorios raíz: 7 organizados ✅]
        D3[Búsqueda: Intuitiva ✅]
        D4[Mantenimiento: Fácil ✅]
        D5[Escalabilidad: Excelente ✅]
    end
    
    ANTES -->|Migración<br/>10-17 días| DESPUES
    
    style ANTES fill:#ff6b6b
    style DESPUES fill:#51cf66
```

## 🔧 Integración de Metodologías

```mermaid
graph TB
    subgraph METODOLOGIAS[Metodologías Integradas]
        M1[Sistema Condicional Automático]
        M2[Metodología TikZ Avanzada]
        M3[Corrección Errores Recurrentes]
        M4[Protocolo Anti-Errores]
    end
    
    subgraph FLUJO[Flujo de Implementación]
        F1[Detección Contenido]
        F2[Replicación Gráfica]
        F3[Validación Continua]
        F4[Generación Multi-formato]
    end
    
    M1 --> F1
    M2 --> F2
    M3 --> F3
    M4 --> F3
    F1 --> F2
    F2 --> F3
    F3 --> F4
    
    style M1 fill:#51cf66
    style M2 fill:#339af0
    style M3 fill:#ffd43b
    style M4 fill:#ff6b6b
    style F4 fill:#9775fa
```

## 🚀 Roadmap de Implementación

```mermaid
gantt
    title Plan de Migración a Nueva Arquitectura
    dateFormat  YYYY-MM-DD
    section Fase 1
    Preparación y Backup           :2025-01-11, 2d
    section Fase 2
    Migración Herramientas        :2025-01-13, 3d
    section Fase 3
    Reorganización Documentación  :2025-01-16, 2d
    section Fase 4
    Migración Contenido           :2025-01-18, 5d
    section Fase 5
    Cleanup y Optimización        :2025-01-23, 2d
    section Fase 6
    Validación Final              :2025-01-25, 3d
```

## 🔐 Control de Versiones y Nomenclatura

```mermaid
graph LR
    subgraph EJERCICIO[Estructura de Ejercicio]
        E1[nombre_componente_competencia_nNivel_vVersion.Rmd]
        E2[Metadatos ICFES]
        E3[Chunk generación datos]
        E4[Chunk validación 300+ versiones]
        E5[Question + Solution]
    end
    
    subgraph SALIDAS_EJ[Salidas Generadas]
        S1[PDF docente con claves]
        S2[PDF estudiante sin claves]
        S3[HTML interactivo]
        S4[DOCX editable]
        S5[XML Moodle]
        S6[NOPS escaneable]
    end
    
    E1 --> E2
    E2 --> E3
    E3 --> E4
    E4 --> E5
    E5 --> S1
    E5 --> S2
    E5 --> S3
    E5 --> S4
    E5 --> S5
    E5 --> S6
    
    style E1 fill:#51cf66
    style E5 fill:#339af0
```

---

## 📝 Leyenda de Colores

- 🟢 **Verde**: Contenido educativo y procesos exitosos
- 🔵 **Azul**: Herramientas e infraestructura
- 🟣 **Morado**: Configuración y templates
- 🔴 **Rojo**: Documentación y referencias
- 🟡 **Amarillo**: Desarrollo y validación
- ⚪ **Gris**: Salidas generadas (temporales)
- 🔷 **Azul claro**: Memoria del proyecto

---

## 🔗 Referencias Cruzadas

Este diagrama complementa el documento principal:
- [Plan de Arquitectura Optimizada](plan_arquitectura_optimizada.md)
- [Contexto Global ICFES](../rules_full/rules_full_v1.md)
- [Guía de Implementación](../guia_implementacion_icfes.md)

---

**Versión:** 1.0  
**Fecha:** 2025-01-11  
**Autor:** Roo Architect (Claude Sonnet 4.5)  
**Herramienta:** Mermaid Diagrams