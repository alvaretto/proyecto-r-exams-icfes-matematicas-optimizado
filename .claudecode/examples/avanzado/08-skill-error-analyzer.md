# Skill Avanzado: Analizador de Errores de Renderizado

**Nivel**: Avanzado  
**Tipo**: Agent Skill - Análisis y Parsing de Logs  
**Propósito**: Analizar logs de error de R-exams y extraer información estructurada para corrección

---

## Definición del Skill

```yaml
# .claudecode/skills/error_analyzer.yml
name: "Error Analyzer"
description: "Analiza logs de error de renderizado R-exams y extrae información estructurada"
version: "1.0.0"

inputs:
  error_log_path:
    type: string
    description: "Ruta al archivo de log de errores"
    required: true
  
  rmd_file_path:
    type: string
    description: "Ruta al archivo .Rmd que generó el error"
    required: true

outputs:
  error_summary:
    type: object
    description: "Resumen estructurado de errores encontrados"
    schema:
      error_type: string
      error_message: string
      location: object
        line_number: integer
        chunk_name: string
      suggested_fix: string
      severity: enum[CRITICAL, ERROR, WARNING]
  
  error_details:
    type: array
    description: "Lista detallada de todos los errores"
  
  fix_recommendations:
    type: array
    description: "Recomendaciones de corrección ordenadas por prioridad"

capabilities:
  - parse_r_errors
  - parse_latex_errors
  - parse_tikz_errors
  - identify_error_patterns
  - suggest_fixes
```

---

## Implementación del Skill

```python
#!/usr/bin/env python3
# .claudecode/skills/error_analyzer.py

"""
Skill: Analizador de Errores de Renderizado R-exams
Analiza logs de error y extrae información estructurada para corrección automática
"""

import re
import json
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from enum import Enum

class ErrorSeverity(Enum):
    CRITICAL = "CRITICAL"
    ERROR = "ERROR"
    WARNING = "WARNING"

class ErrorType(Enum):
    YAML_SYNTAX = "YAML_SYNTAX"
    R_SYNTAX = "R_SYNTAX"
    LATEX_ERROR = "LATEX_ERROR"
    TIKZ_ERROR = "TIKZ_ERROR"
    PACKAGE_MISSING = "PACKAGE_MISSING"
    CHARACTER_ESCAPE = "CHARACTER_ESCAPE"
    CHUNK_CONFIG = "CHUNK_CONFIG"
    UNKNOWN = "UNKNOWN"

class ErrorAnalyzer:
    """Analizador de errores de renderizado R-exams"""
    
    def __init__(self):
        # Patrones de error comunes
        self.error_patterns = {
            ErrorType.YAML_SYNTAX: [
                r"YAML.*parse.*error",
                r"line \d+.*column \d+.*mapping",
                r"could not find expected"
            ],
            ErrorType.R_SYNTAX: [
                r"Error.*:.*unexpected",
                r"Error.*:.*object.*not found",
                r"Error in.*:",
            ],
            ErrorType.LATEX_ERROR: [
                r"! LaTeX Error",
                r"! Undefined control sequence",
                r"! Missing.*inserted",
                r"! Package.*Error",
                r"LaTeX.*failed"
            ],
            ErrorType.TIKZ_ERROR: [
                r"Package tikz Error",
                r"! Undefined control sequence.*tikz",
                r"tikzpicture.*error"
            ],
            ErrorType.PACKAGE_MISSING: [
                r"Package.*not found",
                r"! LaTeX Error: File.*not found",
                r"package.*required"
            ],
            ErrorType.CHARACTER_ESCAPE: [
                r"! Missing.*inserted",
                r"! You can't use.*in.*mode",
                r"! Improper.*horizontal mode"
            ]
        }
        
        # Patrones para extraer ubicación
        self.location_patterns = {
            'line': r"line (\d+)",
            'chunk': r"chunk.*?(\w+)",
            'file': r"in (.+\.Rmd)"
        }
    
    def analyze_error_log(self, error_log_path: str, rmd_file_path: str) -> Dict:
        """Analiza log de errores y retorna resumen estructurado"""
        
        error_log = Path(error_log_path)
        if not error_log.exists():
            return {
                "error": "Log file not found",
                "error_log_path": error_log_path
            }
        
        content = error_log.read_text(encoding='utf-8', errors='ignore')
        
        # Identificar tipo de error principal
        error_type, error_message = self._identify_error_type(content)
        
        # Extraer ubicación
        location = self._extract_location(content, rmd_file_path)
        
        # Generar sugerencias de corrección
        suggestions = self._generate_suggestions(error_type, error_message, content)
        
        # Determinar severidad
        severity = self._determine_severity(error_type, error_message)
        
        return {
            "error_type": error_type.value,
            "error_message": error_message,
            "location": location,
            "severity": severity.value,
            "suggested_fix": suggestions,
            "raw_error": content[:1000]  # Primeros 1000 caracteres
        }
    
    def _identify_error_type(self, content: str) -> Tuple[ErrorType, str]:
        """Identifica el tipo de error principal"""
        
        content_lower = content.lower()
        
        for error_type, patterns in self.error_patterns.items():
            for pattern in patterns:
                match = re.search(pattern, content, re.IGNORECASE)
                if match:
                    error_msg = match.group(0)
                    return error_type, error_msg
        
        return ErrorType.UNKNOWN, content[:200]
    
    def _extract_location(self, content: str, rmd_file: str) -> Dict:
        """Extrae información de ubicación del error"""
        
        location = {
            "file": rmd_file,
            "line_number": None,
            "chunk_name": None
        }
        
        # Buscar número de línea
        line_match = re.search(self.location_patterns['line'], content)
        if line_match:
            location["line_number"] = int(line_match.group(1))
        
        # Buscar chunk
        chunk_match = re.search(self.location_patterns['chunk'], content, re.IGNORECASE)
        if chunk_match:
            location["chunk_name"] = chunk_match.group(1)
        
        return location
    
    def _generate_suggestions(self, error_type: ErrorType, error_msg: str, content: str) -> List[str]:
        """Genera sugerencias de corrección basadas en el tipo de error"""
        
        suggestions = []
        
        if error_type == ErrorType.YAML_SYNTAX:
            suggestions.append("Revisar sintaxis YAML en el encabezado del archivo")
            suggestions.append("Verificar indentación y uso correcto de comillas")
            suggestions.append("Consultar guia_estilo_icfes.md sección 1")
        
        elif error_type == ErrorType.LATEX_ERROR:
            suggestions.append("Revisar caracteres especiales sin escape (&, %, $, #, _, {, })")
            suggestions.append("Verificar que todos los paquetes LaTeX estén en header-includes")
            suggestions.append("Consultar guia_estilo_icfes.md sección 'ERRORES COMUNES'")
        
        elif error_type == ErrorType.TIKZ_ERROR:
            suggestions.append("Verificar sintaxis TikZ en código")
            suggestions.append("Asegurar que \\begin{tikzpicture} y \\end{tikzpicture} estén presentes")
            suggestions.append("Verificar uso de include_tikz() con parámetros correctos")
        
        elif error_type == ErrorType.PACKAGE_MISSING:
            # Extraer nombre del paquete faltante
            package_match = re.search(r"Package\s+['\"]?([^'\"]+)['\"]?\s+not found", content, re.IGNORECASE)
            if package_match:
                package_name = package_match.group(1)
                suggestions.append(f"Agregar paquete faltante '{package_name}' a header-includes en YAML")
        
        elif error_type == ErrorType.R_SYNTAX:
            suggestions.append("Revisar sintaxis R en chunks de código")
            suggestions.append("Verificar que todas las variables estén definidas antes de uso")
            suggestions.append("Verificar paréntesis y llaves balanceadas")
        
        else:
            suggestions.append("Revisar log completo para más detalles")
            suggestions.append("Consultar ejemplos funcionales en A-Produccion/Ejemplos-Funcionales-Rmd/")
        
        return suggestions
    
    def _determine_severity(self, error_type: ErrorType, error_msg: str) -> ErrorSeverity:
        """Determina la severidad del error"""
        
        critical_types = [
            ErrorType.YAML_SYNTAX,
            ErrorType.LATEX_ERROR,
            ErrorType.TIKZ_ERROR
        ]
        
        if error_type in critical_types:
            return ErrorSeverity.CRITICAL
        elif error_type == ErrorType.UNKNOWN:
            return ErrorSeverity.ERROR
        else:
            return ErrorSeverity.WARNING


# Punto de entrada principal
if __name__ == "__main__":
    import sys
    
    if len(sys.argv) < 3:
        print("Uso: error_analyzer.py <error_log_path> <rmd_file_path>")
        sys.exit(1)
    
    error_log_path = sys.argv[1]
    rmd_file_path = sys.argv[2]
    
    analyzer = ErrorAnalyzer()
    result = analyzer.analyze_error_log(error_log_path, rmd_file_path)
    
    # Output JSON para consumo por agente
    print(json.dumps(result, indent=2, ensure_ascii=False))

```

---

## Uso del Skill desde Agente

```python
# Ejemplo de uso en agente Claude Code

from claudecode.skills import ErrorAnalyzer

def fix_rmd_errors(rmd_file_path: str, error_log_path: str):
    """Usa ErrorAnalyzer para analizar errores y generar correcciones"""
    
    # Inicializar skill
    analyzer = ErrorAnalyzer()
    
    # Analizar errores
    analysis = analyzer.analyze_error_log(error_log_path, rmd_file_path)
    
    # Generar correcciones basadas en análisis
    fixes = generate_fixes_from_analysis(analysis, rmd_file_path)
    
    return fixes

def generate_fixes_from_analysis(analysis: dict, rmd_file: str) -> list:
    """Genera lista de correcciones basadas en análisis"""
    
    fixes = []
    
    if analysis["error_type"] == "CHARACTER_ESCAPE":
        # Aplicar correcciones de escape de caracteres
        fixes.append({
            "type": "character_escape",
            "location": analysis["location"],
            "fix": escape_special_characters(rmd_file, analysis["location"])
        })
    
    elif analysis["error_type"] == "PACKAGE_MISSING":
        # Agregar paquetes faltantes al YAML
        fixes.append({
            "type": "add_package",
            "location": "yaml_header",
            "fix": add_missing_packages(rmd_file, analysis["error_message"])
        })
    
    return fixes
```

---

## Integración con Agente Corrector

Este skill se integra con el Agente Corrector Automático (ejemplo 05) para:

1. **Análisis post-renderizado**: Después de fallo de renderizado
2. **Corrección dirigida**: Aplicar correcciones específicas según tipo de error
3. **Iteración automática**: Re-renderizar y re-analizar hasta éxito
