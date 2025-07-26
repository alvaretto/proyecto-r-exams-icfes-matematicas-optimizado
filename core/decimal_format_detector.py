#!/usr/bin/env python3
"""
Detector de Errores de Formato de Decimales en Gráficas
======================================================

Detecta y corrige errores específicos como:
- Tortas que muestran decimales innecesarios (.0)
- Formato incorrecto de porcentajes
"""

import os
import sys
import re
import json
from typing import Dict, List, Any
import logging

logger = logging.getLogger(__name__)

class DecimalFormatDetector:
    """Detector de errores de formato de decimales"""
    
    def __init__(self):
        self.errors_found = []
        self.corrections_suggested = []
    
    def analyze_rmd_decimal_formatting(self, rmd_path: str) -> Dict[str, Any]:
        """
        Analizar archivo .Rmd para detectar errores de formato de decimales
        
        Args:
            rmd_path: Ruta al archivo .Rmd
            
        Returns:
            Diccionario con errores encontrados y correcciones sugeridas
        """
        logger.info(f"Analizando formato de decimales en: {rmd_path}")
        
        results = {
            'file_path': rmd_path,
            'decimal_format_errors': [],
            'code_analysis': {},
            'corrections_suggested': [],
            'summary': {}
        }
        
        try:
            # Leer contenido del archivo
            with open(rmd_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Analizar código R para detectar problemas de formato
            r_analysis = self.analyze_r_code_formatting(content)
            results['code_analysis'] = r_analysis
            
            # Detectar errores específicos de formato de decimales
            decimal_errors = self.detect_decimal_formatting_errors(content)
            results['decimal_format_errors'].extend(decimal_errors)
            
            # Generar correcciones sugeridas
            corrections = self.generate_decimal_corrections(content, decimal_errors)
            results['corrections_suggested'].extend(corrections)
            
            # Generar resumen
            results['summary'] = self.generate_decimal_summary(results)
            
        except Exception as e:
            logger.error(f"Error analizando formato de decimales: {e}")
            results['decimal_format_errors'].append({
                'type': 'analysis_error',
                'message': f"Error interno: {str(e)}",
                'severity': 'error'
            })
            
        return results
    
    def analyze_r_code_formatting(self, content: str) -> Dict[str, Any]:
        """Analizar código R para detectar problemas de formato"""
        
        analysis = {
            'pie_chart_formatting': [],
            'percentage_formatting': [],
            'decimal_patterns': []
        }
        
        # Buscar código de gráficas de torta
        pie_patterns = [
            r'pie\s*\([^)]+\)',
            r'ggplot.*geom_.*pie',
            r'include_tikz.*pie'
        ]
        
        for pattern in pie_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE | re.DOTALL)
            for match in matches:
                analysis['pie_chart_formatting'].append({
                    'code': match.group(0),
                    'start': match.start(),
                    'end': match.end()
                })
        
        # Buscar patrones de formato de porcentajes
        percentage_patterns = [
            r'sprintf\s*\(\s*["\']%\.?\d*f%%?["\']',
            r'paste\s*\([^)]*%[^)]*\)',
            r'format\s*\([^)]*digits\s*='
        ]
        
        for pattern in percentage_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE)
            for match in matches:
                analysis['percentage_formatting'].append({
                    'code': match.group(0),
                    'start': match.start(),
                    'end': match.end()
                })
        
        return analysis
    
    def detect_decimal_formatting_errors(self, content: str) -> List[Dict[str, Any]]:
        """Detectar errores específicos de formato de decimales"""
        
        errors = []
        
        # Error 1: sprintf con formato fijo que siempre muestra decimales
        sprintf_pattern = r'sprintf\s*\(\s*["\']%\.1f%%?["\']'
        matches = re.finditer(sprintf_pattern, content, re.IGNORECASE)
        
        for match in matches:
            errors.append({
                'type': 'fixed_decimal_format',
                'message': 'ERROR: sprintf con formato fijo %.1f siempre muestra decimales, incluso .0',
                'severity': 'error',
                'code': match.group(0),
                'line_start': content[:match.start()].count('\n') + 1,
                'suggestion': 'Usar formato condicional que solo muestre decimales si son distintos de cero'
            })
        
        # Error 2: Formato que no considera decimales cero
        format_patterns = [
            r'format\s*\([^)]*digits\s*=\s*1[^)]*\)',
            r'round\s*\([^)]*,\s*1\s*\)'
        ]
        
        for pattern in format_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE)
            for match in matches:
                errors.append({
                    'type': 'unconditional_decimal_format',
                    'message': 'ADVERTENCIA: Formato que puede mostrar decimales innecesarios (.0)',
                    'severity': 'warning',
                    'code': match.group(0),
                    'line_start': content[:match.start()].count('\n') + 1,
                    'suggestion': 'Verificar que solo muestre decimales cuando sean distintos de cero'
                })
        
        # Error 3: Detectar uso de paste sin formato condicional
        paste_with_decimals = r'paste\s*\([^)]*sprintf[^)]*%.1f[^)]*\)'
        matches = re.finditer(paste_with_decimals, content, re.IGNORECASE)
        
        for match in matches:
            errors.append({
                'type': 'paste_fixed_decimal',
                'message': 'ERROR: paste con sprintf fijo puede mostrar decimales innecesarios',
                'severity': 'error',
                'code': match.group(0),
                'line_start': content[:match.start()].count('\n') + 1,
                'suggestion': 'Implementar formato condicional para decimales'
            })
        
        return errors
    
    def generate_decimal_corrections(self, content: str, errors: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Generar correcciones específicas para errores de formato de decimales"""
        
        corrections = []
        
        for error in errors:
            if error['type'] == 'fixed_decimal_format':
                corrections.append({
                    'error_type': error['type'],
                    'original_code': error['code'],
                    'corrected_code': self.generate_conditional_format_code(),
                    'explanation': 'Reemplazar sprintf fijo con formato condicional que solo muestre decimales si son distintos de cero',
                    'implementation': 'function'
                })
            
            elif error['type'] == 'paste_fixed_decimal':
                corrections.append({
                    'error_type': error['type'],
                    'original_code': error['code'],
                    'corrected_code': self.generate_smart_paste_code(),
                    'explanation': 'Usar función que formatea inteligentemente los decimales',
                    'implementation': 'replacement'
                })
        
        return corrections
    
    def generate_conditional_format_code(self) -> str:
        """Generar código R para formato condicional de decimales"""
        
        return '''
# Función para formatear porcentajes sin decimales innecesarios
format_percentage <- function(value) {
    if (value == round(value)) {
        # Si es entero, no mostrar decimales
        return(paste0(round(value), "%"))
    } else {
        # Si tiene decimales, mostrar un decimal
        return(sprintf("%.1f%%", value))
    }
}

# Uso: format_percentage(25.0) -> "25%"
# Uso: format_percentage(25.3) -> "25.3%"
'''
    
    def generate_smart_paste_code(self) -> str:
        """Generar código R para paste inteligente"""
        
        return '''
# Función para formatear números inteligentemente
smart_format <- function(value, suffix = "") {
    if (abs(value - round(value)) < 0.001) {
        # Es prácticamente entero
        return(paste0(round(value), suffix))
    } else {
        # Tiene decimales significativos
        return(paste0(sprintf("%.1f", value), suffix))
    }
}

# Uso: smart_format(25.0, "%") -> "25%"
# Uso: smart_format(25.3, "%") -> "25.3%"
'''
    
    def generate_decimal_summary(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Generar resumen del análisis de formato de decimales"""
        
        return {
            'total_decimal_errors': len(results['decimal_format_errors']),
            'total_corrections_available': len(results['corrections_suggested']),
            'error_types': list(set(error['type'] for error in results['decimal_format_errors'])),
            'severity_breakdown': {
                'error': len([e for e in results['decimal_format_errors'] if e.get('severity') == 'error']),
                'warning': len([e for e in results['decimal_format_errors'] if e.get('severity') == 'warning'])
            },
            'status': 'error' if results['decimal_format_errors'] else 'success'
        }


def main():
    """Función principal para testing"""
    if len(sys.argv) != 2:
        print("Uso: python decimal_format_detector.py <archivo.Rmd>")
        sys.exit(1)
    
    rmd_file = sys.argv[1]
    
    detector = DecimalFormatDetector()
    results = detector.analyze_rmd_decimal_formatting(rmd_file)
    
    print(json.dumps(results, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
