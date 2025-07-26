#!/usr/bin/env python3
"""
Detector de Pérdida de Decimales Importantes
===========================================

Detecta cuando las gráficas pierden decimales significativos que deberían mostrarse.
"""

import os
import sys
import re
import json
from typing import Dict, List, Any
import logging

logger = logging.getLogger(__name__)

class DecimalLossDetector:
    """Detector de pérdida de decimales importantes en gráficas"""
    
    def __init__(self):
        self.errors_found = []
        
    def analyze_decimal_loss(self, rmd_path: str) -> Dict[str, Any]:
        """
        Analizar archivo .Rmd para detectar pérdida de decimales importantes
        
        Args:
            rmd_path: Ruta al archivo .Rmd
            
        Returns:
            Diccionario con errores de pérdida de decimales
        """
        logger.info(f"Analizando pérdida de decimales en: {rmd_path}")
        
        results = {
            'file_path': rmd_path,
            'decimal_loss_errors': [],
            'code_patterns': {},
            'corrections_suggested': [],
            'summary': {}
        }
        
        try:
            # Leer contenido del archivo
            with open(rmd_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Analizar patrones de código que pueden causar pérdida de decimales
            code_analysis = self.analyze_rounding_patterns(content)
            results['code_patterns'] = code_analysis
            
            # Detectar errores específicos de pérdida de decimales
            loss_errors = self.detect_decimal_loss_errors(content)
            results['decimal_loss_errors'].extend(loss_errors)
            
            # Generar correcciones sugeridas
            corrections = self.generate_decimal_preservation_corrections(content, loss_errors)
            results['corrections_suggested'].extend(corrections)
            
            # Generar resumen
            results['summary'] = self.generate_loss_summary(results)
            
        except Exception as e:
            logger.error(f"Error analizando pérdida de decimales: {e}")
            results['decimal_loss_errors'].append({
                'type': 'analysis_error',
                'message': f"Error interno: {str(e)}",
                'severity': 'error'
            })
            
        return results
    
    def analyze_rounding_patterns(self, content: str) -> Dict[str, Any]:
        """Analizar patrones de código que pueden causar redondeo incorrecto"""
        
        patterns = {
            'round_functions': [],
            'integer_conversions': [],
            'format_without_decimals': [],
            'pie_chart_code': []
        }
        
        # Buscar funciones round() que pueden eliminar decimales
        round_pattern = r'round\s*\([^)]+\)'
        matches = re.finditer(round_pattern, content, re.IGNORECASE)
        for match in matches:
            patterns['round_functions'].append({
                'code': match.group(0),
                'start': match.start(),
                'line': content[:match.start()].count('\n') + 1
            })
        
        # Buscar conversiones a entero
        int_patterns = [r'as\.integer\s*\([^)]+\)', r'floor\s*\([^)]+\)', r'ceiling\s*\([^)]+\)']
        for pattern in int_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE)
            for match in matches:
                patterns['integer_conversions'].append({
                    'code': match.group(0),
                    'start': match.start(),
                    'line': content[:match.start()].count('\n') + 1
                })
        
        # Buscar formatos sin decimales
        format_patterns = [r'sprintf\s*\(\s*["\']%d["\']', r'sprintf\s*\(\s*["\']%.0f["\']']
        for pattern in format_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE)
            for match in matches:
                patterns['format_without_decimals'].append({
                    'code': match.group(0),
                    'start': match.start(),
                    'line': content[:match.start()].count('\n') + 1
                })
        
        # Buscar código de gráficas de torta
        pie_patterns = [r'pie\s*\([^)]+\)', r'include_tikz.*pie']
        for pattern in pie_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE | re.DOTALL)
            for match in matches:
                patterns['pie_chart_code'].append({
                    'code': match.group(0)[:100] + '...' if len(match.group(0)) > 100 else match.group(0),
                    'start': match.start(),
                    'line': content[:match.start()].count('\n') + 1
                })
        
        return patterns
    
    def detect_decimal_loss_errors(self, content: str) -> List[Dict[str, Any]]:
        """Detectar errores específicos de pérdida de decimales"""
        
        errors = []
        
        # Error 1: round() sin especificar decimales (redondea a entero)
        round_no_digits = r'round\s*\(\s*[^,)]+\s*\)'
        matches = re.finditer(round_no_digits, content, re.IGNORECASE)
        
        for match in matches:
            errors.append({
                'type': 'round_to_integer',
                'message': 'ERROR: round() sin especificar decimales elimina información importante',
                'severity': 'error',
                'code': match.group(0),
                'line': content[:match.start()].count('\n') + 1,
                'suggestion': 'Usar round(valor, 1) para preservar un decimal o formato condicional'
            })
        
        # Error 2: sprintf con formato entero para valores que pueden tener decimales
        sprintf_integer = r'sprintf\s*\(\s*["\']%d["\']'
        matches = re.finditer(sprintf_integer, content, re.IGNORECASE)
        
        for match in matches:
            errors.append({
                'type': 'sprintf_integer_format',
                'message': 'ERROR: sprintf con %d elimina decimales importantes',
                'severity': 'error',
                'code': match.group(0),
                'line': content[:match.start()].count('\n') + 1,
                'suggestion': 'Usar formato condicional que preserve decimales cuando sean necesarios'
            })
        
        # Error 3: sprintf con %.0f (elimina todos los decimales)
        sprintf_no_decimals = r'sprintf\s*\(\s*["\']%.0f["\']'
        matches = re.finditer(sprintf_no_decimals, content, re.IGNORECASE)
        
        for match in matches:
            errors.append({
                'type': 'sprintf_no_decimals',
                'message': 'ERROR: sprintf con %.0f elimina decimales que podrían ser importantes',
                'severity': 'error',
                'code': match.group(0),
                'line': content[:match.start()].count('\n') + 1,
                'suggestion': 'Usar formato inteligente que muestre decimales solo cuando sean necesarios'
            })
        
        # Error 4: as.integer() en valores que pueden necesitar decimales
        as_integer = r'as\.integer\s*\([^)]+\)'
        matches = re.finditer(as_integer, content, re.IGNORECASE)
        
        for match in matches:
            errors.append({
                'type': 'forced_integer_conversion',
                'message': 'ADVERTENCIA: Conversión forzada a entero puede eliminar decimales importantes',
                'severity': 'warning',
                'code': match.group(0),
                'line': content[:match.start()].count('\n') + 1,
                'suggestion': 'Verificar si los decimales son importantes para la visualización'
            })
        
        return errors
    
    def generate_decimal_preservation_corrections(self, content: str, errors: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Generar correcciones para preservar decimales importantes"""
        
        corrections = []
        
        for error in errors:
            if error['type'] == 'round_to_integer':
                corrections.append({
                    'error_type': error['type'],
                    'original_code': error['code'],
                    'corrected_code': self.generate_smart_rounding_code(),
                    'explanation': 'Reemplazar round() simple con función que preserve decimales importantes',
                    'implementation': 'function_replacement'
                })
            
            elif error['type'] in ['sprintf_integer_format', 'sprintf_no_decimals']:
                corrections.append({
                    'error_type': error['type'],
                    'original_code': error['code'],
                    'corrected_code': self.generate_smart_format_code(),
                    'explanation': 'Usar formato inteligente que muestre decimales solo cuando sean necesarios',
                    'implementation': 'format_replacement'
                })
        
        return corrections
    
    def generate_smart_rounding_code(self) -> str:
        """Generar código R para redondeo inteligente"""
        
        return '''
# Función para redondeo inteligente que preserva decimales importantes
smart_round <- function(value, max_digits = 1) {
    # Si el valor es prácticamente entero, mostrar como entero
    if (abs(value - round(value)) < 0.001) {
        return(round(value))
    } else {
        # Si tiene decimales significativos, mostrar con decimales
        return(round(value, max_digits))
    }
}

# Uso: smart_round(25.0) -> 25
# Uso: smart_round(25.3) -> 25.3
'''
    
    def generate_smart_format_code(self) -> str:
        """Generar código R para formato inteligente"""
        
        return '''
# Función para formato inteligente de números
smart_format <- function(value, suffix = "") {
    # Redondear a 1 decimal para evaluación
    rounded <- round(value, 1)
    
    # Si es prácticamente entero, mostrar sin decimales
    if (abs(rounded - round(rounded)) < 0.001) {
        return(paste0(round(rounded), suffix))
    } else {
        # Si tiene decimales significativos, mostrarlos
        return(paste0(sprintf("%.1f", rounded), suffix))
    }
}

# Uso: smart_format(25.0, "%") -> "25%"
# Uso: smart_format(25.3, "%") -> "25.3%"
'''
    
    def generate_loss_summary(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Generar resumen del análisis de pérdida de decimales"""
        
        return {
            'total_decimal_loss_errors': len(results['decimal_loss_errors']),
            'total_corrections_available': len(results['corrections_suggested']),
            'error_types': list(set(error['type'] for error in results['decimal_loss_errors'])),
            'severity_breakdown': {
                'error': len([e for e in results['decimal_loss_errors'] if e.get('severity') == 'error']),
                'warning': len([e for e in results['decimal_loss_errors'] if e.get('severity') == 'warning'])
            },
            'patterns_found': {
                'round_functions': len(results['code_patterns'].get('round_functions', [])),
                'integer_conversions': len(results['code_patterns'].get('integer_conversions', [])),
                'format_without_decimals': len(results['code_patterns'].get('format_without_decimals', [])),
                'pie_chart_code': len(results['code_patterns'].get('pie_chart_code', []))
            },
            'status': 'error' if results['decimal_loss_errors'] else 'success'
        }


def main():
    """Función principal para testing"""
    if len(sys.argv) != 2:
        print("Uso: python decimal_loss_detector.py <archivo.Rmd>")
        sys.exit(1)
    
    rmd_file = sys.argv[1]
    
    detector = DecimalLossDetector()
    results = detector.analyze_decimal_loss(rmd_file)
    
    print(json.dumps(results, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
