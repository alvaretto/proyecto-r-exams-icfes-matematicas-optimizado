#!/usr/bin/env python3
"""
Corrector Automático de Errores de Decimales
===========================================

Corrige automáticamente errores de formato de decimales en archivos .Rmd
"""

import os
import sys
import re
import shutil
from datetime import datetime
from typing import Dict, List, Any
import logging

logger = logging.getLogger(__name__)

class DecimalCorrector:
    """Corrector automático de errores de decimales"""
    
    def __init__(self, backup_dir="/tmp/decimal-corrections"):
        self.backup_dir = backup_dir
        self.ensure_backup_dir()
        
    def ensure_backup_dir(self):
        """Crear directorio de backups si no existe"""
        os.makedirs(self.backup_dir, exist_ok=True)
        
    def correct_decimal_errors(self, rmd_path: str) -> Dict[str, Any]:
        """
        Corregir errores de decimales en archivo .Rmd
        
        Args:
            rmd_path: Ruta al archivo .Rmd
            
        Returns:
            Diccionario con resultados de la corrección
        """
        logger.info(f"Iniciando corrección de decimales en: {rmd_path}")
        
        results = {
            'original_file': rmd_path,
            'backup_file': None,
            'corrected_file': None,
            'corrections_applied': [],
            'corrections_failed': [],
            'summary': {}
        }
        
        try:
            # Paso 1: Crear backup
            backup_path = self.create_backup(rmd_path)
            results['backup_file'] = backup_path
            
            # Paso 2: Leer contenido original
            with open(rmd_path, 'r', encoding='utf-8') as f:
                original_content = f.read()
            
            corrected_content = original_content
            
            # Paso 3: Aplicar correcciones específicas
            
            # Corrección 1: sprintf("%.0f" → formato inteligente
            correction1 = self.fix_sprintf_no_decimals(corrected_content)
            if correction1['success']:
                corrected_content = correction1['corrected_content']
                results['corrections_applied'].append(correction1)
            else:
                results['corrections_failed'].append(correction1)
            
            # Corrección 2: round() sin decimales → formato inteligente
            correction2 = self.fix_round_no_decimals(corrected_content)
            if correction2['success']:
                corrected_content = correction2['corrected_content']
                results['corrections_applied'].append(correction2)
            else:
                results['corrections_failed'].append(correction2)
            
            # Corrección 3: Agregar función de formato inteligente
            correction3 = self.add_smart_format_function(corrected_content)
            if correction3['success']:
                corrected_content = correction3['corrected_content']
                results['corrections_applied'].append(correction3)
            else:
                results['corrections_failed'].append(correction3)
            
            # Paso 4: Guardar archivo corregido
            corrected_path = self.save_corrected_file(rmd_path, corrected_content)
            results['corrected_file'] = corrected_path
            
            # Paso 5: Generar resumen
            results['summary'] = self.generate_correction_summary(results)
            
        except Exception as e:
            logger.error(f"Error en corrección de decimales: {e}")
            results['corrections_failed'].append({
                'type': 'correction_error',
                'message': f"Error interno: {str(e)}",
                'success': False
            })
            
        return results
    
    def create_backup(self, rmd_path: str) -> str:
        """Crear backup del archivo original"""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = os.path.basename(rmd_path)
        backup_filename = f"{timestamp}_{filename}.backup"
        backup_path = os.path.join(self.backup_dir, backup_filename)
        
        shutil.copy2(rmd_path, backup_path)
        logger.info(f"Backup creado: {backup_path}")
        
        return backup_path
    
    def fix_sprintf_no_decimals(self, content: str) -> Dict[str, Any]:
        """Corregir sprintf("%.0f" que elimina decimales importantes"""
        
        # Buscar sprintf("%.0f"
        pattern = r'sprintf\s*\(\s*["\']%.0f["\']'
        matches = list(re.finditer(pattern, content, re.IGNORECASE))
        
        if not matches:
            return {
                'type': 'sprintf_no_decimals_fix',
                'message': 'No se encontraron sprintf("%.0f" para corregir',
                'success': False,
                'corrected_content': content
            }
        
        corrected_content = content
        corrections_made = 0
        
        # Reemplazar cada ocurrencia
        for match in reversed(matches):  # Reversed para mantener posiciones
            start, end = match.span()
            
            # Reemplazar sprintf("%.0f" con smart_format_decimal(
            old_code = match.group(0)
            new_code = 'smart_format_decimal('
            
            corrected_content = corrected_content[:start] + new_code + corrected_content[end:]
            corrections_made += 1
        
        return {
            'type': 'sprintf_no_decimals_fix',
            'message': f'sprintf("%.0f" reemplazado con formato inteligente en {corrections_made} lugares',
            'success': True,
            'corrected_content': corrected_content,
            'corrections_count': corrections_made
        }
    
    def fix_round_no_decimals(self, content: str) -> Dict[str, Any]:
        """Corregir round() sin especificar decimales"""
        
        # Buscar round(valor) sin segundo parámetro
        pattern = r'round\s*\(\s*([^,)]+)\s*\)'
        matches = list(re.finditer(pattern, content, re.IGNORECASE))
        
        if not matches:
            return {
                'type': 'round_no_decimals_fix',
                'message': 'No se encontraron round() sin decimales para corregir',
                'success': False,
                'corrected_content': content
            }
        
        corrected_content = content
        corrections_made = 0
        
        # Reemplazar cada ocurrencia
        for match in reversed(matches):  # Reversed para mantener posiciones
            start, end = match.span()
            variable = match.group(1).strip()
            
            # Reemplazar round(valor) con smart_round_decimal(valor)
            old_code = match.group(0)
            new_code = f'smart_round_decimal({variable})'
            
            corrected_content = corrected_content[:start] + new_code + corrected_content[end:]
            corrections_made += 1
        
        return {
            'type': 'round_no_decimals_fix',
            'message': f'round() reemplazado con formato inteligente en {corrections_made} lugares',
            'success': True,
            'corrected_content': corrected_content,
            'corrections_count': corrections_made
        }
    
    def add_smart_format_function(self, content: str) -> Dict[str, Any]:
        """Agregar función de formato inteligente al archivo"""
        
        # Verificar si ya existe la función
        if 'smart_format_decimal' in content or 'smart_round_decimal' in content:
            return {
                'type': 'add_smart_format_function',
                'message': 'Función de formato inteligente ya existe',
                'success': False,
                'corrected_content': content
            }
        
        # Función de formato inteligente en R
        smart_format_code = '''
# ============================================================================
# FUNCIONES DE FORMATO INTELIGENTE DE DECIMALES
# ============================================================================
# Agregadas automáticamente por el corrector de decimales

# Función para formatear números con decimales solo cuando son necesarios
smart_format_decimal <- function(value, suffix = "") {
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

# Función para redondeo inteligente
smart_round_decimal <- function(value) {
    # Si el valor es prácticamente entero, devolver entero
    if (abs(value - round(value)) < 0.001) {
        return(round(value))
    } else {
        # Si tiene decimales significativos, redondear a 1 decimal
        return(round(value, 1))
    }
}

# Ejemplos de uso:
# smart_format_decimal(25.0, "%") -> "25%"
# smart_format_decimal(25.3, "%") -> "25.3%"
# smart_round_decimal(25.0) -> 25
# smart_round_decimal(25.3) -> 25.3

'''
        
        # Buscar el primer chunk de R para insertar las funciones
        lines = content.split('\n')
        insert_position = 0
        
        # Buscar después del header YAML
        yaml_end = False
        for i, line in enumerate(lines):
            if line.strip() == '---' and i > 0:
                yaml_end = True
                insert_position = i + 1
                break
        
        if not yaml_end:
            # Si no hay YAML, insertar al principio
            insert_position = 0
        
        # Insertar las funciones
        lines.insert(insert_position, smart_format_code)
        corrected_content = '\n'.join(lines)
        
        return {
            'type': 'add_smart_format_function',
            'message': 'Funciones de formato inteligente agregadas al archivo',
            'success': True,
            'corrected_content': corrected_content,
            'insert_position': insert_position
        }
    
    def save_corrected_file(self, original_path: str, corrected_content: str) -> str:
        """Guardar archivo corregido"""
        
        # Crear nombre para archivo corregido
        dir_path = os.path.dirname(original_path)
        filename = os.path.basename(original_path)
        name, ext = os.path.splitext(filename)
        
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        corrected_filename = f"{name}_decimales_corregidos_{timestamp}{ext}"
        corrected_path = os.path.join(dir_path, corrected_filename)
        
        # Guardar contenido corregido
        with open(corrected_path, 'w', encoding='utf-8') as f:
            f.write(corrected_content)
        
        logger.info(f"Archivo corregido guardado: {corrected_path}")
        
        return corrected_path
    
    def generate_correction_summary(self, correction_results: Dict[str, Any]) -> Dict[str, Any]:
        """Generar resumen de correcciones aplicadas"""
        
        total_corrections = len(correction_results['corrections_applied'])
        total_failed = len(correction_results['corrections_failed'])
        
        return {
            'total_corrections_applied': total_corrections,
            'total_corrections_failed': total_failed,
            'success_rate': total_corrections / (total_corrections + total_failed) * 100 if (total_corrections + total_failed) > 0 else 0,
            'status': 'success' if total_failed == 0 else 'partial' if total_corrections > 0 else 'failed'
        }


def main():
    """Función principal para testing"""
    if len(sys.argv) != 2:
        print("Uso: python decimal_corrector.py <archivo.Rmd>")
        sys.exit(1)
    
    rmd_file = sys.argv[1]
    
    # Crear corrector
    corrector = DecimalCorrector()
    
    # Aplicar correcciones
    correction_results = corrector.correct_decimal_errors(rmd_file)
    
    # Mostrar resultados
    import json
    print(json.dumps(correction_results, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
