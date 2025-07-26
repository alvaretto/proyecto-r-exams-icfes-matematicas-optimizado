#!/usr/bin/env python3
"""
Sistema de Corrección Automática con IA para Revisor-Rmd 2.0
===========================================================

Corrige automáticamente errores detectados en archivos .Rmd
"""

import os
import sys
import re
import json
import shutil
from datetime import datetime
from typing import Dict, List, Any
import logging

logger = logging.getLogger(__name__)

class AutoCorrector:
    """Corrector automático de errores en archivos .Rmd"""
    
    def __init__(self, config=None):
        self.config = config or {}
        self.backup_dir = self.config.get('backup_dir', '/tmp/revisor-backups')
        self.ensure_backup_dir()
        
    def ensure_backup_dir(self):
        """Crear directorio de backups si no existe"""
        os.makedirs(self.backup_dir, exist_ok=True)
        
    def correct_rmd_file(self, rmd_path: str, analysis_results: Dict[str, Any]) -> Dict[str, Any]:
        """
        Corregir archivo .Rmd basado en análisis de errores
        
        Args:
            rmd_path: Ruta al archivo .Rmd
            analysis_results: Resultados del análisis visual
            
        Returns:
            Diccionario con resultados de la corrección
        """
        logger.info(f"Iniciando corrección automática de: {rmd_path}")
        
        correction_results = {
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
            correction_results['backup_file'] = backup_path
            
            # Paso 2: Leer contenido original
            with open(rmd_path, 'r', encoding='utf-8') as f:
                original_content = f.read()
            
            corrected_content = original_content
            
            # Paso 3: Aplicar correcciones específicas
            for error in analysis_results.get('mathematical_inconsistencies', []):
                correction = self.apply_correction(corrected_content, error)
                if correction['success']:
                    corrected_content = correction['corrected_content']
                    correction_results['corrections_applied'].append(correction)
                else:
                    correction_results['corrections_failed'].append(correction)
            
            # Paso 4: Guardar archivo corregido
            corrected_path = self.save_corrected_file(rmd_path, corrected_content)
            correction_results['corrected_file'] = corrected_path
            
            # Paso 5: Generar resumen
            correction_results['summary'] = self.generate_correction_summary(correction_results)
            
        except Exception as e:
            logger.error(f"Error en corrección automática: {e}")
            correction_results['corrections_failed'].append({
                'type': 'correction_error',
                'message': f"Error interno: {str(e)}",
                'success': False
            })
            
        return correction_results
    
    def create_backup(self, rmd_path: str) -> str:
        """Crear backup del archivo original"""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = os.path.basename(rmd_path)
        backup_filename = f"{timestamp}_{filename}.backup"
        backup_path = os.path.join(self.backup_dir, backup_filename)
        
        shutil.copy2(rmd_path, backup_path)
        logger.info(f"Backup creado: {backup_path}")
        
        return backup_path
    
    def apply_correction(self, content: str, error: Dict[str, Any]) -> Dict[str, Any]:
        """
        Aplicar corrección específica basada en el tipo de error
        
        Args:
            content: Contenido del archivo
            error: Información del error a corregir
            
        Returns:
            Diccionario con resultado de la corrección
        """
        error_type = error.get('type', '')
        
        if error_type == 'negative_question_detected':
            return self.correct_negative_question(content, error)
        elif error_type == 'multiple_graphics_negative_question':
            return self.correct_multiple_graphics_issue(content, error)
        elif error_type == 'percentage_sum_error':
            return self.correct_percentage_sum(content, error)
        else:
            return {
                'type': error_type,
                'message': f"Tipo de error no soportado para corrección automática: {error_type}",
                'success': False,
                'corrected_content': content
            }
    
    def correct_negative_question(self, content: str, error: Dict[str, Any]) -> Dict[str, Any]:
        """Corregir estructura de pregunta negativa PRESERVANDO la filosofía original"""

        # NO cambiar la pregunta negativa - solo agregar advertencia para corrección manual
        warning_comment = """
<!--
CORRECCIÓN REQUERIDA - PREGUNTA NEGATIVA DETECTADA:
La pregunta es del tipo "¿Cuál NO muestra correctamente...?"
VERIFICAR MANUALMENTE que haya exactamente:
- 3 opciones CORRECTAS (que muestran correctamente la información)
- 1 opción INCORRECTA (que NO muestra correctamente - esta es la respuesta)

FILOSOFÍA ORIGINAL PRESERVADA: Pregunta negativa mantenida intencionalmente.
NO cambiar el tipo de pregunta - solo corregir la estructura de opciones.
-->

"""

        # Insertar advertencia al inicio del archivo
        lines = content.split('\n')
        yaml_end = -1

        # Encontrar el final del header YAML
        for i, line in enumerate(lines):
            if line.strip() == '---' and i > 0:
                yaml_end = i
                break

        if yaml_end > -1:
            lines.insert(yaml_end + 1, warning_comment)
        else:
            lines.insert(0, warning_comment)

        corrected_content = '\n'.join(lines)

        return {
            'type': 'negative_question_structure_warning',
            'message': 'FILOSOFÍA PRESERVADA: Pregunta negativa mantenida. Advertencia agregada para verificación manual de estructura 3+1',
            'success': True,
            'corrected_content': corrected_content
        }
    
    def correct_multiple_graphics_issue(self, content: str, error: Dict[str, Any]) -> Dict[str, Any]:
        """Corregir problema de múltiples gráficas en pregunta negativa"""
        
        # Esta corrección requiere análisis manual más profundo
        # Por ahora, agregamos comentario de advertencia
        
        warning_comment = """
<!-- 
CORRECCIÓN AUTOMÁTICA APLICADA:
Se detectó una pregunta negativa con múltiples gráficas.
VERIFICAR MANUALMENTE que solo UNA gráfica sea incorrecta.
Si hay múltiples gráficas incorrectas, la pregunta es ambigua.
-->

"""
        
        # Insertar advertencia al inicio del archivo
        lines = content.split('\n')
        yaml_end = -1
        
        # Encontrar el final del header YAML
        for i, line in enumerate(lines):
            if line.strip() == '---' and i > 0:
                yaml_end = i
                break
        
        if yaml_end > -1:
            lines.insert(yaml_end + 1, warning_comment)
        else:
            lines.insert(0, warning_comment)
        
        corrected_content = '\n'.join(lines)
        
        return {
            'type': 'multiple_graphics_warning',
            'message': 'Advertencia agregada sobre múltiples gráficas en pregunta negativa',
            'success': True,
            'corrected_content': corrected_content
        }
    
    def correct_percentage_sum(self, content: str, error: Dict[str, Any]) -> Dict[str, Any]:
        """Corregir porcentajes que no suman 100%"""
        
        values = error.get('values', [])
        if len(values) != 3:
            return {
                'type': 'percentage_sum_correction',
                'message': 'No se pueden corregir porcentajes: valores insuficientes',
                'success': False,
                'corrected_content': content
            }
        
        # Calcular porcentajes corregidos que sumen 100%
        total = sum(values)
        if total == 0:
            corrected_values = [33.3, 33.3, 33.4]  # Distribución equitativa
        else:
            # Normalizar proporcionalmente
            factor = 100.0 / total
            corrected_values = [round(v * factor, 1) for v in values]
            
            # Ajustar para que sume exactamente 100%
            current_sum = sum(corrected_values)
            diff = 100.0 - current_sum
            corrected_values[-1] += diff
        
        # Buscar y reemplazar los valores en el contenido
        corrected_content = content
        
        # Buscar patrones de porcentajes en el código
        for i, (old_val, new_val) in enumerate(zip(values, corrected_values)):
            # Buscar el valor en contexto de porcentaje
            old_str = str(old_val).replace('.0', '')
            new_str = str(new_val).replace('.0', '')
            
            # Patrones comunes donde aparecen porcentajes
            patterns = [
                rf'\b{re.escape(old_str)}%',
                rf'\b{re.escape(old_str)}\b(?=\s*[,\]])',  # En listas
                rf'= {re.escape(old_str)}\b'  # En asignaciones
            ]
            
            for pattern in patterns:
                if re.search(pattern, corrected_content):
                    corrected_content = re.sub(pattern, 
                                             pattern.replace(re.escape(old_str), new_str), 
                                             corrected_content, count=1)
                    break
        
        return {
            'type': 'percentage_sum_correction',
            'message': f'Porcentajes corregidos: {values} → {corrected_values}',
            'success': True,
            'corrected_content': corrected_content,
            'old_values': values,
            'new_values': corrected_values
        }
    
    # FUNCIÓN ELIMINADA: adjust_answer_for_positive_question
    # RAZÓN: No debemos cambiar la filosofía de la pregunta original
    # Las preguntas negativas deben mantenerse como negativas
    # Solo se debe corregir la estructura de opciones (3 correctas + 1 incorrecta)
    
    def save_corrected_file(self, original_path: str, corrected_content: str) -> str:
        """Guardar archivo corregido"""
        
        # Crear nombre para archivo corregido
        dir_path = os.path.dirname(original_path)
        filename = os.path.basename(original_path)
        name, ext = os.path.splitext(filename)
        
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        corrected_filename = f"{name}_corregido_{timestamp}{ext}"
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
    if len(sys.argv) != 3:
        print("Uso: python auto_corrector.py <archivo.Rmd> <resultados_analisis.json>")
        sys.exit(1)
    
    rmd_file = sys.argv[1]
    analysis_file = sys.argv[2]
    
    # Cargar resultados del análisis
    with open(analysis_file, 'r', encoding='utf-8') as f:
        analysis_results = json.load(f)
    
    # Crear corrector
    corrector = AutoCorrector()
    
    # Aplicar correcciones
    correction_results = corrector.correct_rmd_file(rmd_file, analysis_results)
    
    # Mostrar resultados
    print(json.dumps(correction_results, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
