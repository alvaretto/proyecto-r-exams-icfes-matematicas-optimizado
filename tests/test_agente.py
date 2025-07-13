#!/usr/bin/env python3
"""
🧪 Tests del Agente Graficador Experto TikZ
===========================================

Suite de tests para verificar el funcionamiento correcto
de todos los componentes del agente.

Autor: Agente IA Especializado
Fecha: 2025-01-13
Versión: 1.0.0
"""

import unittest
import sys
import os
from pathlib import Path
import tempfile
import json

# Agregar directorio padre al path
sys.path.insert(0, str(Path(__file__).parent.parent))

try:
    from agente_core import AgenteTikZ, ResultadoProcesamiento
    from analizador_imagenes import AnalizadorImagenes
    from generador_tikz import GeneradorTikZ
    from validador_codigo import ValidadorCodigo
except ImportError as e:
    print(f"❌ Error importando módulos: {e}")
    sys.exit(1)

class TestAgenteTikZ(unittest.TestCase):
    """Tests para el agente principal."""
    
    def setUp(self):
        """Configurar test."""
        self.agente = AgenteTikZ()
    
    def test_inicializacion(self):
        """Test de inicialización del agente."""
        self.assertIsNotNone(self.agente)
        self.assertIsNotNone(self.agente.analizador)
        self.assertIsNotNone(self.agente.generador)
        self.assertIsNotNone(self.agente.validador)
    
    def test_configuracion(self):
        """Test de configuración."""
        self.assertIn('max_iteraciones', self.agente.config)
        self.assertIn('umbral_similitud', self.agente.config)
        self.assertGreater(self.agente.config['max_iteraciones'], 0)
        self.assertGreater(self.agente.config['umbral_similitud'], 0)

class TestAnalizadorImagenes(unittest.TestCase):
    """Tests para el analizador de imágenes."""
    
    def setUp(self):
        """Configurar test."""
        config = {'umbral_deteccion': 0.7, 'resolucion_analisis': (800, 600)}
        self.analizador = AnalizadorImagenes(config)
    
    def test_inicializacion(self):
        """Test de inicialización."""
        self.assertIsNotNone(self.analizador)
        self.assertEqual(self.analizador.umbral_deteccion, 0.7)
    
    def test_analisis_simulado(self):
        """Test de análisis con datos simulados."""
        # Crear imagen temporal simple
        import numpy as np
        import cv2
        
        with tempfile.NamedTemporaryFile(suffix='.png', delete=False) as f:
            # Crear imagen simple: línea diagonal
            img = np.zeros((100, 100, 3), dtype=np.uint8)
            cv2.line(img, (0, 0), (99, 99), (255, 255, 255), 2)
            cv2.imwrite(f.name, img)
            
            # Analizar imagen
            resultado = self.analizador.analizar(f.name, "Línea diagonal", "general")
            
            # Verificar resultado
            self.assertTrue(resultado.get('exitoso', False))
            self.assertIn('tipo_detectado', resultado)
            self.assertIn('dimensiones', resultado)
            
            # Limpiar
            os.unlink(f.name)

class TestGeneradorTikZ(unittest.TestCase):
    """Tests para el generador de código TikZ."""
    
    def setUp(self):
        """Configurar test."""
        config = {'escala_default': 1.2, 'precision_coordenadas': 2}
        self.generador = GeneradorTikZ(config, Path(__file__).parent.parent)
    
    def test_inicializacion(self):
        """Test de inicialización."""
        self.assertIsNotNone(self.generador)
        self.assertEqual(self.generador.escala_default, 1.2)
    
    def test_generacion_funcion(self):
        """Test de generación para función."""
        analisis = {
            'exitoso': True,
            'tipo_detectado': 'funcion',
            'dimensiones': (800, 600),
            'ejes': {'detectados': True},
            'curvas': [{'puntos': [[100, 400], [200, 300]], 'longitud': 100}],
            'puntos_importantes': [[300, 200]],
            'cuadricula': {'presente': True}
        }
        
        codigo = self.generador.generar(analisis)
        
        self.assertIsNotNone(codigo)
        self.assertIn('\\begin{tikzpicture}', codigo)
        self.assertIn('\\end{tikzpicture}', codigo)
        self.assertIn('scale=', codigo)
    
    def test_optimizacion(self):
        """Test de optimización de código."""
        codigo_original = """\\begin{tikzpicture}[scale=1.2]


\\draw[blue] (0,0) -- (1,1);


\\end{tikzpicture}"""
        
        codigo_optimizado = self.generador.optimizar(codigo_original)
        
        self.assertIsNotNone(codigo_optimizado)
        # Verificar que se eliminaron líneas vacías excesivas
        self.assertLess(codigo_optimizado.count('\n\n\n'), codigo_original.count('\n\n\n'))

class TestValidadorCodigo(unittest.TestCase):
    """Tests para el validador de código."""
    
    def setUp(self):
        """Configurar test."""
        config = {'timeout_compilacion': 30, 'umbral_similitud': 0.95}
        self.validador = ValidadorCodigo(config)
    
    def test_inicializacion(self):
        """Test de inicialización."""
        self.assertIsNotNone(self.validador)
        self.assertEqual(self.validador.timeout_compilacion, 30)
    
    def test_validacion_sintaxis(self):
        """Test de validación de sintaxis."""
        # Código válido
        codigo_valido = """\\begin{tikzpicture}[scale=1.2]
\\draw[blue] (0,0) -- (1,1);
\\end{tikzpicture}"""
        
        es_valido, errores = self.validador._validar_sintaxis(codigo_valido)
        self.assertTrue(es_valido)
        self.assertEqual(len(errores), 0)
        
        # Código inválido
        codigo_invalido = """\\begin{tikzpicture}
\\draw[blue] (0,0) -- (1,1);
% Falta \\end{tikzpicture}"""
        
        es_valido, errores = self.validador._validar_sintaxis(codigo_invalido)
        self.assertFalse(es_valido)
        self.assertGreater(len(errores), 0)
    
    def test_balance_llaves(self):
        """Test de balance de llaves."""
        # Llaves balanceadas
        codigo_balanceado = "\\draw{test} \\node{otro}"
        self.assertTrue(self.validador._verificar_balance_llaves(codigo_balanceado))
        
        # Llaves desbalanceadas
        codigo_desbalanceado = "\\draw{test \\node{otro}"
        self.assertFalse(self.validador._verificar_balance_llaves(codigo_desbalanceado))

class TestIntegracion(unittest.TestCase):
    """Tests de integración completa."""
    
    def setUp(self):
        """Configurar test."""
        self.agente = AgenteTikZ()
    
    def test_flujo_completo_simulado(self):
        """Test del flujo completo con datos simulados."""
        # Crear imagen temporal
        import numpy as np
        import cv2
        
        with tempfile.NamedTemporaryFile(suffix='.png', delete=False) as f:
            # Crear imagen con ejes y función simple
            img = np.zeros((200, 200, 3), dtype=np.uint8)
            
            # Ejes
            cv2.line(img, (20, 180), (180, 180), (255, 255, 255), 2)  # X
            cv2.line(img, (20, 20), (20, 180), (255, 255, 255), 2)    # Y
            
            # Función simple (línea)
            cv2.line(img, (20, 180), (180, 20), (0, 255, 0), 2)
            
            cv2.imwrite(f.name, img)
            
            try:
                # Procesar imagen
                resultado = self.agente.procesar_imagen(f.name, "Función lineal", "funcion")
                
                # Verificar resultado
                self.assertIsNotNone(resultado)
                self.assertIsInstance(resultado, ResultadoProcesamiento)
                self.assertIsNotNone(resultado.codigo_tikz)
                self.assertGreater(len(resultado.codigo_tikz), 0)
                
                # Verificar que contiene elementos TikZ básicos
                self.assertIn('\\begin{tikzpicture}', resultado.codigo_tikz)
                self.assertIn('\\end{tikzpicture}', resultado.codigo_tikz)
                
            finally:
                # Limpiar
                os.unlink(f.name)

def ejecutar_tests():
    """Ejecutar todos los tests."""
    print("🧪 EJECUTANDO TESTS DEL AGENTE TIKZ")
    print("=" * 50)
    
    # Crear suite de tests
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Agregar tests
    suite.addTests(loader.loadTestsFromTestCase(TestAgenteTikZ))
    suite.addTests(loader.loadTestsFromTestCase(TestAnalizadorImagenes))
    suite.addTests(loader.loadTestsFromTestCase(TestGeneradorTikZ))
    suite.addTests(loader.loadTestsFromTestCase(TestValidadorCodigo))
    suite.addTests(loader.loadTestsFromTestCase(TestIntegracion))
    
    # Ejecutar tests
    runner = unittest.TextTestRunner(verbosity=2)
    resultado = runner.run(suite)
    
    # Resumen
    print("\n" + "=" * 50)
    print("📊 RESUMEN DE TESTS")
    print("=" * 50)
    print(f"Tests ejecutados: {resultado.testsRun}")
    print(f"Errores: {len(resultado.errors)}")
    print(f"Fallos: {len(resultado.failures)}")
    
    if resultado.errors:
        print("\n❌ ERRORES:")
        for test, error in resultado.errors:
            print(f"  • {test}: {error}")
    
    if resultado.failures:
        print("\n❌ FALLOS:")
        for test, fallo in resultado.failures:
            print(f"  • {test}: {fallo}")
    
    if resultado.wasSuccessful():
        print("\n✅ TODOS LOS TESTS PASARON")
        return True
    else:
        print("\n❌ ALGUNOS TESTS FALLARON")
        return False

if __name__ == "__main__":
    success = ejecutar_tests()
    sys.exit(0 if success else 1)
