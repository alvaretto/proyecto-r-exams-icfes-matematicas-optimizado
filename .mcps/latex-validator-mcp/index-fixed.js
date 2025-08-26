#!/usr/bin/env node

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { exec } from "child_process";
import { promises as fs } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { promisify } from "util";

const execAsync = promisify(exec);
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Esquemas de validación
const ValidateLatexSchema = z.object({
  latex_code: z.string().describe("Código LaTeX/TikZ a validar"),
  type: z.enum(["tikz", "latex", "rexams"]).optional().describe("Tipo de validación"),
  check_syntax: z.boolean().optional().default(true).describe("Verificar sintaxis"),
  check_compilation: z.boolean().optional().default(false).describe("Verificar compilación")
});

const CompileTikzSchema = z.object({
  tikz_code: z.string().describe("Código TikZ a compilar"),
  output_format: z.enum(["pdf", "png", "svg"]).optional().default("pdf").describe("Formato de salida"),
  standalone: z.boolean().optional().default(true).describe("Documento standalone")
});

// Crear servidor MCP
const server = new McpServer(
  {
    name: "latex-validator-mcp",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// Función para validar sintaxis LaTeX básica
function validateLatexSyntax(code) {
  const errors = [];
  const warnings = [];
  
  // Verificar llaves balanceadas
  let braceCount = 0;
  let bracketCount = 0;
  
  for (let i = 0; i < code.length; i++) {
    const char = code[i];
    const prevChar = i > 0 ? code[i-1] : '';
    
    if (char === '{' && prevChar !== '\\') braceCount++;
    if (char === '}' && prevChar !== '\\') braceCount--;
    if (char === '[' && prevChar !== '\\') bracketCount++;
    if (char === ']' && prevChar !== '\\') bracketCount--;
  }
  
  if (braceCount !== 0) {
    errors.push(`Llaves desbalanceadas: ${braceCount > 0 ? 'faltan ' + braceCount + ' llaves de cierre' : 'sobran ' + Math.abs(braceCount) + ' llaves de cierre'}`);
  }
  
  if (bracketCount !== 0) {
    errors.push(`Corchetes desbalanceados: ${bracketCount > 0 ? 'faltan ' + bracketCount + ' corchetes de cierre' : 'sobran ' + Math.abs(bracketCount) + ' corchetes de cierre'}`);
  }
  
  // Verificar comandos TikZ comunes
  if (code.includes('\\begin{tikzpicture}') && !code.includes('\\end{tikzpicture}')) {
    errors.push('Falta \\end{tikzpicture}');
  }
  
  // Verificar caracteres especiales sin escapar
  const unescapedChars = code.match(/[^\\][%$&_#]/g);
  if (unescapedChars) {
    warnings.push(`Posibles caracteres especiales sin escapar: ${unescapedChars.join(', ')}`);
  }
  
  return { errors, warnings };
}

// Función para compilar TikZ
async function compileTikz(tikzCode, outputFormat = 'pdf') {
  const tempDir = join(__dirname, 'temp');
  
  try {
    // Crear directorio temporal
    await fs.mkdir(tempDir, { recursive: true });
    
    // Crear documento LaTeX completo
    const latexDoc = `
\\documentclass[tikz,border=2pt]{standalone}
\\usepackage{tikz}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usetikzlibrary{arrows,shapes,positioning,calc}

\\begin{document}
${tikzCode}
\\end{document}
`;
    
    const texFile = join(tempDir, 'test.tex');
    await fs.writeFile(texFile, latexDoc);
    
    // Compilar con pdflatex
    const { stdout, stderr } = await execAsync(`cd "${tempDir}" && pdflatex -interaction=nonstopmode test.tex`);
    
    const result = {
      success: true,
      output: stdout,
      errors: stderr,
      files_generated: ['test.pdf']
    };
    
    // Convertir a otros formatos si es necesario
    if (outputFormat === 'png') {
      try {
        await execAsync(`cd "${tempDir}" && convert -density 300 test.pdf test.png`);
        result.files_generated.push('test.png');
      } catch (convertError) {
        result.warnings = ['No se pudo convertir a PNG: ImageMagick no disponible'];
      }
    }
    
    return result;
    
  } catch (error) {
    return {
      success: false,
      error: error.message,
      compilation_failed: true
    };
  } finally {
    // Limpiar archivos temporales
    try {
      await execAsync(`rm -rf "${tempDir}"`);
    } catch (cleanupError) {
      console.warn('No se pudieron limpiar archivos temporales:', cleanupError.message);
    }
  }
}

// Registrar herramientas
server.tool("validate_latex", "Validar código LaTeX/TikZ para ejercicios R-exams", ValidateLatexSchema, async ({ latex_code, type = "latex", check_syntax = true, check_compilation = false }) => {
  const result = {
    valid: true,
    type: type,
    syntax_check: null,
    compilation_check: null,
    suggestions: []
  };

  if (check_syntax) {
    result.syntax_check = validateLatexSyntax(latex_code);
    if (result.syntax_check.errors.length > 0) {
      result.valid = false;
    }
  }

  if (check_compilation && type === "tikz") {
    result.compilation_check = await compileTikz(latex_code);
    if (!result.compilation_check.success) {
      result.valid = false;
    }
  }

  // Sugerencias específicas para R-exams
  if (type === "rexams") {
    if (latex_code.includes('$') && !latex_code.includes('\\$')) {
      result.suggestions.push("Considerar usar \\( \\) en lugar de $ $ para matemáticas en R-exams");
    }
    if (latex_code.includes('\\\\') && !latex_code.includes('\\\\[')) {
      result.suggestions.push("Considerar usar \\\\[0.5em] para mejor espaciado en R-exams");
    }
  }

  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(result, null, 2)
      }
    ]
  };
});

server.tool("compile_tikz", "Compilar código TikZ y verificar sintaxis", CompileTikzSchema, async ({ tikz_code, output_format = "pdf", standalone = true }) => {
  let codeToCompile = tikz_code;
  if (standalone && !tikz_code.includes('\\begin{tikzpicture}')) {
    codeToCompile = `\\begin{tikzpicture}\n${tikz_code}\n\\end{tikzpicture}`;
  }

  const result = await compileTikz(codeToCompile, output_format);

  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(result, null, 2)
      }
    ]
  };
});

// Iniciar servidor
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("LaTeX Validator MCP Server iniciado");
}

main().catch((error) => {
  console.error("Error al iniciar servidor:", error);
  process.exit(1);
});
