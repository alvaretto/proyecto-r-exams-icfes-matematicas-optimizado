#!/usr/bin/env node

/**
 * Supermemory MCP Launcher
 * Simple launcher script for VSCode MCP integration
 */

const { spawn } = require('child_process');
const path = require('path');

// Path to the actual supermemory server
const serverPath = path.join(__dirname, 'Auxiliares', 'MCPs', 'SuperMemory', 'supermemory-simple.js');

// Spawn the server
const server = spawn('node', [serverPath], {
  stdio: 'inherit',
  cwd: __dirname
});

// Handle server exit
server.on('exit', (code) => {
  process.exit(code);
});

// Handle errors
server.on('error', (error) => {
  console.error('Error starting supermemory server:', error);
  process.exit(1);
});

// Handle process termination
process.on('SIGTERM', () => {
  server.kill('SIGTERM');
});

process.on('SIGINT', () => {
  server.kill('SIGINT');
});
