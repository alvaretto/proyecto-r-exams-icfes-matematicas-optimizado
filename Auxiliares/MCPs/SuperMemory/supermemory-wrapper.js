#!/usr/bin/env node

/**
 * Supermemory MCP Wrapper
 * Wrapper script to make supermemory compatible with npx-style execution
 */

const { spawn } = require('child_process');
const path = require('path');

// Get the directory where this wrapper is located
const wrapperDir = __dirname;
const serverPath = path.join(wrapperDir, 'supermemory-simple.js');

// Spawn the actual server
const server = spawn('node', [serverPath], {
  stdio: 'inherit',
  cwd: wrapperDir
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
