// Configuración de Playwright para Augment AI en Manjaro
module.exports = {
  use: {
    // Usar Firefox como navegador por defecto (más compatible con Manjaro)
    browserName: 'firefox',
    // Configuración de viewport
    viewport: { width: 1280, height: 720 },
    // Configuración de screenshots
    screenshot: 'only-on-failure',
    // Configuración de video
    video: 'retain-on-failure',
    // Timeout por defecto
    actionTimeout: 30000,
    navigationTimeout: 30000,
  },
  // Configuración de proyectos para diferentes navegadores
  projects: [
    {
      name: 'firefox',
      use: { browserName: 'firefox' },
    },
    {
      name: 'chromium',
      use: { browserName: 'chromium' },
    },
  ],
  // Configuración de workers
  workers: 1,
  // Configuración de reportes
  reporter: 'list',
  // Configuración de timeout global
  timeout: 60000,
};
