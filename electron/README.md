# Electron App for VSCode Prolog Toolkit

This Electron app is the new backend and system shell for the VSCode Prolog Toolkit. It provides:
- A main system menu and web interface
- A modular architecture with the Prolog system as a core extension/module
- Local HTTP/WebSocket API for integration with the VSCode extension and other clients

## Structure
- `main/` - Electron main process, app shell, menu, window management
- `web/` - Web interface (React/Vue/HTML UI)
- `prologBackend/` - Prolog backend logic (migrated from src/prologBackend)
- `features/` - Modular features (apiServer, concurrencyManager, etc.)
- `sdk/` - API/WebSocket client SDK
- `utils/` - Shared utilities

## Roadmap
- Integrate Prolog system as a module/extension
- Expose robust API for extension and web UI
- Add system menu, tray, and web interface
- Support future extensions and modules
