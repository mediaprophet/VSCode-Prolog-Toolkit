// Electron main process entry point
import { app, BrowserWindow, Menu, Tray } from 'electron';
import * as path from 'path';

let mainWindow: BrowserWindow | null = null;
let tray: Tray | null = null;

function createMainWindow() {
  mainWindow = new BrowserWindow({
    width: 1200,
    height: 800,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
    },
    title: 'VSCode Prolog Toolkit System',
  });
  mainWindow.loadFile(path.join(__dirname, '../web/index.html'));
  mainWindow.on('closed', () => { mainWindow = null; });
}

function createAppMenu() {
  const template = [
    {
      label: 'File',
      submenu: [
        { role: 'quit' }
      ]
    },
    {
      label: 'Prolog',
      submenu: [
        { label: 'Start Prolog System', click: () => {/* TODO */ } },
        { label: 'Stop Prolog System', click: () => {/* TODO */ } },
        { type: 'separator' },
        { label: 'Prolog Console', click: () => {/* TODO */ } }
      ]
    },
    {
      label: 'View',
      submenu: [
        { role: 'reload' },
        { role: 'toggledevtools' },
        { type: 'separator' },
        { role: 'resetzoom' },
        { role: 'zoomin' },
        { role: 'zoomout' },
        { type: 'separator' },
        { role: 'togglefullscreen' }
      ]
    }
  ];
  const menu = Menu.buildFromTemplate(template as any);
  Menu.setApplicationMenu(menu);
}

function createTray() {
  tray = new Tray(path.join(__dirname, '../web/icon.png'));
  tray.setToolTip('VSCode Prolog Toolkit');
  tray.on('click', () => {
    if (mainWindow) mainWindow.show();
  });
}

app.on('ready', () => {
  createMainWindow();
  createAppMenu();
  createTray();
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});

app.on('activate', () => {
  if (mainWindow === null) createMainWindow();
});
