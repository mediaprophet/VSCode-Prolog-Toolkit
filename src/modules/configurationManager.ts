'use strict';

import { ExtensionContext, workspace } from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import jsesc from 'jsesc';
import { Utils } from '../utils/utils';
import { PlatformUtils, getPlatform, getPlatformDefaults } from '../utils/platformUtils';
import { AuthConfig } from '../features/apiMiddleware';

export class ConfigurationManager {
  private static instance: ConfigurationManager;

  private constructor() {}

  static getInstance(): ConfigurationManager {
    if (!ConfigurationManager.instance) {
      ConfigurationManager.instance = new ConfigurationManager();
    }
    return ConfigurationManager.instance;
  }

  // Initialization of workspace for dialect
  async initForDialect(context: ExtensionContext): Promise<void> {
    // get the user preferences for the extension
    const section = workspace.getConfiguration('prolog');
    const dialect = section.get<string>('dialect');

    // Use platform-aware executable path resolution
    let exec = section.get<string>('executablePath', '');
    if (!exec) {
      exec = getPlatformDefaults().defaultExecutablePath;
    }
    exec = PlatformUtils.normalizePath(exec);

    Utils.LINTERTRIGGER = section.get<string>('linter.run') || 'never';
    Utils.FORMATENABLED = section.get<boolean>('format.enabled') || false;
    Utils.DIALECT = dialect || 'swi';
    Utils.RUNTIMEPATH = jsesc(exec);

    // Use platform-aware path construction
    const exPath = PlatformUtils.normalizePath(context.extensionPath);
    Utils.EXPATH = exPath;

    // check if the dialect links have already been done
    const diaFile = PlatformUtils.joinPath(exPath, '.vscode', 'dialect.json');

    let lastDialect = '';
    try {
      if (await PlatformUtils.pathExists(diaFile)) {
        const dialectContent = fs.readFileSync(diaFile, 'utf8');
        lastDialect = JSON.parse(dialectContent).dialect;
      }
    } catch (error) {
      // File doesn't exist or is invalid, continue with setup
      const errorMsg = error instanceof Error ? error.message : String(error);
      console.log('[Platform] Dialect file not found or invalid, proceeding with setup:', errorMsg);
    }

    if (lastDialect === dialect) {
      return;
    }

    // creating links for the right dialect using platform-aware paths
    const symLinks = [
      {
        path: PlatformUtils.joinPath(exPath, 'syntaxes'),
        srcFile: `prolog.${dialect}.tmLanguage.json`,
        targetFile: 'prolog.tmLanguage.json',
      },
      {
        path: PlatformUtils.joinPath(exPath, 'snippets'),
        srcFile: `prolog.${dialect}.json`,
        targetFile: 'prolog.json',
      },
    ];

    await Promise.all(
      symLinks.map(async link => {
        const srcPath = PlatformUtils.joinPath(link.path, link.srcFile);
        const targetPath = PlatformUtils.joinPath(link.path, link.targetFile);

        // remove old link
        try {
          if (await PlatformUtils.pathExists(targetPath)) {
            fs.unlinkSync(targetPath);
          }
        } catch (error) {
          // Ignore errors when removing non-existent files, but log for debugging
          const errorMsg = error instanceof Error ? error.message : String(error);
          console.debug('[Platform] Failed to remove old symlink:', targetPath, errorMsg);
        }

        // make link
        try {
          // Try to create symlink, fallback to copy if symlink fails
          try {
            fs.symlinkSync(srcPath, targetPath);
          } catch (error) {
            // If symlink fails (e.g., on Windows without admin), copy the file
            const errorMsg = error instanceof Error ? error.message : String(error);
            console.debug('[Platform] Symlink failed, falling back to copy:', errorMsg);
            fs.copyFileSync(srcPath, targetPath);
          }
        } catch (err) {
          const platform = getPlatform();
          const errorMsg =
            platform === 'windows'
              ? 'VSC-Prolog failed in initialization. Try running VS Code as administrator or enable Developer Mode in Windows Settings.'
              : 'VSC-Prolog failed in initialization. Check file permissions.';
          console.error('[Extension] Initialization failed:', err);
          throw new Error(errorMsg);
        }
      })
    );

    // Ensure .vscode directory exists
    const vscodeDirPath = PlatformUtils.joinPath(exPath, '.vscode');
    if (!(await PlatformUtils.pathExists(vscodeDirPath))) {
      fs.mkdirSync(vscodeDirPath, { recursive: true });
    }

    // write the dialect to the json for later initialization
    fs.writeFileSync(diaFile, JSON.stringify({ dialect: dialect }));
  }

  // Helper function to create auth configuration from VSCode settings
  createAuthConfig(config: any): AuthConfig {
    const method = config.get('apiServer.auth.method', 'api_key') as string;
    // Ensure method is one of the valid values
    const validMethods = ['api_key', 'jwt_token', 'local_only', 'oauth2'] as const;
    const authMethod = validMethods.includes(method as any)
      ? (method as (typeof validMethods)[number])
      : 'api_key';

    return {
      method: authMethod,
      apiKeys: config.get('apiServer.auth.apiKeys', []) as string[],
      jwtSecret: config.get('apiServer.auth.jwtSecret', '') as string,
      localOnly: config.get('apiServer.auth.localOnly', true) as boolean,
      oauth2: {
        providers: config.get('apiServer.auth.oauth2.providers', ['google', 'github']) as string[],
        clientId: config.get('apiServer.auth.oauth2.clientId', '') as string,
        clientSecret: config.get('apiServer.auth.oauth2.clientSecret', '') as string,
        redirectUri: config.get('apiServer.auth.oauth2.redirectUri', '') as string,
        scope: config.get('apiServer.auth.oauth2.scope', 'read') as string,
      },
      roles: {
        admin: config.get('apiServer.auth.roles.admin', []) as string[],
        agent: config.get('apiServer.auth.roles.agent', []) as string[],
        readonly: config.get('apiServer.auth.roles.readonly', []) as string[],
        limited: config.get('apiServer.auth.roles.limited', []) as string[],
      },
      quotas: {
        admin: {
          requestsPerMinute: config.get(
            'apiServer.auth.quotas.admin.requestsPerMinute',
            1000
          ) as number,
          maxConcurrentSessions: config.get(
            'apiServer.auth.quotas.admin.maxConcurrentSessions',
            50
          ) as number,
        },
        agent: {
          requestsPerMinute: config.get(
            'apiServer.auth.quotas.agent.requestsPerMinute',
            300
          ) as number,
          maxConcurrentSessions: config.get(
            'apiServer.auth.quotas.agent.maxConcurrentSessions',
            20
          ) as number,
        },
        readonly: {
          requestsPerMinute: config.get(
            'apiServer.auth.quotas.readonly.requestsPerMinute',
            100
          ) as number,
          maxConcurrentSessions: config.get(
            'apiServer.auth.quotas.readonly.maxConcurrentSessions',
            10
          ) as number,
        },
        limited: {
          requestsPerMinute: config.get(
            'apiServer.auth.quotas.limited.requestsPerMinute',
            30
          ) as number,
          maxConcurrentSessions: config.get(
            'apiServer.auth.quotas.limited.maxConcurrentSessions',
            5
          ) as number,
        },
      },
    };
  }

  // Get current configuration values
  getConfiguration() {
    const config = workspace.getConfiguration('prolog');
    return {
      dialect: config.get('dialect', 'swi') as string,
      executablePath: config.get('executablePath', 'swipl') as string,
      linterTrigger: config.get('linter.run', 'never') as string,
      formatEnabled: config.get('format.enabled', false) as boolean,
      apiServerEnabled: config.get('apiServer.enabled', false) as boolean,
      webSocketEnabled: config.get('webSocketServer.enabled', true) as boolean,
      telemetryEnabled: config.get('telemetry.enabled', false) as boolean,
    };
  }
}