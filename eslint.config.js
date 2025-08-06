// eslint.config.js
// ESLint flat config for VSCode-Prolog-Toolkit (ESLint v9+)
import js from "@eslint/js";
import tseslint from "@typescript-eslint/eslint-plugin";
import tsparser from "@typescript-eslint/parser";

/** @type {import("eslint").Linter.FlatConfig[]} */
export default [
  // Global ignores - files to exclude from all linting
  {
    ignores: [
      "src/extension.ts",
      "node_modules/**",
      "out/**",
      "dist/**",
      "coverage/**",
      "**/*.d.ts"
    ]
  },
  // Configuration for JavaScript files (no TypeScript parser project needed)
  {
    files: ["**/*.js"],
    languageOptions: {
      sourceType: "module",
      ecmaVersion: "latest",
      globals: {
        // Node.js and ES2021 globals
        process: "readonly",
        console: "readonly",
        setTimeout: "readonly",
        clearTimeout: "readonly",
        setInterval: "readonly",
        clearInterval: "readonly",
        Buffer: "readonly",
        __dirname: "readonly",
        require: "readonly",
        module: "readonly",
        exports: "readonly",
        global: "readonly",
        URL: "readonly",
        fetch: "readonly"
      }
    },
    rules: {
      ...js.configs.recommended.rules,
      // Basic rules for JavaScript files
      "no-unused-vars": ["warn", { "argsIgnorePattern": "^_" }],
      "no-redeclare": "error"
    },
  },
  // Configuration for TypeScript files (with TypeScript parser and project)
  {
    files: ["**/*.{ts,tsx}"],
    languageOptions: {
      parser: tsparser,
      parserOptions: {
        project: "./tsconfig.json",
        sourceType: "module",
        ecmaVersion: "latest"
      },
      globals: {
        // Node.js and ES2021 globals
        process: "readonly",
        console: "readonly",
        setTimeout: "readonly",
        clearTimeout: "readonly",
        setInterval: "readonly",
        clearInterval: "readonly",
        Buffer: "readonly",
        __dirname: "readonly",
        require: "readonly",
        module: "readonly",
        exports: "readonly",
        global: "readonly",
        URL: "readonly",
        fetch: "readonly"
      }
    },
    plugins: {
      "@typescript-eslint": tseslint,
    },
    rules: {
      ...js.configs.recommended.rules,
      ...tseslint.configs.recommended.rules,
      // TypeScript-specific rules
      "@typescript-eslint/no-unused-vars": ["warn", { "argsIgnorePattern": "^_" }],
      "@typescript-eslint/explicit-module-boundary-types": "off",
      "@typescript-eslint/no-explicit-any": "off",
      // Phase 2: Enable no-redeclare rule
      "no-redeclare": "error",
      "@typescript-eslint/no-redeclare": "error"
    },
  },
];
